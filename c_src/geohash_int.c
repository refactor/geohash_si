#include "erl_nif.h"
#include <stdio.h>
#include <inttypes.h>

#define MAX_LEVEL 30

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_ERR_TOOBIG_LEVEL;
static ERL_NIF_TERM ATOM_ERR_UNSUPPORTED_MODE;

static ERL_NIF_TERM ATOM_WEST;
static ERL_NIF_TERM ATOM_EAST;
static ERL_NIF_TERM ATOM_SOUTH;
static ERL_NIF_TERM ATOM_NORTH;
static ERL_NIF_TERM ATOM_MODE;

static ERL_NIF_TERM ATOM_NORTHWEST;
static ERL_NIF_TERM ATOM_NORTHEAST;
static ERL_NIF_TERM ATOM_SOUTHEAST;
static ERL_NIF_TERM ATOM_SOUTHWEST;

static ERL_NIF_TERM ATOM_BITS;
static ERL_NIF_TERM ATOM_LEVEL;

static ERL_NIF_TERM ATOM_XMIN;
static ERL_NIF_TERM ATOM_XMAX;
static ERL_NIF_TERM ATOM_YMIN;
static ERL_NIF_TERM ATOM_YMAX;

static ErlNifResourceType* geohash_int_RESOURCE = NULL;

typedef struct
{
} geohash_int_handle;

// Prototypes
static ERL_NIF_TERM geohash_int_fast_encode(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static ERL_NIF_TERM geohash_int_encode(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static ERL_NIF_TERM geohash_int_fast_decode(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static ERL_NIF_TERM geohash_int_get_neighbors(ErlNifEnv*, int, const ERL_NIF_TERM[]);

static ErlNifFunc nif_funcs[] =
{
    {"encode", 4, geohash_int_encode},
    {"fast_encode", 4, geohash_int_fast_encode},
    {"fast_decode", 2, geohash_int_fast_decode},
    {"get_neighbors", 2, geohash_int_get_neighbors},
};

static inline uint64_t interleave64(uint32_t xlo, uint32_t ylo)
{
    static const uint64_t B[] =
        { 0x5555555555555555, 0x3333333333333333, 0x0F0F0F0F0F0F0F0F, 0x00FF00FF00FF00FF, 0x0000FFFF0000FFFF };
    static const unsigned int S[] =
        { 1, 2, 4, 8, 16 };

    uint64_t x = xlo; // Interleave lower  bits of x and y, so the bits of x
    uint64_t y = ylo; // are in the even positions and bits from y in the odd; //https://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN

    // x and y must initially be less than 2**32.

    x = (x | (x << S[4])) & B[4];
    y = (y | (y << S[4])) & B[4];

    x = (x | (x << S[3])) & B[3];
    y = (y | (y << S[3])) & B[3];

    x = (x | (x << S[2])) & B[2];
    y = (y | (y << S[2])) & B[2];

    x = (x | (x << S[1])) & B[1];
    y = (y | (y << S[1])) & B[1];

    x = (x | (x << S[0])) & B[0];
    y = (y | (y << S[0])) & B[0];

    return x | (y << 1);
}

static inline uint64_t deinterleave64(uint64_t interleaved)
{
    static const uint64_t B[] =
        { 0x5555555555555555, 0x3333333333333333, 0x0F0F0F0F0F0F0F0F, 0x00FF00FF00FF00FF, 0x0000FFFF0000FFFF, 0x00000000FFFFFFFF };
    static const unsigned int S[] =
        { 0, 1, 2, 4, 8, 16 };

    uint64_t x = interleaved; ///reverse the interleave process (http://stackoverflow.com/questions/4909263/how-to-efficiently-de-interleave-bits-inverse-morton)
    uint64_t y = interleaved >> 1;

    x = (x | (x >> S[0])) & B[0];
    y = (y | (y >> S[0])) & B[0];

    x = (x | (x >> S[1])) & B[1];
    y = (y | (y >> S[1])) & B[1];

    x = (x | (x >> S[2])) & B[2];
    y = (y | (y >> S[2])) & B[2];

    x = (x | (x >> S[3])) & B[3];
    y = (y | (y >> S[3])) & B[3];

    x = (x | (x >> S[4])) & B[4];
    y = (y | (y >> S[4])) & B[4];

    x = (x | (x >> S[5])) & B[5];
    y = (y | (y >> S[5])) & B[5];

    return x | (y << 32);
}

static ERL_NIF_TERM geohash_int_fast_decode(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[])
{
    if (argc != 2 ||
            !enif_is_map(env, argv[0]) ||
            !enif_is_map(env, argv[1]))
        return enif_make_badarg(env);

    double w, e, n, s;
    char smode[2];
    ERL_NIF_TERM wt, et, nt, st, mt;
    if (!enif_get_map_value(env, argv[0], ATOM_WEST, &wt) ||
        !enif_get_map_value(env, argv[0], ATOM_EAST, &et) ||
        !enif_get_map_value(env, argv[0], ATOM_NORTH, &nt) ||
        !enif_get_map_value(env, argv[0], ATOM_SOUTH, &st) ||
        !enif_get_map_value(env, argv[0], ATOM_MODE, &mt) ||
        !enif_get_double(env, wt, &w) ||
        !enif_get_double(env, et, &e) ||
        !enif_get_double(env, nt, &n) ||
        !enif_get_double(env, st, &s) ||
        !enif_get_atom(env, mt, smode, 2, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    unsigned int level;
    ErlNifUInt64 ebits;
    ERL_NIF_TERM bitst, levelt;
    if (!enif_get_map_value(env, argv[1], ATOM_BITS, &bitst) ||
        !enif_get_map_value(env, argv[1], ATOM_LEVEL, &levelt) ||
        !enif_get_uint64(env, bitst, &ebits) ||
        !enif_get_uint(env, levelt, &level))
        return enif_make_badarg(env);

    uint64_t xyhilo = deinterleave64(ebits);
    double lat_scale = n - s;
    double lon_scale = e - w;
    uint32_t ilato;        //get back the original integer coordinates
    uint32_t ilono;
    if (smode[0] == 'N') {
        ilato = xyhilo;
        ilono = xyhilo >> 32;
    } else if (smode[0] == 'Z') {
        ilato = xyhilo >> 32;
        ilono = xyhilo;
    } else {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ERR_UNSUPPORTED_MODE);
    }

    double ymin = s + (ilato * 1.0 / (1ull << level)) * lat_scale;
    double ymax = s + ((ilato + 1) * 1.0 / (1ull << level)) * lat_scale;
    double xmin = w + (ilono * 1.0 / (1ull << level)) * lon_scale;
    double xmax = w + ((ilono + 1) * 1.0 / (1ull << level)) * lon_scale;

    ERL_NIF_TERM m = enif_make_new_map(env);
    ERL_NIF_TERM m1, m2, m3, m4;
    enif_make_map_put(env, m, ATOM_XMIN, enif_make_double(env, xmin), &m1);
    enif_make_map_put(env, m1, ATOM_XMAX, enif_make_double(env, xmax), &m2);
    enif_make_map_put(env, m2, ATOM_YMIN, enif_make_double(env, ymin), &m3);
    enif_make_map_put(env, m3, ATOM_YMAX, enif_make_double(env, ymax), &m4);
    return enif_make_tuple2(env, ATOM_OK, m4);
}

static ERL_NIF_TERM geohash_int_fast_encode(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[])
{
    double latitude, longitude;
    unsigned int level;
    if (argc != 4 ||
            !enif_is_map(env, argv[0]) ||
            !enif_get_double(env, argv[1], &latitude) ||
            !enif_get_double(env, argv[2], &longitude) ||
            !enif_get_uint(env, argv[3], &level)) 
        return enif_make_badarg(env);

    if (level > MAX_LEVEL) 
        return enif_make_tuple2(env,
                ATOM_ERROR,
                enif_make_tuple2(env,
                    ATOM_ERR_TOOBIG_LEVEL,
                    argv[3]));

    double w, e, n, s;
    char smode[2];
    ERL_NIF_TERM wt, et, nt, st, mt;
    if (!enif_get_map_value(env, argv[0], ATOM_WEST, &wt) ||
        !enif_get_map_value(env, argv[0], ATOM_EAST, &et) ||
        !enif_get_map_value(env, argv[0], ATOM_NORTH, &nt) ||
        !enif_get_map_value(env, argv[0], ATOM_SOUTH, &st) ||
        !enif_get_map_value(env, argv[0], ATOM_MODE, &mt) ||
        !enif_get_double(env, wt, &w) ||
        !enif_get_double(env, et, &e) ||
        !enif_get_double(env, nt, &n) ||
        !enif_get_double(env, st, &s) ||
        !enif_get_atom(env, mt, smode, 2, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    // The algorithm computes the morton code for the geohash location within
    // the range this can be done MUCH more efficiently using the following code

    //compute the coordinate in the range 0-1
    long double lat_offset = (latitude - s) / (n - s);
    long double lon_offset = (longitude - w) / (e - w);

    //convert it to fixed point based on the step size
    lat_offset *= (1 << level);
    lon_offset *= (1 << level);

    uint32_t ilato = (uint32_t) lat_offset;
    uint32_t ilono = (uint32_t) lon_offset;

    //interleave the bits to create the morton code.  No branching and no bounding
    uint64_t bits = 0;
    if (smode[0] == 'N') {
        bits = interleave64(ilato, ilono);
    }
    else if (smode[0] == 'Z') {
        bits = interleave64(ilono, ilato);
    }
    else {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_ERR_UNSUPPORTED_MODE);
    }

    ERL_NIF_TERM m = enif_make_new_map(env);
    ERL_NIF_TERM m1, m2;
    enif_make_map_put(env, m, ATOM_LEVEL, enif_make_uint(env, level), &m1);
    ErlNifUInt64 ebits = bits;
    enif_make_map_put(env, m1, ATOM_BITS, enif_make_uint64(env, ebits), &m2);
    return enif_make_tuple2(env, ATOM_OK, m2);
}

static void encodebit(uint64_t* const pbits, double* const min, double* const max, const double position) {
    *pbits <<= 1;
    if (*max - position >= position - *min) {
        *max = (*max + *min) / 2;
    }
    else {
        *min = (*max + *min) / 2;
        (*pbits) += 1;
    }
}

static ERL_NIF_TERM geohash_int_encode(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[])
{
    double latitude, longitude;
    unsigned int level;
    if (argc != 4 ||
            !enif_is_map(env, argv[0]) ||
            !enif_get_double(env, argv[1], &latitude) ||
            !enif_get_double(env, argv[2], &longitude) ||
            !enif_get_uint(env, argv[3], &level)) 
        return enif_make_badarg(env);

    if (level > MAX_LEVEL) 
        return enif_make_tuple2(env,
                ATOM_ERROR,
                enif_make_tuple2(env,
                    ATOM_ERR_TOOBIG_LEVEL,
                    argv[3]));

    double w, e, n, s;
    char smode[2];
    ERL_NIF_TERM wt, et, nt, st, mt;
    if (!enif_get_map_value(env, argv[0], ATOM_WEST, &wt) ||
        !enif_get_map_value(env, argv[0], ATOM_EAST, &et) ||
        !enif_get_map_value(env, argv[0], ATOM_NORTH, &nt) ||
        !enif_get_map_value(env, argv[0], ATOM_SOUTH, &st) ||
        !enif_get_map_value(env, argv[0], ATOM_MODE, &mt) ||
        !enif_get_double(env, wt, &w) ||
        !enif_get_double(env, et, &e) ||
        !enif_get_double(env, nt, &n) ||
        !enif_get_double(env, st, &s) ||
        !enif_get_atom(env, mt, smode, 2, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    uint64_t bits = 0;
    for(unsigned int i=0; i<level; ++i) {
        if (smode[0] == 'N') {
            encodebit(&bits, &w, &e, longitude);
            encodebit(&bits, &s, &n, latitude);
        }
        else if (smode[0] == 'Z') {
            encodebit(&bits, &s, &n, latitude);
            encodebit(&bits, &w, &e, longitude);
        }
        else {
            return enif_make_tuple2(env,
                    ATOM_ERROR,
                    enif_make_tuple2(env, ATOM_ERR_UNSUPPORTED_MODE, mt));
        }
    }
    ERL_NIF_TERM m = enif_make_new_map(env);
    ERL_NIF_TERM m1, m2;
    enif_make_map_put(env, m, ATOM_LEVEL, enif_make_uint(env, level), &m1);
    ErlNifUInt64 ebits = bits;
    enif_make_map_put(env, m1, ATOM_BITS, enif_make_uint64(env, ebits), &m2);
    return enif_make_tuple2(env, ATOM_OK, m2);
}

static void geohash_move_x(const unsigned int level, uint64_t* bits, const int8_t orientation)
{
    if (orientation == 0)
        return;
    uint64_t x = (*bits) & 0xaaaaaaaaaaaaaaaaLL;
    uint64_t y = (*bits) & 0x5555555555555555LL;

    uint64_t zz = 0x5555555555555555LL >> (64 - level * 2);
    if (orientation > 0) {
        x = x + (zz + 1);
    } else {
        x = x | zz;
        x = x - (zz + 1);
    }
    x &= (0xaaaaaaaaaaaaaaaaLL >> (64 - level * 2));
    *bits = (x | y);
}

static void geohash_move_y(const unsigned int level, uint64_t* bits, const int8_t orientation)
{
    if (orientation == 0)
        return;
    uint64_t x = (*bits) & 0xaaaaaaaaaaaaaaaaLL;
    uint64_t y = (*bits) & 0x5555555555555555LL;

    uint64_t zz = 0xaaaaaaaaaaaaaaaaLL >> (64 - level * 2);
    if (orientation > 0) {
        y = y + (zz + 1);
    } else {
        y = y | zz;
        y = y - (zz + 1);
    }
    y &= (0x5555555555555555LL >> (64 - level * 2));
    *bits = (x | y);
}

static ERL_NIF_TERM geohash_int_get_neighbors(ErlNifEnv* env, int argc,
                                              const ERL_NIF_TERM argv[])
{
    if (argc != 2 ||
            !enif_is_map(env, argv[0]) ||
            !enif_is_map(env, argv[1]))
        return enif_make_badarg(env);

    double w, e, n, s;
    char smode[2];
    ERL_NIF_TERM wt, et, nt, st, mt;
    if (!enif_get_map_value(env, argv[0], ATOM_WEST, &wt) ||
        !enif_get_map_value(env, argv[0], ATOM_EAST, &et) ||
        !enif_get_map_value(env, argv[0], ATOM_NORTH, &nt) ||
        !enif_get_map_value(env, argv[0], ATOM_SOUTH, &st) ||
        !enif_get_map_value(env, argv[0], ATOM_MODE, &mt) ||
        !enif_get_double(env, wt, &w) ||
        !enif_get_double(env, et, &e) ||
        !enif_get_double(env, nt, &n) ||
        !enif_get_double(env, st, &s) ||
        !enif_get_atom(env, mt, smode, 2, ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    unsigned int level;
    ErlNifUInt64 tmpbits;
    ERL_NIF_TERM bitst, levelt;
    if (!enif_get_map_value(env, argv[1], ATOM_BITS, &bitst) ||
        !enif_get_map_value(env, argv[1], ATOM_LEVEL, &levelt) ||
        !enif_get_uint64(env, bitst, &tmpbits) ||
        !enif_get_uint(env, levelt, &level))
        return enif_make_badarg(env);
    const uint64_t BITS = tmpbits;

    ERL_NIF_TERM m = enif_make_new_map(env);
    ERL_NIF_TERM ml, m0;
    enif_make_map_put(env, m, ATOM_LEVEL, enif_make_uint(env, level), &ml);
    enif_make_map_put(env, ml, ATOM_BITS, enif_make_uint64(env, tmpbits), &m0);

    ERL_NIF_TERM m1, m2, m3, m4, m5, m6, m7, m8;
    uint64_t bits;

    // north
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, 0);
        geohash_move_y(level, &bits, 1);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, 1);
        geohash_move_y(level, &bits, 0);
    }
    tmpbits = bits;
    enif_make_map_put(env, m0, ATOM_NORTH, enif_make_uint64(env, tmpbits), &m1);

    // east
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, 1);
        geohash_move_y(level, &bits, 0);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, 0);
        geohash_move_y(level, &bits, 1);
    }
    tmpbits = bits;
    enif_make_map_put(env, m1, ATOM_EAST, enif_make_uint64(env, tmpbits), &m2);

    // west
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, -1);
        geohash_move_y(level, &bits, 0);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, 0);
        geohash_move_y(level, &bits, -1);
    }
    tmpbits = bits;
    enif_make_map_put(env, m2, ATOM_WEST, enif_make_uint64(env, tmpbits), &m3);

    // south
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, 0);
        geohash_move_y(level, &bits, -1);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, -1);
        geohash_move_y(level, &bits, 0);
    }
    tmpbits = bits;
    enif_make_map_put(env, m3, ATOM_SOUTH, enif_make_uint64(env, tmpbits), &m4);

    // south-east
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, 1);
        geohash_move_y(level, &bits, -1);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, -1);
        geohash_move_y(level, &bits, 1);
    }
    tmpbits = bits;
    enif_make_map_put(env, m4, ATOM_SOUTHEAST, enif_make_uint64(env, tmpbits), &m5);

    // north-west
    bits = BITS;
    if (smode[0] == 'N') {
        geohash_move_x(level, &bits, -1);
        geohash_move_y(level, &bits, 1);
    } else if (smode[0] == 'Z') {
        geohash_move_x(level, &bits, 1);
        geohash_move_y(level, &bits, -1);
    }
    tmpbits = bits;
    enif_make_map_put(env, m5, ATOM_NORTHWEST, enif_make_uint64(env, tmpbits), &m6);

    // south-west
    bits = BITS;
    geohash_move_x(level, &bits, -1);
    geohash_move_y(level, &bits, -1);
    tmpbits = bits;
    enif_make_map_put(env, m6, ATOM_SOUTHWEST, enif_make_uint64(env, tmpbits), &m7);

    // north-east
    bits = BITS;
    geohash_move_x(level, &bits, 1);
    geohash_move_y(level, &bits, 1);
    tmpbits = bits;
    enif_make_map_put(env, m7, ATOM_NORTHEAST, enif_make_uint64(env, tmpbits), &m8);

    return enif_make_tuple2(env, ATOM_OK, m8);
}

static void geohash_int_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in geohash_int_handle */
    /* geohash_int_handle* handle = (geohash_int_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    ATOM_ERR_TOOBIG_LEVEL = enif_make_atom(env, "too_big_level");
    ATOM_ERR_UNSUPPORTED_MODE = enif_make_atom(env, "unsupported_world_mode");

    ATOM_EAST = enif_make_atom(env, "east");
    ATOM_WEST = enif_make_atom(env, "west");
    ATOM_NORTH = enif_make_atom(env, "north");
    ATOM_SOUTH = enif_make_atom(env, "south");
    ATOM_MODE = enif_make_atom(env, "mode");

    ATOM_BITS = enif_make_atom(env, "bits");
    ATOM_LEVEL = enif_make_atom(env, "level");

    ATOM_XMIN = enif_make_atom(env, "xmin");
    ATOM_XMAX = enif_make_atom(env, "xmax");
    ATOM_YMIN = enif_make_atom(env, "ymin");
    ATOM_YMAX = enif_make_atom(env, "ymax");

    ATOM_NORTHEAST = enif_make_atom(env, "northeast");
    ATOM_NORTHWEST = enif_make_atom(env, "northwest");
    ATOM_SOUTHEAST = enif_make_atom(env, "southeast");
    ATOM_SOUTHWEST = enif_make_atom(env, "southwest");

    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "geohash_int_resource",
                                                     &geohash_int_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    geohash_int_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(geohash_int, nif_funcs, &on_load, NULL, NULL, NULL);
