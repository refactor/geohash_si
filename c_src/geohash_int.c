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

static ERL_NIF_TERM ATOM_BITS;
static ERL_NIF_TERM ATOM_LEVEL;

static ErlNifResourceType* geohash_int_RESOURCE = NULL;

typedef struct
{
} geohash_int_handle;

// Prototypes
static ERL_NIF_TERM geohash_int_define_world(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM geohash_int_fast_encode(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM geohash_int_encode(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"encode", 4, geohash_int_encode},
    {"fast_encode", 4, geohash_int_fast_encode}
};

static ERL_NIF_TERM geohash_int_define_world(ErlNifEnv* env, int argc,
                                             const ERL_NIF_TERM argv[])
{
    geohash_int_handle* handle = enif_alloc_resource(geohash_int_RESOURCE,
                                                    sizeof(geohash_int_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


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
