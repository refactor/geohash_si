#include "erl_nif.h"
#include <stdio.h>

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
static ERL_NIF_TERM geohash_int_encode(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"encode", 4, geohash_int_encode}
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


static void encodebit(ErlNifUInt64* const pbits, double* const min, double* const max, const double position) {
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

    if (level > 36) 
        return enif_make_tuple2(env,
                ATOM_ERROR,
                enif_make_tuple2(env,
                    ATOM_ERR_TOOBIG_LEVEL,
                    argv[3]));

    double w, e, n, s;
    char smode[2];
    ERL_NIF_TERM wt, et, nt, st, mt;
    ERL_NIF_TERM west, east, north, south, mode;
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

    ErlNifUInt64 bits = 0;
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
    enif_make_map_put(env, m1, ATOM_BITS, enif_make_uint64(env, bits), &m2);
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
