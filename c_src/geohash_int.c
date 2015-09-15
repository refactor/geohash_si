#include "erl_nif.h"

static ErlNifResourceType* geohash_int_RESOURCE = NULL;

typedef struct
{
} geohash_int_handle;

// Prototypes
static ERL_NIF_TERM geohash_int_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM geohash_int_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, geohash_int_new},
    {"myfunction", 1, geohash_int_myfunction}
};

static ERL_NIF_TERM geohash_int_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    geohash_int_handle* handle = enif_alloc_resource(geohash_int_RESOURCE,
                                                    sizeof(geohash_int_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM geohash_int_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void geohash_int_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in geohash_int_handle */
    /* geohash_int_handle* handle = (geohash_int_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
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
