PROJECT = geohash_si
PROJECT_DESCRIPTION = Small, fast, modular geospatial index
PROJECT_VERSION = 0.0.2

ERLC_OPTS += +'nowarn_unused_vars'

TEST_DEPS = proper eunit_formatters
EUNIT_OPTS = verbose

SHELL_DEPS = kjell recon
SHELL_ERL = $(DEPS_DIR)/kjell/bin/kjell
dep_recon_commit = 2.5.0

CFLAGS += -DNIF_DEBUG

include erlang.mk
