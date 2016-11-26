PROJECT = mcluster

# --------------------------------------------------------------------
# Compilation.
# --------------------------------------------------------------------

# global compile options
ERLC_GLOBAL_OPTS = +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_redefining_field
ERLC_GLOBAL_OPTS += +'{parse_transform, lager_transform}'

# default compile mode
ERLC_OPTS = $(ERLC_GLOBAL_OPTS)
ERLC_OPTS += +warn_missing_spec -Werror

# if MODE is not defined it means we are in development enviroment
ifeq ($(MODE),release)
ERLC_OPTS += +native
ERLC_OPTS += +'{hipe, [o3]}'
else
ERLC_OPTS += +debug_info
endif

# tests mode
TEST_ERLC_OPTS += $(ERLC_GLOBAL_OPTS)
TEST_ERLC_OPTS += +debug_info

# --------------------------------------------------------------------
# Dependencies.
# --------------------------------------------------------------------

# if we part of deps directory, we using $(CURDIR)../ as DEPS_DIR
ifeq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
	DEPS_DIR ?= $(shell dirname $(CURDIR))
endif

dep_teaser = git https://github.com/spylik/teaser

# 3rd party
dep_sync = git https://github.com/rustyio/sync
dep_lager = git https://github.com/basho/lager
dep_mnesia_eleveldb = git https://github.com/klarna/mnesia_eleveldb

DEPS = teaser lager mnesia_eleveldb

SHELL_DEPS = sync

SHELL_OPTS = +c true +C multi_time_warp -pa ebin/ test/ -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile -sname mcluster

include erlang.mk
