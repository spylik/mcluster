PROJECT = mcluster

# -------------------------------------------------------------------
# Speedup fetching

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

# --------------------------------------------------------------------
# RocksDB config
# --------------------------------------------------------------------

#https://gitlab.com/barrel-db/erlang-rocksdb/blob/master/doc/customize_rocksdb_build.md
ERLANG_ROCKSDB_OPTS: "-DWITH_BUNDLE_LZ4=ON -DWITH_BUNDLE_SNAPPY=ON"
#ERLANG_ROCKSDB_OPTS: "-DWITH_SYSTEM_ROCKSDB=FALSE -DWITH_LZ4=ON -DWITH_SNAPPY=ON -DWITH_BZ2=ON -DWITH_ZSTD=ON"
#export ERLANG_ROCKSDB_OPTS

# --------------------------------------------------------------------
# Support being sub-project
# --------------------------------------------------------------------

ifeq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
    DEPS_DIR ?= $(shell dirname $(CURDIR))
endif

# --------------------------------------------------------------------
# Defining OTP version for this project which uses by kerl
# --------------------------------------------------------------------
ifneq ($(shell basename $(shell dirname $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST)))))), deps)
ERLANG_OTP = OTP-$(shell cat ./.env | grep ERLANG_VERSION | sed -e s/^ERLANG_VERSION=//)
endif


# --------------------------------------------------------------------
# Compilation.
# --------------------------------------------------------------------

# if ERLC_OPTS not defined in parent project, we going to define by our-self
ERLC_OPTS ?= +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record +warn_missing_spec +warn_missing_spec_all -Werror

# default mode
ERLC_OPTS = $(ERLC_GLOBAL_OPTS)
ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record
ERLC_OPTS += +warn_missing_spec_all
ERLC_OPTS += -Werror
ERLC_OPTS += +debug_info

# tests mode
TEST_ERLC_OPTS += $(ERLC_GLOBAL_OPTS)

dep_teaser = git https://github.com/spylik/teaser develop

# 3rd party
dep_mnesia_rocksdb = git https://github.com/aeternity/mnesia_rocksdb 	master
dep_rocksdb = git https://gitlab.com/seanhinde/erlang-rocksdb.git  		9ae37839
dep_hut = git https://github.com/tolbrino/hut                      		master

dep_sync = git https://github.com/rustyio/sync

DEPS = hut rocksdb mnesia_rocksdb

PLT_APPS = mnesia crypto

SHELL_DEPS = sync teaser

# --------------------------------------------------------------------
# Development enviroment ("make shell" to run it).
# --------------------------------------------------------------------

SHELL_OPTS = +c true +C multi_time_warp -pa ebin/ test/ -eval 'mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile -sname mcluster

# --------------------------------------------------------------------
# We using erlang.mk
# --------------------------------------------------------------------

include erlang.mk

# --------------------------------------------------------------------
# Probably patch leveldb if we use some old version of XCode
# --------------------------------------------------------------------

autopatch-rocksdb::
	if [ -d $(DEPS_DIR)/rocksdb ]; then \
        echo "Firing rocksdb hook" ; \
        cd $(DEPS_DIR)/rocksdb ; \
        ./do_cmake.sh $(ERLANG_ROCKSDB_OPTS) ; \
        ./do_rocksdb.sh $(ERLANG_ROCKSDB_BUILDOPTS) ; \
        cd - ; \
    fi

autopatch-hut::
	if [ -d $(DEPS_DIR)/hut ]; then \
        echo "Firing hut hook" ; \
        rm -rf $(DEPS_DIR)/hut/Makefile ; \
        $(call dep_autopatch2,hut) ; \
    fi

