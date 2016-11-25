PROJECT = mcluster

# global compile options
ERLC_GLOBAL_OPTS = +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_redefining_field
ERLC_GLOBAL_OPTS += +'{parse_transform, lager_transform}'

# default mode
ERLC_OPTS = $(ERLC_GLOBAL_OPTS)
ERLC_OPTS += +warn_missing_spec -Werror

# tests mode
TEST_ERLC_OPTS += $(ERLC_GLOBAL_OPTS)
TEST_ERLC_OPTS += +debug_info

# our
dep_teaser = git https://github.com/spylik/teaser develop

# 3rd party
dep_sync = git https://github.com/rustyio/sync 9c78e7b
dep_lager = git https://github.com/basho/lager
dep_mnesia_eleveldb = git https://github.com/klarna/mnesia_eleveldb

DEPS = teaser lager mnesia_eleveldb

SHELL_DEPS = sync

SHELL_OPTS = +c true +C multi_time_warp -args_file vm.args -config sys.config -pa ebin/ test/ -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile -sname mcluster


include erlang.mk
