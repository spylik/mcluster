PROJECT = mcluster

# our
dep_teaser = git https://github.com/spylik/teaser

# 3rd party
dep_sync = git https://github.com/rustyio/sync
dep_lager = git https://github.com/basho/lager
dep_mnesia_eleveldb = git https://github.com/klarna/mnesia_eleveldb

DEPS = teaser lager mnesia_eleveldb

SHELL_DEPS = sync

SHELL_OPTS = +c true +C multi_time_warp -pa ebin/ test/ -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile -sname mcluster


include erlang.mk
