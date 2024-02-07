-module(mcluster_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

% @doc start maria root supervisor
-spec start_link() -> Result when
    Result :: 'ignore' | {'error',_} | {'ok',pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% @doc init callbacks
-spec init([]) -> Result when
    Result :: {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

init([]) ->
    RestartStrategy = {one_for_one, 4, 3600},

    % garbace collector eventer (in future we need to remove it)
    ClusterDiscrovery = {
        cluster_discrovery,
        {cluster_discrovery, start_link, []},
        permanent,
        5000,
        worker,
        [cluster_discrovery]
    },

    Childrens = [ClusterDiscrovery],
    {ok, {RestartStrategy, Childrens}}.
