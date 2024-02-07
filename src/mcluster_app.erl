-module(mcluster_app).

% Application is here
-behaviour(application).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start/2]).
-export([stop/1]).

% @doc start application
-spec start(Type, Args) -> Result when
    Type :: application:start_type(),
    Args :: term(),
    Result :: {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.

start(normal, _StartArgs) ->
    ?LOG_INFO("here we are"),
    Result = mcluster_sup:start_link(),
    _ = mnesia_cluster:init(),
    Result.

% @doc stop application
-spec stop(State) -> Result when
    State :: term(),
    Result :: ok.

stop(_State) ->
    ok.

