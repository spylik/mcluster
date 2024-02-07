%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%% @copyright 2024 Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc
%% This source code is part of project "Maria".
%%
%% @end
%% --------------------------------------------------------------------------------

-module(cluster_discrovery).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-include_lib("kernel/include/logger.hrl").

% gen_server is here
-behaviour(gen_server).

-record(cluster_state, {
    auto_connect_statuses = #{}      :: #{node() => node_state()},
    clients_statuses = #{}          :: #{node() => node_state()}
}).

-define(RECONNECT_FROM, 1000).
-define(RECONNECT_TO, 2500).
-define(UNIFORM_RANGE, ?RECONNECT_TO - ?RECONNECT_FROM).

-record(up, {
    timestamp   :: pos_integer()
}).

-record(down, {
    down_from       :: pos_integer(),
    last_attempt    :: pos_integer(),
    attempts        :: pos_integer(),
    reconnect_ref   :: reference()
}).

-type node_state() :: #up{} | #down{}.

-type cluster_state() :: #cluster_state{}.

-type known_messages() :: term().

% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

% @doc interface for start server
-spec start_link() -> Result when
    Result      :: {'ok', Pid},
    Pid         :: pid().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ============================ gen_server part =================================

% @doc gen_server init.
% We going subscribe to right channels and create ets table
-spec init([]) -> Result when
    Result      :: {'ok', cluster_state()}.

init([]) ->
    case net_kernel:monitor_nodes(true) of
        ok ->
            erlang:send(self(), connect_all),
            Time = mcluster_utils:timestamp(),
            {
                ok,
                #cluster_state{
                    clients_statuses = lists:foldl(
                        fun(Node, Acc) -> maps:put(Node, #up{timestamp = Time}, Acc) end,
                        #{},
                        nodes()
                    )
                }
            };
        {error, Reason} ->
            {stop, Reason}
    end.

% ============= handle_call =================

% @doc callbacks for gen_server handle_call.
-spec handle_call(Message, From, State) -> Result when
    Message     :: known_messages(),
    From        :: {pid(), Tag},
    Tag         :: term(),
    State       :: cluster_state(),
    Result      :: {reply, term(), State}.

% handle_call for all other thigs
handle_call(Msg, _From, State) ->
    ?LOG_WARNING("got unknown call ~p", [Msg]),
    {reply, ok, State}.

% ---------- end of handle_call -------------


% ============= handle_cast =================

-spec handle_cast(Message, State) -> Result when
    Message :: known_messages(),
    State   :: cluster_state(),
    Result  :: {noreply, State} | {stop, normal, State}.


% handle_cast for stop
handle_cast(stop, State) ->
    {stop, normal, State};

% handle_cast for all other thigs
handle_cast(Msg, State) ->
    ?LOG_WARNING("got unknown cast message ~p", [Msg]),
    {noreply, State}.

%----------- end of handle_cast -------------

% ============= handle_info =================

% @doc callbacks for gen_server handle_info.
-spec handle_info(Message, State) -> Result when
    Message :: known_messages(),
    State   :: cluster_state(),
    Result  :: {noreply, State}.

handle_info({nodedown, Node}, #cluster_state{auto_connect_statuses = AutoConnectStatuses, clients_statuses = ClientsStatuses} = State) ->
    ?LOG_WARNING("mcluster node ~p DOWN", [Node]),
    MaybeUpdatedState = case maps:get(Node, AutoConnectStatuses, undefined) of
        #up{} ->
			NextAttemptRef = schedule_reconnect(Node),
			Time = mcluster_utils:timestamp(),
			State#cluster_state{
				auto_connect_statuses = maps:put(
					Node,
					#down{down_from = Time, last_attempt = Time, attempts = 1, reconnect_ref = NextAttemptRef},
                    AutoConnectStatuses
				)
			};
        _UndefinedOrDown ->
            State
    end,

    {noreply,
        MaybeUpdatedState#cluster_state{
            clients_statuses = maps:remove(Node, ClientsStatuses)
        }
    };

handle_info({nodeup, Node}, #cluster_state{auto_connect_statuses = AutoConnectStatuses, clients_statuses = ClientsStatuses} = State) ->
    ?LOG_WARNING("mcluster node ~p UP", [Node]),
	Up = #up{timestamp = mcluster_utils:timestamp()},
	MaybeUpdatedState = case maps:get(Node, AutoConnectStatuses, undefined) of
		undefined ->
			State#cluster_state{clients_statuses = maps:put(Node, Up, ClientsStatuses)};
		#up{} ->
			State;
		#down{} = Down ->
			may_cancel_timer(Down, Node),
			State#cluster_state{
				auto_connect_statuses = maps:put(Node, Up, AutoConnectStatuses)
			}
	end,
    {noreply, MaybeUpdatedState};

handle_info({try_reconnect, Node}, State) ->
	{noreply, may_connect_node(Node, State)};

handle_info({cancel_timer, _Ref, _IsExpired}, State) ->
    {noreply, State};

handle_info(connect_all, State) ->
    NodesToConnect = mcluster_utils:nodes_to_auto_connect(),
    ConnectedNodes = nodes(),
    Time = mcluster_utils:timestamp(),
    MaybeUpdatedState =
        lists:foldl(fun
            (Node, StateAcc) ->
				may_connect_node(Node, StateAcc, ConnectedNodes, Time)
            end,
            State, NodesToConnect
        ),

    {noreply, MaybeUpdatedState};

% handle_info for all other thigs
handle_info(Msg, State) ->
    ?LOG_WARNING("got unknown message ~p", [Msg]),
    {noreply, State}.

% ---------- end of handle_info -------------


% @doc call back for gen_server terminate
-spec terminate(Reason, State) -> term() when
    Reason      :: 'normal' | 'shutdown' | {'shutdown',term()} | term(),
    State       :: cluster_state().

terminate(Reason, State) ->
    {noreply, Reason, State}.

% @doc call back for gen_server code_change
-spec code_change(OldVsn, State, Extra) -> Result when
    OldVsn      :: Vsn | {down, Vsn},
    Vsn         :: term(),
    State       :: cluster_state(),
    Extra       :: term(),
    Result      :: {ok, NewState},
    NewState    :: cluster_state().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --------------------------- end of gen_server part ---------------------------

-spec may_cancel_timer(NodeStateOrEmpy, Node) -> Result when
	NodeStateOrEmpy	:: node_state() | undefined,
	Node			:: node(),
	Result			:: boolean().

may_cancel_timer(undefined, _Node) -> false;
may_cancel_timer(#up{}, _Node) -> false;
may_cancel_timer(#down{reconnect_ref = ReconnectRef}, Node) ->
	_ = erlang:cancel_timer(ReconnectRef, [{async, true}]),
    receive
    	{try_reconnect, Node} ->
        	true
    after 0 ->
          true
    end.

-spec schedule_reconnect(Node) -> Result when
	Node	:: node(),
	Result	:: reference().

schedule_reconnect(Node) ->
	erlang:send_after(
		rand:uniform(?UNIFORM_RANGE) + ?RECONNECT_FROM,
		self(),
		{try_reconnect, Node}
	).

-spec may_connect_node(Node, State) -> Result when
	Node	:: node(),
	State	:: cluster_state(),
	Result	:: cluster_state().

may_connect_node(Node, State) -> may_connect_node(Node, State, nodes(), mcluster_utils:timestamp()).

-spec may_connect_node(Node, State, NodesFromNodeCommand, Time) -> Result when
    Node                    :: node(),
    State                   :: cluster_state(),
    NodesFromNodeCommand    :: [node()],
    Time                    :: pos_integer(),
    Result                  :: cluster_state().

may_connect_node(Node, #cluster_state{auto_connect_statuses = AutoConnectStatuses, clients_statuses = ClientStatuses} = State, ConnectedNodes, Time) ->
	case lists:member(Node, ConnectedNodes) of
	    false ->
	        case net_kernel:connect_node(Node) of
	            true ->
                    ?LOG_WARNING("mcluster node ~p UP", [Node]),
	                Up = #up{timestamp = Time},
	                _ = may_cancel_timer(maps:get(Node, AutoConnectStatuses, undefined), Node),
	                State#cluster_state{
						auto_connect_statuses = maps:put(Node, Up, AutoConnectStatuses)
	                };
	            false ->
	                NodeState = case maps:get(Node, AutoConnectStatuses, undefined) of
	                    #down{attempts = Attempts} = DownStatus ->
	  	   					_ = may_cancel_timer(DownStatus, Node),
	  	   					NextAttemptRef = schedule_reconnect(Node),
	                		DownStatus#down{
	  	   						attempts = Attempts + 1,
	  	   						last_attempt = Time,
	  	   						reconnect_ref = NextAttemptRef
	  	   					};
	                    _UpOrEmpty ->
	  	   					NextAttemptRef = schedule_reconnect(Node),
	                        #down{down_from = Time, last_attempt = Time, attempts = 1, reconnect_ref = NextAttemptRef}
	                end,
					State#cluster_state{
						auto_connect_statuses = maps:put(Node, NodeState, AutoConnectStatuses),
						clients_statuses = maps:remove(Node, ClientStatuses)
	                }
	        end;
	    true ->
			State#cluster_state{
				auto_connect_statuses = case maps:get(Node, AutoConnectStatuses, undefined) of
					#down{} = Down ->
						_ = may_cancel_timer(Down, Node),
						maps:put(Node, maps:get(Node, ClientStatuses, #up{timestamp = Time}), AutoConnectStatuses);
					undefined ->
						maps:put(Node, maps:get(Node, ClientStatuses, #up{timestamp = Time}), AutoConnectStatuses);
					#up{} ->
						AutoConnectStatuses
				end
			}
 	end.
