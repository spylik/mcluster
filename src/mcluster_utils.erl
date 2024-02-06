%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc
%% This source code is part of project "Maria" (https://github.com/spylik/maria).
%% @end
%% --------------------------------------------------------------------------------

-module(mcluster_utils).
-include("/Users/spyl/projects/kawbo/maria/include/mlogs.hrl").
-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

% @doc public api
-export([
        nodes_to_auto_connect/0,
        config_nodes/0,
        config_nodes/1,
        nodes_specs/0,
        node_spec/0,
        node_spec/1,
        node_roles/0,
        node_roles/1,
		node_has_role/1,
		node_has_role/2,
		nodes_with_role/1,
		nodes_with_role/2,
		live_nodes_with_role/1,
		live_nodes_with_role/2,
        node_markets/0,
        node_markets/1,
        node_cluster_id/0,
        node_cluster_id/1,
        config_runtimes/0,
        whereis_market/1,
		is_market/1,
        default_markets/1
    ]).

-define(app, 'mcluster').

% todo: typing
-type nodes_specs() :: #{node() => node_spec()}.

-type node_spec() :: #{}.

-type role() :: maria | kawbo_gql.

-type market() :: atom().

-type market_config() :: #{}.

% atom cuz we support parralel runtime env (like dev0, staging3, etc).
-type cluster_id() :: dev | prod | staging | atom().

-export_type([nodes_specs/0]).

% ================================ public api ==================================

-spec nodes_to_auto_connect() -> Result when
    Result :: [node()].

nodes_to_auto_connect() ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
    RolesToConnectWith = deep_maps:get_in([node(), auto_connect_to_nodes_with_roles], NodesSpecs, []),
    lists:uniq(lists:foldl(
      fun(Role, Acc) ->
          Acc ++ nodes_with_role(Role, ClusterId, NodesSpecs)
    end, [], RolesToConnectWith)).

% @doc get all known nodes from config
-spec config_nodes() -> Result when
    Result :: [node()].

config_nodes() ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
    config_nodes(ClusterId, NodesSpecs).

config_nodes(ClusterId) ->
    config_nodes(ClusterId, nodes_specs()).

config_nodes(ClusterId, NodesSpecs) ->
	maps:fold(fun
		(Node, #{cluster_id := NodeClusterId}, Acc) when NodeClusterId =:= ClusterId ->
			[Node | Acc];
		(_Node, _SpecRow, Acc) -> Acc
	end, [], NodesSpecs).

% @doc get the current node roles
-spec node_roles() -> [role()].

node_roles() -> node_roles(node()).

% @doc get the roles for a given node.
-spec node_roles(Node) -> Result when
    Node    :: node(),
    Result  :: [role()].

node_roles(Node) ->
    deep_maps:get_in([Node, roles], nodes_specs(), []).

% @doc Check does the current node has given role or not
-spec node_has_role(Role) -> Result when
	Role	:: role(),
	Result	:: boolean().

node_has_role(Role) ->
	node_has_role(node(), Role).

-spec nodes_with_role(Role) -> Result when
    Role        :: role(),
    Result      :: [node()].

nodes_with_role(Role) ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
    nodes_with_role(Role, ClusterId, NodesSpecs).

-spec nodes_with_role(Role, ClusterId) -> Result when
	Role		:: role(),
	ClusterId	:: cluster_id(),
	Result		:: [node()].

nodes_with_role(Role, ClusterId) ->
    nodes_with_role(Role, ClusterId, nodes_specs()).

nodes_with_role(Role, ClusterId, NodesSpecs) ->
	maps:fold(fun
		(Node, #{cluster_id := NodeClusterId, roles := Roles}, Acc) when NodeClusterId =:= ClusterId ->
			case lists:member(Role, Roles) of
				true ->
					[Node | Acc];
				false ->
					Acc
			end;
		(_Node, _SpecRow, Acc) -> Acc
	end, [], NodesSpecs).

-spec live_nodes_with_role(Role) -> Result when
	Role		:: role(),
	Result		:: [node()].

live_nodes_with_role(Role) ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
	NodesWithRole = nodes_with_role(Role, ClusterId, NodesSpecs),
    live_nodes_only(NodesWithRole).

-spec live_nodes_with_role(Role, ClusterId) -> Result when
	Role		:: role(),
	ClusterId	:: cluster_id(),
	Result		:: [node()].

live_nodes_with_role(Role, ClusterId) -> live_nodes_with_role(nodes_with_role(Role, ClusterId)).

-spec live_nodes_only(NodeList) -> Result when
	NodeList	:: [node()],
	Result		:: [node()].

live_nodes_only(NodeList) ->
	[X || X <- [node() | nodes()], Y <- NodeList, X =:= Y].


% @doc Check does the given node has specific role or not
-spec node_has_role(Node, Role) -> Result when
	Node	:: node(),
	Role	:: role(),
	Result	:: boolean().

node_has_role(Node, Role) ->
	lists:member(Role, node_roles(Node)).

-spec config_runtimes() -> Result when
    Result  :: [cluster_id()].

config_runtimes() ->
   lists:uniq(maps:fold(fun(_Node, #{cluster_id := Runtime}, Acc) -> [Runtime | Acc] end, [], nodes_specs())).

-spec whereis_market(Market) -> Result when
    Market  :: market(),
    Result  :: [node()].

whereis_market(Market) ->
    whereis_market(Market, maria).

-spec whereis_market(Market, Role) -> Result when
    Market  :: market(),
	Role	:: role(),
    Result  :: [node()].

whereis_market(Market, Role) ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
    whereis_market(Market, Role, ClusterId, NodesSpecs).

-spec whereis_market(Market, Role, ClusterId, NodesSpecs) -> Result when
    Market  	:: market(),
	Role		:: role(),
	ClusterId	:: cluster_id(),
	NodesSpecs	:: nodes_specs(),
    Result  	:: [node()].

whereis_market(Market, Role, ClusterId, NodesSpecs) ->
    maps:fold(fun
        (Node, #{cluster_id := NodeClusterId, roles := Roles} = Config, Acc) when NodeClusterId =:= ClusterId ->
            case lists:member(Role, Roles) of
                true ->
                    case deep_maps:get_in([Role, markets], Config, default_markets(Role)) of
                        none ->
                            Acc;
                        Markets ->
							case lists:member(Market, Markets) of
								true ->
		                            [Node | Acc];
								false ->
									Acc
							end
                    end;
                false ->
                    Acc
            end;
        (_Node, _Config, Acc) -> Acc
    end, [], NodesSpecs).

-spec node_markets() -> Result when
    Result  :: [market()].

node_markets() ->
    node_markets(node()).

-spec node_markets(Node) -> Result when
    Node    :: node(),
    Result  :: [market()].

node_markets(Node) ->
    node_markets(Node, maria).

-spec node_markets(Node, Role) -> Result when
    Node    :: node(),
    Role    :: role(),
    Result  :: [market()].

node_markets(Node, Role) ->
    case node_spec(Node) of
        #{roles := Roles} = Config ->
            case lists:member(Role, Roles) of
                true ->
                    deep_maps:get_in([Role, markets], Config, default_markets(Role));
				false ->
					[]
			end;
		_Another ->
			[]
	end.

-spec default_markets(Role) -> Result when
    Role    :: role(),
    Result  :: [market()].

default_markets(Role) ->
   _ = application:load(Role),
   case os:getenv("MARKETS") of
       false ->
           case application:get_env(Role, markets, none) of
               none -> [];
               all -> known_markets();
               ListOfMarkets -> ListOfMarkets
           end;
       Markets ->
           [list_to_existing_atom(Market) || Market <- string:tokens(Markets, ",")]
    end.

-spec known_markets() -> Result when
	Result	:: [market()].

known_markets() ->
	known_markets(maria).

-spec known_markets(Role) -> Result when
    Role    :: role(),
    Result  :: [market()].

known_markets(Role) ->
	Node = node(),
	case node_has_role(Node, Role) of
		true ->
			known_markets_from_modules();
		false ->
            % todo:
			nodes_with_role(Role, node_cluster_id(Node))
	end.

% @doc get known markets based on codebase
-spec known_markets_from_modules() -> Result when
    Result  :: [markets:market()].

known_markets_from_modules() -> known_markets_from_modules(not_force_loaded).

-spec known_markets_from_modules(ForceLoadStatus) -> Result when
    ForceLoadStatus :: not_force_loaded | force_loaded,
    Result          :: [markets:market()].

known_markets_from_modules(ForceLoadStatus) ->
    case application:get_key(?app, modules) of
        {ok, Modules} ->
            lists:foldl(fun(ModuleName, Acc) ->
                case is_market(ModuleName) of
                    false ->
                        Acc;
                    true ->
                        [ModuleName | Acc]
                end
            end, [], Modules);
        undefined when ForceLoadStatus =:= not_force_loaded ->
            ok = application:load(?app),
            known_markets(force_loaded);
        undefined ->
            []
    end.

-spec is_market(Input) -> Result when
    Input   :: binary() | atom(),
    Result  :: boolean().

is_market(Input) when is_atom(Input) ->
    try
        case lists:keyfind(behaviour, 1, Input:module_info(attributes)) of
            false -> false;
            {behaviour, Behaviours} ->
                lists:member(?MODULE, Behaviours)
        end
    catch
        _:_ ->
            false
    end;

is_market(Input) when is_binary(Input) ->
    try
        is_market(erlang:binary_to_existing_atom(string:lowercase(Input), utf8))
    catch
        _:_ ->
            false
    end.

% @doc get the node environment
-spec node_cluster_id() -> Result when
    Result  :: cluster_id().

node_cluster_id() -> node_cluster_id(node()).

% @doc get the runtime env for a given node
-spec node_cluster_id(Node) -> Result when
    Node    :: node(),
    Result  :: cluster_id().

node_cluster_id(Node) -> node_cluster_id(Node, nodes_specs()).

% @doc get the runtime env for a given node
-spec node_cluster_id(Node, NodesSpecs) -> Result when
    Node        :: node(),
    NodesSpecs  :: nodes_specs(),
    Result      :: cluster_id().

node_cluster_id(Node, NodesSpecs) ->
    deep_maps:get_in([Node, cluster_id], NodesSpecs, dev).

% @doc get the whole node roles config
-spec nodes_specs() -> Result when
    Result  :: nodes_specs().

nodes_specs() ->
    application:load(?app),
    application:get_env(?app, 'nodes_specs', #{}).

-spec node_spec() -> Result when
    Result  :: node_spec().

node_spec() ->
    node_spec(node()).

-spec node_spec(Node) -> Result when
    Node    :: node(),
    Result  :: node_spec().

node_spec(Node) -> maps:get(Node, nodes_specs()).
