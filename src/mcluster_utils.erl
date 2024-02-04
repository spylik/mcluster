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
        config_nodes/0,
        nodes_specs/0,
        node_spec/0,
        node_spec/1,
        node_roles/0,
        node_roles/1,
		node_has_role/1,
		node_has_role/2,
		nodes_with_role/2,
		live_nodes_with_role/2,
        node_markets/0,
        node_markets/1,
        node_runtime_env/0,
        node_runtime_env/1,
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
-type runtime_env() :: dev | prod | staging | atom().

% ================================ public api ==================================

% @doc get all known nodes from config
-spec config_nodes() -> Result when
    Result :: [node()].

config_nodes() ->
    maps:keys(nodes_specs()).

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

-spec nodes_with_role(Role, RuntimeEnv) -> Result when
	Role		:: role(),
	RuntimeEnv	:: runtime_env(),
	Result		:: [node()].

nodes_with_role(Role, RuntimeEnv) ->
	maps:fold(fun
		(Node, #{runtime_env := NodeRuntimeEnv, roles := Roles}, Acc) when NodeRuntimeEnv =:= RuntimeEnv ->
			case lists:member(Role, Roles) of
				true ->
					[Node | Acc];
				false ->
					Acc
			end;
		(_Node, _SpecRow, Acc) -> Acc
	end, [], nodes_specs()).

-spec live_nodes_with_role(Role, RuntimeEnv) -> Result when
	Role		:: role(),
	RuntimeEnv	:: runtime_env(),
	Result		:: [node()].

live_nodes_with_role(Role, RuntimeEnv) ->
	[X || X <- [node() | nodes()], Y <- nodes_with_role(Role, RuntimeEnv), X =:= Y].


% @doc Check does the given node has specific role or not
-spec node_has_role(Node, Role) -> Result when
	Node	:: node(),
	Role	:: role(),
	Result	:: boolean().

node_has_role(Node, Role) ->
	lists:member(Role, node_roles(Node)).

-spec config_runtimes() -> Result when
    Result  :: [runtime_env()].

config_runtimes() ->
   lists:uniq(maps:fold(fun(_Node, #{runtime_env := Runtime}, Acc) -> [Runtime | Acc] end, [], nodes_specs())).

-spec whereis_market(Market) -> Result when
    Market  :: market(),
    Result  :: [node()].

whereis_market(Market) ->
    whereis_market(Market, maria).

whereis_market(Market, Role) ->
    whereis_market(Market, Role, node_runtime_env()).

whereis_market(Market, Role, RuntimeEnv) ->
    maps:fold(fun
        (Node, #{runtime_env := NodeRuntimeEnv, roles := Roles} = Config, Acc) when NodeRuntimeEnv =:= RuntimeEnv ->
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
    end, [], nodes_specs()).

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
			nodes_with_role(Role, node_runtime_env(Node))
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
-spec node_runtime_env() -> Result when
    Result  :: runtime_env().

node_runtime_env() -> node_runtime_env(node()).

% @doc get the runtime env for a given node
-spec node_runtime_env(Node) -> Result when
    Node    :: node(),
    Result  :: runtime_env().

node_runtime_env(Node) ->
    deep_maps:get_in([Node, runtime_env], nodes_specs(), dev).

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
