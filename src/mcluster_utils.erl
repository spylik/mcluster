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
        node_cluster_id/0,
        node_cluster_id/1,
        node_cluster_id/2,
		node_has_role/1,
		node_has_role/2,
		nodes_with_role/1,
		nodes_with_role/2,
		live_nodes_with_role/1,
        clusters/0,
        timestamp/0
    ]).

-define(app, 'mcluster').

% todo: typing
-type nodes_specs() :: #{node() => node_spec()}.

-type node_spec() :: #{}.

-type role() :: atom().

% atom cuz we support parralel runtime env (like dev0, staging3, etc).
-type cluster_id() :: dev | prod | staging | atom().

-export_type([
        nodes_specs/0,
        role/0,
        cluster_id/0
    ]).

% ================================ public api ==================================

-spec nodes_to_auto_connect() -> Result when
    Result :: [node()].

nodes_to_auto_connect() ->
    NodesSpecs = nodes_specs(),
    ClusterId = node_cluster_id(node(), NodesSpecs),
    RolesToConnectWith = maps:get(auto_connect_to_nodes_with_roles, maps:get(node(), NodesSpecs, #{}), []),
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

-spec config_nodes(ClusterId) -> Result when
    ClusterId   :: cluster_id(),
    Result      :: [node()].

config_nodes(ClusterId) ->
    config_nodes(ClusterId, nodes_specs()).

-spec config_nodes(ClusterId, NodesSpecs) -> Result when
    ClusterId   :: cluster_id(),
    NodesSpecs  :: nodes_specs(),
    Result      :: [node()].

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
    maps:get(roles, maps:get(Node, nodes_specs(), #{}), []).

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

-spec nodes_with_role(Role, ClusterId, NodesSpecs) -> Result when
    Role        :: role(),
    ClusterId   :: cluster_id(),
    NodesSpecs  :: nodes_specs(),
    Result      :: [node()].

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

-spec clusters() -> Result when
    Result  :: [cluster_id()].

clusters() ->
   lists:uniq(maps:fold(fun(_Node, #{cluster_id := Runtime}, Acc) -> [Runtime | Acc] end, [], nodes_specs())).

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
    maps:get(cluster_id, maps:get(Node, NodesSpecs, #{}), dev).

% @doc get the whole node roles config
-spec nodes_specs() -> Result when
    Result  :: nodes_specs().

nodes_specs() ->
    _ = application:load(?app),
    application:get_env(?app, 'nodes_specs', #{}).

-spec node_spec() -> Result when
    Result  :: node_spec().

node_spec() ->
    node_spec(node()).

-spec node_spec(Node) -> Result when
    Node    :: node(),
    Result  :: node_spec().

node_spec(Node) -> maps:get(Node, nodes_specs()).

-spec timestamp() -> Result when
    Result  :: pos_integer().

timestamp() -> erlang:system_time(millisecond).
