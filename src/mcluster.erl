%% --------------------------------------------------------------------------------
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc
%% This source code is part of project "Maria" (https://github.com/spylik/maria).
%% @end
%% --------------------------------------------------------------------------------

-module(mcluster).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

% @doc public api
-export([
        init/0,
        init/2,
        init_table/2,
        init_table/3,
        is_running/0,
        get_running_mnesia_nodes/1,
        ensure_mnesia_not_running/0,
        ensure_mnesia_running/0,
        force_load/0,
        force_load/1,
        reset_mnesia_folder/0,
        reset_mnesia_folder/1
    ]).

-define(mnesiadir, mnesia:system_info(directory)).
-define(app, 'mcluster').
-define(backend_type, 'rocksdb_copies').

% @doc init api
-spec init() -> Result when
    Result :: ok.

init() -> init(true,'undefined').

% @doc init api for rpc calls
-spec init(SendRPC,ForceDisk) -> Result when
    SendRPC     :: boolean(),
    ForceDisk   :: 'undefined' | 'true' | 'false',
    Result      :: ok.

init(SendRPC, ForceDisk) ->
    ensure_mnesia_running(),

    IsDiskNode = case ForceDisk of
        'undefined' ->
            _ = application:load(?app),
            is_disk_node(application:get_env(?app, 'db_rocksdb_copies', []));
        _ -> ForceDisk
    end,

    IsVirginNode = node_is_virgin(),
    ThisNode = node(),

    error_logger:info_msg("Node ~p IsDiskNode is ~p",[ThisNode, IsDiskNode]),

    MnesiaNodes = ensure_connected_to_nodes(
        sets:to_list(
            sets:from_list(
                application:get_env(?app, 'db_ram_copies', []) ++
                application:get_env(?app, 'db_rocksdb_copies', [])
            )
        )
    ),

    NodesWithRunningMnesia = get_running_mnesia_nodes(MnesiaNodes),
    NodesWithoutRunningMnesia = MnesiaNodes -- NodesWithRunningMnesia,

    ok = try_start(IsDiskNode, IsVirginNode),

    % send RPC call to start mnesia if we have another nodes without running mnesia
    _ = case SendRPC of
        true when NodesWithoutRunningMnesia =/= [] ->
            _ = rpc:multicall(NodesWithoutRunningMnesia, mcluster, init, [false,'undefined']),
            rpc:multicall(MnesiaNodes, mnesia, change_config, ['extra_db_nodes', [ThisNode]]);
        _ when MnesiaNodes =/= [] ->
            rpc:multicall(NodesWithRunningMnesia, mnesia, change_config, ['extra_db_nodes', [ThisNode]]);
        _ -> ok
    end,

    _ = case mnesia:change_config('extra_db_nodes', MnesiaNodes) of
        {ok, Value} -> Value;
        {error, Reason} -> error_logger:error_msg("~p",[Reason])
    end,

    case IsDiskNode of
        true when IsVirginNode =:= true ->
            mnesia:change_table_copy_type(schema, node(), ?backend_type);
        _ -> ok
    end.

% @doc try mnesia start and reset folder in case on fail
-spec try_start(IsDiskNode, IsVirginNode) -> Result when
    IsDiskNode  :: boolean(),
    IsVirginNode:: boolean(),
    Result      :: ok.

try_start(IsDiskNode, IsVirginNode) ->
    case mnesia:start() of
        ok ->
            {ok, _} = mnesia_rocksdb:register(),
            ensure_mnesia_running();
        Something when IsDiskNode =:= true andalso IsVirginNode =:= false ->
            error_logger:error_msg("~p",[Something]),
            ok = ensure_mnesia_not_running(),
            ok = reset_mnesia_folder(),
            ok = ensure_mnesia_dir(),
            try_start(IsDiskNode, true)
    end.

% @doc get nodes with running mnesia
-spec get_running_mnesia_nodes(AllNodes) -> Result when
    AllNodes :: [node()],
    Result :: [node()] | [].

get_running_mnesia_nodes(AllNodes) ->
    {Answers, _WrongNodes} = rpc:multicall(AllNodes, mcluster, is_running, []),
    [N || {N, Result} <- Answers, Result =:= 'yes'].

% @doc rpc call for check is mnesia running
-spec is_running() -> Result when
    Result :: {node(), 'yes' | 'no' | 'starting' | 'stopping'}.

is_running() ->
    {node(), mnesia:system_info('is_running')}.

% @doc try connect to mnesia nodes from sys.config
-spec ensure_connected_to_nodes(Nodes) -> Result when
    Nodes   :: [node()],
    Result  :: [node()].
ensure_connected_to_nodes(Nodes) ->
    Self = node(),
    case lists:filter(fun(Node) when Node =:= Self -> false; (_) -> true end, Nodes) of
        [] ->
            Nodes;
        OtherNodes ->
            MnesiaNodes = [N || N <- OtherNodes, net_adm:ping(N) =:= 'pong'],
            case MnesiaNodes of
                [] when Nodes =/= [] ->
                    mlibs:wait_for("Waitings for more nodes"),
                    ensure_connected_to_nodes(Nodes);
                _ ->
                    MnesiaNodes
            end
    end.


% @doc check is it disc node or not (should we create schema or not)
-spec is_disk_node(Nodes) -> Result when
    Nodes   :: [node()],
    Result  :: boolean().

is_disk_node(Nodes) ->
    lists:member(node(), Nodes).

% @doc init table api (read timeout variable from sys.config). Default 1min
-spec init_table(TableName, TableDef) -> Result when
    TableName :: atom(),
    TableDef  :: nonempty_list(),
    Result    :: {atomic, ok} | {aborted, Reason},
    Reason    :: term().

init_table(TableName, TableDef) ->
    init_table(TableName, TableDef, application:get_env(?app, 'table_loading_timeout', 'infinity')).

% @doc init table api with timeout
-spec init_table(TableName, TableDef, Timeout) -> Result when
    TableName :: atom(),
    TableDef  :: nonempty_list(),
    Timeout   :: pos_integer(),
    Result    :: {atomic, ok} | {aborted, Reason},
    Reason    :: term().

init_table(TableName, TableDef, Timeout) ->
    ExtDef = lists:sort(extend_table_def(TableDef)),
    AllTables = mnesia:system_info(tables),
    {_, ok} = case lists:member(TableName, AllTables) of
        false ->
            Result = mnesia:create_table(TableName, ExtDef),
            TableInfoRead = mnesia:table_info(TableName, 'where_to_read'),
            TableInfoWrite = mnesia:table_info(TableName, 'where_to_write'),
            error_logger:info_msg("created table ~p with def ~p, read from ~p write to: ",[ExtDef,TableInfoRead,TableInfoWrite]),
            Result;
        true ->
            error_logger:info_msg("table ~p exists. going to load", [TableName]),
            wait_table([TableName], Timeout)
    end.
    %mnesia_lib:set({TableName, 'where_to_read'}, node()).

-spec wait_table(TableNames, Timeout) -> Result when
    TableNames  :: [atom()],
    Timeout     :: pos_integer(),
    Result      :: {TableNames, ok}.

wait_table(TableNames, Timeout) ->
    case mnesia:wait_for_tables(TableNames, Timeout) of
        ok ->
            error_logger:info_msg("Tables ~p loaded",[TableNames]),
            {TableNames, ok};
        {timeout, BadTabs} ->
            error_logger:warning_msg("Timeout occurs during mnesia:wait_for_tables. ~p",[BadTabs]),
            _ = lists:map(fun(Table) ->
                case get_running_mnesia_nodes(mnesia:table_info(Table, ?backend_type)) of
                    [] ->
                        throw({error, {"Do not have active replicas who have table as ?backend_type", Table}});
                    Replicas when is_list(Replicas) ->
                        error_logger:info_msg("Going to force_load ~p from ?backend_type nodes ~p",[Table,Replicas]),
                        rpc:multicall(Replicas, 'mcluster', force_load, [Table])
                end
            end, BadTabs),
            wait_table(BadTabs, Timeout);
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.


% @doc extend table definition with maria-specific fields
-spec extend_table_def(TableDef) -> Result when
    TableDef  :: nonempty_list(),
    Result    :: nonempty_list().

extend_table_def(TableDef) ->
    RamFromConfig = case application:get_env(?app, 'db_ram_copies') of
        {ok, Value} -> Value;
        undefined -> 'undefined'
    end,

    {TableDefExt0, HaveInDef} =
    case lists:keyfind('ram_copies', 1, TableDef) of
        {'ram_copies', _} ->
            {TableDef, true};
        'false' when RamFromConfig =:= 'undefined' ->
            {TableDef, false};
        'false' ->
            {[{'ram_copies', RamFromConfig}|TableDef], true}
    end,

    TableDefExt1 =
    case lists:keyfind(?backend_type, 1, TableDefExt0) of
        {?backend_type, _} ->
            TableDefExt0;
        'false' when RamFromConfig =:= 'undefined', HaveInDef =:= 'false' ->
            [{'ram_copies', [node()]}|TableDef];
        'false' when RamFromConfig =:= 'undefined', HaveInDef =:= 'true' ->
            TableDef;
        'false' ->
            [{?backend_type, application:get_env(?app, 'db_rocksdb_copies', [])}|TableDefExt0]
    end,
    TableDefExt1.

% @doc ensure mnesia is not running
-spec ensure_mnesia_not_running() -> Result when
    Result :: 'ok'.

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        'no' -> ok;
        'stopping' ->
            mlibs:wait_for("Mnesia stopping. Waiting for mnesia_not_running"),
            ensure_mnesia_not_running();
        Reason when Reason =:= 'yes'; Reason =:= 'starting' ->
            throw({error, mnesia_unexpectedly_running})
    end.

% @doc ensure mnesia is running
-spec ensure_mnesia_running() -> Result when
    Result :: 'ok'.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        'yes' -> ok;
        'starting' ->
            mlibs:wait_for("Mnesia startning. Waiting for full start."),
            ensure_mnesia_running();
        'no' ->
            mnesia:start();
        Reason when Reason =:= 'no'; Reason =:= 'stopping' ->
            throw({error, mnesia_not_running})
    end.

% @doc stop mnesia
-spec stop_mnesia() -> Result when
    Result :: 'ok'.

stop_mnesia() ->
    'stopped' = mnesia:stop(),
    ok = ensure_mnesia_not_running().

% @doc ensure ?mnesiadir accessable
-spec ensure_mnesia_dir() -> Result when
    Result :: 'ok'.

ensure_mnesia_dir() ->
    MnesiaDir = ?mnesiadir ++ "/",
    case filelib:ensure_dir(?mnesiadir) of
        {error, Reason} ->
            error_logger:error_msg("Mnesia dir \"~p\" cann't create", [MnesiaDir]),
            throw({'error', {'cannot_create_mnesia_dir', MnesiaDir, Reason}});
        ok ->
            ok
    end.

% @doc check is it virgin node or not
-spec node_is_virgin() -> Result when
    Result :: boolean().

node_is_virgin() ->
    case prim_file:list_dir(?mnesiadir) of
        {error, enoent} ->
            true;
        {ok, []} ->
            true;
        {ok, Data} when is_list(Data) ->
            false
    end.

% @doc force locally-known tables
-spec force_load() -> Result when
    Result              :: 'yes' | {'already_loaded', node()} | ErrorDescription,
    ErrorDescription    :: term().

force_load() ->
    force_load(mnesia:system_info(local_tables)--[schema]).

% @doc force load specified tables
-spec force_load(Tabs) -> Result when
    Tabs                :: atom() | [atom()],
    Result              :: 'yes' | {'already_loaded', node()} | ErrorDescription,
    ErrorDescription    :: term().

force_load(Tabs) when is_list(Tabs) ->
    lists:map(fun(Table) ->
        force_load(Table)
    end, Tabs);

force_load(Table) when is_atom(Table) ->
    case mnesia:table_info(Table, load_node) of
        'unknown' -> mnesia:force_load_table(Table);
         Node -> {already_loaded, Node}
    end.


% @doc reset mnesia folder with backup due error
-spec reset_mnesia_folder() -> Result when
    Result :: 'ok' | {error, Reason},
    Reason :: term().

reset_mnesia_folder() ->
    stop_mnesia(),
    Name = ?mnesiadir++"_backup_"++integer_to_list(mlibs:get_time()),
    error_logger:info_msg("Going to reset mnesia folder. Backup saved to ~p", [Name]),
    file:rename(?mnesiadir, Name).

% @doc reset mnesia folder and do not save backup
-spec reset_mnesia_folder('nobackup') -> Result when
    Result :: string().

reset_mnesia_folder('nobackup') ->
    stop_mnesia(),
    os:cmd("rm -Rf " ++ ?mnesiadir).
