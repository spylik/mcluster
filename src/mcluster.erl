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
            is_disk_node();
        _ -> ForceDisk
    end,

    _ = case IsDiskNode of
        true ->
            ensure_mnesia_dir(),
            mnesia_eleveldb:register();
        false ->
            ok
    end,

    IsVirginNode = node_is_virgin(),
    ThisNode = node(),

    lager:info("Node ~p IsDiskNode is ~p",[ThisNode, IsDiskNode]),

    MnesiaNodes = ensure_connected_to_nodes(),

    NodesWithRunningMnesia = get_running_mnesia_nodes(MnesiaNodes),
    NodesWithoutRunningMnesia = MnesiaNodes -- NodesWithRunningMnesia,
    
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
        {error, Reason} -> lager:error(Reason)
    end,

    case IsDiskNode of
        true when IsVirginNode =:= true ->
            mnesia:change_table_copy_type(schema, node(), 'disc_copies');
        _ -> ok
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
ensure_connected_to_nodes() ->
    Nodes = application:get_env(?app, 'db_ram_copies', []) ++ application:get_env(?app, 'db_leveldb_copies', []),
    MnesiaNodes = [N || N <- Nodes -- [node()], net_adm:ping(N) =:= 'pong'],
    case MnesiaNodes of
        [] when Nodes =/= [] -> 
            mlibs:wait_for("Waitings for more nodes"),
            ensure_connected_to_nodes();
        _ ->
            MnesiaNodes
    end.
    

% @doc check is it disc node or not (should we create schema or not)
-spec is_disk_node() -> Result when
    Result :: boolean().

is_disk_node() ->
    lists:member(node(), application:get_env(?app, 'db_leveldb_copies', [])).

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
            TableInfoWrite = mnesia:table_info(TableName, 'where_to_read'),
            lager:info("created table ~p with def ~p, read from ~p write to: ",[ExtDef,TableInfoRead,TableInfoWrite]),
            Result;
        true ->
            lager:info("table ~p exists. going to load", [TableName]),
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
            lager:info("Tables ~p loaded",[TableNames]),
            {TableNames, ok};
        {timeout, BadTabs} ->
            lager:warning("Timeout occurs during mnesia:wait_for_tables. ~p",[BadTabs]),
            _ = lists:map(fun(Table) ->
                case get_running_mnesia_nodes(mnesia:table_info(Table, leveldb_copies)) of 
                    [] -> 
                        throw({error, {"Do not have active replicas who have table as leveldb_copies", Table}});
                    Replicas when is_list(Replicas) ->
                        lager:info("Going to force_load ~p from leveldb_copies nodes ~p",[Table,Replicas]),
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
    case lists:keyfind('leveldb_copies', 1, TableDefExt0) of
        {'leveldb_copies', _} ->
            TableDefExt0;
        'false' when RamFromConfig =:= 'undefined', HaveInDef =:= 'false' ->
            [{'ram_copies', [node()]}|TableDef];
        'false' when RamFromConfig =:= 'undefined', HaveInDef =:= 'true' ->
            TableDef;
        'false' ->
            [{'leveldb_copies', application:get_env(?app, 'db_leveldb_copies', [])}|TableDefExt0]
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
            lager:error("Mnesia dir \"~p\" cann't create", [MnesiaDir]),
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
    lager:info("Going to reset mnesia folder. Backup saved to ~p", [Name]),
    file:rename(?mnesiadir, Name).

% @doc reset mnesia folder and do not save backup
-spec reset_mnesia_folder('nobackup') -> Result when
    Result :: string().

reset_mnesia_folder('nobackup') ->
    stop_mnesia(),
    os:cmd("rm -Rf " ++ ?mnesiadir).
