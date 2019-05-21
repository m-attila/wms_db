%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Database handler module for Mnesia db.
%%% @end
%%% Created : 28. Apr 2019 18:34
%%%-------------------------------------------------------------------
-module(wms_db_handler).
-author("Attila Makra").

-include("wms_db.hrl").

-define(WAIT_FOR_INIT_TIMEOUT_MSEC, 5000).

%% API
-export([init/2,
         create_table/4,
         create_kv_table/2,
         read/2,
         read/3,
         read_kv/2,
         write/2,
         write_kv/3,
         delete/2,
         transaction/1,
         delete_table/1, is_transaction/0, convert/1]).


%% =============================================================================
%% Private types
%% =============================================================================

-type db_result(Res) :: {'atomic', Res} | {'aborted', Reason :: term()}.
-record(key_value_record, {
  key :: term(),
  value :: term()
}).

%% =============================================================================
%% Functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Initialization
%% -----------------------------------------------------------------------------

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% init/1
%% ###### Purpose
%% Initialize mnesia database.
%% ###### Arguments
%% Nodes - all mnesia nodes.
%% AvaliableNodes - available mnesia nodes.
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec init([node()], [node()]) ->
  ok | {error, term()} | {error, cluster_not_ready}.
init(AllNodes, AvailableNodes) ->
  ok = application:ensure_started(mnesia),
  SchemaExists = lists:member(node(),
                              mnesia:table_info(schema, disc_copies)),

  case {SchemaExists, list_eq(AllNodes, AvailableNodes)} of
    {false, false} ->
      {error, cluster_not_ready};
    {false, true} ->
      change_config(AllNodes, is_mnesia_started(AllNodes));
    {true, _} ->
      ok
  end.

-spec is_mnesia_started([node()]) ->
  boolean().
is_mnesia_started([Node]) when Node =:= node() ->
  true;
is_mnesia_started(Nodes) ->
  RemoteNodes = lists:delete(node(), Nodes),
  {NodeAnswers, BadNodes} = rpc:multicall(RemoteNodes,
                                          application, which_applications, []),
  case BadNodes of
    [] ->
      lists:any(
        fun(NodeAns) ->
          lists:any(
            fun({App, _, _}) ->
              App =:= mnesia
            end, NodeAns)
        end, NodeAnswers);
    _ ->
      false
  end.

-spec change_config([node()], boolean()) ->
  ok | {error, term()} | {error, cluster_not_ready}.
change_config(_, false) ->
  {error, cluster_not_ready};
change_config(AllNodes, true) ->
  case mnesia:change_config(extra_db_nodes, AllNodes) of
    {ok, _} ->
      init_db();
    Other ->
      Other
  end.

-spec init_db() ->
  ok | {error, term()}.
init_db() ->
  Result = mnesia:change_table_copy_type(schema, node(), disc_copies),
  case Result of
    {atomic, ok} ->
      ok;
    {aborted, {already_exists, _, _, _}} ->
      ok;
    {aborted, Reason} ->
      {error, Reason}
  end.

-spec list_eq([node()], [node()]) ->
  boolean().
list_eq(List1, List2) ->
  lists:usort(List1) =:= lists:usort(List2).

%% -----------------------------------------------------------------------------
%% Table creation.
%% -----------------------------------------------------------------------------

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% create_table/5
%% ###### Purpose
%% Create mnesia table and copy to nodes.
%% ###### Arguments
%% * TableName - name of table
%% * Type - set | ordered_set
%% * RecordName - name of record what will be stored
%% * Attributes - field names of the record
%% * Nodes - mnesia db nodes
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec create_table(atom(), table_types(), atom(), [atom()]) ->
  ok | {error, term()}.

create_table(TableName, Type, RecordName, Attributes) ->
  TableNodes = get_table_nodes(TableName),
  case TableNodes of
    [] ->
      % no table in system
      OpRes = create_disk_copy_table(TableName, Type, RecordName,
                                     Attributes, [node()]),
      ok = check_create_result(OpRes);
    _ ->
      % table already exists on another node
      ok
  end,
  add_table_copy_to_node(TableName).

-spec get_table_nodes(atom()) ->
  [node()].
get_table_nodes(Table) ->
  try
    mnesia:table_info(Table, disc_copies)
  catch
    _:{aborted, {no_exists, Table, disc_copies}} ->
      []
  end.

-spec create_disk_copy_table(atom(), table_types(), atom(), [atom()], [node()]) ->
  db_result(ok).
create_disk_copy_table(TableName, Type, RecordName, Attributes, Nodes) ->
  mnesia:create_table(TableName,
                      [{attributes, Attributes},
                       {type, Type},
                       {disc_copies, Nodes},
                       {record_name, RecordName}]).

-spec check_create_result(db_result(ok)) ->
  ok | term().
check_create_result(Res) ->
  case Res of
    {atomic, ok} ->
      ok;
    {aborted, {already_exists, _}} ->
      ok;
    {aborted, {already_exists, _, _}} ->
      ok;
    {aborted, Result} ->
      Result
  end.

-spec add_table_copy_to_node(atom()) ->
  ok | {error, term()}.
add_table_copy_to_node(TableName) ->
  Node = node(),
  % wait for table
  mnesia:wait_for_tables([TableName], timer:minutes(5)),
  % add copy
  case mnesia:add_table_copy(TableName, Node, disc_copies) of
    {atomic, ok} ->
      ok;
    {aborted, {already_exists, TableName}} ->
      ok;
    {aborted, {already_exists, TableName, Node}} ->
      ok;
    {aborted, Reason} ->
      {error, Reason}
  end.


%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% create_kv_table/3
%% ###### Purpose
%% Create key-value storage table.
%% ###### Arguments
%% * TableName - name of table
%% * Type - set | ordered_set
%% * RecordName - name of record what will be stored
%% * Attributes - field names of the record
%% * Nodes - mnesia db nodes
%% %% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec create_kv_table(atom(), table_types()) ->
  ok | {error, term()}.
create_kv_table(TableName, Type) ->
  create_table(TableName, Type, key_value_record,
               record_info(fields, key_value_record)).

%% -----------------------------------------------------------------------------
%% Delete table
%% -----------------------------------------------------------------------------

-spec delete_table(atom()) ->
  ok | {error, term()}.
delete_table(TableName) ->
  unpack_mnesia_result(mnesia:delete_table(TableName)).

%% -----------------------------------------------------------------------------
%% Read operation
%% -----------------------------------------------------------------------------

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% read/2
%% ###### Purpose
%% Reads any term from database.,
%% ###### Arguments
%% * TableName - name of table
%% * Key - primary key of the record
%% ###### Returns
%% List of values.
%%-------------------------------------------------------------------
%%
%% @end
-spec read(atom(), term()) ->
  {ok, [term()]}.
read(TableName, Key) ->
  read(TableName, Key, read).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% read/3
%% ###### Purpose
%% Reads any term from database for given locking mode
%% ###### Arguments
%% * TableName - name of table
%% * Key - primary key of the record
%% * LockingMode - read | write
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec read(atom(), term(), read | write) ->
  {ok, [term()]} | {error, term()}.
read(TableName, Key, LockingMode) ->
  case mnesia:is_transaction() of
    true ->
      {ok, mnesia:read(TableName, Key, LockingMode)};
    false ->
      wms_db_handler_service:run(
        fun() ->
          {ok, mnesia:dirty_read(TableName, Key)}
        end, ?WAIT_FOR_INIT_TIMEOUT_MSEC)
  end.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% read_kv/2
%% ###### Purpose
%% Reads value from key-value storage table.
%% ###### Arguments
%% * TableName - name of table
%% * Key - primary key of the record
%% ###### Returns
%% List of values.
%%-------------------------------------------------------------------
%%
%% @end
-spec read_kv(atom(), term()) ->
  {ok, [term()]}.
read_kv(TableName, Key) ->
  {ok, KeyValueRecords} = read(TableName, Key),
  {ok, lists:map(
    fun(#key_value_record{value = Value}) ->
      Value
    end, KeyValueRecords)}.

%% -----------------------------------------------------------------------------
%% Write opertation
%% -----------------------------------------------------------------------------

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% write/2
%% ###### Purpose
%% Write any term into table.
%% ###### Arguments
%% * TableName - name of table
%% * Record - record what will be stored in table.
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec write(atom(), term()) ->
  ok | no_return().
write(TableName, Record) ->
  mnesia:write(TableName, Record, write).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% write_kv/3
%% ###### Purpose
%% Write value into key-value storage table.
%% ###### Arguments
%% * TableName - name of table
%% * Key - primary key of value
%% * Value - data on key
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec write_kv(atom(), term(), term()) ->
  ok | no_return().
write_kv(TableName, Key, Value) ->
  write(TableName, #key_value_record{key = Key, value = Value}).

%% -----------------------------------------------------------------------------
%% Delete operation
%% -----------------------------------------------------------------------------

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% delete/2
%% ###### Purpose
%% Delete record from table.
%% ###### Arguments
%% * TableName - name of table
%% * Key - primary key of value%% ###### Returns
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec delete(atom(), term()) ->
  ok.
delete(TableName, Key) ->
  mnesia:delete(TableName,
                Key, write).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% is_transaction/0
%% ###### Purpose
%% Returns if in database transaction.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec is_transaction() ->
  boolean().
is_transaction() ->
  mnesia:is_transaction().

%% -----------------------------------------------------------------------------
%% Transaction
%% -----------------------------------------------------------------------------

-spec transaction(fun()) ->
  ok | {ok | error, term()}.
transaction(TransactionFun) ->
  case mnesia:is_transaction() of
    false ->
      wms_db_handler_service:run(
        fun() ->
          unpack_mnesia_result(mnesia:transaction(TransactionFun))
        end, ?WAIT_FOR_INIT_TIMEOUT_MSEC);
    true ->
      TransactionFun()
  end.

-spec convert({ok, [term()]}) ->
  term().
convert({ok, []}) ->
  not_found;
convert({ok, [R]}) ->
  R.


-spec unpack_mnesia_result({atomic, term()} | {aborted, term()}) ->
  ok | {ok | error, term()}.
unpack_mnesia_result({atomic, ok}) ->
  ok;
unpack_mnesia_result({atomic, Result}) ->
  {ok, Result};
unpack_mnesia_result({aborted, {throw, Error}}) ->
  {error, Error};
unpack_mnesia_result({aborted, {Error, [_ | _]}}) ->
  {error, Error};
unpack_mnesia_result({aborted, Error}) ->
  {error, Error}.