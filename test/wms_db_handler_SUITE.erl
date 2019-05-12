%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Test suites for wms_db_handler_module.
%%% @end
%%% Created : 29. Apr 2019 08:12
%%%-------------------------------------------------------------------
-module(wms_db_handler_SUITE).
-author("Attila Makra").

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("wms_db.hrl").

-define(TEST_NODES, [t1, t2]).
-define(TEST_RECORD, testrec).
-record(testrec, {name :: string(), age :: pos_integer()}).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
  [{key, value}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  [{key, value} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%% Reason = term()
%%   The reason for skipping all test cases and subgroups in the group.
%%
%% Description: Initialization before each test case group.
%%--------------------------------------------------------------------
init_per_group(GroupName, Config) ->
  ?MODULE:GroupName({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%%
%% GroupName = atom()
%%   Name of the test case group that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding configuration data for the group.
%%
%% Description: Cleanup after each test case group.
%%--------------------------------------------------------------------
end_per_group(GroupName, Config) ->
  ?MODULE:GroupName({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({prelude, Config}).

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({postlude, Config}).

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%%
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%%   The name of the group.
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Group properties that may be combined.
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%%   The name of a test case.
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%%
%% Description: Returns a list of test case group definitions.
%%--------------------------------------------------------------------
groups() ->
  [
    {table_create_group,
     [{repeat_until_any_fail, 1}],
     [
       init_test,
       create_delete_table_test,
       read_write_delete_test,
       read_write_kv_test
     ]
    }
  ].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%%
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%%   Name of a test case group.
%% TestCase = atom()
%%   Name of a test case.
%% Reason = term()
%%   The reason for skipping all groups and test cases.
%%
%% Description: Returns the list of groups and test cases that
%%              are to be executed.
%%--------------------------------------------------------------------
all() ->
  [
    {group, table_create_group}
  ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%% Comment = term()
%%   A comment about the test case that will be printed in the html log.
%%
%% Description: Test case function. (The name of it must be specified in
%%              the all/0 list or in a test case group for the test case
%%              to be executed).
%%--------------------------------------------------------------------

%% =============================================================================
%% Table create group
%% =============================================================================

table_create_group({prelude, Config}) ->
  SaveMode = os:getenv("wms_mode"),
  os:putenv("wms_mode", "multi_test"),

  ok = wms_test:start_nodes(?TEST_NODES, [{env, [{"wms_mode", "multi_test"}]}]),
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  ok = wms_test:start_application(?APP_NAME),
  [{started, StartedApps}, {save_mode, SaveMode} | Config];
table_create_group({postlude, Config}) ->
  delete_tables(),
  StartedApps = ?config(started, Config),
  [application:stop(App) || App <- StartedApps],
  ok = wms_test:stop_nodes(?TEST_NODES),
  wms_test:stop_application(?APP_NAME),
  os:putenv("wms_mode", ?config(save_mode, Config)),
  ok.

%%--------------------------------------------------------------------
%% Initialization test
%%
%%--------------------------------------------------------------------

%% test case information
init_test({info, _Config}) ->
  ["Initialization test"];
init_test(suite) ->
  ok;
%% init test case
init_test({prelude, Config}) ->
  Config;
%% destroy test case
init_test({postlude, _Config}) ->
  ok;
%% test case implementation
init_test(_Config) ->
  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)).

%%--------------------------------------------------------------------
%% Create and delete table tests.
%%
%%--------------------------------------------------------------------

%% test case information
create_delete_table_test({info, _Config}) ->
  ["Create and delete table test"];
create_delete_table_test(suite) ->
  ok;
%% init test case
create_delete_table_test({prelude, Config}) ->
  Config;
%% destroy test case
create_delete_table_test({postlude, _Config}) ->
  delete_tables(),
  ok;
%% test case implementation
create_delete_table_test(_Config) ->
  Nodes = [node() | nodes()],

  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),

  % create tables on all nodes

  ok = wms_db_handler:create_table(tab1, set, ?TEST_RECORD,
                                   record_info(fields, ?TEST_RECORD)),
  assert_table_exists(Nodes, tab1, true),

  % delete table on all nodes
  ok = wms_db_handler:delete_table(tab1),
  assert_table_exists(Nodes, tab1, false),

  % create key-value storage table
  ok = wms_db_handler:create_kv_table(tab2, set),
  assert_table_exists(Nodes, tab2, true).

%%--------------------------------------------------------------------
%% Read, write, delete tests
%%
%%--------------------------------------------------------------------

%% test case information
read_write_delete_test({info, _Config}) ->
  [""];
read_write_delete_test(suite) ->
  ok;
%% init test case
read_write_delete_test({prelude, Config}) ->
  Config;
%% destroy test case
read_write_delete_test({postlude, _Config}) ->
  delete_tables(),
  ok;
%% test case implementation
read_write_delete_test(_Config) ->

  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),

  ok = wms_db_handler:create_table(tab1, set, ?TEST_RECORD,
                                   record_info(fields, ?TEST_RECORD)),

  ?assertEqual({ok, []}, wms_db_handler:read(tab1, not_found_key)),

  Record1 = #testrec{name = "Little John", age = 30},

  % write
  ?assertEqual(ok, wms_db_handler:transaction(
    fun() ->
      ok = wms_db_handler:write(tab1, Record1)
    end)),

  % dirty read
  ?assertEqual({ok, [Record1]}, wms_db_handler:read(tab1, "Little John")),

  % transactional read

  ?assertEqual({ok, Record1}, wms_db_handler:transaction(
    fun() ->
      {ok, [Record1]} = wms_db_handler:read(tab1, "Little John"),
      Record1
    end)),

  % modify age
  Record2 = Record1#testrec{age = 31},
  % write
  ?assertEqual(ok, wms_db_handler:transaction(
    fun() ->
      ok = wms_db_handler:write(tab1, Record2)
    end)),
  % dirty read
  ?assertEqual({ok, [Record2]}, wms_db_handler:read(tab1, "Little John")),

  % delete
  ?assertEqual(ok, wms_db_handler:transaction(
    fun() ->
      ok = wms_db_handler:delete(tab1, "Little John")
    end)),

  % not found
  ?assertEqual({ok, []}, wms_db_handler:read(tab1, "Little John")).


%%--------------------------------------------------------------------
%% Read, write key-value storage test
%%
%%--------------------------------------------------------------------

%% test case information
read_write_kv_test({info, _Config}) ->
  [""];
read_write_kv_test(suite) ->
  ok;
%% init test case
read_write_kv_test({prelude, Config}) ->
  Config;
%% destroy test case
read_write_kv_test({postlude, _Config}) ->
  delete_tables(),
  ok;
%% test case implementation
read_write_kv_test(_Config) ->
  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),

  ok = wms_db_handler:create_kv_table(tab1, set),

  % not found
  ?assertEqual({ok, []}, wms_db_handler:read_kv(tab1, key1)),

  % write
  ?assertEqual(ok, wms_db_handler:transaction(
    fun() ->
      ok = wms_db_handler:write_kv(tab1, key1, key1_value1)
    end)),

  % dirty read
  ?assertEqual({ok, [key1_value1]}, wms_db_handler:read_kv(tab1, key1)),

  ok.

%% =============================================================================
%% Private test functions
%% =============================================================================

% Remove db tables from mnesia
delete_tables() ->
  [{atomic, ok} = mnesia:delete_table(Table) ||
    Table <- mnesia:system_info(tables),
   Table =/= schema].

assert_table_exists(Nodes, Table, IsExists) ->
  Results = wms_test:rpc_call(Nodes, mnesia, system_info, [tables]),
  ?assertEqual(length(Nodes), length(Results)),
  [?assert(IsExists =:= lists:member(Table, NodeResult)) || NodeResult <- Results,
   is_list(NodeResult)].