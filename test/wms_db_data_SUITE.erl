%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Test suites for wms_db_data.
%%% @end
%%% Created : 29. Apr 2019 08:12
%%%-------------------------------------------------------------------
-module(wms_db_data_SUITE).
-author("Attila Makra").

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("wms_db_app.hrl").
-include("../src/wms_db_inst_events.hrl").
-include("../src/wms_db_inst_subscribers.hrl").

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
  application:ensure_all_started(?APP_NAME),
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
    {events_group,
     [{repeat_until_any_fail, 1}],
     [
       create_table_test,
       event_test,
       subscriber_test
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
    {group, events_group}
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

events_group({prelude, Config}) ->
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  ok = wms_test:start_application(?APP_NAME),
  [{started, StartedApps} | Config];
events_group({postlude, Config}) ->
  delete_tables(),
  StartedApps = ?config(started, Config),
  [application:stop(App) || App <- StartedApps],
  wms_test:stop_application(?APP_NAME).

%%--------------------------------------------------------------------
%% Create event and subscriber tables test
%%
%%--------------------------------------------------------------------

%% test case information
create_table_test({info, _Config}) ->
  ["Create event and subscriber tables test"];
create_table_test(suite) ->
  ok;
%% init test case
create_table_test({prelude, Config}) ->
  Config;
%% destroy test case
create_table_test({postlude, _Config}) ->
  ok;
%% test case implementation
create_table_test(_Config) ->
  assert_table_exists([node()], ?EVENT_TABLE_NAME, false),
  assert_table_exists([node()], ?SUBS_TABLE_NAME, false),

  ?assertEqual(ok, wms_db_data_events:create([node()])),

  assert_table_exists([node()], ?EVENT_TABLE_NAME, true),
  assert_table_exists([node()], ?SUBS_TABLE_NAME, true).

%%--------------------------------------------------------------------
%% Event test
%%
%%--------------------------------------------------------------------

%% test case information
event_test({info, _Config}) ->
  ["Event test"];
event_test(suite) ->
  ok;
%% init test case
event_test({prelude, Config}) ->
  Config;
%% destroy test case
event_test({postlude, _Config}) ->
  ok;
%% test case implementation
event_test(_Config) ->
  assert_table_exists([node()], ?EVENT_TABLE_NAME, true),

  Ts1 = wms_common:timestamp(),
  Ts2 = wms_common:add(Ts1, {millisecond, 1000}),
  Ts3 = wms_common:add(Ts1, {millisecond, 2000}),
  Ev1 = <<"event01">>,
  Ev2 = <<"event02">>,

  % add event
  ok = wms_db_data_events:add_event(Ts2, Ev1),
  ok = wms_db_data_events:add_event(Ts1, Ev1),

  % read event
  [Ts1, Ts2] = wms_db_data_events:get_event(Ev1),
  [] = wms_db_data_events:get_event(Ev2),

  % remove Ts1
  ok = wms_db_data_events:remove_event(Ts1, Ev1),
  [Ts2] = wms_db_data_events:get_event(Ev1),

  % remove Ts3, what does not exists
  ok = wms_db_data_events:remove_event(Ts3, Ev1),
  [Ts2] = wms_db_data_events:get_event(Ev1),

  % remove Ts2
  ok = wms_db_data_events:remove_event(Ts2, Ev1),
  [] = wms_db_data_events:get_event(Ev1),

  % remove Ts2 again
  ok = wms_db_data_events:remove_event(Ts2, Ev1),
  [] = wms_db_data_events:get_event(Ev1).

%%--------------------------------------------------------------------
%% Subscriber test
%%
%%--------------------------------------------------------------------

%% test case information
subscriber_test({info, _Config}) ->
  ["Subscriber test"];
subscriber_test(suite) ->
  ok;
%% init test case
subscriber_test({prelude, Config}) ->
  Config;
%% destroy test case
subscriber_test({postlude, _Config}) ->
  ok;
%% test case implementation
subscriber_test(_Config) ->
  assert_table_exists([node()], ?SUBS_TABLE_NAME, true),
  assert_table_exists([node()], ?EVENT_TABLE_NAME, true),

  % event timestamp
  TsEvent = wms_common:timestamp(),
  TsEventLater = wms_common:add(TsEvent, {millisecond, 1000}),

  % subscriber timestamp before event timestamp
  TsBSub1 = wms_common:add(TsEvent, {millisecond, -100}),
  TsBSub2 = wms_common:add(TsEvent, {millisecond, -200}),

  % subscriber after event timestamp
  TsASub3 = wms_common:add(TsEvent, {millisecond, 100}),

  Ev = <<"event01">>,
  Task1 = <<"task01">>,
  Task2 = <<"task02">>,
  Task3 = <<"task03">>,

  % add event first
  ok = wms_db_data_events:add_event(TsEvent, Ev),
  [TsEvent] = wms_db_data_events:get_event(Ev),

  % subscribe tasks
  ok = wms_db_data_events:add_subscriber(TsBSub1, Ev, Task1),
  ok = wms_db_data_events:add_subscriber(TsBSub2, Ev, Task2),
  ok = wms_db_data_events:add_subscriber(TsASub3, Ev, Task3),

  % get subscribed tasks before event timestamp
  Result1 = wms_db_data_events:get_subscribers(TsEvent, Ev),
  Expected1 = [{TsBSub2, Ev, Task2},
               {TsBSub1, Ev, Task1}],
  ?assertEqual(Expected1, Result1),

  % get subscribed tasks for later event
  Result2 = wms_db_data_events:get_subscribers(TsEventLater, Ev),
  Expected2 = [{TsASub3, Ev, Task3},
               {TsBSub2, Ev, Task2},
               {TsBSub1, Ev, Task1}],
  ?assertEqual(Expected2, Result2),

  % remove subscriber 1
  ok = wms_db_data_events:remove_subscriber(TsBSub1, Ev, Task1),
  Result3 = wms_db_data_events:get_subscribers(TsEvent, Ev),
  Expected3 = [{TsBSub2, Ev, Task2}],
  ?assertEqual(Expected3, Result3),

  % remove subscriber 2
  ok = wms_db_data_events:remove_subscriber(TsBSub2, Ev, Task2),
  Result4 = wms_db_data_events:get_subscribers(TsEvent, Ev),
  Expected4 = [],
  ?assertEqual(Expected4, Result4),

  [TsEvent] = wms_db_data_events:get_event(Ev),

  % remove subscriber 3
  Result5 = wms_db_data_events:get_subscribers(TsEventLater, Ev),
  Expected5 = [{TsASub3, Ev, Task3}],
  ?assertEqual(Expected5, Result5),

  ok = wms_db_data_events:remove_subscriber(TsASub3, Ev, Task3),
  Result6 = wms_db_data_events:get_subscribers(TsEventLater, Ev),
  Expected6 = [],
  ?assertEqual(Expected6, Result6),

  % delete event
  ok = wms_db_data_events:remove_event(TsEvent, Ev),
  [] = wms_db_data_events:get_event(Ev),

  ok.

%% =============================================================================
%% Private test functions
%% =============================================================================

% Remove db tables from mnesia
delete_tables() ->
  [                                      {atomic, ok} = mnesia:delete_table(Table) ||
    Table <- mnesia:system_info(tables), Table =/= schema].

assert_table_exists(Nodes, Table, IsExists) ->
  Results = wms_test:rpc_call(Nodes, mnesia, system_info, [tables]),
  ?assertEqual(length(Nodes), length(Results)),
  [?assert(IsExists =:= lists:member(Table, NodeResult)) || NodeResult <- Results,
   is_list(NodeResult)].