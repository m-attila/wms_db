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
-include("wms_db.hrl").
-include("../src/wms_db_inst_events.hrl").
-include("../src/wms_db_inst_subscribers.hrl").
-include("../src/wms_db_data_taskdef.hrl").
-include("../src/wms_db_inst_tasks.hrl").

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
    },
    {state_group,
     [{repeat_until_any_fail, 1}],
     [
       private_state_test,
       global_state_test
     ]
    },
    {task_group,
     [{repeat_until_any_fail, 1}],
     [
       taskdef_test,
       task_test,
       taskdef_task_test,
       task_status_test
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
    {group, events_group},
    {group, state_group},
    {group, task_group}
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
%% Events group
%% =============================================================================

events_group({prelude, Config}) ->
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  ok = wms_test:start_application(?APP_NAME),
  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),
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
  assert_table_exists([node()], ?EVENT_TABLE_NAME, true),
  assert_table_exists([node()], ?SUBS_TABLE_NAME, true),

  {_, ok} = mnesia:delete_table(?EVENT_TABLE_NAME),
  {_, ok} = mnesia:delete_table(?SUBS_TABLE_NAME),

  ?assertEqual(ok, wms_db_data_events:create()),

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
  {ok, false} = wms_db_data_events:remove_event(Ts1, Ev1),
  [Ts2] = wms_db_data_events:get_event(Ev1),

  % remove Ts3, what does not exists
  {ok, false} = wms_db_data_events:remove_event(Ts3, Ev1),
  [Ts2] = wms_db_data_events:get_event(Ev1),

  % remove Ts2
  {ok, true} = wms_db_data_events:remove_event(Ts2, Ev1),
  [] = wms_db_data_events:get_event(Ev1),

  % remove Ts2 again
  {ok, false} = wms_db_data_events:remove_event(Ts2, Ev1),
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
  {ok, false} = wms_db_data_events:remove_subscriber(TsBSub1, Ev, Task1),
  Result3 = wms_db_data_events:get_subscribers(TsEvent, Ev),
  Expected3 = [{TsBSub2, Ev, Task2}],
  ?assertEqual(Expected3, Result3),

  % remove subscriber 2
  {ok, false} = wms_db_data_events:remove_subscriber(TsBSub2, Ev, Task2),
  Result4 = wms_db_data_events:get_subscribers(TsEvent, Ev),
  Expected4 = [],
  ?assertEqual(Expected4, Result4),

  [TsEvent] = wms_db_data_events:get_event(Ev),

  % remove subscriber 3
  Result5 = wms_db_data_events:get_subscribers(TsEventLater, Ev),
  Expected5 = [{TsASub3, Ev, Task3}],
  ?assertEqual(Expected5, Result5),

  {ok, true} = wms_db_data_events:remove_subscriber(TsASub3, Ev, Task3),
  Result6 = wms_db_data_events:get_subscribers(TsEventLater, Ev),
  Expected6 = [],
  ?assertEqual(Expected6, Result6),

  % delete event
  {ok, true} = wms_db_data_events:remove_event(TsEvent, Ev),
  [] = wms_db_data_events:get_event(Ev),

  ok.

%% =============================================================================
%% State group
%% =============================================================================

state_group({prelude, Config}) ->
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  ok = wms_test:start_application(?APP_NAME),
  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),
  [{started, StartedApps} | Config];
state_group({postlude, Config}) ->
  delete_tables(),
  StartedApps = ?config(started, Config),
  [application:stop(App) || App <- StartedApps],
  wms_test:stop_application(?APP_NAME).

%%--------------------------------------------------------------------
%% Private state tests
%%
%%--------------------------------------------------------------------

%% test case information
private_state_test({info, _Config}) ->
  [""];
private_state_test(suite) ->
  ok;
%% init test case
private_state_test({prelude, Config}) ->
  ok = wms_db_data_priv_state:create(),
  Config;
%% destroy test case
private_state_test({postlude, _Config}) ->
  ok;
%% test case implementation
private_state_test(_Config) ->
  TaskId = <<"task01">>,

  % TaskInstanceID does not exists
  ?assertEqual(not_found, wms_db_data_priv_state:load(TaskId)),

  % Add private state
  State1 = #{key => value},
  ?assertEqual(ok, wms_db_data_priv_state:save(TaskId, State1)),

  % Read state
  Result1 = wms_db_data_priv_state:load(TaskId),
  ?assertEqual(State1, Result1),

  % Modify state
  State2 = State1#{key1 => value1},
  ?assertEqual(ok, wms_db_data_priv_state:save(TaskId, State2)),

  Result2 = wms_db_data_priv_state:load(TaskId),
  ?assertEqual(State2, Result2),

  % Delete sate
  ok = wms_db_data_priv_state:remove(TaskId),
  ?assertEqual(not_found, wms_db_data_priv_state:load(TaskId)).

%%--------------------------------------------------------------------
%% Globale state tests
%%
%%--------------------------------------------------------------------

%% test case information
global_state_test({info, _Config}) ->
  [""];
global_state_test(suite) ->
  ok;
%% init test case
global_state_test({prelude, Config}) ->
  ok = wms_db_data_global_state:create(),
  Config;
%% destroy test case
global_state_test({postlude, _Config}) ->
  ok;
%% test case implementation
global_state_test(_Config) ->
  SetFun =
    fun(_, NewValue) ->
      NewValue
    end,
  AddFun =
    fun(O1, O2) ->
      O1 + O2
    end,

  Var1 = <<"var1">>,
  Var2 = <<"var2">>,
  Var3 = <<"var3">>,

  % variable not found,
  ?assertEqual(not_found, wms_db_data_global_state:get(Var1)),

  % set value of var1
  ?assertEqual(ok, wms_db_data_global_state:set(Var1, 255)),
  ?assertEqual(255, wms_db_data_global_state:get(Var1)),

  ?assertEqual(ok, wms_db_data_global_state:modify(Var1, SetFun, 12)),
  ?assertEqual(12, wms_db_data_global_state:get(Var1)),

  % add literal to var1
  ?assertEqual(ok, wms_db_data_global_state:modify(Var1, AddFun, 20)),
  ?assertEqual(12 + 20, wms_db_data_global_state:get(Var1)),

  % set var2
  ?assertEqual(ok, wms_db_data_global_state:modify(Var2, SetFun, 100)),
  ?assertEqual(ok, wms_db_data_global_state:modify(Var1, AddFun, {global, Var2})),
  ?assertEqual(12 + 20 + 100, wms_db_data_global_state:get(Var1)),

  % set var3 from var1 + var2
  ?assertEqual(ok, wms_db_data_global_state:modify(Var1, SetFun, 100)),
  ?assertEqual(ok, wms_db_data_global_state:modify(Var2, SetFun, 200)),
  ?assertEqual(ok, wms_db_data_global_state:modify({global, Var1}, Var3,
                                                   AddFun, {global, Var2})),
  ?assertEqual(100 + 200, wms_db_data_global_state:get(Var3)),

  % set var3 from literal + var2
  ?assertEqual(ok, wms_db_data_global_state:modify(Var2, SetFun, 200)),
  ?assertEqual(ok, wms_db_data_global_state:modify(300, Var3,
                                                   AddFun, {global, Var2})),
  ?assertEqual(300 + 200, wms_db_data_global_state:get(Var3)),

  % remove var1
  ?assertEqual(ok, wms_db_data_global_state:remove(Var1)),
  ?assertEqual(not_found, wms_db_data_global_state:get(Var1)),

  ok.


%% =============================================================================
%% Taskdef group
%% =============================================================================

task_group({prelude, Config}) ->
  {ok, StartedApps} = application:ensure_all_started(?APP_NAME),
  ok = wms_test:start_application(?APP_NAME),
  ?assertEqual(ok, wms_db_handler_service:wait_for_initialized(3000)),
  [{started, StartedApps} | Config];
task_group({postlude, Config}) ->
  delete_tables(),
  StartedApps = ?config(started, Config),
  [application:stop(App) || App <- StartedApps],
  wms_test:stop_application(?APP_NAME).

%%--------------------------------------------------------------------
%% Tests for taskdef table
%%
%%--------------------------------------------------------------------

%% test case information
taskdef_test({info, _Config}) ->
  [""];
taskdef_test(suite) ->
  ok;
%% init test case
taskdef_test({prelude, Config}) ->
  ok = wms_db_data_taskdef:create(),
  Config;
%% destroy test case
taskdef_test({postlude, _Config}) ->
  ok;
%% test case implementation
taskdef_test(_Config) ->
  TaskName = <<"task1">>,
  Definition = def1,
  Type = auto,

  % create new taskdef
  ok = wms_db_data_taskdef:add_taskdef(TaskName, Definition, Type),

  % read record
  Expected = #taskdef{
    task_name  = TaskName,
    definition = Definition,
    type       = Type
  },

  Result = wms_db_data_taskdef:get_taskdef(TaskName),
  ?assertEqual(Expected, Result),

  % record does not exists
  not_found = wms_db_data_taskdef:get_taskdef(<<"notask">>),

  % modify record
  Definition1 = def2,
  Type1 = false,

  Expected1 = #taskdef{
    task_name  = TaskName,
    definition = Definition1,
    type       = Type1
  },

  ok = wms_db_data_taskdef:add_taskdef(TaskName, Definition1, Type1),

  Result1 = wms_db_data_taskdef:get_taskdef(TaskName),
  ?assertEqual(Expected1, Result1),

  % return all taskdef records
  ?assertEqual([Expected1],
               wms_db_data_taskdef:get_taskdefs()),

  % remove record
  ok = wms_db_data_taskdef:remove_taskdef(TaskName),
  % record does not exists
  not_found = wms_db_data_taskdef:get_taskdef(TaskName).

%%--------------------------------------------------------------------
%% Tests for task table
%%
%%--------------------------------------------------------------------

%% test case information
task_test({info, _Config}) ->
  [""];
task_test(suite) ->
  ok;
%% init test case
task_test({prelude, Config}) ->
  ok = wms_db_data_tasks:create(),
  Config;
%% destroy test case
task_test({postlude, _Config}) ->
  ok;
%% test case implementation
task_test(_Config) ->
  TaskName = <<"task1">>,

  % no task instances
  Expected = [],
  Result = wms_db_data_tasks:get_task_instances(TaskName),
  ?assertEqual(Expected, Result),

  % add task instance
  TaskInstanceID1 = <<"ID1">>,
  ok = wms_db_data_tasks:add_task_instance(TaskName, TaskInstanceID1),
  Expected1 = [TaskInstanceID1],
  Result1 = wms_db_data_tasks:get_task_instances(TaskName),
  ?assertEqual(Expected1, Result1),

  % add another task instance
  TaskInstanceID2 = <<"ID2">>,
  ok = wms_db_data_tasks:add_task_instance(TaskName, TaskInstanceID2),
  Expected2 = [TaskInstanceID2, TaskInstanceID1],
  Result2 = wms_db_data_tasks:get_task_instances(TaskName),
  ?assertEqual(Expected2, Result2),

  % remove instance2
  ok = wms_db_data_tasks:remove_task_instance(TaskName, TaskInstanceID2),
  Expected3 = [TaskInstanceID1],
  Result3 = wms_db_data_tasks:get_task_instances(TaskName),
  ?assertEqual(Expected3, Result3),

  % remove instance1
  ok = wms_db_data_tasks:remove_task_instance(TaskName, TaskInstanceID1),
  Expected4 = [],
  Result4 = wms_db_data_tasks:get_task_instances(TaskName),
  ?assertEqual(Expected4, Result4),

  ?assertEqual([], mnesia:dirty_all_keys(?TASKS_TABLE_NAME)).

%%--------------------------------------------------------------------
%% Taskdef and Taskinstance tests
%%
%%--------------------------------------------------------------------

%% test case information
taskdef_task_test({info, _Config}) ->
  [""];
taskdef_task_test(suite) ->
  ok;
%% init test case
taskdef_task_test({prelude, Config}) ->
  ok = wms_db_data_tasks:create(),
  ok = wms_db_data_taskdef:create(),
  Config;
%% destroy test case
taskdef_task_test({postlude, _Config}) ->
  ok;
%% test case implementation
taskdef_task_test(_Config) ->
  TaskName1 = <<"tn1">>,
  TaskID1 = <<"tn1_1">>,
  TaskID2 = <<"tn1_2">>,

  wms_db:add_taskdef(TaskName1, def1, auto),
  wms_db:add_task_instance(TaskName1, TaskID1),
  wms_db:add_task_instance(TaskName1, TaskID2),

  TaskName2 = <<"tn2">>,
  wms_db:add_taskdef(TaskName2, def2, manual),

  Expected = [
               {#taskdef{task_name  = TaskName2,
                         definition = def2,
                         type       = manual}, []},
               {#taskdef{task_name  = TaskName1,
                         definition = def1,
                         type       = auto}, [TaskID2, TaskID1]}
             ],
  Result = wms_db:get_taskdef_instances(),
  ?assertEqual(Expected, Result).

%%--------------------------------------------------------------------
%% Tests for task status
%%
%%--------------------------------------------------------------------

%% test case information
task_status_test({info, _Config}) ->
  [""];
task_status_test(suite) ->
  ok;
%% init test case
task_status_test({prelude, Config}) ->
  ok = wms_db_data_task_instance_status:create(),
  Config;
%% destroy test case
task_status_test({postlude, _Config}) ->
  ok;
%% test case implementation
task_status_test(_Config) ->
  TaskInstanceID = <<"I1">>,
  TaskName = <<"task1">>,
  Status = wait,
  Description = for_events,

  ?assertEqual(not_found,
               wms_db_data_task_instance_status:get_status(TaskInstanceID)),

  % write first status record

  ok = wms_db_data_task_instance_status:set_status(TaskInstanceID,
                                                   TaskName,
                                                   Status,
                                                   Description),

  Result1 =
    wms_db_data_task_instance_status:get_status(TaskInstanceID),


  ?assertEqual(TaskInstanceID, Result1#task_instance_status.task_instance),
  ?assertEqual(TaskName, Result1#task_instance_status.task_name),
  ?assertEqual(Status, Result1#task_instance_status.status),
  ?assertEqual(Description, Result1#task_instance_status.description),
  ?assertEqual(
    [
      {Result1#task_instance_status.timestamp,
       undefined,
       Status,
       Description}
    ],
    Result1#task_instance_status.history),

  % add new entry

  Status1 = done,
  Description1 = <<"end">>,

  ok = wms_db_data_task_instance_status:set_status(TaskInstanceID,
                                                   TaskName,
                                                   Status1,
                                                   Description1),

  Result2 =
    wms_db_data_task_instance_status:get_status(TaskInstanceID),

  ?assertEqual(TaskInstanceID, Result2#task_instance_status.task_instance),
  ?assertEqual(TaskName, Result2#task_instance_status.task_name),
  ?assertEqual(Status1, Result2#task_instance_status.status),
  ?assertEqual(Description1, Result2#task_instance_status.description),
  ?assertEqual(
    [
      % second entry
      {Result2#task_instance_status.timestamp,
       undefined,
       Status1,
       Description1},

      % first entry
      {Result1#task_instance_status.timestamp,
       Result2#task_instance_status.timestamp,
       Status,
       Description}
    ],
    Result2#task_instance_status.history),

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