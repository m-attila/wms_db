%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 18:34
%%%-------------------------------------------------------------------
-module(wms_db_inst_tasks_test).
-author("Attila Makra").

-include("../src/wms_db_inst_tasks.hrl").
-include("wms_db_datatypes.hrl").
-include_lib("eunit/include/eunit.hrl").

new_task_and_instance_test() ->
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID">>,

  Instance = wms_db_inst_tasks:new(TaskName, TaskInstanceID),
  #{fields := Fields} = Instance,
  ?assertEqual(TaskName, maps:get(task_name, Fields)),
  ?assertEqual(TaskInstanceID, maps:get(task_instance_id, Fields)).

new_task_record_test() ->
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID">>,

  Instance = #{new_record := NewRecord} = wms_db_inst_tasks:new(TaskName,
                                                                TaskInstanceID),
  Expected = #task{
    task_name = TaskName,
    instances = [TaskInstanceID]
  },

  Result = NewRecord(Instance, TaskInstanceID),
  ?assertEqual(Expected, Result).

add_task_instance_test() ->
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID">>,

  Instance = #{add_child := AddChild} = wms_db_inst_tasks:new(TaskName,
                                                              <<>>),


  Record = #task{
    task_name = TaskName,
    instances = []},

  Expected = Record#task{
    instances = [TaskInstanceID]
  },
  Result = AddChild(Instance, Record, TaskInstanceID),
  ?assertEqual(Expected, Result).

get_task_instances_test() ->
  % task without task instances
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID">>,

  Instance = #{get_children := GetChildren} = wms_db_inst_tasks:new(TaskName,
                                                                    TaskInstanceID),

  Record = #task{
    task_name = TaskName,
    instances = [TaskInstanceID]},

  Expected = [TaskInstanceID],
  Result = GetChildren(Instance, Record),
  ?assertEqual(Expected, Result).

remove_task_instance_test() ->
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID">>,

  Instance = #{remove_child := RemoveChild} = wms_db_inst_tasks:new(TaskName,
                                                                    TaskInstanceID),

  Record = #task{
    task_name = TaskName,
    instances = [TaskInstanceID]
  },

  Expected = [],
  Result = RemoveChild(Instance, Record#task.instances),
  ?assertEqual(Expected, Result).

set_task_instances_test() ->
  TaskName = <<"task1">>,
  Instance = #{set_children := SetChildren} = wms_db_inst_tasks:new(TaskName,
                                                                    <<>>),

  Record = #task{
    task_name = TaskName,
    instances = []
  },

  TaskInstances = [<<"ID1">>, <<"ID2">>],
  Expected = Record#task{instances = TaskInstances},
  Result = SetChildren(Instance, Record, TaskInstances),
  ?assertEqual(Expected, Result).

filter_task_instances_test() ->
  TaskName = <<"task1">>,
  TaskInstanceID = <<"ID1">>,

  Instance = #{filter_children := FilterChildren} =
             wms_db_inst_tasks:new(TaskName,
                                   TaskInstanceID),

  TaskInstances = [<<"ID1">>, <<"ID2">>],
  Expected = [TaskInstanceID],
  Result = FilterChildren(Instance, TaskInstances),
  ?assertEqual(Expected, Result).
