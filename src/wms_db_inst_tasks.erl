%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 14:16
%%%-------------------------------------------------------------------
-module(wms_db_inst_tasks).
-author("Attila Makra").

-include("wms_db_data.hrl").
-include("wms_db_datatypes.hrl").
-include("wms_db_inst_tasks.hrl").

%% API
-export([create_table/0,
         new/2]).

%% =============================================================================
%% Private types
%% =============================================================================

-type task_instance() :: data_instance(task(), task_instance_id()).

%% =============================================================================
%% API functions
%% =============================================================================

-spec create_table() ->
  ok | {error, term()}.
create_table() ->
  wms_db_handler:create_table(
    ?TASKS_TABLE_NAME,
    set,
    task,
    record_info(fields, task)).

-spec new(binary(), task_instance_id() | undefined) ->
  task_instance().
new(TaskName, TaskInstanceID) ->
  Data = #{task_name => TaskName,
           task_instance_id => TaskInstanceID},

  #{
    fields => Data,
    read_record => fun read_task/2,
    new_child => fun new_task_instance/1,
    new_record => fun new_task/2,
    add_child => fun add_task_instance/3,
    write_record => fun write_task/2,
    get_children => fun get_task_instances/2,
    remove_child => fun remove_task_instance/2,
    delete_record =>  fun delete_task/2,
    set_children => fun set_task_instances/3,
    filter_children => fun filter_task_instances/2
  }.

%% =============================================================================
%% Method implementation
%% =============================================================================

-spec read_task(task_instance(), read | write) ->
  task() | not_found.
read_task(#{fields :=#{task_name := TaskName}}, LockingMode) ->
  wms_db_handler:convert(wms_db_handler:read(?TASKS_TABLE_NAME,
                                             TaskName,
                                             LockingMode)).

-spec new_task_instance(task_instance()) ->
  task_instance_id().
new_task_instance(#{fields := #{task_instance_id := TaskInstanceID}}) ->
  TaskInstanceID.

-spec new_task(task_instance(), task_instance_id()) ->
  task().
new_task(#{fields := #{task_name := TaskName}}, TaskInstanceID) ->
  #task{
    task_name = TaskName,
    instances = [TaskInstanceID]
  }.

-spec add_task_instance(task_instance(), task(), task_instance_id()) ->
  task().
add_task_instance(_, Record, TaskInstanceID) ->
  OldInstances = Record#task.instances,
  Record#task{
    instances = [TaskInstanceID | OldInstances]
  }.

-spec write_task(task_instance(), task()) ->
  ok.
write_task(_, Record) ->
  wms_db_handler:write(?TASKS_TABLE_NAME, Record).

-spec get_task_instances(task_instance(), task()) ->
  [task_instance_id()].
get_task_instances(_, #task{instances = Instances}) ->
  Instances.

-spec remove_task_instance(task_instance(), [task_instance_id()]) ->
  [task_instance_id()].
remove_task_instance(#{fields := #{task_instance_id := TaskInstanceID}},
                     TaskInstances) ->
  lists:delete(TaskInstanceID, TaskInstances).

-spec delete_task(task_instance(), task()) ->
  ok.
delete_task(_, #task{task_name = TaskName}) ->
  wms_db_handler:delete(?TASKS_TABLE_NAME, TaskName).

-spec set_task_instances(task_instance_id(), task(), [task_instance_id()]) ->
  task().
set_task_instances(_, Task, TaskInstanceIDs) ->
  Task#task{instances = TaskInstanceIDs}.

-spec filter_task_instances(task_instance(), [task_instance_id()]) ->
  [task_instance_id()].
filter_task_instances(#{fields := #{task_instance_id := TaskInstanceId}},
                      TaskInstances) ->
  lists:filter(
    fun(PTaskInstanceID) ->
      TaskInstanceId =:= undefined orelse TaskInstanceId =:= PTaskInstanceID
    end, TaskInstances).