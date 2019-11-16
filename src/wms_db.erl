%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% API functions for wms_db
%%% @end
%%% Created : 02. May 2019 19:56
%%%-------------------------------------------------------------------
-module(wms_db).
-author("Attila Makra").
-include_lib("wms_common/include/wms_common.hrl").
-include("wms_db_data_taskdef.hrl").
-include("wms_db.hrl").

%% API
-export([get_global_variable/1,
         set_global_variable/2,
         variable_transaction/2,
         save_private_state/2,
         add_event/2,
         remove_event/2,
         get_event/1,
         add_subscriber/3,
         remove_subscriber/3,
         get_subscribers/2,
         add_taskdef/3,
         remove_taskdef/1,
         get_taskdef/1,
         get_taskdefs/0,
         add_task_instance/2,
         remove_task_instance/2,
         get_task_instances/1,
         get_taskdef_instances/0,
         remove_private_state/1,
         load_private_state/1,
         set_task_instance_status/4,
         get_task_instance_status/1,
         filter_global_variables/1]).

%% =============================================================================
%% API functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Events
%% -----------------------------------------------------------------------------

% store fired event in database
-spec add_event(timestamp(), binary()) ->
  ok.
add_event(Timestamp, EventID) ->
  wms_db_data_events:add_event(Timestamp, EventID).

% remove fired event which is identified by timestamp
-spec remove_event(timestamp(), binary()) ->
  {ok, EventRemoved :: boolean()}.
remove_event(Timestamp, EventID) ->
  wms_db_data_events:remove_event(Timestamp, EventID).

% returns all ordered timestamps of given event.
-spec get_event(binary()) ->
  [timestamp()].
get_event(EventID) ->
  wms_db_data_events:get_event(EventID).

%% -----------------------------------------------------------------------------
%% Subscribers
%% -----------------------------------------------------------------------------

% add subscriber task for given event
-spec add_subscriber(timestamp(), binary(), binary()) ->
  ok.
add_subscriber(Timestamp, EventID, TaskInstanceID) ->
  wms_db_data_events:add_subscriber(Timestamp, EventID, TaskInstanceID).

% remove subscriber for given event
-spec remove_subscriber(timestamp(), binary(), binary()) ->
  {ok, SubscribtionRemoved :: boolean()}.
remove_subscriber(Timestamp, EventID, TaskInstanceID) ->
  wms_db_data_events:remove_subscriber(Timestamp, EventID, TaskInstanceID).

% return subscribers which was subscriber for event before
% timestamp of event firing
-spec get_subscribers(timestamp() | dont_care, binary()) ->
  [{timestamp(), binary(), binary()}].
get_subscribers(Timestamp, EventID) ->
  wms_db_data_events:get_subscribers(Timestamp, EventID).

%% -----------------------------------------------------------------------------
%% Variable handling.
%% -----------------------------------------------------------------------------

-spec get_global_variable(global_state_variable()) ->
  not_found | global_state_value().
get_global_variable(VariableName) ->
  wms_db_data_global_state:get(VariableName).

-spec filter_global_variables(binary()) ->
  {ok, map()}.
filter_global_variables(VariablePattern) ->
  wms_db_data_global_state:filter(VariablePattern).

-spec set_global_variable(global_state_value(), global_state_value()) ->
  ok.
set_global_variable(VariableName, Value) ->
  wms_db_data_global_state:set(VariableName, Value).

-spec variable_transaction(StartEnvironment :: map(),
                           Transaction :: fun()) ->
                            {ok, map()} | {error, term()}.
variable_transaction(StartEnvironment, Transaction) ->
  wms_db_handler:transaction(
    fun() ->
      Transaction(StartEnvironment)
    end).

-spec save_private_state(binary(), map()) ->
  ok | no_return().
save_private_state(TaskInstanceID, Environment) ->
  wms_db_data_priv_state:save(TaskInstanceID, Environment).

-spec remove_private_state(binary()) ->
  ok | no_return().
remove_private_state(TaskInstanceID) ->
  wms_db_data_priv_state:remove(TaskInstanceID).

-spec load_private_state(binary()) ->
  not_found | map().
load_private_state(TaskInstanceID) ->
  wms_db_data_priv_state:load(TaskInstanceID).

%% -----------------------------------------------------------------------------
%% Taskdef handling
%% -----------------------------------------------------------------------------

-spec add_taskdef(binary(), term(), taskdef_type()) ->
  ok.
add_taskdef(TaskName, Definition, Type) ->
  wms_db_data_taskdef:add_taskdef(TaskName, Definition, Type).

-spec remove_taskdef(binary()) ->
  ok.
remove_taskdef(TaskName) ->
  wms_db_data_taskdef:remove_taskdef(TaskName).

-spec get_taskdef(binary()) ->
  taskdef() | not_found.
get_taskdef(TaskName) ->
  wms_db_data_taskdef:get_taskdef(TaskName).

-spec get_taskdefs() ->
  [taskdef()].
get_taskdefs() ->
  wms_db_data_taskdef:get_taskdefs().

%% -----------------------------------------------------------------------------
%% Task handling
%% -----------------------------------------------------------------------------

-spec add_task_instance(binary(), binary()) ->
  ok.
add_task_instance(TaskName, TaskInstanceID) ->
  wms_db_data_tasks:add_task_instance(TaskName, TaskInstanceID).

-spec remove_task_instance(binary(), binary()) ->
  ok.
remove_task_instance(TaskName, TaskInstanceID) ->
  wms_db_data_tasks:remove_task_instance(TaskName, TaskInstanceID).

-spec get_task_instances(binary()) ->
  [binary()].
get_task_instances(TaskName) ->
  wms_db_data_tasks:get_task_instances(TaskName).

-spec get_taskdef_instances() ->
  [{taskdef(), [binary()]}].
get_taskdef_instances() ->
  TaskDefs = get_taskdefs(),

  lists:map(
    fun(#taskdef{task_name = Name} = TaskDef) ->
      {TaskDef, get_task_instances(Name)}
    end, TaskDefs).

%% -----------------------------------------------------------------------------
%% Task instance status handling
%% -----------------------------------------------------------------------------

-spec set_task_instance_status(binary(), binary(), task_status(), term()) ->
  ok.
set_task_instance_status(TaskInstanceID, TaskName, Status, Description) ->
  wms_db_data_task_instance_status:set_status(TaskInstanceID, TaskName,
                                              Status, Description).

-spec get_task_instance_status(binary()) ->
  task_instance_status() | not_found.
get_task_instance_status(TaskInstanceID) ->
  wms_db_data_task_instance_status:get_status(TaskInstanceID).