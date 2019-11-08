%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2019 22:32
%%%-------------------------------------------------------------------
-module(wms_db_data_tasks).
-author("Attila Makra").

-include("wms_db_inst_tasks.hrl").

%% API
-export([create/0,
         add_task_instance/2,
         remove_task_instance/2,
         get_task_instances/1]).

%% =============================================================================
%% Table creation
%% =============================================================================

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% create/1
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec create() ->
  ok.
create() ->
  ok = wms_db_inst_tasks:create_table().

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add_task_instance/2
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end

-spec add_task_instance(binary(), binary()) ->
  ok.
add_task_instance(TaskName, TaskInstanceID) ->
  Instance = wms_db_inst_tasks:new(TaskName, TaskInstanceID),
  wms_db_data:add_child_to_key(Instance).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove_task_instance/2
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end

-spec remove_task_instance(binary(), binary()) ->
  ok.
remove_task_instance(TaskName, TaskInstanceID) ->
  Instance = wms_db_inst_tasks:new(TaskName, TaskInstanceID),
  {ok, _} = wms_db_data:remove_child_from_key(Instance),
  ok.


%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_task_instances/1
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_task_instances(binary()) ->
  [binary()].
get_task_instances(TaskName) ->
  Instance = wms_db_inst_tasks:new(TaskName, undefined),
  wms_db_data:get_children_from_key(Instance).


