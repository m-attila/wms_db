%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Task definicios tabla
%%% @end
%%% Created : 04. Nov 2019 22:33
%%%-------------------------------------------------------------------
-module(wms_db_data_taskdef).
-author("Attila Makra").

-include("wms_db_datatypes.hrl").
-include("wms_db_data_taskdef.hrl").

%% API
-export([create/0,
         add_taskdef/3,
         remove_taskdef/1,
         get_taskdef/1,
         get_taskdefs/0]).

%% =============================================================================
%% Table creations.
%% =============================================================================

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% create/1
%% ###### Purpose
%% Create taskdef table
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
  ok = wms_db_handler:create_table(
    ?TASKDEF_TABLE_NAME,
    set,
    taskdef, record_info(fields, taskdef)).

%% =============================================================================
%% Table handling
%% =============================================================================

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add_taskdef/3
%% ###### Purpose
%% Insert or update taskdef record
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec add_taskdef(binary(), term(), taskdef_type()) ->
  ok.
add_taskdef(TaskName, Definition, Type) ->
  ok = wms_db_handler:transaction(
    fun() ->
      wms_db_handler:write(?TASKDEF_TABLE_NAME,
                           #taskdef{
                             task_name  = TaskName,
                             definition = Definition,
                             type       = Type
                           })
    end).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove/1
%% ###### Purpose
%% Remove taskdef record
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec remove_taskdef(binary()) ->
  ok.
remove_taskdef(TaskName) ->
  ok = wms_db_handler:transaction(
    fun() ->
      wms_db_handler:delete(?TASKDEF_TABLE_NAME, TaskName)
    end).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_taskdef/1
%% ###### Purpose
%% Read taskdef record
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_taskdef(binary()) ->
  taskdef() | not_found.
get_taskdef(TaskName) ->
  case wms_db_handler:read(?TASKDEF_TABLE_NAME, TaskName) of
    {ok, [Ret]} ->
      Ret;
    {ok, []} ->
      not_found
  end.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_taskdefs/0
%% ###### Purpose
%% Returns all taskdef records
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_taskdefs() ->
  [taskdef()].
get_taskdefs() ->
  {ok, Result} =
    wms_db_handler:transaction(
      fun() ->
        mnesia:foldr(
          fun(Record, Acc) ->
            [Record | Acc]
          end, [], ?TASKDEF_TABLE_NAME
        )
      end),
  Result.
