%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2019 12:38
%%%-------------------------------------------------------------------
-module(wms_db_data_task_instance_status).
-author("Attila Makra").

-include("wms_db_data_task_instance_status.hrl").
-include("wms_db_datatypes.hrl").

%% API
-export([create/0, set_status/4, get_status/1]).

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
%% Create task_instance_status table
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
    ?TABLE_NAME,
    set,
    task_instance_status,
    record_info(fields, task_instance_status)).

-spec set_status(binary(), binary(), task_status(), term()) ->
  ok.
set_status(TaskInstanceID, TaskName, Status, Description) ->
  TFun =
    fun() ->
      Now = wms_common:timestamp(),
      HistoryEntry = {Now, undefined, Status, Description},

      Record =
        case wms_db_handler:read(?TABLE_NAME, TaskInstanceID) of
          {ok, []} ->
            #task_instance_status{
              task_instance = TaskInstanceID,
              task_name     = TaskName,
              timestamp     = Now,
              status        = Status,
              description   = Description,
              history       = [HistoryEntry]
            };
          {ok, [Entry = #task_instance_status{
            history = [{LastStart, _, LastStatus, LastDecription} |
                       RestHistory]}]} ->

            NewLastHistory = {LastStart, Now, LastStatus, LastDecription},

            Entry#task_instance_status{
              timestamp   = Now,
              status      = Status,
              description = Description,
              history     = [HistoryEntry, NewLastHistory | RestHistory]
            }
        end,
      wms_db_handler:write(?TABLE_NAME, Record)
    end,
  ok = wms_db_handler:transaction(TFun).

-spec get_status(binary()) ->
  task_instance_status() | not_found.
get_status(TaskInstanceID) ->
  case wms_db_handler:read(?TABLE_NAME, TaskInstanceID) of
    {ok, [Ret]} ->
      Ret;
    {ok, []} ->
      not_found
  end.