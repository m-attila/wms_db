%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% DB storage for PrivateState
%%% @end
%%% Created : 03. May 2019 08:48
%%%-------------------------------------------------------------------
-module(wms_db_data_priv_state).
-author("Attila Makra").

-include("wms_db_data_priv_state.hrl").

%% API
-export([create/1, save/2, load/1, remove/1]).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% create/1
%% ###### Purpose
%% Create private state storage.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec create([node()]) ->
  ok | {error, term()}.
create(Nodes) ->
  wms_db_handler:create_kv_table(?TABLE_NAME, set, Nodes).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% save/2
%% ###### Purpose
%% Save private state
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec save(binary(), map()) ->
  ok | no_return().
save(TaskInstanceID, State) ->
  Transaction =
    fun() ->
      wms_db_handler:write_kv(?TABLE_NAME, TaskInstanceID, State)
    end,
  wms_db_handler:transaction(Transaction).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% load/1
%% ###### Purpose
%% Load private state
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec load(binary()) ->
  not_found | map().
load(TaskInstanceID) ->
  case wms_db_handler:read_kv(?TABLE_NAME, TaskInstanceID) of
    {ok, []} ->
      not_found;
    {ok, [State]} ->
      State
  end.


%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove/1
%% ###### Purpose
%% Remove private state
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec remove(binary()) ->
  ok.
remove(TaskInstanceID) ->
  Transaction =
    fun() ->
      wms_db_handler:delete(?TABLE_NAME, TaskInstanceID)
    end,
  wms_db_handler:transaction(Transaction).