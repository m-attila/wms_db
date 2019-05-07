%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% DB storage for PrivateState
%%% @end
%%% Created : 03. May 2019 09:20
%%%-------------------------------------------------------------------
-module(wms_db_data_global_state).
-author("Attila Makra").

-include("wms_db_data_global_state.hrl").
%% API
-export([create/1,
         load/1,
         update/3,
         update/4,
         remove/1]).

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
%% read/1
%% ###### Purpose
%% Reads global value of global variable.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec load(global_state_variable()) ->
  term().
load(Variable) ->
  case wms_db_handler:read_kv(?TABLE_NAME, Variable) of
    {ok, []} ->
      not_found;
    {ok, [Value]} ->
      Value
  end.

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% modify/3
%% ###### Purpose
%% Modify value of global state' variable
%% ###### Arguments
%% 
%% ###### Returns
%% 
%%-------------------------------------------------------------------
%%
%% @end
-spec update(global_state_variable(),
             global_state_operator(),
             global_state_operand()) ->
              term().
update(Variable, Function, Operand) ->
  update(Variable, Variable, Function, Operand).

-spec update(global_state_variable(),
             global_state_variable(),
             global_state_operator(),
             global_state_operand()) ->
              term().
update(SourceVariable, DestinationVariable, Function, Operand) ->
  Transaction =
    fun() ->
      CurrentValue = load(SourceVariable),

      OperandValue =
        case Operand of
          {global, VariableName} ->
            load(VariableName);
          Other ->
            Other
        end,

      NewValue = Function(CurrentValue, OperandValue),
      wms_db_handler:write_kv(?TABLE_NAME, DestinationVariable, NewValue)
    end,
  wms_db_handler:transaction(Transaction).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove/1
%% ###### Purpose
%% Removes variable from global state.
%% ###### Arguments
%% 
%% ###### Returns
%% 
%%-------------------------------------------------------------------
%%
%% @end
-spec remove(binary()) ->
  ok.
remove(Variable) ->
  Transaction =
    fun() ->
      wms_db_handler:delete(?TABLE_NAME, Variable)
    end,
  wms_db_handler:transaction(Transaction).
