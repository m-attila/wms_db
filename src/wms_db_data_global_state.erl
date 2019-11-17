%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% DB storage for PrivateState
%%% @end
%%% Created : 03. May 2019 09:20
%%%-------------------------------------------------------------------
-module(wms_db_data_global_state).
-author("Attila Makra").

-compile({no_auto_import, [get/1]}).

-include("wms_db_handler.hrl").
-include("wms_db_data_global_state.hrl").
%% API
-export([create/0,
         get/1,
         set/2,
         modify/3,
         modify/4,
         remove/1,
         filter/1]).

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
-spec create() ->
  ok.
create() ->
  ok = wms_db_handler:create_kv_table(?TABLE_NAME, set).

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
-spec get(global_state_variable()) ->
  term().
get(Variable) ->
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
%% set/2
%% ###### Purpose
%% Set global state variable value
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec set(global_state_variable(), global_state_value()) ->
  ok.
set(Variable, Value) ->
  modify(Value, Variable, fun set_op/2, undefined).

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
-spec modify(global_state_variable(),
             global_state_operator(),
             global_state_value_or_reference()) ->
              ok.
modify(Variable, Function, Operand) ->
  modify({global, Variable}, Variable, Function, Operand).

-spec modify(global_state_value_or_reference(),
             global_state_variable(),
             global_state_operator(),
             global_state_value_or_reference()) ->
              ok.
modify(SourceVariable, DestinationVariable, Function, Operand) ->
  Transaction =
    fun() ->
      CurrentValue =
        case SourceVariable of
          {global, Name} ->
            get(Name);
          _ ->
            SourceVariable
        end,

      OperandValue =
        case Operand of
          {global, VariableName} ->
            get(VariableName);
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

-spec filter(binary()) ->
  {ok, map()}.
filter(<<>>) ->
  FilterFun =
    fun([#key_value_record{value = Value}]) ->
      {true, Value}
    end,
  do_filtering(FilterFun);
filter(BeginOfVariableName) ->
  FilterFun =
    fun([#key_value_record{key   = Key,
                          value = Value}]) ->
      case binary:match(Key, BeginOfVariableName) of
        {0, _} ->
          {true, Value};
        nomatch ->
          false
      end
    end,
  do_filtering(FilterFun).

%% =============================================================================
%% Private functions
%% =============================================================================

-spec set_op(term(), undefined) ->
  term().
set_op(Value, _) ->
  Value.

-spec do_filtering(filter_fun()) ->
  {ok, map()}.
do_filtering(FilterFun) ->
  wms_db_handler:filter(?TABLE_NAME, FilterFun).