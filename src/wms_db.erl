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
-include("wms_db.hrl").

%% API
-export([load_config/0,
         get_global_variable/1,
         set_global_variable/2,
         variable_transaction/2, save_private_state/2]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec load_config() ->
  ok.
load_config() ->
  load_config(wms_cfg:get(?APP_NAME, load_config, true)).

-spec get_global_variable(global_state_variable()) ->
  not_found | global_state_value().
get_global_variable(VariableName) ->
  wms_db_data_global_state:get(VariableName).

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


%% =============================================================================
%% Private functions
%% =============================================================================

-spec load_config(boolean()) ->
  ok.
load_config(true) ->
  Path = filename:join(code:priv_dir(?APP_NAME), "wms_db.config"),
  ok = wms_cfg:overload_config(wms_cfg:get_mode(), [Path]);
load_config(_) ->
  ok.
