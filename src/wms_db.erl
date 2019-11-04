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
         get_subscribers/2]).

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
-spec get_subscribers(timestamp(), binary()) ->
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