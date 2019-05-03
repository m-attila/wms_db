%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Event data handler for WMS system.
%%% @end
%%% Created : 29. Apr 2019 18:08
%%%-------------------------------------------------------------------
-module(wms_db_data_events).
-author("Attila Makra").

-include("wms_db_data.hrl").
-include("wms_db_inst_subscribers.hrl").
-include("wms_db_inst_events.hrl").
%% API
-export([create/1,
         add_subscriber/3,
         remove_subscriber/3,
         get_subscribers/2,
         add_event/2,
         remove_event/2,
         get_event/1]).

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
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec create([node()]) ->
  ok.
create(Nodes) ->
  ok = wms_db_inst_subscribers:create_table(Nodes),
  ok = wms_db_inst_events:create_table(Nodes).

%% =============================================================================
%% Subscriber handling
%% =============================================================================

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add_subscriber/3
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec add_subscriber(timestamp(), binary(), binary()) ->
  ok.
add_subscriber(Timestamp, EventID, TaskInstanceID) ->
  Instance = wms_db_inst_subscribers:new(Timestamp,
                                         EventID,
                                         TaskInstanceID),
  wms_db_data:add_child_to_key(Instance).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove_subscriber/3
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end

-spec remove_subscriber(timestamp(), binary(), binary()) ->
  {ok, SubscribtionRemoved :: boolean()}.
remove_subscriber(Timestamp, EventID, TaskInstanceID) ->
  Instance = wms_db_inst_subscribers:new(Timestamp,
                                         EventID,
                                         TaskInstanceID),
  wms_db_data:remove_child_from_key(Instance).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_subscriber/2
%% ###### Purpose
%%
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_subscribers(timestamp(), binary()) ->
  [{timestamp(), binary(), binary()}].
get_subscribers(Timestamp, EventID) ->
  Instance = wms_db_inst_subscribers:new(Timestamp,
                                         EventID,
                                         undefined),
  Subscribers = wms_db_data:get_children_from_key(Instance),
  lists:map(
    fun(#subscriber{timestamp = TimestampI, task_instance_id = TaskInstanceID}) ->
      {TimestampI, EventID, TaskInstanceID}
    end, Subscribers).
%% =============================================================================
%% Event handling
%% =============================================================================

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add_event/2
%% ###### Purpose
%% Register fired event.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec add_event(timestamp(), binary()) ->
  ok.
add_event(Timestamp, EventID) ->
  Instance = wms_db_inst_events:new(Timestamp, EventID),
  wms_db_data:add_child_to_key(Instance).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% remove_event/2
%% ###### Purpose
%% Remove fired event.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec remove_event(timestamp(), binary()) ->
  {ok, EventRemoved :: boolean()}.
remove_event(Timestamp, EventID) ->
  Instance = wms_db_inst_events:new(Timestamp, EventID),
  wms_db_data:remove_child_from_key(Instance).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% get_event/1
%% ###### Purpose
%% Returns event timestamps by ordered.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec get_event(binary()) ->
  [timestamp()].
get_event(EventID) ->
  Instance = wms_db_inst_events:new(undefined, EventID),
  lists:sort(wms_db_data:get_children_from_key(Instance)).

