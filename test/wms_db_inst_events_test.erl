%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Unit tests for wms_db_inst_events
%%% @end
%%% Created : 30. Apr 2019 16:16
%%%-------------------------------------------------------------------
-module(wms_db_inst_events_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").
-include("../src/wms_db_inst_events.hrl").
-define(EVENT_ID, <<"event01">>).

%% =============================================================================
%% Test functions
%% =============================================================================

new_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = new_instance(Timestamp),

  #{fields := Fields} = Instance,
  ?assertEqual(Timestamp, maps:get(timestamp, Fields)),
  ?assertEqual(?EVENT_ID, maps:get(event_id, Fields)).

new_timestamp_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{new_child :=NewChild} = new_instance(Timestamp),
  Expected = Timestamp,
  Result = NewChild(Instance),
  ?assertEqual(Expected, Result).

new_event_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{new_record :=NewRecord} = new_instance(Timestamp),
  Expected = #event{event_id   = ?EVENT_ID,
                    mandatory  = false,
                    timestamps = [Timestamp]},
  Result = NewRecord(Instance, Timestamp),
  ?assertEqual(Expected, Result),

  Instance1 = #{new_record :=NewRecord} = wms_db_inst_events:new(
    Timestamp,
    {mandatory, ?EVENT_ID}),

  Expected1 = #event{event_id   = ?EVENT_ID,
                    mandatory  = true,
                    timestamps = [Timestamp]},
  Result1 = NewRecord(Instance1, Timestamp),
  ?assertEqual(Expected1, Result1).

add_timestamp_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{add_child :=AddChild} = new_instance(Timestamp),

  OldTimestamp = {native, 0},
  ValidTimestamp = wms_common:timestamp(),

  % new timestamp (old removed)

  Event = #event{
    event_id   = ?EVENT_ID,
    timestamps = [OldTimestamp, ValidTimestamp]
  },

  Result = AddChild(Instance, Event, Timestamp),
  Expected = Event#event{timestamps = [Timestamp, ValidTimestamp]},
  ?assertEqual(Expected, Result),

  % timestamp already exists
  Result1 = AddChild(Instance, Event, ValidTimestamp),
  Expected1 = Event#event{timestamps = [ValidTimestamp]},
  ?assertEqual(Expected1, Result1).

get_timestamps_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{get_children :=GetChildren} = new_instance(Timestamp),

  OldTimestamp = {native, 0},
  ValidTimestamp = wms_common:timestamp(),

  % new timestamp (old removed)

  Event = #event{
    event_id   = ?EVENT_ID,
    timestamps = [OldTimestamp, ValidTimestamp]
  },

  Result = GetChildren(Instance, Event),
  Expected = [ValidTimestamp],
  ?assertEqual(Expected, Result).

remove_timestamp_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{remove_child :=RemoveChild} = new_instance(Timestamp),

  ValidTimestamp = wms_common:timestamp(),

  % new timestamp (old removed)

  Result = RemoveChild(Instance, [ValidTimestamp, Timestamp]),
  Expected = [ValidTimestamp],
  ?assertEqual(Expected, Result).

set_timestamps_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{set_children :=SetChildren} = new_instance(Timestamp),

  Event = #event{
    event_id   = ?EVENT_ID,
    timestamps = []
  },

  Result = SetChildren(Instance, Event, [Timestamp]),
  Expected = Event#event{timestamps = [Timestamp]},
  ?assertEqual(Expected, Result).

filter_timestamp_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{filter_children :=FilterChildren} = new_instance(Timestamp),

  Result = FilterChildren(Instance, [Timestamp]),
  Expected = [Timestamp],
  ?assertEqual(Expected, Result).

%% =============================================================================
%% Private functions.
%% =============================================================================

new_instance(Timestamp) ->
  wms_db_inst_events:new(Timestamp, ?EVENT_ID).