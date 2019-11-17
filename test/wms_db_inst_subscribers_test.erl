%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Unit test for wms_inst_subscriber module.
%%% @end
%%% Created : 30. Apr 2019 11:19
%%%-------------------------------------------------------------------
-module(wms_db_inst_subscribers_test).
-author("Attila Makra").

-include("../src/wms_db_inst_subscribers.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("wms_common/include/wms_common.hrl").

-define(EVENT_ID, <<"event01">>).
-define(TASK_INST_ID, <<"instance1">>).
-define(TASK_INST_ID1, <<"instance2">>).
-define(TASK_INST_ID2, <<"instance3">>).

%% =============================================================================
%% Testcases
%% =============================================================================

new_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = new_instance(Timestamp),

  #{fields := Fields} = Instance,
  ?assertEqual(Timestamp, maps:get(timestamp, Fields)),
  ?assertEqual(?EVENT_ID, maps:get(event_id, Fields)),
  ?assertEqual(?TASK_INST_ID, maps:get(task_instance_id, Fields)).

new_subscriber_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{new_child := NewChild} = new_instance(Timestamp),
  Result = NewChild(Instance),
  Expected = #subscriber{timestamp        = Timestamp,
                         task_instance_id = ?TASK_INST_ID},
  ?assertEqual(Expected, Result).

new_subscription_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{new_record := NewRecord} = new_instance(Timestamp),
  NewSubscriber = new_subscriber(Timestamp),
  Result = NewRecord(Instance, NewSubscriber),
  Expected = #subscribtion{
    event_id    = ?EVENT_ID,
    subscribers = [NewSubscriber]
  },
  ?assertEqual(Expected, Result).

add_subscriber_get_subscribers_test() ->
  Timestamp = wms_common:timestamp(),

  OldSubscriber = #subscriber{task_instance_id = ?TASK_INST_ID,
                              timestamp        = {native, 0}},
  ValidSubscriber = #subscriber{
    task_instance_id = ?TASK_INST_ID,
    timestamp        = wms_common:timestamp()
  },

  NewSubscriber = #subscriber{
    task_instance_id = ?TASK_INST_ID1,
    timestamp        = wms_common:timestamp()
  },

  Subscribtion = #subscribtion{event_id    = ?EVENT_ID,
                               subscribers = [OldSubscriber, ValidSubscriber]},

  Instance = #{add_child := AddChild,
               get_children := GetChildren} = new_instance(Timestamp),

  Result = AddChild(Instance, Subscribtion, NewSubscriber),
  Expected = Subscribtion#subscribtion{subscribers = [NewSubscriber,
                                                      ValidSubscriber]},
  ?assertEqual(Expected, Result),

  Result1 = GetChildren(Instance, Result),
  Expected1 = [NewSubscriber,
               ValidSubscriber],
  ?assertEqual(Expected1, Result1).

remove_subscriber_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{remove_child := RemoveChild} = new_instance(Timestamp),

  RemainSubs1 = #subscriber{
    task_instance_id = ?TASK_INST_ID1,
    timestamp        = Timestamp
  },

  timer:sleep(100),

  RemainSubs2 = #subscriber{
    task_instance_id = ?TASK_INST_ID,
    timestamp        = wms_common:timestamp()
  },

  RemovedSubs = #subscriber{
    task_instance_id = ?TASK_INST_ID,
    timestamp        = Timestamp
  },

  Subscribers = [RemainSubs1, RemainSubs2, RemovedSubs],
  Result = RemoveChild(Instance, Subscribers),
  Expected = [RemainSubs1, RemainSubs2],
  ?assertEqual(Expected, Result).

set_subscribers_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{set_children := SetSubscribers} = new_instance(Timestamp),

  Subs1 = #subscriber{
    task_instance_id = ?TASK_INST_ID1,
    timestamp        = Timestamp
  },
  Subs2 = #subscriber{
    task_instance_id = ?TASK_INST_ID,
    timestamp        = wms_common:timestamp()
  },

  Subscribtion = #subscribtion{
    event_id    = ?EVENT_ID,
    subscribers = []
  },

  Result = SetSubscribers(Instance, Subscribtion, [Subs1, Subs2]),
  Expected = Subscribtion#subscribtion{subscribers = [Subs1, Subs2]},
  ?assertEqual(Expected, Result).

filter_subscriber_test() ->
  Timestamp = wms_common:timestamp(),
  Instance = #{filter_children := FilterSubscriber} = new_instance(Timestamp),


  Subs1 = #subscriber{
    task_instance_id = ?TASK_INST_ID1,
    timestamp        = wms_common:add(Timestamp, {millisecond, -100})
  },
  Subs2 = #subscriber{
    task_instance_id = ?TASK_INST_ID,
    timestamp        = wms_common:add(Timestamp, {millisecond, 100})
  },

  Result = FilterSubscriber(Instance, [Subs1, Subs2]),
  Expected = [Subs1],
  ?assertEqual(Expected, Result).

%% =============================================================================
%% Private functions
%% =============================================================================

new_instance(Timestamp) ->
  wms_db_inst_subscribers:new(Timestamp, ?EVENT_ID,
                              ?TASK_INST_ID).

new_subscriber(Timestamp) ->
  #subscriber{task_instance_id = ?TASK_INST_ID,
              timestamp        = Timestamp
  }.
