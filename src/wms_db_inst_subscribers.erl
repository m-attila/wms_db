%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Subscriber instance
%%% @end
%%% Created : 30. Apr 2019 09:44
%%%-------------------------------------------------------------------
-module(wms_db_inst_subscribers).
-author("Attila Makra").

-include("wms_db_data.hrl").
-include("wms_db_inst_subscribers.hrl").
-include_lib("wms_common/include/wms_common.hrl").

%% API
-export([create_table/1,
         new/3]).

-define(MAX_SUBSCRIPTION_AGE_SECOND, 60 * 60 * 24 * 7).

%% =============================================================================
%% Private types
%% =============================================================================

-type subscribtion_instance() :: data_instance(subscribtion(), subscriber()).

%% =============================================================================
%% API functions
%% =============================================================================

-spec create_table([node()]) ->
  ok | {error, term()}.
create_table(Nodes) ->
  wms_db_handler:create_table(
    ?SUBS_TABLE_NAME,
    set,
    subscribtion, record_info(fields, subscribtion), Nodes).

-spec new(timestamp(), binary(), binary() | undefined) ->
  any().
new(Timestamp, EventID, TaskInstanceID) ->
  Data = #{timestamp => Timestamp,
           event_id => EventID,
           task_instance_id => TaskInstanceID},
  #{
    fields => Data,

    read_record => fun read_subscribtion/2,
    new_child => fun new_subscriber/1,
    new_record => fun new_subscribtion/2,
    add_child => fun add_subscriber/3,
    write_record => fun write_subscribtion/2,
    get_children => fun get_subscribers/2,
    remove_child => fun remove_subscriber/2,
    delete_record => fun delete_subscription/2,
    set_children => fun set_subscribers/3,
    filter_children => fun filter_subscribers/2
  }.

%% =============================================================================
%% Method implementations
%% =============================================================================

-spec read_subscribtion(subscribtion_instance(), read | write) ->
  subscribtion() | not_found.
read_subscribtion(#{fields := #{event_id:=EventID}}, LockingMode) ->
  wms_db_handler:convert(wms_db_handler:read(?SUBS_TABLE_NAME,
                                             EventID, LockingMode)).

-spec new_subscriber(subscribtion_instance()) ->
  subscriber().
new_subscriber(#{fields := #{task_instance_id :=TaskInstanceID,
                             timestamp := Timestamp}}) ->
  #subscriber{
    task_instance_id = TaskInstanceID,
    timestamp        = Timestamp
  }.

-spec new_subscribtion(subscribtion_instance(), subscriber()) ->
  subscribtion().
new_subscribtion(#{fields := #{event_id :=EventID}}, Subscriber) ->
  #subscribtion{
    event_id    = EventID,
    subscribers = [Subscriber]
  }.

-spec add_subscriber(subscribtion_instance(), subscribtion(), subscriber()) ->
  subscribtion().
add_subscriber(_, Record, Subscriber) ->
  OldSubscribers = Record#subscribtion.subscribers,
  Record#subscribtion{
    subscribers = [Subscriber |
                   remove_old(OldSubscribers)]}.

-spec write_subscribtion(subscribtion_instance(), subscribtion()) ->
  ok.
write_subscribtion(_, Record) ->
  wms_db_handler:write(?SUBS_TABLE_NAME, Record).

-spec get_subscribers(subscribtion_instance(), subscribtion()) ->
  [subscriber()].
get_subscribers(_, #subscribtion{subscribers = Subscribers}) ->
  Subscribers.

-spec remove_subscriber(subscribtion_instance(), [subscriber()]) ->
  [subscriber()].
remove_subscriber(#{fields :=#{task_instance_id := TaskInstanceID,
                               timestamp := Timestamp}}, Subscribers) ->
  lists:filter(
    fun(#subscriber{timestamp        = TimeStampS,
                    task_instance_id = TaskInstanceIDS}) ->
      TaskInstanceID =/= TaskInstanceIDS orelse Timestamp =/= TimeStampS
    end,
    Subscribers).

-spec delete_subscription(subscribtion_instance(), subscribtion()) ->
  ok.
delete_subscription(_, #subscribtion{event_id = EventID}) ->
  wms_db_handler:delete(?SUBS_TABLE_NAME, EventID).

-spec set_subscribers(subscribtion_instance(), subscribtion(), [subscriber()]) ->
  subscribtion().
set_subscribers(_, Subscription, Subscribers) ->
  Subscription#subscribtion{subscribers = Subscribers}.

-spec filter_subscribers(subscribtion_instance(), [subscriber()]) ->
  [subscriber()].
filter_subscribers(#{fields := #{timestamp := Timestamp}},
                   Subscribers) ->
  lists:filter
  (fun(#subscriber{timestamp = TimestampS}) ->
    wms_common:compare(TimestampS, Timestamp) =< 0
   end,
   Subscribers).

%% -----------------------------------------------------------------------------
%% Private functions
%% -----------------------------------------------------------------------------

-spec remove_old([subscriber()]) ->
  [subscriber()].
remove_old(Subscribers) ->
  lists:filter(
    fun(#subscriber{timestamp = TimeStamp}) ->
      {second, SecondValue} = wms_common:elapsed(TimeStamp, second),
      SecondValue < ?MAX_SUBSCRIPTION_AGE_SECOND
    end, Subscribers).

