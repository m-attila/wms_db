%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Events instance
%%% @end
%%% Created : 30. Apr 2019 15:29
%%%-------------------------------------------------------------------
-module(wms_db_inst_events).
-author("Attila Makra").

-include("wms_db_data.hrl").
-include("wms_db_inst_events.hrl").

-define(MAX_EVENT_AGE_SECOND, 60 * 60 * 24 * 7).

%% API
-export([create_table/0,
         new/2]).

%% =============================================================================
%% Private types
%% =============================================================================

-type event_instance() :: data_instance(event(), timestamp()).

%% =============================================================================
%% API functions
%% =============================================================================

-spec create_table() ->
  ok |{error, term()}.
create_table() ->
  wms_db_handler:create_table(
    ?EVENT_TABLE_NAME,
    set,
    event, record_info(fields, event)).

-spec new(timestamp() | undefined, binary() | {mandatory, binary()}) ->
  any().
new(Timestamp, EventSpec) ->

  {Mandatory, EventID} = case EventSpec of
                           {mandatory, ID} ->
                             {true, ID};
                           OnlyEventID ->
                             {false, OnlyEventID}
                         end,

  Data = #{timestamp => Timestamp,
           mandatory => Mandatory,
           event_id => EventID},
  #{
    fields => Data,

    read_record => fun read_event/2,
    new_child => fun new_timestamp/1,
    new_record => fun new_event/2,
    add_child => fun add_timestamp/3,
    write_record => fun write_event/2,
    get_children => fun get_timestamps/2,
    remove_child => fun remove_timestamp/2,
    delete_record => fun delete_event/2,
    set_children => fun set_timestamps/3,
    filter_children => fun filter_timestamps/2
  }.

%% =============================================================================
%% Method implementations
%% =============================================================================

-spec read_event(event_instance(), read | write) ->
  event() | not_found.
read_event(#{fields := #{event_id:=EventID}}, LockingMode) ->
  wms_db_handler:convert(wms_db_handler:read(?EVENT_TABLE_NAME,
                                             EventID, LockingMode)).

-spec new_timestamp(event_instance()) ->
  timestamp().
new_timestamp(#{fields := #{timestamp := Timestamp}}) ->
  Timestamp.

-spec new_event(event_instance(), timestamp()) ->
  event().
new_event(#{fields := #{event_id :=EventID,
                        mandatory := Mandatory}}, Timestamp) ->
  #event{
    event_id   = EventID,
    mandatory  = Mandatory,
    timestamps = [Timestamp]
  }.

-spec add_timestamp(event_instance(), event(), timestamp()) ->
  event().
add_timestamp(_, Record, Timestamp) ->
  OldTimestamps = remove_old(Record#event.timestamps),
  NewTimestamps =
    case lists:member(Timestamp, OldTimestamps) of
      true ->
        OldTimestamps;
      false ->
        [Timestamp | OldTimestamps]
    end,
  Record#event{timestamps = NewTimestamps}.

-spec write_event(event_instance(), event()) ->
  ok.
write_event(_, Record) ->
  wms_db_handler:write(?EVENT_TABLE_NAME, Record).

-spec get_timestamps(event_instance(), event()) ->
  [timestamp()].
get_timestamps(_, #event{timestamps = Timestamps}) ->
  remove_old(Timestamps).

-spec remove_timestamp(event_instance(), [timestamp()]) ->
  [timestamp()].
remove_timestamp(#{fields :=#{timestamp := Timestamp}}, Timestamps) ->
  lists:filter(
    fun(TimeStampS) ->
      Timestamp =/= TimeStampS
    end,
    Timestamps).

-spec delete_event(event_instance(), event()) ->
  ok.
delete_event(#{fields := #{event_id := EventID}}, _) ->
  wms_db_handler:delete(?EVENT_TABLE_NAME, EventID).

-spec set_timestamps(event_instance(), event(), [timestamp()]) ->
  event().
set_timestamps(_, Event, Timestamps) ->
  Event#event{timestamps = Timestamps}.

-spec filter_timestamps(event_instance(), [timestamp()]) ->
  [timestamp()].
filter_timestamps(_, Timestamps) ->
  Timestamps.

%% -----------------------------------------------------------------------------
%% Private functions
%% -----------------------------------------------------------------------------

-spec remove_old([timestamp()]) ->
  [timestamp()].
remove_old(Timestamps) ->
  lists:filter(
    fun(TimeStamp) ->
      {second, SecondValue} = wms_common:elapsed(TimeStamp, second),
      SecondValue < ?MAX_EVENT_AGE_SECOND
    end, Timestamps).