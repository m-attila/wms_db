%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Header for wms_db_events
%%% @end
%%% Created : 30. Apr 2019 15:29
%%%-------------------------------------------------------------------
-author("Attila Makra").

-include_lib("wms_common/include/wms_common.hrl").

-define(EVENT_TABLE_NAME, events).

-record(event, {
  event_id :: binary(),
  mandatory :: boolean(),
  timestamps :: [timestamp()]
}).
-type event() :: #event{}.