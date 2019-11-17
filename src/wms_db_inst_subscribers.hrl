%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2019 11:04
%%%-------------------------------------------------------------------
-author("Attila Makra").

-define(SUBS_TABLE_NAME, event_subscriber).

-record(subscriber, {
  timestamp :: timestamp(),
  task_instance_id :: binary()
}).
-type subscriber() :: #subscriber{}.

-record(subscribtion, {
  event_id :: binary(),
  subscribers :: [subscriber()]
}).
-type subscribtion() :: #subscribtion{}.
