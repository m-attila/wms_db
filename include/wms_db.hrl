%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2019 19:29
%%%-------------------------------------------------------------------
-author("Attila Makra").

-define(APP_NAME, wms_db).

%% =============================================================================
%% Types
%% =============================================================================

-type table_types() :: set | ordered_set | bag.

-type global_state_variable() :: binary().
-type global_state_value() :: term() | [term()].

-type global_state_reference() :: {global, global_state_variable()}.
-type global_state_value_or_reference() :: global_state_reference() | global_state_value().

-type global_state_operator() ::
fun((global_state_value(),  global_state_value())->
  term()).