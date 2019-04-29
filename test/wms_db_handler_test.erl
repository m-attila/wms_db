%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 29. Apr 2019 07:34
%%%-------------------------------------------------------------------
-module(wms_db_handler_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").
-include("wms_db_app.hrl").

-define(TEST_NODES, [testnode1, testnode2]).

init_test() ->
  Node = [node()],

  % node are not running
  ?assertMatch({error, {node_not_running, _}}, wms_db_handler:init(Node)).

%% =============================================================================
%% Private functions
%% =============================================================================