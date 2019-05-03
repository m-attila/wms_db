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
-include("wms_db.hrl").

-define(TEST_NODES, [testnode1, testnode2]).

init_test() ->
  Node = [node()],

  % node are not running
  ?assertMatch({error, {node_not_running, _}}, wms_db_handler:init(Node)).

convert_test() ->
  ?assertEqual(not_found, wms_db_handler:convert({ok, []})),
  ?assertEqual(record, wms_db_handler:convert({ok, [record]})).