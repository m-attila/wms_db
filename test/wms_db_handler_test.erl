%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%%
%%% @end
%%% Created : 29. Apr 2019 07:34
%%%-------------------------------------------------------------------
-module(wms_db_handler_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").
-include("wms_db.hrl").

init_test() ->
  Node = [node()],

  ?assertMatch(ok, wms_db_handler:init(Node, Node)).

convert_test() ->
  ?assertEqual(not_found, wms_db_handler:convert({ok, []})),
  ?assertEqual(record, wms_db_handler:convert({ok, [record]})).