%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, Attila Makra.
%%% @doc
%%% Tests for wms_db_data module.
%%% @end
%%% Created : 30. Apr 2019 14:23
%%%-------------------------------------------------------------------
-module(wms_db_data_test).
-author("Attila Makra").

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test functions
%% =============================================================================

add_child_to_key_test() ->
  put(write_record_executed, false),
  % no record on given key
  Instance =
    #{
      new_child =>
      fun(_) ->
        new_child_value
      end,
      read_record =>
      fun(_, _) ->
        not_found
      end,
      new_record =>
      fun(_, Child) ->
        [Child]
      end,
      add_child =>
      fun(_, _, _) ->
        throw(not_implemented)
      end,
      write_record =>
      fun(_, Record) ->
        ?assertEqual([new_child_value], Record),
        put(write_record_executed, true),
        ok
      end
    },
  TestFun =
    fun() ->
      wms_db_data:add_child_to_key(Instance)
    end,
  ?assertEqual(ok, run_test(TestFun)),
  ?assert(get(write_record_executed)),

  put(write_record_executed, false),
  % record already exist on key
  Instance1 = Instance#{
    read_record :=
    fun(_, _) ->
      [old_child]
    end,
    new_record :=
    fun(_, _) ->
      throw(not_implemented)
    end,
    add_child :=
    fun(_, Record, NewChild) ->
      [NewChild | Record]
    end,
    write_record =>
    fun(_, Record) ->
      ?assertEqual([new_child_value, old_child], Record),
      put(write_record_executed, true),
      ok
    end
  },
  TestFun1 =
    fun() ->
      wms_db_data:add_child_to_key(Instance1)
    end,
  ?assertEqual(ok, run_test(TestFun1)),
  ?assert(get(write_record_executed)).

remove_child_from_key_test() ->
  % record not found ->
  Instance =
    #{
      read_record =>
      fun(_, _) ->
        not_found
      end,
      get_children =>
      fun(_, _) ->
        throw(not_implemented)
      end,
      remove_child =>
      fun(_, _) ->
        throw(not_implemented)
      end,
      delete_record =>
      fun(_, _) ->
        throw(not_implemented)
      end,
      set_children =>
      fun(_, _, _) ->
        throw(not_implemented)
      end,
      write_record =>
      fun(_, _) ->
        throw(not_implemented)
      end
    },
  TestFun1 =
    fun() ->
      wms_db_data:remove_child_from_key(Instance)
    end,
  ?assertEqual({ok, false}, run_test(TestFun1)),

  % record found, but child does not exists
  Instance2 = Instance#{
    read_record =>
    fun(_, _) ->
      {record, [remain_child]}
    end,
    get_children =>
    fun(_, {record, Children}) ->
      Children
    end,
    remove_child =>
    fun(_, Children) ->
      lists:delete(missing_child, Children)
    end
  },
  TestFun2 =
    fun() ->
      wms_db_data:remove_child_from_key(Instance2)
    end,
  ?assertEqual({ok, false}, run_test(TestFun2)),

  % record found, child found, but children remains
  put(write_record_executed, false),
  Instance3 = Instance2#{
    read_record =>
    fun(_, _) ->
      {record, [remain_child, found_child]}
    end,
    get_children =>
    fun(_, {record, Children}) ->
      Children
    end,
    remove_child =>
    fun(_, Children) ->
      lists:delete(found_child, Children)
    end,
    set_children =>
    fun(_, {record, _}, NewChildren) ->
      {record, NewChildren}
    end,
    write_record =>
    fun(_, {record, [remain_child]}) ->
      put(write_record_executed, true),
      ok
    end
  },
  TestFun3 =
    fun() ->
      wms_db_data:remove_child_from_key(Instance3)
    end,
  ?assertEqual({ok, false}, run_test(TestFun3)),
  ?assert(get(write_record_executed)),

  % record found, child found, no children will be remain
  put(delete_record_executed, false),
  Instance4 = Instance2#{
    read_record =>
    fun(_, _) ->
      {record, [found_child]}
    end,
    get_children =>
    fun(_, {record, Children}) ->
      Children
    end,
    remove_child =>
    fun(_, Children) ->
      lists:delete(found_child, Children)
    end,
    set_children =>
    fun(_, {record, _}, NewChildren) ->
      {record, NewChildren}
    end,
    write_record =>
    fun(_, {record, [remain_child]}) ->
      throw(not_implemented)
    end,
    delete_record =>
    fun(_, {record, []}) ->
      put(delete_record_executed, true),
      ok
    end
  },
  TestFun4 =
    fun() ->
      wms_db_data:remove_child_from_key(Instance4)
    end,
  ?assertEqual({ok, true}, run_test(TestFun4)),
  ?assert(get(delete_record_executed)).

get_children_from_key_test() ->
  % record not found
  Instance1 =
    #{
      read_record =>
      fun(_, _) ->
        not_found
      end,
      filter_children =>
      fun(_, _) ->
        throw(not_implemented)
      end,
      get_children =>
      fun(_, _) ->
        throw(not_implemented)
      end
    },
  TestFun1 =
    fun() ->
      wms_db_data:get_children_from_key(Instance1)
    end,
  ?assertEqual([], run_test(TestFun1)),

  % record was found
  Instance2 = Instance1#{
    read_record =>
    fun(_, _) ->
      {record, [1, 2, 3]}
    end,
    filter_children =>
    fun(_, Children) ->
      lists:filter(
        fun(N) ->
          N > 1
        end, Children)
    end,
    get_children =>
    fun(_, {record, Children}) ->
      Children
    end
  },
  TestFun2 =
    fun() ->
      wms_db_data:get_children_from_key(Instance2)
    end,
  ?assertEqual([2, 3], run_test(TestFun2)).

%% =============================================================================
%% Private functions
%% =============================================================================
run_test(TestFun) ->
  try
    ok = meck:new(wms_db_handler),
    ok = meck:expect(wms_db_handler, transaction,
                     fun(F) ->
                       case F() of
                         ok ->
                           ok;
                         Result ->
                           {ok, Result}
                       end
                     end),
    TestFun()
  after
    meck:unload(wms_db_handler)
  end.