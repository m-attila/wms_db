%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% Common data handling functions.
%%% @end
%%% Created : 30. Apr 2019 08:47
%%%-------------------------------------------------------------------
-module(wms_db_data).
-author("Attila Makra").

-include("wms_db_data.hrl").

%% API
-export([add_child_to_key/1,
         remove_child_from_key/1,
         get_children_from_key/1]).

%% @doc
%%
%%-------------------------------------------------------------------
%%
%% ### Function
%% add_key_to_child/1
%% ###### Purpose
%% Generic implementation to add children data for specified key.
%% ###### Arguments
%%
%% ###### Returns
%%
%%-------------------------------------------------------------------
%%
%% @end
-spec add_child_to_key(data_instance()) ->
  ok.
add_child_to_key(#{new_child := NewChildFun,
                   read_record := ReadRecordFun,
                   new_record := NewRecordFun,
                   add_child := AddChildFun,
                   write_record := WriteRecordFun} = Instance) ->
  Transaction =
    fun() ->
      NewChild = NewChildFun(Instance),

      Record = ReadRecordFun(Instance, write),

      NewRecord =
        case Record of
          not_found ->
            NewRecordFun(Instance, NewChild);
          _ ->
            AddChildFun(Instance, Record, NewChild)
        end,
      ok = WriteRecordFun(Instance, NewRecord)
    end,
  ok = wms_db_handler:transaction(Transaction).

-spec remove_child_from_key(data_instance()) ->
  {ok, ParentRemoved :: boolean()}.
remove_child_from_key(#{
                        read_record := ReadRecordFun,
                        get_children := GetChildrenFun,
                        remove_child := RemoveChildFun,
                        delete_record := DeleteRecordFun,
                        set_children := SetChildrenFun,
                        write_record := WriteRecordFun} = Instance) ->
  Transaction =
    fun() ->
      Record = ReadRecordFun(Instance, write),

      {PrevChildren, NewChildren} =
        case Record of
          not_found ->
            {[], []};
          _ ->
            Prev = GetChildrenFun(Instance, Record),
            {Prev, RemoveChildFun(Instance, Prev)}
        end,

      case NewChildren of
        PrevChildren ->
          false;
        [] ->
          ok = DeleteRecordFun(Instance, SetChildrenFun(Instance,
                                                        Record, NewChildren)),
          true;
        _ ->
          ok = WriteRecordFun(Instance, SetChildrenFun(Instance,
                                                       Record, NewChildren)),
          false
      end
    end,
  wms_db_handler:transaction(Transaction).

-spec get_children_from_key(data_instance()) ->
  [term()].
get_children_from_key(#{read_record := ReadRecordFun,
                        filter_children := FilterChildrenFun,
                        get_children := GetChildrenFun} = Instance) ->
  Transaction =
    fun() ->
      Record = ReadRecordFun(Instance, read),
      case Record of
        not_found ->
          [];
        _ ->
          FilterChildrenFun(Instance, GetChildrenFun(Instance, Record))
      end
    end,
  {ok, Ret} = wms_db_handler:transaction(Transaction),
  Ret.

