%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2019 08:56
%%%-------------------------------------------------------------------
-author("Attila Makra").

-type data_instance(RecordType, ChildType) :: #{
new_child := fun((data_instance())->
  ChildType),
read_record := fun((data_instance(), read | write)->
  not_found | RecordType),
new_record := fun((data_instance(), ChildType)->
  RecordType),
add_child := fun((data_instance(), RecordType, ChildType)->
  RecordType),
write_record := fun((data_instance(), RecordType) ->
  ok),
get_children := fun((data_instance(), RecordType) ->
  [ChildType]),
remove_child := fun((data_instance(), [ChildType]) ->
  [ChildType]),
delete_record := fun((data_instance(), RecordType) ->
  ok),
set_children := fun((data_instance(), RecordType, [ChildType]) ->
  RecordType),
filter_children := fun((data_instance(), [ChildType]) ->
  [ChildType]),
fields := #{
atom() => term()
}
}.

-type data_instance() :: data_instance(term(), term()).
