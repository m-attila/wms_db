%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% wms_db_handler initialized
%%% @end
%%% Created : 09. May 2019 15:04
%%%-------------------------------------------------------------------
-module(wms_db_handler_service).
-author("Attila Makra").
-behaviour(gen_server).

-include_lib("wms_logger/include/wms_logger.hrl").

%% API
-export([start_link/0,
         is_initialized/0,
         wait_for_initialized/1,
         run/2]).

-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2, initializer/2]).

-define(TABLE_MODULES, [wms_db_data_events,
                        wms_db_data_global_state,
                        wms_db_data_priv_state,
                        wms_db_data_taskdef,
                        wms_db_data_tasks,
                        wms_db_data_task_instance_status]).

%% =============================================================================
%% Private types
%% =============================================================================
-type phase() :: init_schema | create_tables | initialized.

-record(state, {
  phase = init_schema :: phase(),
  tables = [] :: [atom()]
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  Ret = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  ?info("stared."),
  Ret.

-spec is_initialized() ->
  boolean().
is_initialized() ->
  gen_server:call(?MODULE, {is_initialized}).

-spec run(fun(), integer()) ->
  term() | {error, database_unavailabe}.
run(_, TimeoutMsec) when TimeoutMsec =< 0 ->
  {error, database_unavailabe};
run(Fun, TimeoutMsec) ->
  case is_initialized() of
    true ->
      Fun();
    false ->
      timer:sleep(100),
      run(Fun, TimeoutMsec - 100)
  end.

-spec wait_for_initialized(integer()) ->
  ok | {error, database_unavailabe}.
wait_for_initialized(TimeoutMsec) ->
  run(
    fun() ->
      ok
    end, TimeoutMsec).

%% =============================================================================
%% gen_server behaviour
%% =============================================================================
-spec init(Args :: term()) ->
  {ok, State :: state()}.
init(_) ->
  process_flag(trap_exit, true),
  self() ! start,
  {ok, #state{}}.

-spec handle_info(Info :: any(), State :: state()) ->
  {noreply, State :: state()}.
handle_info(start, #state{phase = init_schema} = State) ->
  spawn_link(?MODULE, initializer, [init_schema, []]),
  {noreply, State};
handle_info(start, #state{phase  = create_tables,
                          tables = Tables} = State) ->
  spawn_link(?MODULE, initializer, [create_tables, Tables]),
  {noreply, State};

handle_info({'EXIT', _, {ready, init_schema, ok}}, State) ->
  ?info("Schema initialized."),
  self() ! start,
  {noreply, State#state{phase = create_tables}};
handle_info({'EXIT', _, {ready, init_schema, _}}, State) ->
  erlang:send_after(1000, self(), start),
  {noreply, State};

handle_info({'EXIT', _, {ready, create_tables, CreatedTables}}, State) ->
  NewState = case lists:subtract(?TABLE_MODULES, CreatedTables) of
               [] ->
                 ?info("Tables created."),
                 State#state{phase = initialized};
               _ ->
                 erlang:send_after(1000, self(), start),
                 State#state{tables = CreatedTables}
             end,
  {noreply, NewState};
handle_info(Msg, State) ->
  ?warning("Unknown message: ~0p", [Msg]),
% unknown message
  {noreply, State}.

-spec handle_call(Info :: any(), From :: {pid(), term()}, State :: state())
                 ->
                   {reply, term(), State :: state()}.

handle_call({is_initialized}, _From,
            #state{phase = Phase} = State) ->
  {reply, Phase =:= initialized, State}.

-spec handle_cast(Request :: any(), State :: state())
                 ->
                   {noreply, State :: state()}.
handle_cast(_, State) ->
  {noreply, State}.

%% =============================================================================
%% Private functions
%% =============================================================================

-spec initializer(phase(), [term()]) ->
  any().
initializer(Phase, Args) ->
  RetVal = try
             Ret = do_init(Phase, Args),
             ?debug("Phase ready : ~0p", [Phase]),
             Ret
           catch
             C:Reason ->
               ?error("Phase initialization error: ~0p:~0p", [C, Reason]),
               {error, Reason}
           end,
  exit({ready, Phase, RetVal}).

-spec do_init(phase(), [term()]) ->
  ok.
do_init(init_schema, []) ->
  AllNodes = wms_dist:get_dst_nodes(all),
  AvailableNodes = wms_dist:get_dst_nodes(connected),
  wms_db_handler:init(AllNodes, AvailableNodes);
do_init(create_tables, CreatedTables) ->
  RestTableModules =
    lists:subtract(?TABLE_MODULES, CreatedTables),
  do_table_creation(RestTableModules, [] ++ CreatedTables).

do_table_creation([], SuccessCreated) ->
  SuccessCreated;
do_table_creation([TableModule | Rest], Success) ->
  try
    apply(TableModule, create, []),
    ?info("~s table creation was successed", [TableModule]),
    do_table_creation(Rest, [TableModule | Success])
  catch
    C:R ->
      ?error("Error at ~s table creation : ~p : ~p", [TableModule, C, R]),
      do_table_creation(Rest, Success)
  end.