%%%-------------------------------------------------------------------
%% @doc wms_db public API
%% @end
%%%-------------------------------------------------------------------

-module(wms_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(Type :: application:start_type(), Args :: term()) ->
  {ok, Pid :: pid()} |
  {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
  init(),
  {ok, _} = wms_db_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(any()) ->
  atom().
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec init() ->
  any().
init() ->
  Nodes = [node() | nodes()],
  ok = wms_db_handler:init(Nodes).