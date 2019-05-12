%%%-------------------------------------------------------------------
%% @doc wms_db public API
%% @end
%%%-------------------------------------------------------------------

-module(wms_db_app).

-include("wms_db.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(Type :: application:start_type(), Args :: term()) ->
  {ok, Pid :: pid()} |
  {error, Reason :: term()}.
start(_StartType, []) ->
  wms_dist:load_config(),
  wms_db:load_config(),
  application:start(wms_dist),
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
  ok.