%%%-------------------------------------------------------------------
%% @doc wms_db public API
%% @end
%%%-------------------------------------------------------------------

-module(wms_db_app).

-include("wms_db.hrl").
-include_lib("wms_logger/include/wms_logger.hrl").

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
  ok = wms_cfg:start_apps(?APP_NAME, [wms_dist]).