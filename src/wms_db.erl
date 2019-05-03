%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%% API functions for wms_db
%%% @end
%%% Created : 02. May 2019 19:56
%%%-------------------------------------------------------------------
-module(wms_db).
-author("Attila Makra").
-include("wms_db.hrl").

%% API
-export([load_config/0]).

%% =============================================================================
%% API functions
%% =============================================================================

-spec load_config() ->
  ok.
load_config()->
  load_config(wms_cfg:get(?APP_NAME, load_config, true)).

%% =============================================================================
%% Private functions
%% =============================================================================

-spec load_config(boolean()) ->
  ok.
load_config(true) ->
  Path = filename:join(code:priv_dir(?APP_NAME), "wms_db.config"),
  ok = wms_cfg:overload_config(wms_cfg:get_mode(), [Path]);
load_config(_) ->
  ok.