%%%-------------------------------------------------------------------
%% @doc wms_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wms_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
  any().
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(Args :: term()) ->
  {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}
  | ignore.
init([]) ->
  ChildSpecs = [#{id => wms_db_handler_service,
                  start => {wms_db_handler_service, start_link, []}
                }
               ],
  SupFlags = #{strategy => one_for_one,
               intensity => 5,
               period => 1
             },
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
