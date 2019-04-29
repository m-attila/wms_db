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
  'ignore'
  | { 'ok'
    , {   {'one_for_all', non_neg_integer(), pos_integer()}
          | {'one_for_one', non_neg_integer(), pos_integer()}
          | {'rest_for_one', non_neg_integer(), pos_integer()}
          | {'simple_one_for_one', non_neg_integer(), pos_integer()}
        , [{_
        ,   {   atom()
              , atom()
              , 'undefined'
                | [any()]}
        ,   'permanent'
            | 'temporary'
            | 'transient'
        ,   'brutal_kill'
            | 'infinity'
            | non_neg_integer()
        ,   'supervisor'
            | 'worker'
        ,   'dynamic'
            | [atom()]}
          ]}}.
init([]) ->
  {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
