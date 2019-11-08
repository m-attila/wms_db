%%%-------------------------------------------------------------------
%%% @author Attila Makra
%%% @copyright (C) 2019, OTP Bank Nyrt.
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2019 05:54
%%%-------------------------------------------------------------------
-author("Attila Makra").

%% -----------------------------------------------------------------------------
%% taskdef
%% -----------------------------------------------------------------------------

-type taskdef_type() :: auto | manual | disabled.

-record(taskdef, {
  task_name :: binary(),
  definition :: term(),
  type :: taskdef_type()
}).

-type taskdef() :: #taskdef{}.

%% -----------------------------------------------------------------------------
%% tasks
%% -----------------------------------------------------------------------------

-type task_instance_id() :: binary().

-record(task, {
  task_name :: binary(),
  instances :: [task_instance_id()]
}).

-type task() :: #task{}.

%% -----------------------------------------------------------------------------
%% task_status
%% -----------------------------------------------------------------------------

-type task_status() :: started | wait | fire | interaction | aborted | done.
-type task_history() :: {
  StartAt :: integer(),
  EndAt :: integer() | undefined,
  task_status(),
  Description :: binary()}.

-record(task_instance_status, {
  task_instance :: binary(),
  task_name :: binary(),
  timestamp :: integer(),
  status :: task_status(),
  description :: term(),
  history :: [task_history()]
}).

-type task_instance_status() :: #task_instance_status{}.