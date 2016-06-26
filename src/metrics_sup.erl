
%% Created by benoitc on 26/06/16.

-module(metrics_sup).
-author("Benoit Chesneau").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  Metrics = {metrics,
    {metrics, start_link, []},
    permanent, brutal_kill,	worker,[metrics]},

  ProcessTracker = {metrics_process_tracker,
    {metrics_process_tracker, start_link, []},
    permanent, brutal_kill,	worker,[metrics_process_tracker]},
  

  {ok, {{one_for_one, 4, 3600}, [Metrics, ProcessTracker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

