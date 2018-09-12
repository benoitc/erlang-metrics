%% Copyright (c) 2016-2018, Benoit Chesneau.
%%
%% This file is part of erlang-metrics released under the BSD license.
%% See the NOTICE for more information.

-module(metrics).
-author("Benoit Chesneau").
-behaviour(gen_server).
-behaviour(application).

%% API
-export([new/2]).
-export([update/1, update/2]).
-export([update_or_create/3]).
-export([backend/0, backend/1]).
-export([delete/1]).

-export([start_link/0]).

%% application callbacks

-export([start/2, stop/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,terminate/2, code_change/3]).

-include_lib("syntax_tools/include/merl.hrl").

-define(SERVER, ?MODULE).

-define(TAB, metrics).

-record(state, {mod}).

%%%===================================================================
%%% Types
%%%===================================================================

-type metric_name() :: list().
-type metric() :: counter | histogram | gauge | meter | spiral | duration.
-type value() :: any().
-type probe() :: {c, integer()} | timer_start | timer_end | {duration_fun, fun()} | value().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc  initialise a metric
-spec new(Metric, Name) -> Result when
    Metric :: metric(),
    Name :: metric_name(),
    Result :: ok.
new(Type, Name) ->
  metrics_mod:new(Type, Name).

%% @doc increment a counter with 1
-spec update(Name) -> Result when
    Name :: metric_name(),
    Result :: ok | any().
update(Name) ->
  update(Name, {c, 1}).

%% @doc update a metric
-spec update(Name, Probe) -> Result when
    Name :: metric_name(),
    Probe :: probe(),
    Result :: ok | any().
update(Name, Probe) ->
  metrics_mod:update(Name, Probe).

%% @doc update a metric and create it if it doesn't exists
-spec update_or_create(Name, Probe, Metric) -> Result when
    Name :: metric_name(),
    Probe :: probe(),
    Metric :: metric(),
    Result :: ok | any().
update_or_create(Name, Probe, Type) ->
  metrics_mod:update_or_create(Name, Probe, Type).

%% @doc delete a metric
-spec delete(Name) -> Result when
    Name :: metric_name(),
    Result :: ok | any().
delete(Name) ->
  metrics_mod:delete(Name).


%% @doc retrieve the current backend name
-spec backend() -> atom().
backend() -> gen_server:call(?MODULE, get_backend).

%% @doc set the backend to use
-spec backend(atom()) -> ok.
backend(Mod) ->
  _ = code:ensure_loaded(Mod),
  case erlang:function_exported(Mod, update, 3) of
    true -> ok;
    false -> erlang:error(bad_modmetric)
  end,
  gen_server:call(?MODULE, {set_backend, Mod}).



-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% application callbacks
%%%===================================================================
%%%
-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  'metrics_sup':start_link().

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%

init([]) ->
  Mod = metrics_mod(),
  Config = init_mod(Mod),
  ok = build_metrics_mod(Mod, Config),

  {ok, #state{mod=Mod}}.



handle_call(get_backend, _From, State) ->
  {reply, State#state.mod, State};
handle_call({set_backend, Mod}, _From, State) ->
  if
    State#state.mod /= Mod ->
      Config = init_mod(Mod),
      build_metrics_mod(Mod, Config);
    true -> ok
  end,
  {reply, ok, State}.


handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


metrics_mod() ->
  Mod = application:get_env(metrics, metrics_mod, metrics_noop),
  _ = code:ensure_loaded(Mod),
  case erlang:function_exported(Mod, update, 3) of
    false ->
      error_logger:error_msg("bad metric module"),
      erlang:error(bad_modmetric);
    true ->
      Mod
  end.

init_mod(Mod) ->
  case erlang:function_exported(Mod, init, 0) of
    false -> #{};
    true -> Mod:init()
  end.

build_metrics_mod(Mod, Config) when is_atom(Mod), is_map(Config) ->
  error_logger:info_msg("build metrics module: ~s~n", [Mod]),
  Module = ?Q("-module('metrics_mod')."),
  Exported = ?Q("-export(['new'/2, 'update'/2, 'update_or_create'/3, 'delete'/1])."),
  Functions = ?Q([
    "new(Type, Name) -> _@Mod@:new(Type, Name, _@Config@).",

    "update(Name, Probe) -> _@Mod@:update(Name, Probe, _@Config@).",

    "update_or_create(Name, Probe, Type) ->",
    "  _@Mod@:update_or_create(Name, Probe, Type, _@Config@).",

    "delete(Name) -> _@Mod@:delete(Name, _@Config@)."
  ]),
  Forms = lists:flatten([Module, Exported, Functions]),
  merl:compile_and_load(Forms, [verbose]),
  ok.
