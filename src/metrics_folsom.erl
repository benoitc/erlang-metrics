%% Copyright (c) 2016-2018, Benoit Chesneau.
%%
%% This file is part of erlang-metrics released under the BSD license.
%% See the NOTICE for more information.

%% @doc folsom backend

-module(metrics_folsom).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3, update_or_create/4, delete/2]).

-spec new(atom(), any(), map()) -> ok | {error, term()}.
new(counter, Name, _Config) ->
  M = module(),
  M:new_counter(Name);
new(histogram, Name, _Config) ->
  M = module(),
  M:new_histogram(Name);
new(gauge, Name, _Config) ->
  M = module(),
  M:new_gauge(Name);
new(meter, Name, _Config) ->
  M = module(),
  M:new_meter(Name);
new(spiral, Name, _Config) ->
  M = module(),
  M:new_spiral(Name);
new(duration, Name, _Config) ->
  M = module(),
  M:new_duration(Name);
new(_, _, _) ->
  {error, unsupported_type}.

update(Name, {c, I}, _Config) when I >= 0 ->
  notify(Name, {inc, I});
update(Name, {c, I}, _Config) when I < 0 ->
  notify(Name, {dec, abs(I)});
update(Name, {duration_fun, Fun}, _Config) ->
  notify(Name, timer_start),
  try Fun()
  after notify(Name, timer_end)
  end;
update(Name, Val, _Config) ->
  notify(Name, Val).

notify(Name, Val) ->
  M = module(),
  M:notify(Name, Val).

update_or_create(Name, Probe, Type, Config) ->
  case update(Name, Probe, Config) of
    ok -> ok;
    {error, Name, nonexistent_metric} ->
      new(Type, Name, Config),
      update(Name, Probe, Config);
    Error ->
      Error
  end.

delete(Name, _Config) ->
  M = module(),
  M:delete_metric(Name).

module() -> folsom_metrics.
