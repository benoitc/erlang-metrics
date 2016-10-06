%% Copyright (c) 2016, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.

%% @doc folsom backend

%% Created by benoitc on 26/06/16.

-module(metrics_folsom).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3, update_or_create/4, delete/2]).

-spec new(atom(), any(), map()) -> ok | {error, term()}.
new(counter, Name, _Config) ->
  folsom_metrics:new_counter(Name);
new(histogram, Name, _Config) ->
  folsom_metrics:new_histogram(Name);
new(gauge, Name, _Config) ->
  folsom_metrics:new_gauge(Name);
new(meter, Name, _Config) ->
  folsom_metrics:new_meter(Name);
new(spiral, Name, _Config) ->
  folsom_metrics:new_spiral(Name);
new(_, _, _) ->
  {error, unsupported_type}.

update(Name, {c, I}, _Config) when I >= 0 ->
  folsom_metrics:notify(Name, {inc, I});
update(Name, {c, I}, _Config) when I < 0 ->
  folsom_metrics:notify(Name, {dec, abs(I)});
update(Name, Val, _Config) ->
  folsom_metrics:notify(Name, Val).

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
  folsom_metrics:delete_metric(Name).
