%% Copyright (c) 2016-2017, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.
%%
%% @doc exometer backend


-module(metrics_exometer).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3, update_or_create/4, delete/2]).

-spec new(atom(), any(), map()) -> ok | {error, metric_exists | unsupported_type}.
new(counter, Name, _Config) ->
  ensure(Name, counter);
new(histogram, Name, _Config) ->
  ensure(Name, histogram);
new(gauge, Name, _Config) ->
  ensure(Name, gauge);
new(meter, Name, _Config) ->
  ensure(Name, meter);
new(spiral, Name, _Config) ->
  ensure(Name, spiral);
new(duration, Name, _Config) ->
  ensure(Name, duration);
new(_, _, _) ->
  {error, unsupported_type}.


ensure(Name, Type) ->
  M = module(),
  M:ensure(Name, Type).


update(Name, {c, I}, _Config) when is_integer(I) -> update(Name, I);
update(Name, Val, _Config) ->  update(Name, Val).

update(Name, Val) ->
  M = module(),
  M:update(Name, Val).

update_or_create(Name, {c, I}, Type, _Config) when is_integer(I) ->
  update_or_create(Name, I, Type);
update_or_create(Name, Val, Type, _Config) ->
  update_or_create(Name, Val, Type).


update_or_create(Name, Val, Type) ->
  M = module(),
  M:update_or_create(Name, Val, Type, []).


delete(Name, _Config) ->
  M = module(),
  M:delete(Name).

module() -> exometer.
