%% Copyright (c) 2016, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.


%% Created by benoitc on 26/06/16.

-module(metrics_exometer).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3]).


-spec new(atom(), any(), map()) -> ok | {error, metric_exists | unsupported_type}.
new(counter, Name, _Config) ->
  exometer:ensure(Name, counter, []);
new(histogram, Name, _Config) ->
  exometer:ensure(Name, histogram, []);
new(gauge, Name, _Config) ->
  exometer:ensure(Name, gauge, []);
new(meter, Name, _Config) ->
  exometer:ensure(Name, meter, []);
new(spiral, Name, _Config) ->
  exometer:ensure(Name, spiral, []);
new(_, _, _) ->
  {error, unsupported_type}.

update(Name, {c, I}, _Config) when is_integer(I) -> exometer:update(Name, I);
update(Name, Val, _Config) -> exometer:update(Name, Val).
