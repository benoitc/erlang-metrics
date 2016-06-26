%% Copyright (c) 2016, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.
%%
%% @doc noop backend


%% Created by benoitc on 26/06/16.

-module(metrics_noop).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3]).

new(_Name, _Type, _Config) ->  ok.
update(_Name, _Probe, _Config) ->  ok.
