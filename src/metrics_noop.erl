%% Copyright (c) 2016-2018, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.
%%
%% @doc noop backend


-module(metrics_noop).
-author("Benoit Chesneau").

%% API
-export([new/3, update/3, update_or_create/4, delete/2]).

new(_Name, _Type, _Config) ->  ok.
update(_Name, _Probe, _Config) ->  ok.
update_or_create(_Name, _Probe, _Type, _Config) -> ok.
delete(_Name, _Config) -> ok.
