%%% -*- erlang -*-
%%%
%%% This file is part of metrics released under the BSD license.
%%% See the LICENSE for more information.
%%%
%%% @doc dummy metric module
%%%
-module(metrics_dummy).


-export([
    new/2,
    delete/1,
    sample/1,
    get_value/1,
    increment_counter/1,
    increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_gauge/2,
    update_meter/2,
    increment_spiral/1,
    increment_spiral/2,
    decrement_spiral/1,
    decrement_spiral/2]).


new(_, _) ->
    ok.

delete(_) ->
    ok.

sample(_) ->
    {error, unsuported}.

get_value(_) ->
    {error, unsuported}.
    
increment_counter(_) ->
    ok.

increment_counter(_, _) ->
    ok.

decrement_counter(_) ->
    ok.

decrement_counter(_, _) ->
    ok.

update_histogram(_, Fun) when is_function(Fun, 0) ->
    Fun();
update_histogram(_, _) ->
    ok.

update_gauge(_, _) ->
    ok.

update_meter(_, _) ->
    ok.

increment_spiral(_) ->
    ok.

increment_spiral(_, _) ->
    ok.

decrement_spiral(_) ->
    ok.

decrement_spiral(_, _) ->
    ok.
