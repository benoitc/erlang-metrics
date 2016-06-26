%% Copyright (c) 2016, Benoit Chesneau.
%%
%% This file is part of barrel_metrics released under the BSD license.
%% See the NOTICE for more information.

%% Created by benoitc on 26/06/16.

-module(metrics_tests).
-author("Benoit Chesneau").


-include_lib("eunit/include/eunit.hrl").

setup() ->
  {ok, _} =  application:ensure_all_started(metrics),
  {ok, _} =  application:ensure_all_started(folsom),
  {ok, _} =  application:ensure_all_started(exometer_core),
  ok.

teardown(_) ->
  application:stop(folsom),
  application:stop(exometer_core),
  application:stop(metrics),
  ok.


folsom_test_() ->
  {
    "Folsom backend test",
    {foreach,
      fun setup/0, fun teardown/1,
      [

        fun folsom_counter_test_/1,
        fun folsom_counter_test_inc_/1,
        fun folsom_counter_test_mul_/1,
        fun folsom_gauge_test_/1
      ]
    }
  }.


exometer_test_() ->
  {
    "Exometer backend test",
    {foreach,
      fun setup/0, fun teardown/1,
      [

        fun exometer_counter_test_/1,
        fun exometer_counter_test_inc_/1,
        fun exometer_counter_test_mul_/1,
        fun exometer_gauge_test_/1
      ]
    }
  }.

folsom_counter_test_(_) ->
  ok = metrics:backend(metrics_folsom),
  ok = metrics:new(counter, "c"),
  metrics:update("c"),
  ?_assertEqual(1, folsom_metrics:get_metric_value("c")).

folsom_counter_test_inc_(_) ->
  ok = metrics:backend(metrics_folsom),
  ok = metrics:new(counter, "c"),
  metrics:update("c", {c, 1}),
  ?_assertEqual(1, folsom_metrics:get_metric_value("c")).

folsom_counter_test_mul_(_) ->
  ok = metrics:backend(metrics_folsom),
  ok = metrics:new(counter, "c"),
  metrics:update("c", {c, 1}),
  metrics:update("c", {c, 1}),
  metrics:update("c", {c, 4}),
  metrics:update("c", {c, -1}),
  ?_assertEqual(5, folsom_metrics:get_metric_value("c")).

folsom_gauge_test_(_) ->
  ok = metrics:backend(metrics_folsom),
  ok = metrics:new(gauge, "g"),
  metrics:update("g", 1),
  ?_assertEqual(1, folsom_metrics:get_metric_value("g")).

exometer_counter_test_(_) ->
  ok = metrics:backend(metrics_exometer),
  ok = metrics:new(counter, "c1"),
  metrics:update("c1"),
  ?_assertMatch({ok, [{value, 1}, _]}, exometer:get_value("c1")).

exometer_counter_test_inc_(_) ->
  ok = metrics:backend(metrics_exometer),
  ok = metrics:new(counter, "c1"),
  metrics:update("c1", {c, 1}),
  ?_assertMatch({ok, [{value, 1}, _]}, exometer:get_value("c1")).

exometer_counter_test_mul_(_) ->
  ok = metrics:backend(metrics_exometer),
  ok = metrics:new(counter, "c1"),
  metrics:update("c1", {c, 1}),
  metrics:update("c1", {c, 1}),
  metrics:update("c1", {c, 4}),
  metrics:update("c1", {c, -1}),
  ?_assertMatch({ok, [{value, 5}, _]}, exometer:get_value("c1")).

exometer_gauge_test_(_) ->
  ok = metrics:backend(metrics_exometer),
  ok = metrics:new(gauge, "g1"),
  metrics:update("g1", 1),
  ?_assertMatch({ok, [{value, 1}, _]}, exometer:get_value("g1")).


