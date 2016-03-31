%%% -*- erlang -*-
%%%
%%% This file is part of metrics released under the BSD license.
%%% See the LICENSE for more information.
%%%
%% @doc metric module for folsom
%%
-module(metrics_folsom).


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

-spec new(atom(), any()) -> ok | {error, term()}.
new(counter, Name) ->
    folsom_metrics:new_counter(Name);
new(histogram, Name) ->
    folsom_metrics:new_histogram(Name);
new(gauge, Name) ->
    folsom_metrics:new_gauge(Name);
new(meter, Name) ->
    folsom_metrics:new_meter(Name);
new(spiral, Name) ->
    folsom_metrics:new_spiral(Name);
new(_, _) ->
    {error, unsupported_type}.

delete(Name) ->
    folsom_metrics:delete_metric(Name).


sample(Name) ->
    case folsom_metrics:get_metric_info(Name) of
        [{Name, Info}] ->
            Type = proplists:get_value(type, Info),
            case Type of
                histogram ->
                    Vals = folsom_metrics:get_histogram_statistics(Name),
                    try [filter_dp(D, Vals) || D <- datapoints(Type)]
                    catch
                        error:_Error ->
                            unavailable
                    end;
                counter ->
                    ok;
                _ ->
                    {error, unsuported}
            end;
        [] ->
            {error, not_found}
    end.

get_value(Name) ->
    case folsom_metrics:get_metric_info(Name) of
        [{Name, Info}] ->
            Type = proplists:get_value(type, Info),
            Vals = get_value_(Name, Type),
            try [filter_dp(D, Vals) || D <- datapoints(Type)]
            catch
                error:_Error ->
                    unavailable
            end;
        [] ->
            {error, not_found}
    end.


get_value_(Name, counter) ->
    [{value, folsom_metrics_counter:get_value(Name)}];
get_value_(Name, gauge) ->
    [{value, folsom_metrics_gauge:get_value(Name)}];
get_value_(Name, histogram) ->
    folsom_metrics:get_histogram_statistics(Name);
get_value_(Name, meter) ->
    folsom_metrics:get_metric_value(Name);
get_value_(Name, spiral) ->
    folsom_metrics_spiral:get_values(Name).

-spec increment_counter(any()) -> ok | {error, term()}.
increment_counter(Name) ->
    notify(Name, {inc, 1}, counter).

-spec increment_counter(any(), pos_integer()) ->  ok | {error, term()}.
increment_counter(Name, Value) ->
    notify(Name, {inc, Value}, counter).

-spec decrement_counter(any()) ->  ok | {error, term()}.
decrement_counter(Name) ->
    notify(Name, {dec, 1}, counter).

-spec decrement_counter(any(), pos_integer()) ->  ok | {error, term()}.
decrement_counter(Name, Value) ->
    notify(Name, {dec, Value}, counter).

-spec update_histogram(any(), number()) ->  ok | {error, term()};
                      (any(), function()) ->  ok | {error, term()}.
update_histogram(Name, Fun) when is_function(Fun, 0) ->
    Begin = os:timestamp(),
    Result = Fun(),
    Duration = timer:now_diff(os:timestamp(), Begin) div 1000,
    case notify(Name, Duration, histogram) of
        ok -> Result;
        Error -> throw(Error)
    end;
update_histogram(Name, Value) when is_number(Value) ->
    notify(Name, Value, histogram).

-spec update_gauge(any(), number()) ->  ok | {error, term()}.
update_gauge(Name, Value) ->
    notify(Name, Value, gauge).

-spec update_meter(any(), number()) ->  ok | {error, term()}.
update_meter(Name, Value) ->
    notify(Name, Value, meter).

-spec increment_spiral(any()) -> ok | {error, term()}.
increment_spiral(Name) ->
    notify(Name, 1, spiral).

-spec increment_spiral(any(), pos_integer()) ->  ok | {error, term()}.
increment_spiral(Name, Value) ->
    notify(Name, Value, spiral).

-spec decrement_spiral(any()) ->  ok | {error, term()}.
decrement_spiral(Name) ->
    notify(Name, 1, spiral).

-spec decrement_spiral(any(), pos_integer()) ->  ok | {error, term()}.
decrement_spiral(Name, Value) ->
    notify(Name, Value, spiral).


-spec notify(any(), any(), atom()) ->  ok | {error, term()}.
notify(Name, Op, Type) ->
    case folsom_metrics:notify(Name, Op) of
        ok -> ok;
        {error, Name, nonexistent_metric} ->
            %% the metric doesn't exists, create it.
            new(Type, Name),
            %% then notify
            folsom_metrics:notify(Name, Op);
        Error ->
            io:format("error is ~p~n", [Error]),

            Error
    end.

%% @private
%% identical format to exometer

datapoints(counter) ->
    [value];
datapoints(gauge) ->
    [value];
datapoints(histogram) ->
    stats_datapoints();
datapoints(duration) ->
    [count, last |stats_datapoints()];
datapoints(spiral) ->
    [one, count];
datapoints(meter) ->
    [count,one,five,fifteen,day,mean,acceleration];
datapoints(history) ->
    [events, info].

stats_datapoints() ->
    [n,mean,min,max,median,50,75,90,95,99,999].

filter_dp(Mean, DPs) when Mean==mean; Mean==arithmetic_mean ->
    case lists:keyfind(mean, 1, DPs) of
        false ->
            case lists:keyfind(arithmetic_mean, 1, DPs) of
                false -> {mean, 0};
                {_,V} -> {mean, maybe_trunc(V)}
            end;
        {_,V} -> {mean, maybe_trunc(V)}
    end;
filter_dp(H, DPs) when is_integer(H) ->
    case lists:keyfind(H, 1, DPs) of
        false ->
            case lists:keyfind(percentile, 1, DPs) of
                false -> {H, 0};
                {_, Ps} -> get_dp(H, Ps)
            end;
        {_,V} -> {H, maybe_trunc(V)}
    end;
filter_dp(H, DPs) ->
    get_dp(H, DPs).

maybe_trunc(V) when is_float(V) -> trunc(V);
maybe_trunc(V) -> V.

get_dp(K, DPs) ->
    case lists:keyfind(K, 1, DPs) of
        false -> {K, 0};
        {_, V} -> {K, maybe_trunc(V)}
    end.
