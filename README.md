

# metrics: A generic interface to different metrics systems in Erlang. #

Copyright (c) 2017-2018 Benoît Chesneau.

__Version:__ 2.5.0

# metrics

A generic interface to folsom, exometer, grapherl or any compliant interface. This
application has been extracted from
[hackney](https://github.com/benoitc/hackney).

Currently supported backend are:

- [Folsom](https://github.com/folsom-project/folsom): `metrics_folsom`
- [Exometer](https://github.com/Feuerlabs/exometer): `metrics_exometer`

> If you need the support for another backend, please [open a ticket](https://github.com/benoitc/erlang-metrics/issues).

[![Hex pm](http://img.shields.io/hexpm/v/metrics.svg?style=flat)](https://hex.pm/packages/metrics)

## Usage

### Set a backend

The backend can be set in the application environment using the `metrics_mod` setting or using `metrics:backend/1`.

### register a new metric:

```
metrics:new(counter, "c").
```

Depending on the backend the following metrics types can be passed: counter | histogram | gauge | meter | spiral

### update a metric

A counter can simply be incremented by 1 using `metrics:update/1`. or by passing a positive or negative integer like this:

```
metrics:update("c", {c, 1}).
```

Other metrics are updated via `metrics:update/2`.

## Example:

```erlang

1> application:ensure_all_started(metrics).
{ok,[metrics]}
2> application:ensure_all_started(folsom).
{ok,[bear,folsom]}
3> metrics:backend(metrics_folsom).
ok
4> metrics:new(counter, "c").
ok
5> metrics:update("c").
ok
6> folsom_metrics:get_metric_value("c").
1
7> metrics:update("c", {c, 1}).
ok
8> folsom_metrics:get_metric_value("c").
2

```

## Documentation

Full doc is available in the [`metrics`](http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics.md) module.

## Build

```
$ rebar3 compile
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics.md" class="module">metrics</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics_exometer.md" class="module">metrics_exometer</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics_folsom.md" class="module">metrics_folsom</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics_noop.md" class="module">metrics_noop</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics_process_tracker.md" class="module">metrics_process_tracker</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics_sup.md" class="module">metrics_sup</a></td></tr></table>

