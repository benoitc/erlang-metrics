

# metrics: A generic interface to different metrics systems in Erlang. #

Copyright (c) 2015 Benoît Chesneau.

__Version:__ 0.1.0.

# metrics

A generic interface to folsom or exometer or any compliant interface. This
application have been extracted from
[hackney](https://github.com/benoitc/hackney).

Currently support [Folsom](https://github.com/folsom-project/folsom) and [Exometer](https://github.com/Feuerlabs/exometer)

[![Hex pm](http://img.shields.io/hexpm/v/metrics.svg?style=flat)](https://hex.pm/packages/metrics)

Example:
--------

```erlang


%% initialize an engine
Engine = metrics:init(metrics_exometer),

%% create a counter named TestCounter
ok = metrics:new(Engine, counter, TestCounter),

%% Increment the counter

metrics:increment_counter(Engine, TestCounter).
```

## Documentation

Full doc is available in the [`metrics`](http://github.com/benoitc/erlang-metrics/blob/master/doc/metrics.md) module.

## Build

```
$ rebar3 compile
```

