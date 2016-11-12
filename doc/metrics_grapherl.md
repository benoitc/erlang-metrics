

# Module metrics_grapherl #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

metrics handler is https://github.com/processone/grapherl.

<a name="description"></a>

## Description ##

This backend can be set via the barrel_metrics env:

- port: 11111 by default, the port to connect
- peer: 127.0.0.1 by default, the address to connect
- host: to use in the basic ID of the metric<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#send_metrics-3">send_metrics/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td></td></tr><tr><td valign="top"><a href="#update_or_create-4">update_or_create/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-2"></a>

### delete/2 ###

`delete(Name, Config) -> any()`

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

<a name="new-3"></a>

### new/3 ###

`new(X1, Name, Config) -> any()`

<a name="send_metrics-3"></a>

### send_metrics/3 ###

`send_metrics(Name, Probe, Config) -> any()`

<a name="update-3"></a>

### update/3 ###

`update(Name, Probe, Config) -> any()`

<a name="update_or_create-4"></a>

### update_or_create/4 ###

`update_or_create(Name, Probe, Type, Config) -> any()`

