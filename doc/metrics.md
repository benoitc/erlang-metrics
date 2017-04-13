

# Module metrics #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md), [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-metric">metric()</a> ###


<pre><code>
metric() = counter | histogram | gauge | meter | spiral | duration
</code></pre>




### <a name="type-metric_name">metric_name()</a> ###


<pre><code>
metric_name() = list()
</code></pre>




### <a name="type-probe">probe()</a> ###


<pre><code>
probe() = {c, integer()} | timer_start | timer_end | {duration_fun, function()} | <a href="#type-value">value()</a>
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = any()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#backend-0">backend/0</a></td><td>retrieve the current backend name.</td></tr><tr><td valign="top"><a href="#backend-1">backend/1</a></td><td>set the backend to use.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>delete a metric.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td> initialise a metric.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#update-1">update/1</a></td><td>increment a counter with 1.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>update a metric.</td></tr><tr><td valign="top"><a href="#update_or_create-3">update_or_create/3</a></td><td>update a metric and create it if it doesn't exists.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="backend-0"></a>

### backend/0 ###

<pre><code>
backend() -&gt; atom()
</code></pre>
<br />

retrieve the current backend name

<a name="backend-1"></a>

### backend/1 ###

<pre><code>
backend(Mod::atom()) -&gt; ok
</code></pre>
<br />

set the backend to use

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-metric_name">metric_name()</a></code></li><li><code>Result = ok | any()</code></li></ul>

delete a metric

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Metric, Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Metric = <a href="#type-metric">metric()</a></code></li><li><code>Name = <a href="#type-metric_name">metric_name()</a></code></li><li><code>Result = ok</code></li></ul>

initialise a metric

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::<a href="application.md#type-start_type">application:start_type()</a>, StartArgs::any()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(State::atom()) -&gt; ok
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="update-1"></a>

### update/1 ###

<pre><code>
update(Name) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-metric_name">metric_name()</a></code></li><li><code>Result = ok | any()</code></li></ul>

increment a counter with 1

<a name="update-2"></a>

### update/2 ###

<pre><code>
update(Name, Probe) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-metric_name">metric_name()</a></code></li><li><code>Probe = <a href="#type-probe">probe()</a></code></li><li><code>Result = ok | any()</code></li></ul>

update a metric

<a name="update_or_create-3"></a>

### update_or_create/3 ###

<pre><code>
update_or_create(Name, Probe, Metric) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Name = <a href="#type-metric_name">metric_name()</a></code></li><li><code>Probe = <a href="#type-probe">probe()</a></code></li><li><code>Metric = <a href="#type-metric">metric()</a></code></li><li><code>Result = ok | any()</code></li></ul>

update a metric and create it if it doesn't exists

