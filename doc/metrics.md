

# Module metrics #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md), [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-metric">metric()</a> ###


<pre><code>
metric() = counter | histogram | gauge | meter | spiral
</code></pre>




### <a name="type-probe">probe()</a> ###


<pre><code>
probe() = {c, integer()} | <a href="#type-value">value()</a>
</code></pre>




### <a name="type-state">state()</a> ###


<pre><code>
state() = #state{}
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

<pre><code>
code_change(OldVsn::term(), State::<a href="#type-state">state()</a>, Extra::term()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Name::list()) -&gt; ok | any()
</code></pre>
<br />

delete a metric

<a name="handle_call-3"></a>

### handle_call/3 ###

<pre><code>
handle_call(X1::term(), From::term(), State::<a href="#type-state">state()</a>) -&gt; {reply, term(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="handle_cast-2"></a>

### handle_cast/2 ###

<pre><code>
handle_cast(Msg::term(), State::<a href="#type-state">state()</a>) -&gt; {noreply, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="handle_info-2"></a>

### handle_info/2 ###

<pre><code>
handle_info(Info::term(), State::<a href="#type-state">state()</a>) -&gt; {noreply, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::term()) -&gt; {ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Type::<a href="#type-metric">metric()</a>, Name::list()) -&gt; ok
</code></pre>
<br />

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

<pre><code>
terminate(Reason::term(), State::<a href="#type-state">state()</a>) -&gt; ok
</code></pre>
<br />

<a name="update-1"></a>

### update/1 ###

<pre><code>
update(Name::list()) -&gt; ok | any()
</code></pre>
<br />

increment a counter with 1

<a name="update-2"></a>

### update/2 ###

<pre><code>
update(Name::list(), Probe::<a href="#type-probe">probe()</a>) -&gt; ok | any()
</code></pre>
<br />

update a metric

<a name="update_or_create-3"></a>

### update_or_create/3 ###

<pre><code>
update_or_create(Name::list(), Probe::<a href="#type-probe">probe()</a>, Type::<a href="#type-metric">metric()</a>) -&gt; ok | any()
</code></pre>
<br />

update a metric and create it if it doesn't exists

