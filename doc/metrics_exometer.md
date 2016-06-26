

# Module metrics_exometer #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

exometer backend.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(X1::atom(), Name::any(), Config::#{}) -&gt; ok | {error, metric_exists | unsupported_type}
</code></pre>
<br />

<a name="update-3"></a>

### update/3 ###

`update(Name, Val, Config) -> any()`

