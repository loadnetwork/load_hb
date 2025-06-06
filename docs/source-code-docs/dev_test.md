

# Module dev_test #

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>Example implementation of a <code>compute</code> handler.</td></tr><tr><td valign="top"><a href="#compute_test-0">compute_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#device_with_function_key_module_test-0">device_with_function_key_module_test/0*</a></td><td>Tests the resolution of a default function.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Exports a default_handler function that can be used to test the
handler resolution mechanism.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Example <code>init/3</code> handler.</td></tr><tr><td valign="top"><a href="#mul-2">mul/2</a></td><td>Example implementation of an <code>imported</code> function for a WASM
executor.</td></tr><tr><td valign="top"><a href="#postprocess-3">postprocess/3</a></td><td>Set the <code>postprocessor-called</code> key to true in the HTTP server.</td></tr><tr><td valign="top"><a href="#restore-3">restore/3</a></td><td>Example <code>restore/3</code> handler.</td></tr><tr><td valign="top"><a href="#restore_test-0">restore_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td>Do nothing when asked to snapshot.</td></tr><tr><td valign="top"><a href="#test_func-1">test_func/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compute-3"></a>

### compute/3 ###

`compute(Msg1, Msg2, Opts) -> any()`

Example implementation of a `compute` handler. Makes a running list of
the slots that have been computed in the state message and places the new
slot number in the results key.

<a name="compute_test-0"></a>

### compute_test/0 * ###

`compute_test() -> any()`

<a name="device_with_function_key_module_test-0"></a>

### device_with_function_key_module_test/0 * ###

`device_with_function_key_module_test() -> any()`

Tests the resolution of a default function.

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Exports a default_handler function that can be used to test the
handler resolution mechanism.

<a name="init-3"></a>

### init/3 ###

`init(Msg, Msg2, Opts) -> any()`

Example `init/3` handler. Sets the `Already-Seen` key to an empty list.

<a name="mul-2"></a>

### mul/2 ###

`mul(Msg1, Msg2) -> any()`

Example implementation of an `imported` function for a WASM
executor.

<a name="postprocess-3"></a>

### postprocess/3 ###

`postprocess(Msg, X2, Opts) -> any()`

Set the `postprocessor-called` key to true in the HTTP server.

<a name="restore-3"></a>

### restore/3 ###

`restore(Msg, Msg2, Opts) -> any()`

Example `restore/3` handler. Sets the hidden key `Test/Started` to the
value of `Current-Slot` and checks whether the `Already-Seen` key is valid.

<a name="restore_test-0"></a>

### restore_test/0 * ###

`restore_test() -> any()`

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(Msg1, Msg2, Opts) -> any()`

Do nothing when asked to snapshot.

<a name="test_func-1"></a>

### test_func/1 ###

`test_func(X1) -> any()`

