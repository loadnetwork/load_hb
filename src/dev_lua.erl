%%% @doc A device that calls a Lua script upon a request and returns the result.
-module(dev_lua).
-export([info/1, init/3, snapshot/3, normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%%% The set of functions that will be sandboxed by default if `sandbox` is set 
%%% to only `true`. Setting `sandbox` to a map allows the invoker to specify
%%% which functions should be sandboxed and what to return instead. Providing
%%% a list instead of a map will result in all functions being sandboxed and
%%% returning `sandboxed'.
-define(DEFAULT_SANDBOX, [
    {['_G', io], <<"sandboxed">>},
    {['_G', file], <<"sandboxed">>},
    {['_G', os, execute], <<"sandboxed">>},
    {['_G', os, exit], <<"sandboxed">>},
    {['_G', os, getenv], <<"sandboxed">>},
    {['_G', os, remove], <<"sandboxed">>},
    {['_G', os, rename], <<"sandboxed">>},
    {['_G', os, tmpname], <<"sandboxed">>},
    {['_G', package], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', require], <<"sandboxed">>},
    {['_G', dofile], <<"sandboxed">>},
    {['_G', load], <<"sandboxed">>},
    {['_G', loadfile], <<"sandboxed">>},
    {['_G', loadstring], <<"sandboxed">>}
]).

%% @doc All keys that are not directly available in the base message are 
%% resolved by calling the Lua function in the script of the same name.
info(Base) ->
    #{
        default => fun compute/4,
        excludes => [<<"keys">>, <<"set">>] ++ maps:keys(Base)
    }.

%% @doc Initialize the device state, loading the script into memory if it is 
%% a reference.
init(Base, Req, Opts) ->
    ensure_initialized(Base, Req, Opts).

%% @doc Find the script in the base message, either by ID or by string.
find_script(Base, Opts) ->
    case hb_ao:get(<<"script-id">>, {as, <<"message@1.0">>, Base}, Opts) of
        not_found ->
            case hb_ao:get(<<"script">>, {as, <<"message@1.0">>, Base}, Opts) of
                not_found ->
                    {error, <<"missing-script-id-or-script">>};
                Script ->
                    {ok, Script}
            end;
        ScriptID ->
            case hb_cache:read(ScriptID, Opts) of
                {ok, Script} ->
                    Data = hb_ao:get(<<"data">>, Script, #{}),
                    ?event(debug_lua, { data, Data}),
                    {ok, Data};
                {error, Error} ->
                    {error, #{
                        <<"status">> => 404,
                        <<"body">> =>
                            <<"Lua script '", ScriptID/binary, "' not available.">>,
                        <<"cache-error">> => Error
                    }}
            end
    end.

%% @doc Initialize the Lua VM if it is not already initialized. Optionally takes
%% the script as a  Binary string. If not provided, the script will be loaded
%% from the base message.
ensure_initialized(Base, _Req, Opts) ->
    case hb_private:from_message(Base) of
        #{<<"state">> := _} -> 
            ?event(debug_lua, lua_state_already_initialized),
            {ok, Base};
        _ ->
            ?event(debug_lua, initializing_lua_state),
            case find_script(Base, Opts) of
                {ok, Script} ->
                    initialize(Base, Script, Opts);
                Error ->
                    Error
            end
    end.

%% @doc Initialize a new Lua state with a given base message and script.
initialize(Base, Script, Opts) ->
    State0 = luerl:init(),
    {ok, _, State1} = luerl:do_dec(Script, State0),
    State2 =
        case hb_ao:get(<<"sandbox">>, {as, <<"message@1.0">>, Base}, false, Opts) of
            false -> State1;
            true -> sandbox(State1, ?DEFAULT_SANDBOX, Opts);
            Spec -> sandbox(State1, Spec, Opts)
        end,
    {ok, State3} = add_ao_core_resolver(Base, State2, Opts),
    % Return the base message with the state added to it.
    {ok, hb_private:set(Base, <<"state">>, State3, Opts)}.

%% @doc Sandbox (render inoperable) a set of Lua functions. Each function is
%% referred to as if it is a path in AO-Core, with its value being what to 
%% return to the caller. For example, 'os.exit' would be referred to as
%% referred to as `os/exit'. If preferred, a list rather than a map may be
%% provided, in which case the functions all return `sandboxed'.
sandbox(State, Map, Opts) when is_map(Map) ->
    sandbox(State, maps:to_list(Map), Opts);
sandbox(State, [], _Opts) ->
    State;
sandbox(State, [{Path, Value} | Rest], Opts) ->
    {ok, NextState} = luerl:set_table_keys_dec(Path, Value, State),
    sandbox(NextState, Rest, Opts);
sandbox(State, [Path | Rest], Opts) ->
    {ok, NextState} = luerl:set_table_keys_dec(Path, <<"sandboxed">>, State),
    sandbox(NextState, Rest, Opts).

%% @doc Add a HTTP-style AO-Core resolution function to the Lua environment.
%% Optionally, limit the devices that the environment can make use of.
add_ao_core_resolver(Base, State, Opts) ->
    % Calculate and set the new `preloaded_devices' option.
    AllDevs = hb_opts:get(preloaded_devices, Opts),
    DevSandboxDef =
        hb_ao:get(
            <<"device-sandbox">>,
            {as, <<"message@1.0">>, Base},
            false,
            Opts
        ),
    AdmissibleDevs =
        case DevSandboxDef of
            false -> AllDevs;
            DevNames ->
                lists:map(
                    fun(Name) ->
                        [Dev] =
                            lists:filter(
                                fun(X) ->
                                    hb_ao:get(<<"name">>, X, Opts) == Name
                                end,
                                AllDevs
                            ),
                        Dev
                    end,
                    hb_util:message_to_ordered_list(DevNames)
                )
        end,
    ?event({adding_ao_core_resolver, {devs, AdmissibleDevs}}),
    ExecOpts = Opts#{ preloaded_devices => AdmissibleDevs },
    % Initialize the AO-Core resolver.
    BaseAOTable =
        case luerl:get_table_keys_dec([ao], State) of
            {ok, nil, _} ->
                ?event(no_ao_table),
                #{};
            {ok, ExistingTable, _} ->
                ?event({existing_ao_table, ExistingTable}),
                decode(ExistingTable)
        end,
    ?event({base_ao_table, BaseAOTable}),
    {ok, State2} =
        luerl:set_table_keys_dec(
            [ao],
            encode(BaseAOTable),
            State
        ),
    % Add the AO-Core resolver to the base AO table.
    luerl:set_table_keys_dec(
        [ao, resolve],
        fun([EncodedMsg], ExecState) ->
            AOMsg = decode(luerl:decode(EncodedMsg, ExecState)),
            ?event({ao_core_resolver, {msg, AOMsg}}),
            ParsedMsgs = hb_singleton:from(AOMsg),
            ?event({parsed_msgs_to_resolve, ParsedMsgs}),
            try hb_ao:resolve_many(ParsedMsgs, ExecOpts) of
                {Status, Res} ->
                    ?event({resolved_msgs, {status, Status}, {res, Res}}),
                    {[Status, encode(Res)], ExecState}
            catch
                Error ->
                    ?event({ao_core_resolver_error, Error}),
                    {error, Error}
            end
        end,
        State2
    ).

%% @doc Call the Lua script with the given arguments.
compute(Key, RawBase, Req, Opts) ->
    ?event(debug_lua, compute_called),
    {ok, Base} = ensure_initialized(RawBase, Req, Opts),
    ?event(debug_lua, ensure_initialized_done),
    % Get the state from the base message's private element.
    OldPriv = #{ <<"state">> := State } = hb_private:from_message(Base),
    % TODO: looks like the script is injected in multiple places, does the 
    % script need to be passed?
    % Get the Lua function to call from the base message.
    Function =
        hb_ao:get_first(
            [
                {Req, <<"body/function">>},
                {Req, <<"function">>},
                {{as, <<"message@1.0">>, Base}, <<"function">>}
            ],
            Key,
            Opts#{ hashpath => ignore }
        ),
    ?event(debug_lua, function_found),
    Params =
        hb_ao:get_first(
            [
                {Req, <<"body/parameters">>},
                {Req, <<"parameters">>},
                {{as, <<"message@1.0">>, Base}, <<"parameters">>}
            ],
            [
                hb_private:reset(Base),
                Req,
                #{}
            ],
            Opts#{ hashpath => ignore }
        ),
    ?event(debug_lua, parameters_found),
    % Call the VM function with the given arguments.
    ?event({calling_lua_func, {function, Function}, {args, Params}, {req, Req}}),
    ?event(debug_lua, calling_lua_func),
    % ?event(debug_lua, {lua_params, Params}),
    case luerl:call_function_dec([Function], encode(Params), State) of
        {ok, [LuaResult], NewState} when is_map(LuaResult) ->
            ?event(debug_lua, got_lua_result),
            Result = decode(LuaResult),
            ?event(debug_lua, decoded_result),
            {ok, Result#{
                <<"priv">> => OldPriv#{
                    <<"state">> => NewState
                }
            }};
        {ok, [LuaResult], _NewState} ->
            ?event(debug_lua, got_lua_result),
            {ok, LuaResult};
        {lua_error, Error, Details} ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => Error,
                <<"details">> => Details
            }}
    end.

%% @doc Snapshot the Lua state from a live computation. Normalizes its `priv'
%% state element, then serializes the state to a binary.
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        State ->
            {ok,
                #{
                    <<"body">> =>
                        term_to_binary(luerl:externalize(State))
                }
            }
    end.

%% @doc Restore the Lua state from a snapshot, if it exists.
normalize(Base, _Req, RawOpts) ->
    Opts = RawOpts#{ hashpath => ignore },
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            DeviceKey =
                case hb_ao:get(<<"device-key">>, {as, <<"message@1.0">>, Base}, Opts) of
                    not_found -> [];
                    Key -> [Key]
                end,
            ?event(
                {attempting_to_restore_lua_state,
                    {msg1, Base}, {device_key, DeviceKey}
                }
            ),
            SerializedState =
                hb_ao:get(
                    [<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                    {as, dev_message, Base},
                    Opts
                ),
            case SerializedState of
                not_found -> throw({error, no_lua_state_snapshot_found});
                State ->
                    ExternalizedState = binary_to_term(State),
                    InternalizedState = luerl:internalize(ExternalizedState),
                    {ok, hb_private:set(Base, <<"state">>, InternalizedState, Opts)}
            end;
        _ ->
            ?event(state_already_initialized),
            {ok, Base}
    end.

%% @doc Decode a Lua result into a HyperBEAM `structured@1.0' message.
decode(Map = [{_K, _V} | _]) when is_list(Map) ->
    maps:map(fun(_, V) -> decode(V) end, maps:from_list(Map));
decode(Other) ->
    Other.

%% @doc Encode a HyperBEAM `structured@1.0' message into a Lua result.
encode(Map) when is_map(Map) ->
    maps:to_list(maps:map(fun(_, V) -> encode(V) end, Map));
encode(List) when is_list(List) ->
    lists:map(fun encode/1, List);
encode(Other) ->
    Other.

%%% Tests
simple_invocation_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"parameters">> => []
    },
    ?assertEqual(2, hb_ao:get(<<"assoctable/b">>, Base, #{})).

sandboxed_failure_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"parameters">> => [],
        <<"sandbox">> => true
    },
    ?assertMatch({error, _}, hb_ao:resolve(Base, <<"sandboxed_fail">>, #{})).

%% @doc Run an AO-Core resolution from the Lua environment.
ao_core_sandbox_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"parameters">> => [],
        <<"device-sandbox">> => [<<"message@1.0">>]
    },
    ?assertMatch({error, _}, hb_ao:resolve(Base, <<"ao_relay">>, #{})),
    ?assertMatch({ok, _}, hb_ao:resolve(Base, <<"ao_resolve">>, #{})).

%% @doc Run an AO-Core resolution from the Lua environment.
ao_core_resolution_from_lua_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"parameters">> => []
    },
    {ok, Res} = hb_ao:resolve(Base, <<"ao_resolve">>, #{}),
    ?assertEqual(<<"Hello, AO world!">>, Res).

%% @doc Benchmark the performance of Lua executions.
direct_benchmark_test() ->
    BenchTime = 3,
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"parameters">> => []
    },
    Iterations = hb:benchmark(
        fun(X) ->
            {ok, _} = hb_ao:resolve(Base, <<"assoctable">>, #{}),
            ?event({iteration, X})
        end,
        BenchTime
    ),
    ?event({iterations, Iterations}),
    hb_util:eunit_print(
        "Computed ~p Lua executions in ~ps (~.2f calls/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 10).

%% @doc Call a non-compute key on a Lua device message and ensure that the
%% function of the same name in the script is called.
invoke_non_compute_key_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"test-value">> => 42
    },
    {ok, Result1} = hb_ao:resolve(Base, <<"hello">>, #{}),
    ?event({result1, Result1}),
    ?assertEqual(42, hb_ao:get(<<"test-value">>, Result1, #{})),
    ?assertEqual(<<"world">>, hb_ao:get(<<"hello">>, Result1, #{})),
    {ok, Result2} =
        hb_ao:resolve(
            Base,
            #{<<"path">> => <<"hello">>, <<"name">> => <<"Alice">>},
            #{}
        ),
    ?event({result2, Result2}),
    ?assertEqual(<<"Alice">>, hb_ao:get(<<"hello">>, Result2, #{})).

%% @doc Use a Lua script as a preprocessor on the HTTP server via `~meta@1.0'.
lua_http_preprocessor_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(
        #{
            preprocessor =>
                #{
                    <<"device">> => <<"lua@5.3a">>,
                    <<"script">> => Script
                }
        }),
    {ok, Res} = hb_http:get(Node, <<"/hello?hello=world">>, #{}),
    ?assertMatch(#{ <<"body">> := <<"i like turtles">> }, Res).

%% @doc Call a process whose `execution-device' is set to `lua@5.3a'.
pure_lua_process_test() ->
    Process = generate_lua_process("test/test.lua"),
    {ok, _} = hb_cache:write(Process, #{}),
    Message = generate_test_message(Process),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
    ?event({results, Results}),
    ?assertEqual(42, hb_ao:get(<<"results/output/body">>, Results, #{})).

pure_lua_process_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchMsgs = 200,
        Process = generate_lua_process("test/test.lua"),
        Message = generate_test_message(Process),
        lists:foreach(
            fun(X) ->
                hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
                ?event(debug_lua, {scheduled, X})
            end,
            lists:seq(1, BenchMsgs)
        ),
        ?event(debug_lua, {executing, BenchMsgs}),
        BeforeExec = os:system_time(millisecond),
        {ok, _} = hb_ao:resolve(
            Process,
            <<"now">>,
            #{ hashpath => ignore, process_cache_frequency => 50 }
        ),
        AfterExec = os:system_time(millisecond),
        ?event(debug_lua, {execution_time, (AfterExec - BeforeExec) / BenchMsgs}),
        hb_util:eunit_print(
            "Computed ~p pure Lua process executions in ~ps (~.2f calls/s)",
            [
                BenchMsgs,
                (AfterExec - BeforeExec) / 1000,
                BenchMsgs / ((AfterExec - BeforeExec) / 1000)
            ]
        )
    end}.

invoke_aos_test() ->
    Process = generate_lua_process("test/hyper-aos.lua"),
    {ok, _} = hb_cache:write(Process, #{}),
    Message = generate_test_message(Process),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output/data">>, #{}),
    ?assertEqual(<<"1">>, Results).

aos_authority_not_trusted_test() ->
    Process = generate_lua_process("test/hyper-aos.lua"),
    ProcID = hb_message:id(Process, all),
    {ok, _} = hb_cache:write(Process, #{}),
    GuestWallet = ar_wallet:new(),
    Message = hb_message:commit(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(
                #{
                    <<"target">> => ProcID,
                    <<"type">> => <<"Message">>,
                    <<"data">> => <<"1 + 1">>,
                    <<"random-seed">> => rand:uniform(1337),
                    <<"action">> => <<"Eval">>

        }, GuestWallet)
      }, GuestWallet
    ),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
    {ok, Results} = hb_ao:resolve(Process, <<"now/results/output/data">>, #{}),
    ?assertEqual(Results, <<"Message is not trusted.">>).

%% @doc Benchmark the performance of Lua executions.
aos_process_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchMsgs = 200,
        Process = generate_lua_process("test/hyper-aos.lua"),
        Message = generate_test_message(Process),
        lists:foreach(
            fun(X) ->
                hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
                ?event(debug_lua, {scheduled, X})
            end,
            lists:seq(1, BenchMsgs)
        ),
        ?event(debug_lua, {executing, BenchMsgs}),
        BeforeExec = os:system_time(millisecond),
        {ok, _} = hb_ao:resolve(
            Process,
            <<"now">>,
            #{ hashpath => ignore, process_cache_frequency => 50 }
        ),
        AfterExec = os:system_time(millisecond),
        ?event(debug_lua, {execution_time, (AfterExec - BeforeExec) / BenchMsgs}),
        hb_util:eunit_print(
            "Computed ~p AOS process executions in ~ps (~.2f calls/s)",
            [
                BenchMsgs,
                (AfterExec - BeforeExec) / 1000,
                BenchMsgs / ((AfterExec - BeforeExec) / 1000)
            ]
        )
    end}.

%%% Test helpers

%% @doc Generate a Lua process message.
generate_lua_process(File) ->
    Wallet = hb:wallet(),
    {ok, Script} = file:read_file(File),
    hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"type">> => <<"Process">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"execution-device">> => <<"lua@5.3a">>,
        <<"script">> => Script,
        <<"authority">> => [ 
          hb:address(), 
          <<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
        ], 
        <<"scheduler-location">> =>
            hb_util:human_id(ar_wallet:to_address(Wallet)),
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet).

%% @doc Generate a test message for a Lua process.
generate_test_message(Process) ->
    ProcID = hb_message:id(Process, all),
    Wallet = hb:wallet(),
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"data">> => <<"Count = 0\n function add() Send({Target = 'Foo', Data = 'Bar' }); Count = Count + 1 end\n add()\n return Count">>,
                        <<"random-seed">> => rand:uniform(1337),
                        <<"action">> => <<"Eval">>
                    },
                    Wallet
                )
        },
        Wallet
    ).

%% @doc Generate a stack message for the Lua process.
generate_stack(File) ->
    Wallet = hb:wallet(),
    {ok, Script} = file:read_file(File),
    Msg1 = #{
        <<"device">> => <<"Stack@1.0">>,
        <<"device-stack">> =>
            [
                <<"json-iface@1.0">>,
                <<"lua@5.3a">>,
                <<"multipass@1.0">>
            ],
        <<"function">> => <<"json_result">>,
        <<"passes">> => 2,
        <<"stack-keys">> => [<<"init">>, <<"compute">>],
        <<"script">> => Script,
        <<"process">> => 
            hb_message:commit(#{
                <<"type">> => <<"Process">>,
                <<"script">> => Script,
                <<"scheduler">> => hb:address(),
                <<"authority">> => hb:address()
            }, Wallet)
    },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    Msg2.

execute_aos_call(Base) ->
    Req =
        hb_message:commit(#{
                <<"action">> => <<"Eval">>,
                <<"function">> => <<"json_result">>,
                <<"data">> => <<"return 2">>
            },
            hb:wallet()
        ),
    execute_aos_call(Base, Req).
execute_aos_call(Base, Req) ->
    hb_ao:resolve(Base,
        #{
            <<"path">> => <<"compute">>,
            <<"body">> => Req
        },
        #{}
    ).