%%% @doc a device to interact with the Quantum Runtime execution client
-module(dev_quantum).
-export([info/1, info/3, compute/3, list_functions/3]).

info(_) ->
    #{
        <<"default">> => dev_message,
        handlers => #{
            <<"info">> => fun info/3,
            <<"compute">> => fun compute/3,
            <<"list_functions">> => fun list_functions/3
        }
    }.

%% @doc return quantum device info
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Quantum Runtime device for serverless quantum functions">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"compute">> => <<"Execute serverless quantum function">>,
            <<"list_functions">> => <<"List available quantum functions">>
        },
        <<"available_functions">> => [
            <<"superposition">>,
            <<"quantum_rng">>
        ]
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc list available quantum functions
list_functions(_Msg1, _Msg2, _Opts) ->
    Functions = #{
        <<"functions">> => [
            #{
                <<"id">> => <<"superposition">>,
                <<"description">> => <<"Creates quantum superposition state">>,
                <<"recommended_qubits">> => 1,
                <<"recommended_measurements">> => [0]
            },
            #{
                <<"id">> => <<"quantum_rng">>,
                <<"description">> => <<"Quantum random number generator">>,
                <<"recommended_qubits">> => 4,
                <<"recommended_measurements">> => [0, 1, 2, 3]
            }
        ]
    },
    {ok, #{<<"status">> => 200, <<"body">> => Functions}}.

%% @doc execute a quantum function
compute(Msg1, _Msg2, Opts) ->
    try
        % decode the JSON body
        RawBody = hb_ao:get(<<"body">>, Msg1, not_found, Opts),
        Body = hb_json:decode(RawBody),
        
        % Get parameters from decoded body
        FunctionId = maps:get(<<"function_id">>, Body),
        NumQubits = maps:get(<<"num_qubits">>, Body, 2),  % default to 2 qubits
        Measurements = maps:get(<<"measurements">>, Body, [0, 1]),  % default measurements
        
        % Validate inputs
        case validate_compute_params(FunctionId, NumQubits, Measurements) of
            ok ->
                % Execute quantum computation
                Result = quantum_runtime_nif:compute(NumQubits, FunctionId, Measurements),
                {ok, #{<<"status">> => 200, <<"body">> => Result}};
            {error, ErrorMsg} ->
                {error, #{
                    <<"status">> => 400,
                    <<"body">> => #{
                        <<"error">> => <<"Invalid parameters">>,
                        <<"details">> => ErrorMsg
                    }
                }}
        end
    catch
        error:Error ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to execute quantum computation">>,
                    <<"details">> => Error
                }
            }}
    end.

%% @doc validate compute parameters
validate_compute_params(FunctionId, NumQubits, Measurements) ->
    ValidFunctions = [<<"superposition">>, <<"quantum_rng">>],
    case lists:member(FunctionId, ValidFunctions) of
        false ->
            {error, <<"Invalid function_id. Available: superposition, quantum_rng">>};
        true ->
            case is_integer(NumQubits) andalso NumQubits > 0 andalso NumQubits =< 32 of
                false ->
                    {error, <<"num_qubits must be integer between 1 and 32">>};
                true ->
                    case is_list(Measurements) andalso length(Measurements) > 0 of
                        false ->
                            {error, <<"measurements must be non-empty list">>};
                        true ->
                            case lists:all(fun(M) -> is_integer(M) andalso M >= 0 andalso M < NumQubits end, Measurements) of
                                false ->
                                    {error, <<"all measurements must be valid qubit indices">>};
                                true ->
                                    ok
                            end
                    end
            end
    end.

