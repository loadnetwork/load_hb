%%% @doc A device that allows querying the Arweave permaweb, inspired by roam.ar.io
%%% built on top of ureq HTTP client, and goldsky GQL gateway the roam@1.0 device provides

-module(dev_roam).
-export([info/1, info/3, query_permaweb/3, hello/3, test_ao/0]).

info(_) ->
    #{
        <<"default">> => dev_message,
        handlers => #{
            <<"info">> => fun info/3,
            <<"query_permaweb">> => fun query_permaweb/3,
            <<"hello">> => fun hello/3
        }
    }.

%% @doc return roam device info
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Roam device for interacting with my_device_nif">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"query_permaweb">> => <<"Query the Arweave permaweb">>,
            <<"hello">> => <<"Simple hello world test">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc simple hello world endpoint
hello(_Msg1, _Msg2, _Opts) ->
    try
        Result = my_device_nif:hello(),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error:Stack ->
            io:format("~nError: ~p~n", [Error]),
            io:format("Stack: ~p~n", [Stack]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to call hello">>,
                    <<"details">> => Error
                }
            }}
    end.

%% @doc query the Arweave permaweb
query_permaweb(_Msg1, _Msg2, _Opts) ->
    try
        Result = my_device_nif:query(),
        {ok, #{<<"status">> => 200, <<"body">> => Result}}
    catch
        error:Error:Stack ->
            io:format("~nError: ~p~n", [Error]),
            io:format("Stack: ~p~n", [Stack]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"Failed to query permaweb">>,
                    <<"details">> => Error
                }
            }}
    end.

%  example calls:
% curl -X GET http://localhost:10001/~roam@1.0/hello
% curl -X GET http://localhost:10001/~roam@1.0/query_permaweb 

% in erlang shell call: dev_roam:test_ao().
test_ao() ->
    io:format("~n__test_roam_device__~n"),
    try
        % Get current wallet
        Wallet = hb:wallet(),
        % Get wallet address in human readable form  
        Address = hb_util:human_id(ar_wallet:to_address(Wallet)),

        % Create the process message
        {ok, Script} = file:read_file("test/roam-device.lua"),
        Process = hb_message:commit(#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"script">> => Script,
            <<"scheduler-location">> => Address,
            <<"authority">> => [Address],  % Add authority
            <<"test-random-seed">> => rand:uniform(1337)
        }, Wallet),

        % Cache the process
        {ok, _} = hb_cache:write(Process, #{}),

        % Get process ID
        ProcID = hb_message:id(Process, all),

        % Create schedule message  
        Message = hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> => hb_message:commit(#{
                <<"target">> => ProcID,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>
            }, Wallet)
        }, Wallet),

        % Schedule the message
        {ok, _} = hb_ao:resolve(Process, Message, #{}),

        % Get the results
        {ok, Results} = hb_ao:resolve(Process, <<"now">>, #{}),
        io:format("~nRoam test result: ~p~n", [Results])
    catch
        Error:Reason:Stack ->
            io:format("Error running Roam test: ~p:~p~n~p~n", [Error, Reason, Stack])
    end.