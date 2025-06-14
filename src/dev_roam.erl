%%% @doc A device that allows querying the Arweave permaweb, inspired by roam.ar.io
%%% built on top of ureq HTTP client, and goldsky GQL gateway the roam@1.0 device provides

-module(dev_roam).
-export([info/1, info/3, query_permaweb/3, hello/3]).

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
% curl -X GET http://localhost:8734/~roam@1.0/hello
% curl -X GET http://localhost:8734/~roam@1.0/query_permaweb 