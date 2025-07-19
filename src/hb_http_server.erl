%%% @doc A router that attaches a HTTP server to the AO-Core resolver.
%%% Because AO-Core is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the AO-Core resolver. 
%%% 
%%% `hb_http:reply/4' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts' message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2]).
-export([set_opts/1, set_opts/2, get_opts/0, get_opts/1]).
-export([set_default_opts/1, set_proc_server_id/1]).
-export([start_node/0, start_node/1]).
-export([handle_s3_request_direct/4, build_s3_response_headers/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Handle ~s3@1.0 device requests directly, bypassing AO-Core path segmentation
%% the handler function expects to extract the AccessKeyId from the request's Authorization Header
%% then in dev_s3.erl we use the request's extracted AccessKeyId as access API KEY validated against
%% the s3_device.config access_key_id
handle_s3_request_direct(Req, Body, Path, NodeMsg) ->
    try
        Method = cowboy_req:method(Req),
        ReqHeaders = cowboy_req:headers(Req),
        Query = cowboy_req:qs(Req),
        
        % Extract AccessKeyId from authorization header
        AuthHeader = maps:get(<<"authorization">>, ReqHeaders, <<>>),
        AccessKeyId = case binary:split(AuthHeader, <<"Credential=">>) of
            [_, Rest] ->
                case binary:split(Rest, <<"/">>) of
                    [AKI | _] -> AKI;
                    _ -> undefined
                end;
            _ -> undefined
        end,
        
        S3Msg = #{
            <<"method">> => Method,
            <<"path">> => Path,
            <<"headers">> => ReqHeaders,
            <<"body">> => Body,
            <<"query">> => Query,
            request_access_key_id => AccessKeyId
        },
        
        case dev_s3:handle_s3_request(Method, Path, S3Msg, NodeMsg) of
            {ok, Response} ->
                Status = maps:get(<<"status">>, Response, 200),
                ResponseBody = maps:get(<<"body">>, Response, <<>>),
                ResponseHeaders = build_s3_response_headers(Response),
                {ok, cowboy_req:reply(Status, ResponseHeaders, ResponseBody, Req), no_state};
            {error, ErrorResponse} ->
                Status = maps:get(<<"status">>, ErrorResponse, 500),
                ErrorBody = maps:get(<<"body">>, ErrorResponse, <<"Internal Server Error">>),
                {ok, cowboy_req:reply(Status, #{
                    <<"content-type">> => <<"text/plain">>,
                    <<"access-control-allow-origin">> => <<"*">>
                }, ErrorBody, Req), no_state}
        end
    catch
        Type:Exception:_Stacktrace ->
            io:format("Exception: ~p:~p~n", [Type, Exception]),
            {ok, cowboy_req:reply(500, #{
                <<"content-type">> => <<"text/plain">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, <<"S3 request failed">>, Req), no_state}
    end.


%% @doc Build HTTP response headers from S3 response
build_s3_response_headers(Response) ->
    BaseHeaders = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>
    },
    
    % Add S3 specific headers if present
    S3Headers = maps:fold(fun
        (<<"etag">>, ETag, Acc) when ETag =/= <<>> ->
            Acc#{<<"etag">> => ETag};
        (<<"last-modified">>, LastMod, Acc) when LastMod =/= <<>> ->
            Acc#{<<"last-modified">> => LastMod};
        (<<"content-type">>, ContentType, Acc) when ContentType =/= <<>> ->
            Acc#{<<"content-type">> => ContentType};
        (<<"content-length">>, ContentLength, Acc) when ContentLength =/= <<>> ->
            Acc#{<<"content-length">> => ContentLength};
        (<<"accept-ranges">>, AcceptRanges, Acc) when AcceptRanges =/= <<>> ->
            Acc#{<<"accept-ranges">> => AcceptRanges};
        (_, _, Acc) -> Acc
    end, BaseHeaders, Response),
    
    S3Headers.

%% @doc Starts the HTTP server. Optionally accepts an `Opts' message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts' argument to use for all AO-Core resolution requests downstream.
start() ->
    ?event(http, {start_store, <<"cache-mainnet">>}),
    Store = hb_opts:get(store, no_store, #{}),
    hb_store:start(Store),
    Loaded =
        case hb_opts:load(Loc = hb_opts:get(hb_config_location, <<"config.flat">>)) of
            {ok, Conf} ->
                ?event(boot, {loaded_config, Loc, Conf}),
                Conf;
            {error, Reason} ->
                ?event(boot, {failed_to_load_config, Loc, Reason}),
                #{}
        end,
    MergedConfig =
        hb_maps:merge(
            hb_opts:default_message(),
            Loaded
        ),
    PrivWallet =
        hb:wallet(
            hb_opts:get(
                priv_key_location,
                <<"hyperbeam-key.json">>,
                Loaded
            )
        ),
    FormattedConfig = hb_util:debug_fmt(MergedConfig, MergedConfig, 2),
    io:format("~n"
        "===========================================================~n"
        "==    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗           ==~n"
        "==    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗          ==~n"
        "==    ███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝          ==~n"
        "==    ██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗          ==~n"
        "==    ██║  ██║   ██║   ██║     ███████╗██║  ██║          ==~n"
        "==    ╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝          ==~n"
        "==                                                       ==~n"
        "==        ██████╗ ███████╗ █████╗ ███╗   ███╗ VERSION:   ==~n"
        "==        ██╔══██╗██╔════╝██╔══██╗████╗ ████║      v~p. ==~n"
        "==        ██████╔╝█████╗  ███████║██╔████╔██║            ==~n"
        "==        ██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║ EAT GLASS, ==~n"
        "==        ██████╔╝███████╗██║  ██║██║ ╚═╝ ██║ BUILD THE  ==~n"
        "==        ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    FUTURE. ==~n"
        "===========================================================~n"
        "== Node activate at: ~s ==~n"
        "== Operator: ~s ==~n"
        "===========================================================~n"
        "== Config:                                               ==~n"
        "===========================================================~n"
        "   ~s~n"
        "===========================================================~n",
        [
            ?HYPERBEAM_VERSION,
            string:pad(
                lists:flatten(
                    io_lib:format(
                        "http://~s:~p",
                        [
                            hb_opts:get(host, <<"localhost">>, Loaded),
                            hb_opts:get(port, 8734, Loaded)
                        ]
                    )
                ),
                35, leading, $ 
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ),
    start(
        Loaded#{
            priv_wallet => PrivWallet,
            store => Store,
            port => hb_opts:get(port, 8734, Loaded),
            cache_writers => [hb_util:human_id(ar_wallet:to_address(PrivWallet))]
        }
    ).
start(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    hb:init(),
    BaseOpts = set_default_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.

%% @doc Trigger the creation of a new HTTP server node. Accepts a `NodeMsg'
%% message, which is used to configure the server. This function executed the
%% `start' hook on the node, giving it the opportunity to modify the `NodeMsg'
%% before it is used to configure the server. The `start' hook expects gives and
%% expects the node message to be in the `body' key.
new_server(RawNodeMsg) ->
    RawNodeMsgWithDefaults =
        hb_maps:merge(
            hb_opts:default_message(),
            RawNodeMsg#{ only => local }
        ),
    HookMsg = #{ <<"body">> => RawNodeMsgWithDefaults },
    NodeMsg =
        case dev_hook:on(<<"start">>, HookMsg, RawNodeMsgWithDefaults) of
            {ok, #{ <<"body">> := NodeMsgAfterHook }} -> NodeMsgAfterHook;
            Unexpected ->
                ?event(http,
                    {failed_to_start_server,
                        {unexpected_hook_result, Unexpected}
                    }
                ),
                throw(
                    {failed_to_start_server,
                        {unexpected_hook_result, Unexpected}
                    }
                )
        end,
    % Put server ID into node message so it's possible to update current server
    hb_http:start(),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(
                    priv_wallet,
                    no_wallet,
                    NodeMsg
                )
            )
        ),
    % Put server ID into node message so it's possible to update current server
    % params
    NodeMsgWithID = hb_maps:put(http_server, ServerID, NodeMsg),
    Dispatcher = cowboy_router:compile([{'_', [{'_', ?MODULE, ServerID}]}]),
    ProtoOpts = #{
        env => #{dispatch => Dispatcher, node_msg => NodeMsgWithID},
        stream_handlers => [cowboy_stream_h],
        max_connections => infinity,
        idle_timeout => hb_opts:get(idle_timeout, 300000, NodeMsg)
    },
    PrometheusOpts =
        case hb_opts:get(prometheus, not hb_features:test(), NodeMsg) of
            true ->
                ?event(prometheus,
                    {starting_prometheus, {test_mode, hb_features:test()}}
                ),
                % Attempt to start the prometheus application, if possible.
                try
                    application:ensure_all_started([prometheus, prometheus_cowboy]),
                    ProtoOpts#{
                        metrics_callback =>
                            fun prometheus_cowboy2_instrumenter:observe/1,
                        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
                    }
                catch
                    Type:Reason ->
                        % If the prometheus application is not started, we can
                        % still start the HTTP server, but we won't have any
                        % metrics.
                        ?event(prometheus,
                            {prometheus_not_started, {type, Type}, {reason, Reason}}
                        ),
                        ProtoOpts
                end;
            false ->
                ?event(prometheus,
                    {prometheus_not_started, {test_mode, hb_features:test()}}
                ),
                ProtoOpts
        end,
    DefaultProto =
        case hb_features:http3() of
            true -> http3;
            false -> http2
        end,
    {ok, Port, Listener} =
        case Protocol = hb_opts:get(protocol, DefaultProto, NodeMsg) of
            http3 ->
                start_http3(ServerID, PrometheusOpts, NodeMsg);
            Pro when Pro =:= http2; Pro =:= http1 ->
                % The HTTP/2 server has fallback mode to 1.1 as necessary.
                start_http2(ServerID, PrometheusOpts, NodeMsg);
            _ -> {error, {unknown_protocol, Protocol}}
        end,
    ?event(http,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol},
            {store, hb_opts:get(store, no_store, NodeMsg)}
        }
    ),
    {ok, Listener, Port}.

start_http3(ServerID, ProtoOpts, _NodeMsg) ->
    ?event(http, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
            application:ensure_all_started(quicer),
            {ok, Listener} = cowboy:start_quic(
                ServerID, 
                TransOpts = #{
                    socket_opts => [
                        {certfile, "test/test-tls.pem"},
                        {keyfile, "test/test-tls.key"}
                    ]
                },
                ProtoOpts
            ),
            {ok, {_, GivenPort}} = quicer:sockname(Listener),
            ranch_server:set_new_listener_opts(
                ServerID,
                1024,
                ranch:normalize_opts(
                    hb_maps:to_list(TransOpts#{ port => GivenPort })
                ),
                ProtoOpts,
                []
            ),
            ranch_server:set_addr(ServerID, {<<"localhost">>, GivenPort}),
            % Bypass ranch's requirement to have a connection supervisor define
            % to support updating protocol opts.
            % Quicer doesn't use a connection supervisor, so we just spawn one
            % that does nothing.
            ConnSup = spawn(fun() -> http3_conn_sup_loop() end),
            ranch_server:set_connections_sup(ServerID, ConnSup),
            Parent ! {ok, GivenPort},
            receive stop -> stopped end
        end),
    receive {ok, GivenPort} -> {ok, GivenPort, ServerPID}
    after 2000 ->
        {error, {timeout, staring_http3_server, ServerID}}
    end.

http3_conn_sup_loop() ->
    receive
        _ -> 
            % Ignore any other messages
            http3_conn_sup_loop()
    end.

start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http2, ServerID}),
    StartRes = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, NodeMsg)}
        ],
        ProtoOpts
    ),
    case StartRes of
        {ok, Listener} ->
            ?event(debug_router_info, {http2_started, {listener, Listener}, {port, Port}}),
            {ok, Port, Listener};
        {error, {already_started, Listener}} ->
            ?event(http, {http2_already_started, {listener, Listener}}),
            ?event(debug_router_info,
                {restarting,
                    {id, ServerID},
                    {node_msg, NodeMsg}
                }
            ),
            cowboy:set_env(ServerID, node_msg, #{}),
            % {ok, Port, Listener}
            cowboy:stop_listener(ServerID),
            start_http2(ServerID, ProtoOpts, NodeMsg)
    end.

%% @doc Entrypoint for all HTTP requests. Receives the Cowboy request option and
%% the server ID, which can be used to lookup the node message.
init(Req, ServerID) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID)
    end.

%% @doc Helper to grab the full body of a HTTP request, even if it's chunked.
read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%% @doc Reply to CORS preflight requests.
cors_reply(Req, _ServerID) ->
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req),
    ?event(http_debug, {cors_reply, {req, Req}, {req2, Req2}}),
    {ok, Req2, no_state}.

%% @doc Handle all non-CORS preflight requests as AO-Core requests. Execution 
%% starts by parsing the HTTP request into HyerBEAM's message format, then
%% passing the message directly to `meta@1.0' which handles calling AO-Core in
%% the appropriate way.
handle_request(RawReq, Body, ServerID) ->
    % Insert the start time into the request so that it can be used by the
    % `hb_http' module to calculate the duration of the request.
    StartTime = os:system_time(millisecond),
    Req = RawReq#{ start_time => StartTime },
    NodeMsg = get_opts(#{ http_server => ServerID }),
    put(server_id, ServerID),
    case {cowboy_req:path(RawReq), cowboy_req:qs(RawReq)} of
        {<<"/">>, <<>>} ->
            % If the request is for the root path, serve a redirect to the default 
            % request of the node.
            cowboy_req:reply(
                302,
                #{
                    <<"location">> =>
                        hb_opts:get(
                            default_request,
                            <<"/~hyperbuddy@1.0/dashboard">>,
                            NodeMsg
                        )
                },
                RawReq
            );
        _ ->
            % ~s3@1.0: Check for S3 paths before AO-Core processing
            Path = cowboy_req:path(RawReq),
            case binary:match(Path, <<"/~s3@1.0/">>) of
                {0, _} ->
                    % intercept s3 device requests
                    handle_s3_request_direct(Req, Body, Path, NodeMsg);
                nomatch ->
                    % The request is of normal AO-Core form, so we parse it and invoke
                    % the meta@1.0 device to handle it.
                    ?event(http,
                        {
                            http_inbound,
                            {cowboy_req, {explicit, Req}, {body, {string, Body}}}
                        }
                    ),
                    TracePID = hb_tracer:start_trace(),
                    % Parse the HTTP request into HyerBEAM's message format.
                    try 
                        ReqSingleton = hb_http:req_to_tabm_singleton(Req, Body, NodeMsg),
                        CommitmentCodec = hb_http:accept_to_codec(ReqSingleton, NodeMsg),
                        ?event(http,
                            {parsed_singleton,
                                {req_singleton, ReqSingleton},
                                {accept_codec, CommitmentCodec}},
                            #{trace => TracePID}
                        ),
                        % hb_tracer:record_step(TracePID, request_parsing),
                        % Invoke the meta@1.0 device to handle the request.
                        {ok, Res} =
                            dev_meta:handle(
                                NodeMsg#{
                                    commitment_device => CommitmentCodec,
                                    trace => TracePID
                                },
                                ReqSingleton
                            ),
                        hb_http:reply(Req, ReqSingleton, Res, NodeMsg)
                    catch
                        Type:Details:Stacktrace ->
                            Trace = hb_tracer:get_trace(TracePID),
                            FormattedError =
                                hb_util:bin(hb_message:format(
                                    hb_private:reset(#{
                                        <<"type">> => Type,
                                        <<"details">> => Details,
                                        <<"stacktrace">> => Stacktrace
                                    })
                                )),
                            {ok, ErrorPage} = dev_hyperbuddy:return_error(FormattedError),
                            ?event(
                                http_error,
                                {http_error,
                                    {details,
                                        {explicit,
                                            #{
                                                type => Type,
                                                details => Details,
                                                stacktrace => Stacktrace
                                            }
                                        }
                                    }
                                }
                            ),
                            hb_http:reply(
                                Req,
                                #{},
                                ErrorPage#{ <<"status">> => 500 },
                                NodeMsg
                            )
                    end
            end
    end.

%% @doc Return the list of allowed methods for the HTTP server.
allowed_methods(Req, State) ->
    {
        [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>],
        Req,
        State
    }.

%% @doc Merges the provided `Opts' with uncommitted values from `Request',
%% preserves the http_server value, and updates node_history by prepending
%% the `Request'. If a server reference exists, updates the Cowboy environment
%% variable 'node_msg' with the resulting options map.
set_opts(Opts) ->
    case hb_opts:get(http_server, no_server_ref, Opts) of
        no_server_ref ->
            ok;
        ServerRef ->
            ok = cowboy:set_env(ServerRef, node_msg, Opts)
    end.
set_opts(Request, Opts) ->
    PreparedOpts =
        hb_opts:mimic_default_types(
            Opts,
            false,
            Opts
        ),
    PreparedRequest =
        hb_opts:mimic_default_types(
            hb_message:uncommitted(Request),
            false,
            Opts
        ),
    MergedOpts =
        maps:merge(
            PreparedOpts,
            PreparedRequest
        ),
    ?event(set_opts, {merged_opts, {explicit, MergedOpts}}),
    History =
        hb_opts:get(node_history, [], Opts)
            ++ [ hb_private:reset(maps:without([node_history], PreparedRequest)) ],
    FinalOpts = MergedOpts#{
        http_server => hb_opts:get(http_server, no_server, Opts),
        node_history => History
    },
    {set_opts(FinalOpts), FinalOpts}.

%% @doc Get the node message for the current process.
get_opts() ->
    get_opts(#{ http_server => get(server_id) }).
get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).

%% @doc Initialize the server ID for the current process.
set_proc_server_id(ServerID) ->
    put(server_id, ServerID).

%% @doc Apply the default node message to the given opts map.
set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults.
    TempOpts = Opts#{ only => local },
    % Generate a random port number between 10000 and 30000 to use
    % for the server.
    Port =
        case hb_opts:get(port, no_port, TempOpts) of
            no_port ->
                rand:seed(exsplus, erlang:system_time(microsecond)),
                10000 + rand:uniform(50000);
            PassedPort -> PassedPort
        end,
    Wallet =
        case hb_opts:get(priv_wallet, no_viable_wallet, TempOpts) of
            no_viable_wallet -> ar_wallet:new();
            PassedWallet -> PassedWallet
        end,
    Store =
        case hb_opts:get(store, no_store, TempOpts) of
            no_store ->
                TestDir = <<"cache-TEST/run-fs-", (integer_to_binary(Port))/binary>>,
                filelib:ensure_dir(binary_to_list(TestDir)),
                #{ <<"store-module">> => hb_store_fs, <<"name">> => TestDir };
            PassedStore -> PassedStore
        end,
    ?event({set_default_opts,
        {given, TempOpts},
        {port, Port},
        {store, Store},
        {wallet, Wallet}
    }),
    Opts#{
        port => Port,
        store => Store,
        priv_wallet => Wallet,
        address => hb_util:human_id(ar_wallet:to_address(Wallet)),
        force_signed => true
    }.

%% @doc Test that we can start the server, send a message, and get a response.
start_node() ->
    start_node(#{}).
start_node(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = set_default_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.

%%% Tests
%%% The following only covering the HTTP server initialization process. For tests
%%% of HTTP server requests/responses, see `hb_http.erl'.

%% @doc Ensure that the `start' hook can be used to modify the node options. We
%% do this by creating a message with a device that has a `start' key. This 
%% key takes the message's body (the anticipated node options) and returns a
%% modified version of that body, which will be used to configure the node. We
%% then check that the node options were modified as we expected.
set_node_opts_test() ->
    Node =
        start_node(#{
            on => #{
                <<"start">> => #{
                    <<"device">> =>
                        #{
                            <<"start">> =>
                                fun(_, #{ <<"body">> := NodeMsg }, _) ->
                                    {ok, #{
                                        <<"body">> =>
                                            NodeMsg#{ <<"test-success">> => true }
                                    }}
                                end
                        }
                }
            }
        }),
    {ok, LiveOpts} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assert(hb_ao:get(<<"test-success">>, LiveOpts, false, #{})).

%% @doc Test the set_opts/2 function that merges request with options,
%% manages node history, and updates server state.
set_opts_test() ->
    DefaultOpts = hb_opts:default_message(),
    start_node(DefaultOpts#{ 
        priv_wallet => Wallet = ar_wallet:new(), 
        port => rand:uniform(10000) + 10000 
    }),
    Opts = ?MODULE:get_opts(#{ 
        http_server => hb_util:human_id(ar_wallet:to_address(Wallet))
    }),
    NodeHistory = hb_opts:get(node_history, [], Opts),
    ?event(debug_node_history, {node_history_length, length(NodeHistory)}),
    ?assert(length(NodeHistory) == 0),
    % Test case 1: Empty node_history case
    Request1 = #{
        <<"hello">> => <<"world">>
    },             
    {ok, UpdatedOpts1} = set_opts(Request1, Opts),
    NodeHistory1 = hb_opts:get(node_history, not_found, UpdatedOpts1),
    Key1 = hb_opts:get(<<"hello">>, not_found, UpdatedOpts1),
    ?event(debug_node_history, {node_history_length, length(NodeHistory1)}),
    ?assert(length(NodeHistory1) == 1),
    ?assert(Key1 == <<"world">>),
    % Test case 2: Non-empty node_history case
    Request2 = #{
        <<"hello2">> => <<"world2">>
    },
    {ok, UpdatedOpts2} = set_opts(Request2, UpdatedOpts1),
    NodeHistory2 = hb_opts:get(node_history, not_found, UpdatedOpts2),
    Key2 = hb_opts:get(<<"hello2">>, not_found, UpdatedOpts2),
    ?event(debug_node_history, {node_history_length, length(NodeHistory2)}),
    ?assert(length(NodeHistory2) == 2),
    ?assert(Key2 == <<"world2">>),
    % Test case 3: Non-empty node_history case
    {ok, UpdatedOpts3} = set_opts(#{}, UpdatedOpts2#{ <<"hello3">> => <<"world3">> }),
    NodeHistory3 = hb_opts:get(node_history, not_found, UpdatedOpts3),
    Key3 = hb_opts:get(<<"hello3">>, not_found, UpdatedOpts3),
    ?event(debug_node_history, {node_history_length, length(NodeHistory3)}),
    ?assert(length(NodeHistory3) == 3),
    ?assert(Key3 == <<"world3">>).

restart_server_test() ->
    Wallet = ar_wallet:new(),
    BaseOpts = #{
        <<"test-key">> => <<"server-1">>,
        priv_wallet => Wallet
    },
    _ = start_node(BaseOpts),
    N2 = start_node(BaseOpts#{ <<"test-key">> => <<"server-2">> }),
    ?assertEqual(
        {ok, <<"server-2">>},
        hb_http:get(N2, <<"/~meta@1.0/info/test-key">>, #{})
    ).