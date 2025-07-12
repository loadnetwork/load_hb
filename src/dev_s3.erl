-module(dev_s3).
-export([info/1, info/3, handle/4, handle_s3_request/4]).
-include("include/hb.hrl").

info(_) ->
    #{
        handler => fun handle/4,
        handlers => #{
            <<"info">> => fun info/3
        }
    }.

info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"S3 device - S3 compatible API + cluster connector">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"handle">> => <<"Handle S3 requests">>
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.


handle(Key, Msg1, Msg2, Opts) ->
    io:format("S3 DEBUG: handle called with Key=~p~n", [Key]),
    
    % Handle internal AO-Core operations
    case Key of
        <<"set">> -> dev_message:set(Msg1, Msg2, Opts);
        <<"remove">> -> dev_message:remove(Msg1, Msg2, Opts);
        <<"keys">> -> dev_message:keys(Msg1, Msg2, Opts);
        _ ->
            % Get the priginal HTTP path from the private section
            OriginalPath = case hb_private:from_message(Msg2) of
                #{ <<"ao-core">> := #{ <<"original-path">> := Path } } ->
                    Path;
                _ ->
                    % Fallback: try request-path
                    hb_ao:get(<<"request-path">>, Msg2, <<"/~s3@1.0/", Key/binary>>, Opts)
            end,
            
            Method = hb_ao:get(<<"method">>, Msg2, <<"GET">>, Opts),
            
            io:format("S3 DEBUG: OriginalPath=~p, Method=~p~n", [OriginalPath, Method]),
            
            handle_s3_request(Method, OriginalPath, Msg1, Opts)
    end.

handle_s3_request(<<"GET">>, Path, Msg, Opts) ->
    io:format("S3 DEBUG: handle_s3_request GET with Path=~p~n", [Path]),
    case parse_s3_path(Path) of
        {object, Bucket, Key} ->
            io:format("S3 DEBUG: Parsed as object, Bucket=~p, Key=~p~n", [Bucket, Key]),
            %% GetObjectCommand
            get_object_handler(Bucket, Key, Msg, Opts);
        {bucket, Bucket} ->
            io:format("S3 DEBUG: Parsed as bucket=~p~n", [Bucket]),
            {error, #{<<"status">> => 501, <<"body">> => <<"ListObjectsCommand not implemented yet">>}};
        root ->
            io:format("S3 DEBUG: Parsed as root~n"),
            {error, #{<<"status">> => 501, <<"body">> => <<"ListBucketsCommand not implemented yet">>}};
        invalid ->
            io:format("S3 DEBUG: Invalid path~n"),
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid S3 path">>}}
    end;

handle_s3_request(Method, _Path, _Msg, _Opts) ->
    io:format("S3 DEBUG: Unsupported method=~p~n", [Method]),
    {error, #{
        <<"status">> => 405,
        <<"body">> => <<"Method ", Method/binary, " not supported">>
    }}.

parse_s3_path(Path) ->
    io:format("S3 DEBUG: parsing path=~p~n", [Path]),
    Result = case Path of
        <<"/~s3@1.0">> -> root;
        <<"/~s3@1.0/">> -> root;
        <<"/~s3@1.0/", Rest/binary>> ->
            case binary:split(Rest, <<"/">>, [global]) of
                [<<>>] -> root;
                [Bucket] when Bucket =/= <<>> -> {bucket, Bucket};
                [Bucket, Key] when Bucket =/= <<>>, Key =/= <<>> -> {object, Bucket, Key};
                [Bucket | KeyParts] when Bucket =/= <<>> ->
                    case lists:filter(fun(Part) -> Part =/= <<>> end, KeyParts) of
                        [] -> {bucket, Bucket};
                        FilteredParts ->
                            Key = iolist_to_binary(lists:join(<<"/">>, FilteredParts)),
                            {object, Bucket, Key}
                    end;
                _ -> invalid
            end;
        _ -> invalid
    end,
    io:format("S3 DEBUG: parsed result=~p~n", [Result]),
    Result.

%% GetObjectCommand handler
get_object_handler(Bucket, Key, _Msg, Opts) ->
    io:format("S3 DEBUG: get_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>), % load network s3 cluster
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>), % cloud.load.network
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
    io:format("S3 DEBUG: Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:get_object~n"),
    % args are passed as binary
    case s3_nif:get_object(
        Endpoint,        
        AccessKeyId,     
        SecretAccessKey, 
        Region,          
        Bucket,          
        Key              
    ) of

        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif success, S3Response=~p~n", [S3Response]),
            % Convert string body to binary
            RawBody = maps:get(<<"body">>, S3Response),
            Body = case RawBody of
                B when is_binary(B) -> B;
                S when is_list(S) -> list_to_binary(S);
                _ -> <<>>
            end,
            ETag = maps:get(<<"etag">>, S3Response, <<>>),
            LastModified = maps:get(<<"last_modified">>, S3Response, <<>>),
            ContentType = maps:get(<<"content_type">>, S3Response, <<>>),
            ContentLength = maps:get(<<"content_length">>, S3Response, <<>>),
            
            io:format("S3 DEBUG: Extracted - Body size=~p, ETag=~p~n", [byte_size(Body), ETag]),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => Body,
                <<"etag">> => ETag,
                <<"last-modified">> => LastModified,
                <<"content-type">> => ContentType,
                <<"content-length">> => ContentLength,
                <<"accept-ranges">> => <<"bytes">>
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.
