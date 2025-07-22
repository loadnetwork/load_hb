-module(dev_s3).
-export([info/1, info/3, handle/4, handle_s3_request/4]).
-export([get_request_credentials/2]).
-export([get_object_handler/4]).
-include("include/hb.hrl").

load_s3_config() ->
    case file:consult("s3_device.config") of
        {ok, Terms} -> 
            maps:from_list(Terms); 
        {error, enoent} -> 
            #{};
        {error, Reason} -> 
            error({s3_config_error, Reason})
    end.

%% @doc Load S3 credentials from request
load_s3_credentials(Msg) ->
    PreLoadedConfig = load_s3_config(),
    io:format("S3 CREDS DEBUG: Starting credential loading (request-only mode)~n"),
    io:format("S3 CREDS DEBUG: Message keys: ~p~n", [maps:keys(Msg)]),
    
    % Extract only the RequestAccessKeyId from the s3 request credentials
    RequestAccessKeyId = maps:get(request_access_key_id, Msg, undefined),
    % The rest of s3 configs are loaded from the s3_device.config file
    SecretAccessKey = maps:get(secret_access_key, PreLoadedConfig, undefined),
    Endpoint = maps:get(endpoint, PreLoadedConfig, undefined),
    PublicEndpoint = maps:get(public_endpoint, PreLoadedConfig, undefined),
    Region = maps:get(region, PreLoadedConfig, undefined),
    PreLoadedAccessKeyId = maps:get(access_key_id, PreLoadedConfig, undefined),
    
    io:format("S3 CREDS DEBUG: Request AccessKeyId: ~p~n", [RequestAccessKeyId]),
    io:format("S3 CREDS DEBUG: Config AccessKeyId: ~p~n", [PreLoadedAccessKeyId]),
    io:format("S3 CREDS DEBUG: Config SecretAccessKey: ~p~n", [SecretAccessKey]),
    io:format("S3 CREDS DEBUG: Config Endpoint: ~p~n", [Endpoint]),
    io:format("S3 CREDS DEBUG: Config Endpoint: ~p~n", [PublicEndpoint]),
    io:format("S3 CREDS DEBUG: Config Region: ~p~n", [Region]),

    % Validate request's access_key_id parity with the s3_device.config access_key_id
    true = (RequestAccessKeyId =:= PreLoadedAccessKeyId andalso RequestAccessKeyId =/= undefined) orelse error({assertion_failed, invalid_access_key_id}),

    % Validate that we have all creds to connect to the s3 cluster

    true = (SecretAccessKey =/= undefined) orelse error({missing_credential, secret_access_key}),
    true = (Endpoint =/= undefined) orelse error({missing_credential, endpoint}),
    true = (PublicEndpoint =/= undefined) orelse error({missing_credential, public_endpoint}),
    true = (Region =/= undefined) orelse error({missing_credential, region}),
    
    Credentials = #{
        endpoint => Endpoint,
        public_endpoint => PublicEndpoint,
        access_key_id => RequestAccessKeyId,
        secret_access_key => SecretAccessKey,
        region => Region
    },

    Credentials.

info(_) ->
    #{
        handler => fun handle/4,
        handlers => #{
            <<"info">> => fun info/3
        }
    }.

info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"S3 device - HyperBEAM native object storage">>,
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
        {cache, Bucket, Key} ->
            io:format("S3 DEBUG: CACHE request, Bucket=~p, Key=~p~n", [Bucket, Key]),
            FinalKey = case Key of
                <<"">> ->
                    QueryParams = hb_ao:get(<<"query-params">>, Msg, #{}, Opts),
                    maps:get(<<"key">>, QueryParams, <<"">>);
                _ -> Key
            end,
            get_cached_object_handler(Bucket, FinalKey, Msg, Opts);
        {object, Bucket, Key} ->
            io:format("S3 DEBUG: Parsed as object, Bucket=~p, Key=~p~n", [Bucket, Key]),
            get_object_handler(Bucket, Key, Msg, Opts);
        {bucket, Bucket} ->
            io:format("S3 DEBUG: Parsed as bucket=~p, calling list_objects_handler~n", [Bucket]),
            list_objects_handler(Bucket, Msg, Opts);
        root ->
            io:format("S3 DEBUG: Parsed as root~n"),
            {error, #{<<"status">> => 501, <<"body">> => <<"ListBucketsCommand not implemented yet">>}};
        invalid ->
            io:format("S3 DEBUG: Invalid path~n"),
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid S3 path">>}};
        {error, Reason} ->
            io:format("S3 DEBUG: Path parsing error: ~p~n", [Reason]),
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid S3 path">>}}
    end;


handle_s3_request(<<"PUT">>, Path, Msg, Opts) ->
    io:format("S3 DEBUG: handle_s3_request PUT with Path=~p~n", [Path]),
    case parse_s3_path(Path) of
        {object, Bucket, Key} ->
            Body = maps:get(<<"body">>, Msg, <<>>),
            io:format("S3 DEBUG: PUT object, Bucket=~p, Key=~p, Body size=~p~n", [Bucket, Key, byte_size(Body)]),
            put_object_handler(Bucket, Key, Body, Msg, Opts);
        {bucket, Bucket} ->
            % PUT to bucket root = CREATE BUCKET
            io:format("S3 DEBUG: CREATE bucket=~p~n", [Bucket]),
            create_bucket_handler(Bucket, Msg, Opts);
        _ -> 
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid PUT path">>}}
    end;

handle_s3_request(<<"HEAD">>, Path, Msg, Opts) ->
    io:format("S3 DEBUG: handle_s3_request HEAD with Path=~p~n", [Path]),
    case parse_s3_path(Path) of
        {object, Bucket, Key} ->
            io:format("S3 DEBUG: HEAD object, Bucket=~p, Key=~p~n", [Bucket, Key]),
            head_object_handler(Bucket, Key, Msg, Opts);
        {bucket, Bucket} ->
            io:format("S3 DEBUG: HEAD bucket, Bucket=~p~n", [Bucket]),
            head_bucket_handler(Bucket, Msg, Opts);
        _ ->
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid HEAD path">>}}
    end;

handle_s3_request(<<"DELETE">>, Path, Msg, Opts) ->
    io:format("S3 DEBUG: handle_s3_request DELETE with Path=~p~n", [Path]),
    case parse_s3_path(Path) of
        {object, Bucket, Key} ->
            io:format("S3 DEBUG: DELETE object, Bucket=~p, Key=~p~n", [Bucket, Key]),
            delete_object_handler(Bucket, Key, Msg, Opts);
        _ ->
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid DELETE path - can only DELETE objects">>}}
    end;

handle_s3_request(<<"POST">>, Path, Msg, Opts) ->
    io:format("S3 DEBUG: handle_s3_request POST with Path=~p~n", [Path]),
    case parse_s3_path(Path) of
        get_presigned ->
            io:format("S3 DEBUG: GET_PRESIGNED request~n"),
            get_presigned_url_handler(Msg, Opts);
        {bucket, Bucket} ->
            Body = maps:get(<<"body">>, Msg, <<>>),
            io:format("S3 DEBUG: POST to bucket=~p, calling delete_objects_handler~n", [Bucket]),
            delete_objects_handler(Bucket, Body, Msg, Opts);
        _ -> 
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid POST path">>}}
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
        <<"/~s3@1.0/get-presigned">> -> get_presigned;
        <<"/~s3@1.0/cache/", Rest/binary>> ->
            case binary:split(Rest, <<"/">>, [global]) of
                [<<>>] -> {error, invalid_cache_path};
                [Bucket] when Bucket =/= <<>> -> {cache, Bucket, <<"">>};
                [Bucket, Key] when Bucket =/= <<>>, Key =/= <<>> -> {cache, Bucket, Key};
                [Bucket | KeyParts] when Bucket =/= <<>> ->
                    case lists:filter(fun(Part) -> Part =/= <<>> end, KeyParts) of
                        [] -> {cache, Bucket, <<"">>};
                        FilteredParts ->
                            Key = iolist_to_binary(lists:join(<<"/">>, FilteredParts)),
                            {cache, Bucket, Key}
                    end;
                _ -> {error, invalid_cache_path}
            end;
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

%% LRU CACHE FUNCTIONS
get_cached_object_handler(Bucket, Key, _Msg, _Opts) ->
    % i think here for now we can keep it creds free because it reads from cache
    % and cache object retrieval does not have AWS S3 Authorization format 
    S3Config = load_s3_config(),
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    % no range
    case s3_nif:get_cached_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key, <<"">>) of
        {ok, S3Response} ->
            Body = maps:get(<<"body">>, S3Response, <<>>),
            ETag = maps:get(<<"etag">>, S3Response, <<>>),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => Body,
                <<"etag">> => ETag,
                <<"content-type">> => <<"binary/octet-stream">>
            }};
        {error, Reason} ->
            {error, #{
                <<"status">> => 404,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

%% Presigned URLs functionalities
get_presigned_url_handler(Msg, _Opts) ->
    io:format("S3 DEBUG: get_presigned_url_handler~n"),
        S3Config = load_s3_credentials(Msg),
    
    Body = maps:get(<<"body">>, Msg, <<"{}">>),
    io:format("S3 DEBUG: Raw body: ~p~n", [Body]),
    
    try hb_json:decode(Body) of
        Params when is_map(Params) ->
            io:format("S3 DEBUG: Parsed params: ~p~n", [Params]),
            Bucket = maps:get(<<"bucket">>, Params, <<"">>),
            Key = maps:get(<<"key">>, Params, <<"">>),
            Duration = maps:get(<<"duration">>, Params, 3600),
            
            io:format("S3 DEBUG: Extracted - Bucket=~p, Key=~p, Duration=~p~n", [Bucket, Key, Duration]),
            
            Endpoint = maps:get(endpoint, S3Config),
            PublicEndpoint = maps:get(public_endpoint, S3Config),
            AccessKeyId = maps:get(access_key_id, S3Config),
            SecretAccessKey = maps:get(secret_access_key, S3Config),
            Region = maps:get(region, S3Config),
            
            io:format("S3 DEBUG: Calling s3_nif:presigned_get_object~n"),
            
            case s3_nif:presigned_get_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key, Duration) of
                {ok, PresignedUrl} ->
                    io:format("S3 DEBUG: Success! PresignedUrl=~p~n", [PresignedUrl]),
                    % replace the internal endpoint with public endpoint so the user can properly use it
                    % in the case where the developer is using locally spawned MinIO cluster.
                    PublicUrl = binary:replace(PresignedUrl, Endpoint, PublicEndpoint, [global]),
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => PublicUrl,
                        <<"content-type">> => <<"text/plain">>
                    }};
                {error, Reason} ->
                    io:format("S3 DEBUG: NIF error: ~p~n", [Reason]),
                    {error, #{<<"status">> => 500, <<"body">> => list_to_binary(Reason)}}
            end;
        Other ->
            io:format("S3 DEBUG: JSON decode returned non-map: ~p~n", [Other]),
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid JSON format">>}}
    catch 
        Error:Reason ->
            io:format("S3 DEBUG: JSON decode failed - Error=~p, Reason=~p~n", [Error, Reason]),
            {error, #{<<"status">> => 400, <<"body">> => <<"Invalid JSON">>}}
    end.


%% S3 API COMPLIANT METHODS

%% GetObjectCommand handler
get_object_handler(Bucket, Key, Msg, Opts) ->
    io:format("S3 DEBUG: get_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    % get_object_handler is used internally hb_gateway_s3:read() therefore
    % we load the admin internal config directly
    S3Config = case maps:get(internal, Opts, false) of
        true ->
            io:format("internal get_object_handler call"),
            load_s3_config();
        false ->
            load_s3_credentials(Msg)
    end,

    %% Extract Range header from the message headers
    Range = case maps:get(<<"headers">>, Msg, #{}) of
        Headers when is_map(Headers) ->
            case maps:get(<<"range">>, Headers, undefined) of
                undefined ->
                    % Try capitalized version for clients compatibility safety
                    maps:get(<<"Range">>, Headers, <<"">>);
                RangeValue ->
                    RangeValue
            end;
        _ ->
            <<"">>
    end,
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),

    io:format("S3 DEBUG: Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:get_object~n"),
    % args are passed as binary
    case s3_nif:get_object(
        Endpoint,        
        AccessKeyId,     
        SecretAccessKey, 
        Region,          
        Bucket,          
        Key,
        Range              
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

%% PutObjectCommand handler
put_object_handler(Bucket, Key, Body, Msg, _Opts) ->
    io:format("S3 DEBUG: put_object_handler Bucket=~p Key=~p Body size=~p~n", [Bucket, Key, byte_size(Body)]),
    S3Config = load_s3_credentials(Msg),

    ExpiryDays = case maps:get(<<"headers">>, Msg, #{}) of
        Headers when is_map(Headers) ->
            ExpiryBinary = case maps:get(<<"x-amz-meta-expiry-days">>, Headers, undefined) of
                undefined -> <<"0">>;
                Value -> Value
            end,
            io:format("S3 DEBUG: Extracted expiry binary: ~p~n", [ExpiryBinary]),
            try binary_to_integer(ExpiryBinary)
            catch _:_ -> 0
            end;
        _ ->
            0
    end,

    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    io:format("S3 DEBUG: PUT Config - Endpoint=~p, AccessKeyId=~p, Region=~p, Expiry=~p~n", [Endpoint, AccessKeyId, Region, ExpiryDays]),
    io:format("S3 DEBUG: Calling s3_nif:put_object~n"),
    
    case s3_nif:put_object(
        Endpoint,        
        AccessKeyId,     
        SecretAccessKey, 
        Region,          
        Bucket,          
        Key,
        Body,
        ExpiryDays       
    ) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif put_object success, S3Response=~p~n", [S3Response]),
            
            % Extract ETag from response
            ETag = maps:get(<<"etag">>, S3Response, <<>>),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => <<>>,  % PUT responses typically have empty body
                <<"etag">> => ETag,
                <<"accept-ranges">> => <<"bytes">>
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif put_object error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

%% CreateBucketCommand handler
create_bucket_handler(BucketName, Msg, _Opts) ->
    io:format("S3 DEBUG: create_bucket_handler BucketName=~p~n", [BucketName]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    io:format("S3 DEBUG: CREATE_BUCKET Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:create_bucket~n"),
    
    case s3_nif:create_bucket(
        Endpoint,        
        AccessKeyId,     
        SecretAccessKey, 
        Region,          
        BucketName
    ) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif create_bucket success, S3Response=~p~n", [S3Response]),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => <<>>,  % CREATE_BUCKET responses typically empty
                <<"location">> => <<"/", BucketName/binary>>
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif create_bucket error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

head_object_handler(Bucket, Key, Msg, _Opts) ->
    io:format("S3 DEBUG: head_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    case s3_nif:head_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key) of
        {ok, S3Response} ->
            ETag = maps:get(<<"etag">>, S3Response, <<>>),
            LastModified = maps:get(<<"last_modified">>, S3Response, <<>>),
            ContentType = maps:get(<<"content_type">>, S3Response, <<>>),
            ContentLength = maps:get(<<"content_length">>, S3Response, <<>>),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => <<>>,  % HEAD responses have no body
                <<"etag">> => ETag,
                <<"last-modified">> => LastModified,
                <<"content-type">> => ContentType,
                <<"content-length">> => ContentLength,
                <<"accept-ranges">> => <<"bytes">>
            }};
        {error, Reason} ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

%% DeleteObjectCommand handler
delete_object_handler(Bucket, Key, Msg, _Opts) ->
    io:format("S3 DEBUG: delete_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    io:format("S3 DEBUG: DELETE Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:delete_object~n"),
    
    case s3_nif:delete_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif delete_object success, S3Response=~p~n", [S3Response]),
            
            DeleteMarker = maps:get(<<"delete_marker">>, S3Response, <<"false">>),
            VersionId = maps:get(<<"version_id">>, S3Response, <<>>),
            
            {ok, #{
                <<"status">> => 204,  
                <<"body">> => <<>>,   
                <<"x-amz-delete-marker">> => DeleteMarker,
                <<"x-amz-version-id">> => VersionId
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif delete_object error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

%% HeadBucketCommand handler
head_bucket_handler(Bucket, Msg, _Opts) ->
    io:format("S3 DEBUG: head_bucket_handler Bucket=~p~n", [Bucket]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    io:format("S3 DEBUG: HEAD_BUCKET Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:head_bucket~n"),
    
    case s3_nif:head_bucket(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif head_bucket success, S3Response=~p~n", [S3Response]),
            
            BucketRegion = maps:get(<<"bucket_region">>, S3Response, Region),
            AccessPointAlias = maps:get(<<"access_point_alias">>, S3Response, <<"false">>),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => <<>>,  % HEAD responses have no body
                <<"x-amz-bucket-region">> => BucketRegion,
                <<"x-amz-access-point-alias">> => AccessPointAlias
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif head_bucket error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 404,
                <<"body">> => <<"NoSuchBucket">>
            }}
    end.

%% ListObjectsCommand handler
list_objects_handler(Bucket, Msg, Opts) ->
    io:format("S3 DEBUG: list_objects_handler Bucket=~p~n", [Bucket]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    % Extract query parameters from the message with defaults
    QueryParams = hb_ao:get(<<"query-params">>, Msg, #{}, Opts),
    
    Prefix = maps:get(<<"prefix">>, QueryParams, <<"">>),
    Delimiter = maps:get(<<"delimiter">>, QueryParams, <<"">>),
    Marker = maps:get(<<"marker">>, QueryParams, <<"">>),
    MaxKeysParam = maps:get(<<"max-keys">>, QueryParams, <<"1000">>),
    
    % Ensure MaxKeys is a binary string
    MaxKeys = case MaxKeysParam of
        B when is_binary(B) -> B;
        I when is_integer(I) -> integer_to_binary(I);
        L when is_list(L) -> list_to_binary(L);
        _ -> <<"1000">>
    end,
    
    io:format("S3 DEBUG: LIST_OBJECTS Params - Prefix=~p, Delimiter=~p, Marker=~p, MaxKeys=~p~n", 
              [Prefix, Delimiter, Marker, MaxKeys]),
    io:format("S3 DEBUG: LIST_OBJECTS Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", 
              [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:list_objects~n"),
    
    case s3_nif:list_objects(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Prefix, Delimiter, Marker, MaxKeys) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif list_objects success, S3Response keys=~p~n", [maps:keys(S3Response)]),
            
            % Build S3 XML response
            XmlBody = build_list_objects_xml(Bucket, S3Response),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => XmlBody,
                <<"content-type">> => <<"application/xml">>,
                <<"accept-ranges">> => <<"bytes">>
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif list_objects error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.

%% Build S3-compatible XML response for ListObjects - utility function
build_list_objects_xml(Bucket, S3Response) ->
    IsTruncated = maps:get(<<"is_truncated">>, S3Response, <<"false">>),
    Marker = maps:get(<<"marker">>, S3Response, <<"">>),
    NextMarker = maps:get(<<"next_marker">>, S3Response, <<"">>),
    MaxKeys = maps:get(<<"max_keys">>, S3Response, <<"1000">>),
    Prefix = maps:get(<<"prefix">>, S3Response, <<"">>),
    Delimiter = maps:get(<<"delimiter">>, S3Response, <<"">>),
    
    ObjectCount = binary_to_integer(
        case maps:get(<<"object_count">>, S3Response, <<"0">>) of
            ObjCountVal when is_binary(ObjCountVal) -> ObjCountVal;
            ObjCountVal when is_list(ObjCountVal) -> list_to_binary(ObjCountVal);
            _ -> <<"0">>
        end
    ),
    CommonPrefixCount = binary_to_integer(
        case maps:get(<<"common_prefix_count">>, S3Response, <<"0">>) of
            PrefixCountVal when is_binary(PrefixCountVal) -> PrefixCountVal;
            PrefixCountVal when is_list(PrefixCountVal) -> list_to_binary(PrefixCountVal);
            _ -> <<"0">>
        end
    ),
    
    % Build objects XML
    ObjectsXml = case ObjectCount > 0 of
        true ->
            lists:map(fun(I) ->
                Key = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_key"), S3Response, <<"">>),
                LastModified = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_last_modified"), S3Response, <<"">>),
                ETag = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_etag"), S3Response, <<"">>),
                Size = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_size"), S3Response, <<"0">>),
                StorageClass = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_storage_class"), S3Response, <<"STANDARD">>),
                OwnerDisplayName = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_owner_display_name"), S3Response, <<"">>),
                OwnerId = maps:get(list_to_binary("object_" ++ integer_to_list(I) ++ "_owner_id"), S3Response, <<"">>),
                
                [<<"<Contents>">>,
                 <<"<Key>">>, Key, <<"</Key>">>,
                 <<"<LastModified>">>, LastModified, <<"</LastModified>">>,
                 <<"<ETag>">>, ETag, <<"</ETag>">>,
                 <<"<Size>">>, Size, <<"</Size>">>,
                 <<"<StorageClass>">>, StorageClass, <<"</StorageClass>">>,
                 case {OwnerDisplayName, OwnerId} of
                     {<<"">>, <<"">>} -> [];
                     _ -> [<<"<Owner>">>,
                           <<"<DisplayName>">>, OwnerDisplayName, <<"</DisplayName>">>,
                           <<"<ID>">>, OwnerId, <<"</ID>">>,
                           <<"</Owner>">>]
                 end,
                 <<"</Contents>">>]
            end, lists:seq(0, ObjectCount-1));
        false -> []
    end,
    
    % Build common prefixes XML
    CommonPrefixesXml = case CommonPrefixCount > 0 of
        true ->
            lists:map(fun(I) ->
                PrefixValue = maps:get(list_to_binary("common_prefix_" ++ integer_to_list(I)), S3Response, <<"">>),
                [<<"<CommonPrefixes>">>,
                 <<"<Prefix>">>, PrefixValue, <<"</Prefix>">>,
                 <<"</CommonPrefixes>">>]
            end, lists:seq(0, CommonPrefixCount-1));
        false -> []
    end,
    
    % Build complete XML
    XmlParts = [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
        <<"<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">">>,
        <<"<Name>">>, Bucket, <<"</Name>">>,
        <<"<Prefix>">>, Prefix, <<"</Prefix>">>,
        <<"<Marker>">>, Marker, <<"</Marker>">>,
        <<"<MaxKeys>">>, MaxKeys, <<"</MaxKeys>">>,
        <<"<IsTruncated>">>, IsTruncated, <<"</IsTruncated>">>,
        case NextMarker of
            <<"">> -> [];
            _ -> [<<"<NextMarker>">>, NextMarker, <<"</NextMarker>">>]
        end,
        case Delimiter of
            <<"">> -> [];
            _ -> [<<"<Delimiter>">>, Delimiter, <<"</Delimiter>">>]
        end,
        ObjectsXml,
        CommonPrefixesXml,
        <<"</ListBucketResult>">>
    ],
    
    iolist_to_binary(XmlParts).

%% DeleteObjectsCommand handler
delete_objects_handler(Bucket, RequestBody, Msg, Opts) ->
    io:format("S3 DEBUG: delete_objects_handler Bucket=~p~n", [Bucket]),
    S3Config = load_s3_credentials(Msg),
    
    Endpoint = maps:get(endpoint, S3Config),
    AccessKeyId = maps:get(access_key_id, S3Config),
    SecretAccessKey = maps:get(secret_access_key, S3Config),
    Region = maps:get(region, S3Config),
    
    QueryParams = hb_ao:get(<<"query-params">>, Msg, #{}, Opts),
    io:format("S3 DEBUG: QueryParams=~p~n", [QueryParams]),
    
    % Parse XML body to extract keys to delete
    KeysToDelete = parse_delete_request_body(RequestBody),
    
    io:format("S3 DEBUG: DELETE_OBJECTS Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", 
              [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Keys to delete: ~p~n", [KeysToDelete]),
    io:format("S3 DEBUG: Calling s3_nif:delete_objects~n"),
    
    case s3_nif:delete_objects(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, KeysToDelete) of
        {ok, S3Response} ->
            io:format("S3 DEBUG: s3_nif delete_objects success, S3Response=~p~n", [S3Response]),
            
            % Build S3 XML response
            XmlBody = build_delete_objects_xml(S3Response),
            
            {ok, #{
                <<"status">> => 200,
                <<"body">> => XmlBody,
                <<"content-type">> => <<"application/xml">>,
                <<"accept-ranges">> => <<"bytes">>
            }};
        {error, Reason} ->
            io:format("S3 DEBUG: s3_nif delete_objects error: ~p~n", [Reason]),
            {error, #{
                <<"status">> => 500,
                <<"body">> => list_to_binary(Reason)
            }}
    end.


%% Helper function to parse delete request XML body
parse_delete_request_body(Body) ->
    % Simple regex-based extraction for now
    case re:run(Body, <<"<Key>(.*?)</Key>">>, [global, {capture, [1], binary}]) of
        {match, Matches} ->
            [Key || [Key] <- Matches];
        nomatch ->
            []
    end.

%% Build S3-compatible XML response for DeleteObjects
build_delete_objects_xml(S3Response) ->
    DeletedCount = binary_to_integer(
        case maps:get(<<"deleted_count">>, S3Response, <<"0">>) of
            DelCountVal when is_binary(DelCountVal) -> DelCountVal;
            DelCountVal when is_list(DelCountVal) -> list_to_binary(DelCountVal);
            _ -> <<"0">>
        end
    ),
    ErrorCount = binary_to_integer(
        case maps:get(<<"error_count">>, S3Response, <<"0">>) of
            ErrCountVal when is_binary(ErrCountVal) -> ErrCountVal;
            ErrCountVal when is_list(ErrCountVal) -> list_to_binary(ErrCountVal);
            _ -> <<"0">>
        end
    ),
    
    % Build deleted objects XML
    DeletedXml = case DeletedCount > 0 of
        true ->
            lists:map(fun(I) ->
                Key = maps:get(list_to_binary("deleted_" ++ integer_to_list(I) ++ "_key"), S3Response, <<"">>),
                VersionId = maps:get(list_to_binary("deleted_" ++ integer_to_list(I) ++ "_version_id"), S3Response, <<"">>),
                DeleteMarker = maps:get(list_to_binary("deleted_" ++ integer_to_list(I) ++ "_delete_marker"), S3Response, <<"false">>),
                
                [<<"<Deleted>">>,
                 <<"<Key>">>, Key, <<"</Key>">>,
                 case VersionId of
                     <<"">> -> [];
                     _ -> [<<"<VersionId>">>, VersionId, <<"</VersionId>">>]
                 end,
                 <<"<DeleteMarker>">>, DeleteMarker, <<"</DeleteMarker>">>,
                 <<"</Deleted>">>]
            end, lists:seq(0, DeletedCount-1));
        false -> []
    end,
    
    % Build error objects XML
    ErrorsXml = case ErrorCount > 0 of
        true ->
            lists:map(fun(I) ->
                Key = maps:get(list_to_binary("error_" ++ integer_to_list(I) ++ "_key"), S3Response, <<"">>),
                Code = maps:get(list_to_binary("error_" ++ integer_to_list(I) ++ "_code"), S3Response, <<"">>),
                Message = maps:get(list_to_binary("error_" ++ integer_to_list(I) ++ "_message"), S3Response, <<"">>),
                VersionId = maps:get(list_to_binary("error_" ++ integer_to_list(I) ++ "_version_id"), S3Response, <<"">>),
                
                [<<"<Error>">>,
                 <<"<Key>">>, Key, <<"</Key>">>,
                 <<"<Code>">>, Code, <<"</Code>">>,
                 <<"<Message>">>, Message, <<"</Message>">>,
                 case VersionId of
                     <<"">> -> [];
                     _ -> [<<"<VersionId>">>, VersionId, <<"</VersionId>">>]
                 end,
                 <<"</Error>">>]
            end, lists:seq(0, ErrorCount-1));
        false -> []
    end,
    
    % Build complete XML
    XmlParts = [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>,
        <<"<DeleteResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">">>,
        DeletedXml,
        ErrorsXml,
        <<"</DeleteResult>">>
    ],
    
    iolist_to_binary(XmlParts).

%% @doc Extract S3 request credentials from Authorization header
extract_aws_credentials(Headers) ->
    case maps:get(<<"authorization">>, Headers, undefined) of
        undefined ->
            % Try lowercase version
            case maps:get(<<"Authorization">>, Headers, undefined) of
                undefined -> {error, no_auth_header};
                AuthHeader -> parse_aws_auth_header(AuthHeader)
            end;
        AuthHeader ->
            parse_aws_auth_header(AuthHeader)
    end.

%% @doc Parse AWS4-HMAC-SHA256 Authorization header
parse_aws_auth_header(AuthHeader) ->
    case binary:split(AuthHeader, <<" ">>, [global]) of
        [<<"AWS4-HMAC-SHA256">>, CredentialPart, _SignedHeadersPart, _SignaturePart] ->
            parse_aws4_credential(CredentialPart);
        [<<"AWS">>, AccessKeyAndSignature] ->
            % Handle older AWS signature format: AWS AccessKeyId:Signature
            case binary:split(AccessKeyAndSignature, <<":">>) of
                [AccessKeyId, _Signature] ->
                    {ok, #{access_key_id => AccessKeyId, secret_access_key => undefined}};
                _ ->
                    {error, invalid_aws_auth_format}
            end;
        _ ->
            {error, unsupported_auth_format}
    end.

%% @doc Parse AWS4 credential part: Credential=AKIAIOSFODNN7EXAMPLE/20230101/us-west-2/s3/aws4_request
parse_aws4_credential(CredentialPart) ->
    case binary:split(CredentialPart, <<"=">>) of
        [<<"Credential">>, CredentialValue] ->
            case binary:split(CredentialValue, <<"/">>) of
                [AccessKeyId | _Rest] ->
                    {ok, #{access_key_id => AccessKeyId, secret_access_key => undefined}};
                _ ->
                    {error, invalid_credential_format}
            end;
        _ ->
            {error, missing_credential}
    end.

%% @doc Extract credentials from query parameters (for pre-signed URLs)
extract_credentials_from_query(Query) ->
    QueryParams = parse_query_string(Query),
    case maps:get(<<"X-Amz-Credential">>, QueryParams, undefined) of
        undefined ->
            {error, no_credentials_in_query};
        CredentialValue ->
            case binary:split(CredentialValue, <<"/">>) of
                [AccessKeyId | _Rest] ->
                    {ok, #{access_key_id => AccessKeyId, secret_access_key => undefined}};
                _ ->
                    {error, invalid_query_credential_format}
            end
    end.

%% @doc Simple query string parser
parse_query_string(<<>>) -> #{};
parse_query_string(Query) ->
    Pairs = binary:split(Query, <<"&">>, [global]),
    lists:foldl(fun(Pair, Acc) ->
        case binary:split(Pair, <<"=">>) of
            [Key, Value] ->
                DecodedKey = uri_decode(Key),
                DecodedValue = uri_decode(Value),
                Acc#{DecodedKey => DecodedValue};
            [Key] ->
                Acc#{uri_decode(Key) => <<>>};
            _ ->
                Acc
        end
    end, #{}, Pairs).

%% @doc Simple URI decode
uri_decode(Bin) ->
    uri_decode(Bin, <<>>).

uri_decode(<<$%, H1, H2, Rest/binary>>, Acc) ->
    Char = list_to_integer([H1, H2], 16),
    uri_decode(Rest, <<Acc/binary, Char>>);
uri_decode(<<$+, Rest/binary>>, Acc) ->
    uri_decode(Rest, <<Acc/binary, $\s>>);
uri_decode(<<C, Rest/binary>>, Acc) ->
    uri_decode(Rest, <<Acc/binary, C>>);
uri_decode(<<>>, Acc) ->
    Acc.

%% @doc Get credentials with fallback priority: Headers -> Query -> Config
get_request_credentials(Headers, Query) ->
    case extract_aws_credentials(Headers) of
        {ok, #{access_key_id := AccessKeyId}} ->
            io:format("S3 DEBUG: Extracted AccessKeyId from headers: ~p~n", [AccessKeyId]),
            {ok, AccessKeyId, undefined}; % SecretAccessKey is not extractable from headers
        {error, _} ->
            case extract_credentials_from_query(Query) of
                {ok, #{access_key_id := AccessKeyId}} ->
                    io:format("S3 DEBUG: Extracted AccessKeyId from query: ~p~n", [AccessKeyId]),
                    {ok, AccessKeyId, undefined};
                {error, _} ->
                    io:format("S3 DEBUG: No credentials in request, using config~n"),
                    {error, no_request_credentials}
            end
    end.
