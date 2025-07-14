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
            get_object_handler(Bucket, Key, Msg, Opts);
        {bucket, Bucket} ->
            io:format("S3 DEBUG: Parsed as bucket=~p, calling list_objects_handler~n", [Bucket]),
            list_objects_handler(Bucket, Msg, Opts);
        root ->
            io:format("S3 DEBUG: Parsed as root~n"),
            {error, #{<<"status">> => 501, <<"body">> => <<"ListBucketsCommand not implemented yet">>}};
        invalid ->
            io:format("S3 DEBUG: Invalid path~n"),
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

%% PutObjectCommand handler
put_object_handler(Bucket, Key, Body, _Msg, Opts) ->
    io:format("S3 DEBUG: put_object_handler Bucket=~p Key=~p Body size=~p~n", [Bucket, Key, byte_size(Body)]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
    io:format("S3 DEBUG: PUT Config - Endpoint=~p, AccessKeyId=~p, Region=~p~n", [Endpoint, AccessKeyId, Region]),
    io:format("S3 DEBUG: Calling s3_nif:put_object~n"),
    
    case s3_nif:put_object(
        Endpoint,        
        AccessKeyId,     
        SecretAccessKey, 
        Region,          
        Bucket,          
        Key,
        Body             % The request body
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
create_bucket_handler(BucketName, _Msg, Opts) ->
    io:format("S3 DEBUG: create_bucket_handler BucketName=~p~n", [BucketName]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
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

head_object_handler(Bucket, Key, _Msg, Opts) ->
    io:format("S3 DEBUG: head_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
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
delete_object_handler(Bucket, Key, _Msg, Opts) ->
    io:format("S3 DEBUG: delete_object_handler Bucket=~p Key=~p~n", [Bucket, Key]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
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
head_bucket_handler(Bucket, _Msg, Opts) ->
    io:format("S3 DEBUG: head_bucket_handler Bucket=~p~n", [Bucket]),
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
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
    S3Config = hb_opts:get(<<"s3-config">>, #{}, Opts),
    
    Endpoint = maps:get(<<"endpoint">>, S3Config, <<"https://s3.load.rs">>),
    AccessKeyId = maps:get(<<"access-key-id">>, S3Config, <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>),
    SecretAccessKey = maps:get(<<"secret-access-key">>, S3Config, <<"">>),
    Region = maps:get(<<"region">>, S3Config, <<"eu-west-2">>),
    
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
    
    ObjectCount = binary_to_integer(maps:get(<<"object_count">>, S3Response, <<"0">>)),
    CommonPrefixCount = binary_to_integer(maps:get(<<"common_prefix_count">>, S3Response, <<"0">>)),
    
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
