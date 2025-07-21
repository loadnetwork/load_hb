-module(s3_nif_test).
-export([test_get_object/0, test_put_object/0, test_create_bucket/0, test_head_object/0, test_delete_object/0, test_head_bucket/0, test_list_objects/0, test_delete_objects/0]).

test_head_object() ->
    io:format("=== S3 HEAD_OBJECT NIF TEST ==="),

     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,
     Key = <<"test-1.txt">>,

    io:format("Testing s3_nif:head_object with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Key: ~p~n", [Key]),

    case s3_nif:head_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            ETag = maps:get(<<"etag">>, Response, <<"no etag">>),
            ContentType = maps:get(<<"content_type">>, Response, <<"no content_type">>),
            io:format("ETag: ~p, ContentType: ~p~n", [ETag, ContentType]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_create_bucket() ->
    io:format("=== S3 CREATE_BUCKET NIF TEST ==="),

     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     BucketName = list_to_binary(integer_to_list(erlang:system_time(second))),

    io:format("Testing s3_nif:create_bucket with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  BucketName: ~p~n", [BucketName]),

    case s3_nif:create_bucket(Endpoint, AccessKeyId, SecretAccessKey, Region, BucketName) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.



test_put_object() ->
    io:format("=== S3 PUT_OBJECT NIF TEST ===~n"),
     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,
     Key = list_to_binary(integer_to_list(erlang:system_time(second))), % unique binary object name,
     Data = list_to_binary("hello world"),
     Expiry = 0,

    io:format("Testing s3_nif:put_object with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Key: ~p~n", [Key]),
    io:format("  Data: ~p~n", [Data]),
    io:format("  Expiry: ~p~n", [Expiry]),

    case s3_nif:put_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key, Data) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_get_object() ->
    io:format("=== S3 GET_OBJECT NIF TEST ===~n"),
    
    Endpoint = <<"https://s3.load.rs">>,
    AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
    SecretAccessKey = <<"">>,
    Region = <<"eu-west-2">>,
    Bucket = <<"darwin-1">>,
    Key = <<"test-1.txt">>,
    
    io:format("Testing s3_nif:get_object with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Key: ~p~n", [Key]),
    
    case s3_nif:get_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_delete_object() ->
    io:format("=== S3 DELETE_OBJECT NIF TEST ===~n"),
     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,
     Key = <<"test.txt">>,

    io:format("Testing s3_nif:delete_object with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Key: ~p~n", [Key]),

    case s3_nif:delete_object(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Key) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_head_bucket() ->
    io:format("=== S3 HEAD_BUCKET NIF TEST ===~n"),
     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,

    io:format("Testing s3_nif:head_bucket with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),

    case s3_nif:head_bucket(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_list_objects() ->
    io:format("=== S3 LIST_OBJECTS NIF TEST ===~n"),
    Endpoint = <<"https://s3.load.rs">>,
    AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
    SecretAccessKey = <<"">>,
    Region = <<"eu-west-2">>,
    Bucket = <<"darwin-1">>,
    Prefix = <<"">>,        % List all objects
    Delimiter = <<"">>,     % No delimiter
    Marker = <<"">>,        % No marker
    MaxKeys = <<"100">>,    % Max 100 objects

    io:format("Testing s3_nif:list_objects with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Prefix: ~p~n", [Prefix]),
    io:format("  MaxKeys: ~p~n", [MaxKeys]),

    case s3_nif:list_objects(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Prefix, Delimiter, Marker, MaxKeys) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            
            % Extract object count
            ObjectCount = binary_to_integer(maps:get(<<"object_count">>, Response, <<"0">>)),
            io:format("Object Count: ~p~n", [ObjectCount]),
            
            % List all objects
            case ObjectCount > 0 of
                true ->
                    io:format("=== OBJECTS IN BUCKET ===~n"),
                    lists:foreach(fun(I) ->
                        KeyField = list_to_binary("object_" ++ integer_to_list(I) ++ "_key"),
                        SizeField = list_to_binary("object_" ++ integer_to_list(I) ++ "_size"),
                        ETagField = list_to_binary("object_" ++ integer_to_list(I) ++ "_etag"),
                        LastModifiedField = list_to_binary("object_" ++ integer_to_list(I) ++ "_last_modified"),
                        
                        Key = maps:get(KeyField, Response, <<"unknown">>),
                        Size = maps:get(SizeField, Response, <<"0">>),
                        ETag = maps:get(ETagField, Response, <<"unknown">>),
                        LastModified = maps:get(LastModifiedField, Response, <<"unknown">>),
                        
                        io:format("  ~p. Key: ~s~n", [I+1, Key]),
                        io:format("     Size: ~s bytes~n", [Size]),
                        io:format("     ETag: ~s~n", [ETag]),
                        io:format("     Last Modified: ~s~n", [LastModified]),
                        io:format("~n")
                    end, lists:seq(0, ObjectCount-1));
                false ->
                    io:format("No objects found in bucket~n")
            end,
            
            % Check for common prefixes (folders)
            CommonPrefixCount = binary_to_integer(maps:get(<<"common_prefix_count">>, Response, <<"0">>)),
            case CommonPrefixCount > 0 of
                true ->
                    io:format("=== COMMON PREFIXES (FOLDERS) ===~n"),
                    lists:foreach(fun(I) ->
                        PrefixField = list_to_binary("common_prefix_" ++ integer_to_list(I)),
                        Prefix = maps:get(PrefixField, Response, <<"unknown">>),
                        io:format("  ~p. Prefix: ~s~n", [I+1, Prefix])
                    end, lists:seq(0, CommonPrefixCount-1));
                false ->
                    io:format("No common prefixes found~n")
            end,
            
            % Show pagination info
            IsTruncated = maps:get(<<"is_truncated">>, Response, <<"false">>),
            NextMarker = maps:get(<<"next_marker">>, Response, <<"">>),
            io:format("Is Truncated: ~s~n", [IsTruncated]),
            case NextMarker of
                <<"">> -> ok;
                _ -> io:format("Next Marker: ~s~n", [NextMarker])
            end,
            
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.

test_delete_objects() ->
    io:format("=== S3 DELETE_OBJECTS NIF TEST ===~n"),
     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,
     Keys = [<<"test.txt">>],

    io:format("Testing s3_nif:delete_objects with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Keys: ~p~n", [Keys]),

    case s3_nif:delete_objects(Endpoint, AccessKeyId, SecretAccessKey, Region, Bucket, Keys) of
        {ok, Response} ->
            io:format("SUCCESS: Response: ~p~n", [Response]),
            Body = maps:get(<<"body">>, Response, <<"no body">>),
            io:format("Body: ~p~n", [Body]),
            ok;
        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            error;
        Other ->
            io:format("UNEXPECTED RESULT: ~p~n", [Other]),
            unexpected
    end.