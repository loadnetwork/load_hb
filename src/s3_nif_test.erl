-module(s3_nif_test).
-export([test_get_object/0, test_put_object/0, test_create_bucket/0, test_head_object/0, test_delete_object/0, test_head_bucket/0]).

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

    io:format("Testing s3_nif:put_object with:~n"),
    io:format("  Cluster Endpoint: ~p~n", [Endpoint]),
    io:format("  AccessKeyId: ~p~n", [AccessKeyId]),
    io:format("  Bucket: ~p~n", [Bucket]),
    io:format("  Key: ~p~n", [Key]),
    io:format("  Data: ~p~n", [Data]),

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