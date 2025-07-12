-module(s3_nif_test).
-export([test_get_object/0, test_put_object/0]).

test_put_object() ->
    io:format("=== S3 PUT_OBJECT NIF TEST ===~n"),
     Endpoint = <<"https://s3.load.rs">>,
     AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
     SecretAccessKey = <<"">>,
     Region = <<"eu-west-2">>,
     Bucket = <<"darwin-1">>,
     Key = list_to_binary(integer_to_list(erlang:system_time(second))), % unique binary obkect name,
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
    io:format("=== S3 NIF TEST ===~n"),
    
    Endpoint = <<"https://s3.load.rs">>,
    AccessKeyId = <<"load_acc_XLrIyYcF6vdwr9tiug2wrLRSuSPmtucZ">>,
    SecretAccessKey = <<"">>,
    Region = <<"eu-west-2">>,
    Bucket = <<"darwin-1">>,
    Key = <<"test.txt">>,
    
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
