-module(s3_nif).
-export([get_object/7, put_object/7, create_bucket/5, head_object/6, delete_object/6, head_bucket/5, list_objects/9, delete_objects/6, get_cached_object/7]).
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-spec get_object(string(), string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
get_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key, _Range) ->
    ?NOT_LOADED.

-spec head_object(string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
head_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key) ->
    ?NOT_LOADED.

-spec put_object(string(), string(), string(), string(), string(), string(), binary()) -> {ok, map()} | {error, string()}.
put_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key, _Body) ->
    ?NOT_LOADED.

-spec delete_object(string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
delete_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key) ->
    ?NOT_LOADED.

-spec delete_objects(string(), string(), string(), string(), string(), list()) -> {ok, map()} | {error, string()}.
delete_objects(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Keys) ->
    ?NOT_LOADED.

-spec create_bucket(string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
create_bucket(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket) ->
    ?NOT_LOADED.

-spec head_bucket(string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
head_bucket(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket) ->
    ?NOT_LOADED.

-spec list_objects(string(), string(), string(), string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
list_objects(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Prefix, _Delimiter, _Marker, _MaxKeys) ->
    ?NOT_LOADED.

-spec get_cached_object(string(), string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
get_cached_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key, _Range) ->
    ?NOT_LOADED.

init() ->
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~p~n", [Cwd]),
    
    PrivDir = case code:priv_dir(hb) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    
    NifPath = filename:join([PrivDir, "crates", "s3_nif", "s3_nif"]),
    io:format("NIF path: ~p~n", [NifPath]),
    
    NifSoPath = NifPath ++ ".so",
    io:format("NIF .so exists: ~p~n", [filelib:is_file(NifSoPath)]),
    
    Result = erlang:load_nif(NifPath, 0),
    io:format("Load result: ~p~n", [Result]),
    Result.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
