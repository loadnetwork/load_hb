-module(s3_nif).
-export([get_object/6]).
-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-spec get_object(string(), string(), string(), string(), string(), string()) -> {ok, map()} | {error, string()}.
get_object(_Endpoint, _AccessKeyId, _SecretAccessKey, _Region, _Bucket, _Key) ->
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
