-module(my_device_nif).
-export([hello/0, query/0]).
-on_load(init/0).

-define(NOT_LOADED, not_loaded(?LINE)).

-spec hello() -> binary().
hello() ->
    ?NOT_LOADED.

-spec query() -> binary().
query() ->
    ?NOT_LOADED.

init() ->
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~p~n", [Cwd]),
        
    PrivDir = case code:priv_dir(hb) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    
    % Same path structure, just different crate name
    NifPath = filename:join([PrivDir, "crates", "my_device", "my_device"]),
    io:format("NIF path: ~p~n", [NifPath]),
    
    NifSoPath = NifPath ++ ".so",
    io:format("NIF .so exists: ~p~n", [filelib:is_file(NifSoPath)]),
    
    Result = erlang:load_nif(NifPath, 0),
    io:format("Load result: ~p~n", [Result]),
    
    Result.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).