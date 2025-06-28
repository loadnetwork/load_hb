-module(quantum_runtime_nif).
-export([hello/0, compute/3]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-spec hello() -> binary().
hello() ->
    ?NOT_LOADED.

-spec compute(non_neg_integer(), binary(), [non_neg_integer()]) -> map().
compute(NumQubits, FunctionId, Measurements) when is_integer(NumQubits), is_binary(FunctionId), is_list(Measurements) ->
    ?NOT_LOADED.

init() ->
    % Get the current directory for debugging
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~p~n", [Cwd]),
        
    PrivDir = case code:priv_dir(hb) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    
    % Build the NIF path using the priv directory
    NifPath = filename:join([PrivDir, "crates", "quantum_runtime_nif", "quantum_runtime_nif"]),
    io:format("NIF path: ~p~n", [NifPath]),
    
    % Check if the NIF file exists
    NifSoPath = NifPath ++ ".so",
    io:format("NIF .so exists: ~p~n", [filelib:is_file(NifSoPath)]),
    
    % Try to load the NIF
    Result = erlang:load_nif(NifPath, 0),
    io:format("Load result: ~p~n", [Result]),
    
    Result.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
