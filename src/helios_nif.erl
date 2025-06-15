-module(helios_nif).
-export([start_helios/4, stop_server/1, server_address/1]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

start_helios(Address, ExecutionRpc, ConsensusRpc, DataDir) ->
    ?NOT_LOADED.

stop_server(ServerRef) ->
    ?NOT_LOADED.

server_address(ServerRef) ->
    ?NOT_LOADED.

init() ->
    PrivDir = case code:priv_dir(hb) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    
    % Build the NIF path using the priv directory
    NifPath = filename:join([PrivDir, "crates", "helios_nif", "helios_nif"]),
    
    % Try to load the NIF
    erlang:load_nif(NifPath, 0).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
