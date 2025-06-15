-module(dev_helios).
-export([info/1, info/3, start/0, start/4, stop/1, get_address/1, auto_start/0]).

%% @doc Starts a Helios Ethereum light client with default settings
%% Uses default values: localhost:8545, public RPC endpoints, and a temp data directory
start() ->
    Address = "127.0.0.1:8545",
    ExecutionRpc = "https://eth.llamarpc.com",
    ConsensusRpc = "https://ethereum.operationsolarstorm.org",
    DataDir = "/tmp/helios",
    start(Address, ExecutionRpc, ConsensusRpc, DataDir).

%% @doc Starts a Helios Ethereum light client with HTTP JSON-RPC server
%% @param Address String representation of IP:Port (e.g., "0.0.0.0:8545")
%% @param ExecutionRpc URL of the execution layer RPC (e.g., "https://eth.llamarpc.com")
%% @param ConsensusRpc URL of the consensus layer RPC (e.g., "https://ethereum.operationsolarstorm.org")
%% @param DataDir Directory to store Helios data
%% @returns {ok, ServerRef} | {error, Reason}
start(Address, ExecutionRpc, ConsensusRpc, DataDir) ->
    % Convert string parameters to binaries if needed
    BinAddress = list_to_binary(Address),
    BinExecutionRpc = list_to_binary(ExecutionRpc),
    BinConsensusRpc = list_to_binary(ConsensusRpc),
    BinDataDir = list_to_binary(DataDir),
    
    case helios_nif:start_helios(BinAddress, BinExecutionRpc, BinConsensusRpc, BinDataDir) of
        {error, Reason} -> {error, Reason};
        ServerRef -> {ok, ServerRef}
    end.


%% @doc Stops a running Helios server
%% @param ServerRef Reference returned from start/4
%% @returns ok | {error, Reason}
stop(ServerRef) ->
    case helios_nif:stop_server(ServerRef) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Gets the address of a running server
%% @param ServerRef Reference returned from start/4
%% @returns {ok, Address} | {error, Reason}
get_address(ServerRef) ->
    case helios_nif:server_address(ServerRef) of
        {error, Reason} -> {error, Reason};
        Address -> {ok, Address}
    end.

%% @doc Device API information
info(_) ->
    #{
        <<"default">> => dev_message,
        handlers => #{
            <<"info">> => fun info/3
        }
    }.

%% @doc return helios device info
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Helios Ethereum light client with HTTP JSON-RPC server">>,
        <<"version">> => <<"0.1.0">>,
        <<"status">> => get_server_status(),
        <<"server_address">> => get_server_address()
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Auto-start function that will be called during application startup
auto_start() ->
    io:format("Starting Helios Ethereum light client...~n"),
    case start() of
        {ok, ServerRef} ->
            % Store the server reference in the application environment
            application:set_env(hb, helios_server_ref, ServerRef),
            {ok, Address} = get_address(ServerRef),
            io:format("Helios server started and listening at ~s~n", [Address]),
            {ok, ServerRef};
        {error, Reason} ->
            io:format("Failed to start Helios server: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Helper function to get the server status for info
get_server_status() ->
    case application:get_env(hb, helios_server_ref) of
        {ok, _} -> <<"running">>;
        undefined -> <<"not running">>
    end.

%% @doc Helper function to get the server address for info
get_server_address() ->
    case application:get_env(hb, helios_server_ref) of
        {ok, ServerRef} ->
            case get_address(ServerRef) of
                {ok, Address} -> Address;
                {error, _} -> <<"not available">>
            end;
        undefined ->
            <<"not running">>
    end.
