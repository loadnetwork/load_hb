%%%-------------------------------------------------------------------
%% @doc The main HyperBEAM application module.
%% @end
%%%-------------------------------------------------------------------

-module(hb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("include/hb.hrl").

% start(_StartType, _StartArgs) ->
%     hb:init(),
%     hb_sup:start_link(),
%     ok = dev_scheduler_registry:start(),
%     _TimestampServer = ar_timestamp:start(),
%     {ok, _} = hb_http_server:start().

start(_StartType, _StartArgs) ->
    hb:init(),
    hb_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    
    % Start the Helios server
    case dev_helios:auto_start() of
        {ok, _} -> ok;
        {error, Reason} -> 
            io:format("Warning: Failed to start Helios server: ~p~n", [Reason])
    end,
    
    {ok, _} = hb_http_server:start().

stop(_State) ->
    ok.