%%%-------------------------------------------------------------------
%% @doc kademlia_dht public API
%% @end
%%%-------------------------------------------------------------------

-module(kademlia_dht).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kademlia_dht_assist:start_link().

stop(_State) ->
    ok.

%% internal functions
