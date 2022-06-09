%%%-------------------------------------------------------------------
%% @doc kademlia_dht assist.
%% @end
%%%-------------------------------------------------------------------

-module(kademlia_dht_assist).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-import(simulation, [contacts/1, transformation/2]).

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% start_link() -> Result
%%  Result =
%%      {ok, PID}
%%      {error, Reason}
%% INFO:
%%      start and link gen_server
%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%
%% init() -> Result
%%  Result =
%%      {ok, STATE}
%% INFO:
%%      initiate gen_server
%%
init([]) ->
    register(assist, self()),
    ok=storage:init(),%% ensure DHT tables available
    info(),
    { ok, _STATE=#{} }.

%%
%% handle_call(_RPC, _Caller, STATE) -> Result
%%  Result =
%%      {reply, {error, unexpected}, STATE}
%% INFO:
%%      not supported
%%
handle_call(_RPC, _Caller, STATE) ->
    {reply, {error, unexpected}, STATE}.

%%
%% handle_cast(_RPC, STATE) -> Result
%%  Result =
%%      {noreply, STATE}
%% INFO:
%%      not supported
%%
handle_cast(_RPC, STATE) ->
    %%  unexpected
    {noreply, STATE}.

%%
%% handle_info(RPC, STATE) -> Result
%%  RPC =
%%      {insert, Counter}
%%      {transform, K_index, K_rate}
%%      dht_report
%%      help
%%  Result =
%%      {noreply, STATE}
%% INFO:
%%      validate the RPC and call corresponding procedures
%%
handle_info({insert, Counter}, STATE)
   when is_number(Counter)->
    contacts(Counter),%% simulation:contacts(Counter)
    {noreply, STATE};
handle_info({transform, K_index, K_rate}, STATE)
  when is_number(K_index)
   andalso is_number(K_rate)->
        %% simulation:transformation(K_index, K_rate)
    transformation(K_index, K_rate),
    {noreply, STATE};
handle_info(dht_report, STATE) ->
    storage:report(),
    {noreply, STATE};
handle_info(help, STATE) ->
    info(),
    {noreply, STATE};    
handle_info(_RPC, STATE) ->
    io:format("bad RPC, use ' assist ! help ' for more information! ~n"),
    {noreply, STATE}.

%%
%% terminate(_Reason, _STATE) -> Result
%%  Result =
%%      ok
%% INFO:
%%      not supported
%%
terminate(_Reason, _STATE) ->
    ok.

%%
%% code_change(_OldVsn, STATE, _Extra) -> Result
%%  Result =
%%      {ok, STATE}
%% INFO:
%%      not supported
%%
code_change(_OldVsn, STATE, _Extra) ->
    {ok, STATE}.
%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% info() -> Result
%%  Result =
%%      -
%% INFO:
%%      print Simulator information and how to use
%%
info() ->
    io:format("##################################~n"),
    io:format("##################################~n"),
    io:format("Kademlia~n"),
    io:format("Distributed Hash Table~n"),
    io:format("Simulator version 0.1.0~n"),
    io:format("Send RPC to assist genserver~n"),
    io:format("     assist ! RPC ~n"),
    io:format("Available RPCs : ~n"),
    io:format("{insert, Counter} ~n"),
    io:format("{transform, K_index, K_rate} ~n"),
    io:format("dht_report ~n"),
    io:format("help ~n"),
    io:format("##################################~n"),
    io:format("##################################~n").