-module(simulation).

-export([contacts/1]).
-export([transformation/2]).

-include_lib("db_lib.hrl").

-import(storage, [report/0]).
-import(distributed_hash_table, [new_contact/1]).
-import(transformation, [k_indexer/2]).

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% contacts(Iter)-> Result
%%  Iter = number, iteration index of recursion
%%  Result = 
%%          -
%% INFO:    
%%      simulates domain hash table functionality by insert new random contacts
%%      Iter represents the volume of contacts
%%
contacts(Iter) ->
    Time_stamp = erlang:monotonic_time(millisecond),
    Sys_rec = {system, time_stamp, [Time_stamp]},
    ok=mnesia:dirty_write(Sys_rec),
    
    STATE =
        [
        _Inserted=0,
        _Eliminated_new=0,
        _Eliminated_old=0,
        _ERR=[]
        ],
    rand_contact(Iter,STATE).

%%
%% transformation(K_index, K_rate)-> Result
%%  K_index = number, is the new static value of bucket capacity
%%  K_rate = number, is the new Exponential growth rate of bucket capacity
%%  Result = 
%%          -
%% INFO:    
%%      simulates domain hash table transformation by insert new k_index, k_rate
%%
transformation(K_index, K_rate) ->
    Time_stamp = erlang:monotonic_time(millisecond),
    Sys_rec = {system, time_stamp, [Time_stamp]},
    ok=mnesia:dirty_write(Sys_rec),
    transform(K_index, K_rate).

%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% rand_contact(Iter, STATE)-> Result
%%  Iter = number, iteration index of recursion
%%  STATE = list of simulation factor
%%          [
%%              Inserted = number of inserted new contacts
%%              ,Eliminated_new = number of new contacts have eliminated current contacts
%%              ,Eliminated_old = number of new contacts are eliminated by current contacts
%%              ,ERR = list of error reasons
%%          ]
%%  Result = 
%%          -
%% INFO:    
%%      simulates domain hash table functionality by insert new random contacts
%%      Iter represents the volume of contacts to insert
%%      for simulation purposes, contact information will be fill with fake info
%%
rand_contact(_Iter=0,
        _STATE=[Inserted,Eliminated_new,Eliminated_old, ERR]) ->
    
    Time_stamp1 = erlang:monotonic_time(millisecond),
    [{system, time_stamp, [Time_stamp0]}] = mnesia:dirty_read(system, time_stamp),
    ok = mnesia:dirty_delete(system, time_stamp),
    Time_stamp = Time_stamp1 - Time_stamp0,

    io:format("~p", ["##########"]),
    io:format(" Simulation "),
    io:format("~p~n", ["##########"]),
    io:format("Inserted new contacts:           ~p~n", [Inserted]),
    io:format("New contacts have eliminated:    ~p~n", [Eliminated_new]),
    io:format("New contacts are eliminated:     ~p~n", [Eliminated_old]),
    io:format("Error list:                      ~p~n", [ERR]),
    io:format("Computation duration(milisec):   ~p~n", [Time_stamp]),
    io:format("~p", ["##########"]),
    io:format(" End Of Simulation "),
    io:format(" ~p~n", ["##########"]),
    report();
rand_contact(Iter, 
        _STATE=[Inserted,Eliminated_new,Eliminated_old, ERR]) ->
    Status = rand:uniform(6)-1,%% to cover status range 0-5
    Flag = case rand:uniform(1000) of
        N when N=<10-> [trust];
        N when N>10 andalso N<30 -> [necessary];
        _-> []
    end,
    Contact =
        #contact{
            id = crypto:strong_rand_bytes(32),
            sid = crypto:strong_rand_bytes(4),
            pid = crypto:strong_rand_bytes(8),
            status = Status,
            flags = Flag
        },
    case new_contact(Contact) of    
        {atomic,State} ->
            %io:format("State: ~p    new_contact ~p~n", [State,Contact]),
            New_state =
                case State of
                    {ok, inserted} ->
                        [Inserted+1,Eliminated_new,Eliminated_old, ERR];
                    {ok, eliminated}->%% Candidate eliminated_old an old contact
                        [Inserted,Eliminated_new+1,Eliminated_old,ERR];
                    {error, eliminated}->%% Candidate is eliminated_old
                        [Inserted,Eliminated_new,Eliminated_old+1,ERR];
                    {error, Reason}->
                        [Inserted,Eliminated_new,Eliminated_old+1,ERR++[Reason]]
                end,

            rand_contact(Iter - 1, New_state);
        {aborted, Reason} ->
            io:format("~n Simulation got error on Iter: ~p ERR: ~p~n", [Iter, Reason])
    end.

%%
%% transform(K_index, K_rate)-> Result
%%  K_index = number, is the new static value of bucket capacity
%%  K_rate = number, is the new Exponential growth rate of bucket capacity
%%  Result = 
%%          -
%% INFO:    
%%      print the transformation state
%%
transform(K_index, K_rate)->
    Time_stamp1 = erlang:monotonic_time(millisecond),
    [{system, time_stamp, [Time_stamp0]}] = mnesia:dirty_read(system, time_stamp),
    ok = mnesia:dirty_delete(system, time_stamp),
    Time_stamp = Time_stamp1 - Time_stamp0,

    io:format("~p", ["##########"]),
    io:format(" Transformation "),
    io:format("~p~n", ["##########"]),
    io:format("New K-index:                     ~p~n", [K_index]),
    io:format("New K_rate:                      ~p~n", [K_rate]),
    {ok, [Inserted,Eliminated_new,Eliminated_old,ERR]} = 
        k_indexer(K_index, K_rate),%%transformation:k_indexer/2
    io:format("Inserted new contacts:           ~p~n", [Inserted]),
    io:format("New contacts have eliminated:    ~p~n", [Eliminated_new]),
    io:format("New contacts are eliminated:     ~p~n", [Eliminated_old]),
    io:format("Error list:                      ~p~n", [ERR]),
    io:format("Computation duration(milisec):   ~p~n", [Time_stamp]),
    io:format("~p", ["##########"]),
    io:format(" End Of Transformation "),
    io:format("~p~n", ["##########"]),
    report().

%%lists:foreach(fun(Key)->compile:file(Key) end, [simulation,transformation,storage,domain_hash_table,xor_metric]).