-module(transformation).

-export([k_indexer/2]).

-include_lib("db_lib.hrl").

-import(storage, [report/0, load_table/1]).
-import(distributed_hash_table, [new_contact/1]).




%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% k_indexer(K_index, K_rate)-> Result
%%  K_index = number, is the static value of bucket capacity
%%  K_rate = number, is the Exponential growth rate of bucket capacity
%%  Result =
%%           Result of dht_rebuild(K_index, K_rate)
%% INFO:    
%%      beginning the transformation DHT to new k_index and k_rate
%%      backup the information of database
%%      delete current tables
%%      rebuild the tables of database
%%      cleanup the backup information
%%
k_indexer(K_index, K_rate) ->
    ok=backup(transformation),
    io:format("Transformation backup:           ~p~n", [ok]),
    ok=tabel_rebuild(transformation),
    io:format("Transformation tabel rebuild:    ~p~n", [ok]),
    {ok, Result}=dht_rebuild(K_index, K_rate),
    io:format("Transformation DHT rebuild:      ~p~n", [ok]),
    ok=dht_cleanup(),
    io:format("Transformation clean_up:         ~p~n", [ok]),
    {ok, Result}.


%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% backup(transformation)-> Result
%%  Result =
%%           ok
%% INFO:    
%%      backup the DHT tables
%%
backup(transformation) ->
    ok=backup(table, {domain, domain_backup}),
    ok=backup(table, {in_domain, in_domain_backup}),
    ok=backup(table, {contact, contact_backup}),
    ok.
%%
%% backup(table, {Tab, Tab_backup})-> Result
%%  Tab = atom, table name to backup
%%  Tab_backup = atom, name of backup table
%%  Result =
%%           Result of backup(information, {Tab, Tab_backup})
%% INFO:    
%%      ensure backup table is fresh
%%      backup the table information
%%
backup(table, {domain, domain_backup}) ->
    case mnesia:force_load_table(domain_backup) of
        yes ->
            %%  there are last backup information
            mnesia:delete_table(domain_backup),
            backup(table, {domain, domain_backup});
        {error,{no_exists,domain_backup}}->
            mnesia:create_table(domain_backup,
                        [{attributes, record_info(fields, domain_backup)},
                            {type, set}, {ram_copies, [node()]}]),
            backup(information, {domain, domain_backup})
    end;
backup(table, {in_domain, in_domain_backup}) ->
    case mnesia:force_load_table(in_domain_backup) of
        yes ->
            %%  there are last backup information
            mnesia:delete_table(in_domain_backup),
            backup(table, {in_domain, in_domain_backup});
        {error,{no_exists,in_domain_backup}}->
            mnesia:create_table(in_domain_backup,
                        [{attributes, record_info(fields, in_domain_backup)},
                            {type, bag}, {ram_copies, [node()]}]),
            backup(information, {in_domain, in_domain_backup})
    end;
backup(table, {contact, contact_backup}) ->
    case mnesia:force_load_table(contact_backup) of
        yes ->
            %%  there are last backup information
            mnesia:delete_table(contact_backup),
            backup(table, {contact, contact_backup});
        {error,{no_exists,contact_backup}}->
            mnesia:create_table(contact_backup,
                        [{attributes, record_info(fields, contact_backup)},
                            {type, set}, {ram_copies, [node()]}]),
            backup(information, {contact, contact_backup})
    end;
%%
%% backup(information, {Tab, Tab_backup})-> Result
%%  Tab = atom, table name to backup
%%  Tab_backup = atom, name of backup table
%%  Result =
%%           ok
%%           {error, [Reason]}
%%               Reason = mnesia error reason of abortion
%% INFO:    
%%      read all keys on table Tab
%%      convert Tab keys to Tab_backup keys
%%      update Tab_backup
%%
backup(information, {Tab, Tab_backup}) ->
    Fun = 
        fun() ->
            Tab_keys = mnesia:all_keys(Tab),
            STATE = lists:foldl (
                fun (Key, ok) ->
                    Result = mnesia:read(Tab, Key),
                    Records = convert_record(Result),
                    
                    lists:foreach(
                        fun (Record) ->
                            %Is_rec = is_record(Record, Tab_backup),
                            %io:format("Record~p~n", [Is_rec]),
                            mnesia:write(Tab_backup, Record, write)
                        end,
                        Records),
                    ok
                end,
                ok,
                Tab_keys),
            STATE
        end,
    case mnesia:sync_transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            {error, [Reason]}
    end.
%%
%% convert_record(Rec)-> Result
%%  Rec = list of records to convert
%%  Result =
%%           list of converted records
%% INFO:    
%%      convert Tab keys to Tab_backup keys
%%
convert_record(_Rec=[]) ->
    [];
convert_record(_Rec=[{domain , Index, Status}]) ->
    %%  type set
    Domain = {domain_backup, Index, Status},
    [Domain];
convert_record(Rec=[{in_domain, _,_}|_Rest]) ->
    %%  type bag
    Fun = fun ({in_domain, Domain, Contact}, Records) ->
            InDomain = {in_domain_backup, Domain, Contact},
            Records ++ [InDomain]
        end,
    lists:foldl(Fun, [], Rec);
convert_record(_Rec=[{contact, ID, SID, PID, Status, Flags}]) ->
    %%  type set
    Contact = {contact_backup, ID, SID, PID, Status, Flags},
    [Contact].

%%
%% tabel_rebuild(transformation)-> Result
%%  Result =
%%           ok
%% INFO:    
%%      delete and create DHT tables
%%
tabel_rebuild(transformation) ->
    ok=tabel_rebuild(domain),
    ok=tabel_rebuild(in_domain),
    ok=tabel_rebuild(contact),
    ok;
%%
%% tabel_rebuild(Tab)-> Result
%%  Tab = atom, table name to rebuild
%%  Result =
%%      ok
%% INFO:    
%%      delete and create DHT table
%%
tabel_rebuild(Tab) ->
    {_,_} = mnesia:delete_table(Tab),
    load_table(Tab).
%%
%% dht_rebuild(K_index, K_rate)-> Result
%%  K_index = number, is the static value of bucket capacity
%%  K_rate = number, is the Exponential growth rate of bucket capacity
%%  Result =
%%           {ok, STATE}
%%              STATE = [
%%                  Inserted = number of inserted new contacts
%%                  ,Eliminated_new = number of new contacts have eliminated current contacts
%%                  ,Eliminated_old = number of new contacts are eliminated by current contacts
%%                  ,ERR = list of error reasons
%%               ]
%%           {error, [Reason]}
%%               Reason = mnesia error reason of abortion
%% INFO:
%%      update the dht on system table with new k_index and k_rate
%%      insert backup contacts to new DHT
%%
dht_rebuild(K_index, K_rate)->
    Fun = fun () ->
        %%  update dht on system table
        [{system, dht, DHT}] = mnesia:read(system, dht),
        DHT_kIndex = lists:keyreplace(k_index, 1, DHT, {k_index ,K_index}),
        DHT_kRate = lists:keyreplace(k_rate, 1, DHT_kIndex, {k_rate ,K_rate}),
        DHT_new = {system, dht, DHT_kRate},
        mnesia:write(DHT_new),
        %%  insert backup contacts to new DHT
        Contact_list = mnesia:all_keys(contact_backup),
        STATE = lists:foldl (
            fun (Contact_ID, [Inserted,Eliminates,Eliminated, ERR]) ->
                case mnesia:read(contact_backup, Contact_ID) of
                    %[{contact_backup, Contact_ID, SID, PID, Status}] ->
                    [Contact_backup] ->
                        Contact =
                            {
                                contact,
                                Contact_backup#contact_backup.id,
                                Contact_backup#contact_backup.sid,
                                Contact_backup#contact_backup.pid,
                                Contact_backup#contact_backup.status,
                                Contact_backup#contact_backup.flags
                            },
                        case new_contact(Contact) of
                            {atomic, {ok, inserted}} ->
                                [Inserted+1,Eliminates,Eliminated, ERR];
                            {atomic, {ok, eliminated}}->
                                [Inserted,Eliminates+1,Eliminated,ERR];
                            {atomic, {error, eliminated}}->
                                [Inserted,Eliminates,Eliminated+1,ERR];
                            {atomic, {error, Reason}}->
                                [Inserted,Eliminates,Eliminated+1,ERR++[{contact,Reason}]];
                            {aborted, Reason} ->
                                [Inserted,Eliminates,Eliminated+1,ERR++[{db,Reason}]]
                        end
                end
            end,
            [0,0,0, []],
            Contact_list),
        {ok, STATE}
    end,
    case mnesia:sync_transaction(Fun) of
        {atomic, {ok, STATE}} -> {ok, STATE};
        {aborted, Reason} -> {error, [Reason]}
    end.

%%
%% dht_cleanup()-> Result
%%  Result = 
%%          ok
%% INFO:    
%%      delete backup tables
%%
dht_cleanup() ->
    mnesia:delete_table(domain_backup),
    mnesia:delete_table(in_domain_backup),
    mnesia:delete_table(contact_backup),
    ok.





