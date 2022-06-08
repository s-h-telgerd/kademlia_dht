-module(storage).

-export([init/0]).
-export([report/0]).
-export([k_indexer/1]).
-export([load_table/1]).
-export([delete_schema/0]).

-include_lib("db_lib.hrl").

%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% init() -> Result
%%  Result =
%%      ok
%%      {error, [Reason]}
%%              Reason=mnesia reason for the error
%% INFO:
%%      create or load mnesia db schema
%%      setup default information of tables and load
%%
init()->
    stopped=mnesia:stop(),
    case mnesia:create_schema([node()]) of
        ok ->
            ok=mnesia:start(),
            io:format("system_storage| started!~n"),
            ok=table_set_up(system),
            ok=table_set_up(domain),
            ok=table_set_up(in_domain),
            ok=table_set_up(contact),
            io:format("system_storage| created!~n"),
            ok;
        {error,{_NODE,{already_exists,_}}}->
            ok=mnesia:start(),
            ok=table_set_up(system),
            ok=table_set_up(domain),
            ok=table_set_up(in_domain),
            ok=table_set_up(contact),
            io:format("system_storage| loaded!~n"),
            ok;
        {error,Reason}->
            io:format("system_storage| error: ~p~n", [Reason]),
            {error, [Reason]}
    end.
%%
%% report() -> Result
%%  Tab = atom, table name to load
%%  Result =
%%      -
%% INFO:
%%      print information of the DHT state
%%
report()->
    Fun = fun() -> report([]) end,
    mnesia:sync_transaction(Fun).
%%
%% k_indexer(Domain)-> Result
%%  Domain = list of number, index of proposing domain name to compute
%%  Result =
%%      K_index = number, the coputed capacity of the bucket on the Domain
%% INFO:
%%      compute the bucket capacity of the Domain
%%      k_index number, is the static value of bucket capacity
%%          default: k_index=8
%%      k_rate, number, is the Exponential growth rate of bucket capacity
%%          default: k_rate=2
%%      if domain is root=[], k_index is the result
%%      else if last member of domain is 0, k_index is the result
%%      else, last member of domain is 1, K_index * ( ( K_rate - length(Domain) +1 ) / K_rate ) is the result
%%
k_indexer(_Domain=[])->
        [{system, dht, DHT}] = mnesia:read(system, dht),
        {k_index, K_index} = lists:keyfind(k_index, 1, DHT),
        K_index;
k_indexer(Domain)->
    [{system, dht, DHT}] = mnesia:read(system, dht),
    {k_index, K_index} = lists:keyfind(k_index, 1, DHT),
    case lists:last(Domain) of
        0 ->
            K_index;
        1 ->
            {k_rate, K_rate} = lists:keyfind(k_rate, 1, DHT),
            case length(Domain) < K_rate of
                true ->
                    K_index * ( ( K_rate - length(Domain) +1 ) / K_rate );
                false -> 
                    K_index
            end
    end.
%%
%% load_table(Tab) -> Result
%%  Tab = atom, table name to load
%%  Result =
%%      Result of table_set_up(Table)
%% INFO:
%%
load_table(Tab) ->
    table_set_up(Tab).
%%
delete_schema() ->
    stopped=mnesia:stop(),
    Status = mnesia:delete_schema([node()]),
    io:format("delete_schema with Status: ~p~n", [Status]).
%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% table_set_up(Table) -> Result
%%  Result = 
%%      ok
%%      {error, unknown_table}
%% INFO:
%%      load or create the table Table
%%      setup default information in case of creation 
%%
table_set_up(system)->
    case mnesia:force_load_table(system) of
        yes ->
            ok;
        {error,{no_exists,system}}->
            mnesia:create_table(system,
                                [{attributes, record_info(fields, system)},
                                 {type, set},{disc_copies, [node()]}]),
            %% node id has to be 32 Octets
            Node_id = crypto:strong_rand_bytes(32),
            io:format("Node_id: ~p", [Node_id]),
            Node =
                #system{key = node,
                        val = [{id ,Node_id}]},
            DHT =
                #system{key = dht,
                        val = [{k_index ,4}, {k_rate ,2}]},                     
            TX = fun() ->
                    mnesia:write(Node),
                    mnesia:write(DHT),
                    ok
                end,
            {atomic, ok} = mnesia:sync_transaction(TX),
            yes=mnesia:force_load_table(system),
            ok
    end;
table_set_up(domain)->
    case mnesia:force_load_table(domain) of
        yes ->
            ok;
        {error,{no_exists,domain}}->
            mnesia:create_table(domain,
                            [{attributes, record_info(fields, domain)},
                                {type, set}, {disc_copies, [node()]}]),
            Domain_main = #domain{index = [], status = bucket},
            TX = fun() ->
                    mnesia:write(Domain_main),
                    ok
                end,
            {atomic, ok} = mnesia:sync_transaction(TX),
            yes=mnesia:force_load_table(domain),
            ok
    end;
table_set_up(in_domain)->
    case mnesia:force_load_table(in_domain) of
        yes ->
            ok;
        {error,{no_exists,in_domain}}->
            mnesia:create_table(in_domain,
                            [{attributes, record_info(fields, in_domain)},
                                {type, bag}, {disc_copies, [node()]}]),
            yes=mnesia:force_load_table(in_domain),
            ok
    end;
table_set_up(contact)->
    case mnesia:force_load_table(contact) of
        yes ->
            ok;
        {error,{no_exists,contact}}->
            mnesia:create_table(contact,
                            [{attributes, record_info(fields, contact)},
                                {type, set}, {disc_copies, [node()]}]),
            yes=mnesia:force_load_table(contact),
            ok
    end;
table_set_up(_Table)->
    {error, unknown_table}.
%%
%% report(Domain) -> Result
%%  Domain = list of number, index of proposing domain name to report
%%  Result =
%%      -
%% INFO:
%%      print information of the DHT state on the Domain
%%      in case of Domain status is split, recursion event, report subdomains 0 and 1
%%      in case of Domain status is bucket, report the bucket information
%%      in case of Domain status is bucket and domain is [1], summerize the report
%%
report(_Domain=[])->
    io:format("~p ", ["##########"]),
    io:format(" Hash Table Report   "),
    io:format("~p~n", ["##########"]),

    [{domain, _, Status}]=mnesia:read(domain, []),
    io:format("~p    Status: ~p~n", [[], Status]),
    case Status of
        bucket->
            report_summerize();
        split->
            report([0]),report([1]),
            Keys = mnesia:all_keys(in_domain),
            Fun = fun (Key, Length)->
                    Length + length(mnesia:read(in_domain, Key))
                end,
            Length = lists:foldl(Fun, 0, Keys),
            Contacts = length(mnesia:all_keys(contact)),
            io:format(" All contacts:       ~p~n", [Contacts]),
            io:format(" Bucket contacts:    ~p~n", [Length]),
            io:format("~p ", ["##########"]),
            io:format("~p ", ['End Of Report']),
            io:format("~p~n", ["##########"])
            
    end;
report(Domain)->
    [{domain, _, Status}]=mnesia:read(domain, Domain),
    Depth = length(Domain),
    Space = lists:flatten(lists:duplicate(Depth, "-")),
    Buckets = mnesia:read(in_domain, Domain),
    io:format("~p ~p    Status: ~p  Bucket_volume: ~p Depth: ~p~n", 
            [Space, Domain, Status, length(Buckets), Depth]),
    case Status of
        bucket when Domain == [1] ->
            report_summerize();
        bucket ->
            ok;
        split->
            report(Domain++[0]),report(Domain++[1])
    end.
%%
%% report_summerize() -> Result
%%  Result =
%%      -
%% INFO:
%%      print summerize the report of the DHT state
%%      Buckets = list of bucket key on in_domain table
%%      Tr = number of trusted contacts
%%      Nes = number of necessary contacts
%%      Nor = number of normal contacts
%%
report_summerize() ->
    Contacts_fun =
        fun ({in_domain, _Domain ,ContactID}, {Tr,Nes,Nor}) ->
            case mnesia:read(contact, ContactID) of
                [Contact] when Contact#contact.flags == [trust]->
                    {
                        Tr +1,
                        Nes,
                        Nor 
                    };
                [Contact] when Contact#contact.flags == [necessary]->
                    {
                        Tr,
                        Nes +1,
                        Nor
                    };
                [Contact] when Contact#contact.flags == []->
                    {
                        Tr,
                        Nes,
                        Nor +1
                    }
                end
        end,
    Buckets_keys = mnesia:all_keys(in_domain),
    Buckets_fun =
        fun (Key, {Tr,Nes,Nor}) ->
            Bucket = mnesia:read(in_domain, Key),
            {Tr_,Nes_,Nor_} = lists:foldl(Contacts_fun, {0,0,0}, Bucket),
            {
                Tr+Tr_,
                Nes+Nes_,
                Nor+Nor_
            }
        end,
    {Tr,Nes,Nor} = lists:foldl(Buckets_fun, {0,0,0}, Buckets_keys),
    io:format("~p", ["##########"]),
    io:format(" Summerize "),
    io:format("~p~n", ["##########"]),
    io:format(" Buckets:            ~p~n", [length(Buckets_keys)]),
    io:format(" K_index:            ~p~n", [k_indexer(_domain=[1])]),
    io:format(" Trusted contacts:   ~p~n", [Tr]),
    io:format(" Necessary contacts: ~p~n", [Nes]),
    io:format(" Normal contacts:    ~p~n", [Nor]).