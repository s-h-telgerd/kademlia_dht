-module(distributed_hash_table).

-export([new_contact/1,new_contact/2]).

-include_lib("db_lib.hrl").

-import(storage, [report/0, k_indexer/1]).
-import(xor_metric, [distance/3]).
%%%%%%%%%%%%
%%   API  %%
%%%%%%%%%%%%

%%
%% new_contact(Contact)-> Result
%%  Contact = record informatin of new contact
%%  Result =
%%      {atomic,State}
%%          State =
%%              {ok, inserted} new contact is inserted
%%              {ok, eliminated} new contact eliminated an old one
%%              {error, eliminated} new contact is eliminated
%%              {error, Reason}
%%                  Reason =
%%                      already_exist
%%                      domain, Domain
%%                      insert, bad_flags
%%                      
%%      {aborted, Reason}
%%          Reason = mneisa error reason of abortion
%% INFO:
%%      mechanism of developing the DHT by new contact
%%      check if Contact is already exist
%%      find an available domain to insert new contacts
%%      or find an available domain to judge of insertion
%%
new_contact(Contact)->
    Fun = 
        fun() ->
            case new_contact(check, Contact#contact.id) of
                {ok, new} ->
                    new_contact(insert, Contact);
                {error, Reason} ->
                    {error, [contact]++Reason}
            end
          end,
    mnesia:sync_transaction(Fun).
%%%%%%%%%%%%
%%   BIF  %%
%%%%%%%%%%%%

%%
%% new_contact(check, Contact)-> Result
%%  Contact = record informatin of new contact
%%  Result =
%%      {ok, new}
%%      {error, Reason}
%%          Reason =
%%              already_exist
%% INFO:
%%      check if Contact is already exist
%%
new_contact(check, Contact)->
    case mnesia:read(contact, Contact) of
        [] -> {ok, new};
        [_] -> {error, [already_exist]}
    end;
%%
%% new_contact(insert, Contact)-> Result
%%  Contact = record informatin of new contact
%%  Result =
%%           Result of domain(insert, Domain, Candidate)
%% INFO:    
%%      beginning the insertion
%%
new_contact(insert, Contact)->
    domain(insert, [], Contact).
%%
%% domain(insert, Domain, Candidate)-> Result
%%  Domain = list of number, index of proposing domain name to insert
%%  Candidate = record informatin of new contact
%%  Result = 
%%           Result of domain(split, Domain, {Bucket, Candidate})
%%           Result of bucket(insert_candidate, [Domain, Candidate])
%%           Result of bucket(judgement, [Domain, Bucket, Candidate])
%%           {error, Reason}
%%              Reason =
%%                domain, Domain
%%                insert, bad_flags
%% INFO:    
%%      find a free domain to insert contact
%%      find a full domain to judge the situation
%%
domain(insert, Domain, Candidate) ->
    [{system, node, [{id ,Node_id}]}] = mnesia:read(system, node),
    case mnesia:read(domain, Domain) of
        [] ->
            {error, [domain, Domain]};
        [{domain, Domain, split}] ->
            Subdomain =
                case distance(Node_id, Candidate#contact.id, length(Domain) + 1) of
                    false ->
                        Domain ++ [0];
                    true ->
                        Domain ++ [1]
                end,
            domain(insert, Subdomain, Candidate);

        [{domain, Domain, bucket}] ->
            Bucket = mnesia:read(in_domain, Domain),
            K_index = k_indexer(Domain),
            case length(Bucket) < K_index of
                true ->
                    bucket(insert_candidate, [Domain, Candidate]);
                false when Domain == []->
                    domain(split, Domain, {Bucket, Candidate});
                false->
                    case lists:last(Domain) of
                        0 ->
                            domain(split, Domain, {Bucket, Candidate});
                        1 ->
                            case Candidate#contact.flags of
                                []->
                                    bucket(judgement, [Domain, Bucket, Candidate]);
                                Flags ->
                                    case lists:member(trust, Flags) 
                                            or lists:member(necessary, Flags) of

                                        true ->
                                            bucket(insert_candidate, [Domain, Candidate]);
                                        _ ->
                                            %% in future if there are another flags
                                            {error, [insert, bad_flags]}
                                    end
                            end
                    end
            end
    end;
%%
%% domain(split, Domain, {Contacts, Candidate})-> Result
%%  Domain = list of number, index of proposing domain name to insert
%%  Contacts = list of current contacts in the Domain
%%  Candidate = record informatin of new contact
%%  Result = 
%%          {ok, inserted} new contact is inserted
%%          Result of bucket(judgement,[Domain,Contacts, Candidate])
%% INFO:    
%%      split a domain to two Subdomains={Subdomain0, Subdomain1}
%%      -Subdomains are not free, new contact is inserted
%%      -Subdomain0 is free, split is aborted, new contact goes to judge
%%      -Subdomain1 is free, new split for Subdomain0, Subdomain1 is empty bucket
%%       
domain(split, Domain, {Contacts, Candidate}) ->
    {Subdomain0, Subdomain1} = domain(distance, Domain, {Contacts, Candidate}),
    case length(Subdomain0) > 0 of
        true when length(Subdomain1) > 0 ->
            DomainObj = #domain{index = Domain, status = split},
            mnesia:write(domain, DomainObj, write),
            mnesia:write(contact, Candidate, write),
            mnesia:delete(in_domain, Domain, write),
            domain(subdomain, Domain ++ [0], Subdomain0),
            domain(subdomain, Domain ++ [1], Subdomain1),
            {ok, inserted};
        true ->
            Domain_split = #domain{index = Domain, status = split},
            mnesia:write(domain, Domain_split, write),
            mnesia:delete(in_domain, Domain, write),
            %%  create new empty right subdomain bucket
            Domain1 = #domain{index = Domain ++ [1], status = bucket},
            mnesia:write(domain, Domain1, write),
            %%  create new left subdomain, load contacts, try again
            Subdomain =  lists:sublist(Subdomain0, 1, length(Subdomain0)-1),
            domain(subdomain, Domain ++ [0], Subdomain),
            domain(split, Domain ++ [0], {Contacts, Candidate});
        false ->
            bucket(judgement,[Domain,Contacts, Candidate])
    end;
%%
%% domain(distance, Domain, {Contacts, Candidate})-> Result
%%  Domain = list of number, index of proposing domain name to compute distance
%%  Contacts = list of current contacts in the Domain
%%  Candidate = record informatin of new contact
%%  Result = 
%%          {Subdomain0, Subdomain1}
%% INFO:    
%%      distribute contacts and candidate among two Subdomains={Subdomain0, Subdomain1}
%%
domain(distance, Domain, {Contacts, Candidate}) ->
    [{system, node, Node}] = mnesia:read(system, node),
   {id ,NodeID} = lists:keyfind(id, 1, Node),
    FUN = fun ({in_domain, _Domain, ContactID}, {Sub0, Sub1}) ->
                  XOR = distance(NodeID, ContactID, length(Domain) + 1),
                  case XOR of
                      false ->
                          {Sub0 ++ [ContactID], Sub1};
                      true ->
                          {Sub0, Sub1 ++ [ContactID]}
                  end;
              ({candidate, CandidateID}, {Sub0, Sub1}) ->
                  XOR = distance(NodeID, CandidateID, length(Domain) + 1),
                  case XOR of
                      false ->
                          {Sub0 ++ [CandidateID], Sub1};
                      true ->
                          {Sub0, Sub1 ++ [CandidateID]}
                  end
          end,
    Contact_list = Contacts ++ [{candidate, Candidate#contact.id}],
    lists:foldl(FUN, {[], []}, Contact_list);
%%
%% domain(subdomain, Subdomain, Contact_list)-> Result
%%  Subdomain = list of number, index of proposing domain name to write contacts
%%  Contact_list = list of contact ID to write on in_domain table
%%  Result = 
%%          -
%% INFO:    
%%      write in_domain record={in_domain, Subdomain, ContactID} for the Subdomain
%%      in_domain table represents buckets of contacts
%%
domain(subdomain, Subdomain, Contact_list) ->
    Domain = #domain{index = Subdomain, status = bucket},
    mnesia:write(Domain),
    FUN = fun(ContactID) ->
             In_domain = {in_domain, Subdomain, ContactID},
             mnesia:write(In_domain)
          end,
    lists:foreach(FUN, Contact_list).
%%
%% bucket(judgement, [Domain, Bucket, Candidate])-> Result
%%  Domain = list of number, index of proposing domain name to judge
%%  Bucket = list of in_domain record of contact in the Domain
%%  Candidate = record informatin of new contact
%%  Result = 
%%          Result of bucket(insert_candidate, [Domain, Candidate])
%%          Result of bucket(elimination, [Domain, List, Candidate])
%% INFO:    
%%      read the flags of contacts to collect normal contacts to judge
%%      if normal contact list is lesser than k_index, new contact go to insert
%%      else an elimination event will be raised
%%
bucket(judgement, [Domain, Bucket, Candidate])->
    Contact_list_fun =
        fun (In_domain = {in_domain, _Domain ,ContactID}, ContactList) ->
            case mnesia:read(contact, ContactID) of
                [Contact] when Contact#contact.flags == []->
                    ContactList ++ [Contact];
                [_Contact]->
                    ContactList;
                _->
                    mnesia:delete_object(in_domain, In_domain, write),
                    ContactList
                end
        end,
    NormalContactList =
        lists:foldl(
            Contact_list_fun,
            _ContactList=[],
            Bucket),
    K_index = k_indexer(Domain),
    case NormalContactList of
        List when length(List) < K_index ->
            bucket(insert_candidate, [Domain, Candidate]);
        List ->
            bucket(elimination, [Domain, List, Candidate])
    end;
%%
%% bucket(elimination, [Domain, ContactList, Candidate])-> Result
%%  Domain = list of number, index of proposing domain name to eliminate
%%  ContactList = list of normal contact ID in the Domain
%%  Candidate = record informatin of new contact
%%  Result = 
%%          {ok, eliminated}, new contact won the competition and has eliminated an old contact
%%          {error, eliminated}, new contact failed in the competition and is eliminated
%% INFO:    
%%      sort contacts based on contact's status
%%      if the status of last contact is lesser than candidate's status, candidate eliminates the contact
%%      else candidate is eliminated
%%
bucket(elimination, [Domain, ContactList, Candidate])->
    Sorted = bucket(sort_status, ContactList),
        
    case lists:last(Sorted) of
        Contact when Candidate#contact.status > Contact#contact.status ->
            ID = Contact#contact.id,
            PID = Contact#contact.pid,
            % delete contact can be called by its corresponding session
            mnesia:delete(contact, ID, write),
            InDomain = {in_domain, Domain, ID},
            mnesia:delete_object(in_domain, InDomain, write),
            case is_pid(PID) andalso is_process_alive(PID) of
                true ->
                    PID ! {distributed_hash_table, 'EOS'}; %end_of_service
                false ->
                    ignore
            end,
            {ok, inserted} = bucket(insert_candidate, [Domain, Candidate]),
            {ok, eliminated};
        _->
            {error, eliminated}
    end;
%%
%% bucket(insert_candidate, [Domain, Candidate])-> Result
%%  Domain = list of number, index of proposing domain name to insert
%%  Candidate = record informatin of new contact
%%  Result = 
%%          {ok, inserted}
%% INFO:    
%%      insert a contact to bucket of the Domain
%%      write the contact information on table contact
%%      write the contact information on table in_domain
%%
bucket(insert_candidate, [Domain, Candidate])->
    In_domain = {in_domain, Domain, Candidate#contact.id},
    ok=mnesia:write(Candidate),
    ok=mnesia:write(In_domain),
    {ok, inserted};
%%
%% bucket(sort_status, Contacts)-> Result
%%  Contacts = list of record information of contacts
%%  Result = 
%%          Sorted_list
%% INFO:    
%%      sort contacts based on their status
%%
bucket(sort_status, Contacts) ->
    Fun = fun (ContactA, ContactB) -> ContactA#contact.status < ContactB#contact.status
          end,
    lists:sort(Fun, Contacts).