%%
%% record(system, {key, val})
%%  key = atom, current keys: node, dht
%%  val = list of term
%%  Default =
%%    [
%%      {
%%          node,
%%          [ { id, Node_id } ]
%%      },
%%      {
%%          dht,
%%          [
%%              {k_index, 4},
%%              {k_rate, 2}
%%          ]
%%      }
%%    ]
%% INFO:
%%      Node_id = binary 32 Octets, system pulic key
%%      Node_id is random for simulation purposes
%%      k_index = number, is the static value of bucket capacity
%%      k_rate = number, is the Exponential growth rate of bucket capacity
%%      Capacity = K_index * ( ( K_rate - length(Domain) +1 ) / K_rate )
%%
-record(system, {key, val}).
%%
%% record(domain, {index, status})
%%  index = list of number, index of domain
%%  status = atom, available: bucket, split
%% INFO:
%%      record the status of a domain index
%%
-record(domain, {index, status}).
%%
%% record(in_domain, {index, contact})
%%  index = list of number, index of domain
%%  contact = binary 32 Octets, ID of contact
%% INFO:
%%      in_domain record represents bucket of the domain
%%      record bucket contact
%%
-record(in_domain, {domain, contact}).
%%
%% record(contact, {id, sid, pid, status, flags})
%%  id = binary 32 Octets, ID of contact
%%  sid = binary 4 Octets, ID of session handles the connection
%%  pid = OS32:4 OS64:8 Octets, the allocated pid of the session
%%  status = number, available: 0=excellent, 1=very strong, 2=strong, 3=normal, 4=weak, 5=very weak
%%  flags = list of atom, available: [trust], [necessary], []
%% INFO:
%%      id represents public key of contact
%%      status has been set by corresponding session handler
%%      flags [trust] represents trusted node type
%%      flags [necessary] represents normal necessary type
%%      flags [] represents normal node type
%%
-record(contact, 
    {
        id,
        sid,
        pid,
        status,
        flags
    }).


%%
%% record(*_backup, {...})
%%  *_backup = atom, available: domain_backup, in_domain_backup, contact_backup
%%  {...} = corresponding table key and value/s
%% INFO:
%%      backup DHT tables 
%%
-record(domain_backup, {index, status}).
-record(in_domain_backup, {domain, contact}).
-record(contact_backup, {id, sid, pid, status, flags}).