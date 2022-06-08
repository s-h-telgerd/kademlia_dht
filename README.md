# Overview
It is a

# Kademlia DHT
### A Peer-to-peer Information System based on the XOR Metric.

Kademlia is a distributed hash table for decentralized peer-to-peer computer networks designed by Petar Maymounkov and David Mazières in 2002. It specifies the structure of the network and the exchange of information through node lookups. Kademlia nodes communicate among themselves using UDP. A virtual or overlay network is formed by the participant nodes. 
Each node is identified by a number or node ID. The node ID serves not only as identification, but the Kademlia algorithm uses the node ID to locate values (usually file hashes or keywords). In fact, the node ID provides a direct map to file hashes and that node stores information on where to obtain the file or resource.
[more information][ref1]

- ![alt text][ref2]

# Protocol Optimisation
### K Index
- To cover more nodes on a bucket with logical distance 1, k_rate, Exponential growth rate, leverages the k_index based on domain lenght.
```sh
if domain is root=[], k_index is the result
else if last member of domain is 0, k_index is the result
else last member of domain is 1:
    K_index * ( ( K_rate - length(Domain) +1 ) / K_rate )
```
### Flags
- To provide stable services for specific contacts
- Available flags
1. [ trust ] , flag of trusted conracts
2. [ necessary ] , flag of necessary conracts
- Flag list [ ] represents normal conracts

# Build
1. Build-in release - visit ' Install and run '

2. Build manually
- to build manually ' rebar3 ' is required
```sh
    mkdir kademlia_dht
    git clone https://github.com/s-h-telgerd/kademlia_dht.git
    cd kademlia_dht
    rebar3 as prod tar
```

# Install and run

- To deploy kademlia_dht release
```sh
    mkdir kademlia_dht
    git clone https://github.com/s-h-telgerd/kademlia_dht.git
    cd kademlia_dht
    cd releases
    mkdir kademlia_dht-0.1.0
    tar -zxvf kademlia_dht-0.1.0.tar.gz  -C ./kademlia_dht-0.1.0
```
- To run kademlia_dht release
```sh
    cd kademlia_dht-0.1.0
    ./bin/kademlia_dht console
```

# Simulation
- RPC = { simulation, Counter }
```sh
assist ! {simulation,1000}.
########## Simulation ##########
Inserted new contacts:           62
New contacts have eliminated:    3
New contacts are eliminated:     935
Error list:                      []
Computation duration(milisec):   231
########## End Of Simulation  ##########
```
- RPC = { transformation, K_index, K_rate }
```sh
assist ! {transformation, 2, 1.5}.
########## Transformation ##########
New K-index:                     2
New K_rate:                      1.5
Transformation backup:           ok
Transformation tabel rebuild:    ok
Transformation DHT rebuild:      ok
Transformation clean_up:         ok
Inserted new contacts:           90
New contacts have eliminated:    5
New contacts are eliminated:     12
Error list:                      []
Computation duration(milisec):   0
########## End Of Transformation ##########
```
- RPC = dht_report
```sh
assist ! dht_report.
########## Hash Table Report ##########
[]    Status: split
"-" [0]    Status: split  Bucket_volume: 0 Depth: 1
"--" [0,0]    Status: split  Bucket_volume: 0 Depth: 2
"---" [0,0,0]    Status: split  Bucket_volume: 0 Depth: 3
"----" [0,0,0,0]    Status: split  Bucket_volume: 0 Depth: 4
"-----" [0,0,0,0,0]    Status: split  Bucket_volume: 0 Depth: 5
"------" [0,0,0,0,0,0]    Status: split  Bucket_volume: 0 Depth: 6
"-------" [0,0,0,0,0,0,0]    Status: split  Bucket_volume: 0 Depth: 7
"--------" [0,0,0,0,0,0,0,0]    Status: split  Bucket_volume: 0 Depth: 8
"---------" [0,0,0,0,0,0,0,0,0]    Status: bucket  Bucket_volume: 1 Depth: 9
"---------" [0,0,0,0,0,0,0,0,1]    Status: bucket  Bucket_volume: 2 Depth: 9
"--------" [0,0,0,0,0,0,0,1]    Status: bucket  Bucket_volume: 2 Depth: 8
"-------" [0,0,0,0,0,0,1]    Status: bucket  Bucket_volume: 2 Depth: 7
"------" [0,0,0,0,0,1]    Status: bucket  Bucket_volume: 4 Depth: 6
"-----" [0,0,0,0,1]    Status: bucket  Bucket_volume: 5 Depth: 5
"----" [0,0,0,1]    Status: bucket  Bucket_volume: 9 Depth: 4
"---" [0,0,1]    Status: bucket  Bucket_volume: 11 Depth: 3
"--" [0,1]    Status: bucket  Bucket_volume: 23 Depth: 2
"-" [1]    Status: bucket  Bucket_volume: 31 Depth: 1
########## Summerize ##########
 Buckets:            10
 K_index:            2.0
 Trusted contacts:   20
 Necessary contacts: 51
 Normal contacts:    19
 All contacts:       90
 Bucket contacts:    90
########## 'End Of Report' ##########
```

- RPC = help
```sh
assist ! help.
##################################
##################################
Kademlia
Distributed Hash Table
Simulator version 0.1.0
Send RPC to assist genserver
     assist ! RPC 
Available RPCs : 
{simulation, Counter} 
{transformation, K_index, K_rate} 
dht_report 
help 
##################################
##################################

```

[ref1]: <https://github.com/s-h-telgerd/kademlia_dht/blob/main/doc/maymounkov-kademlia-lncs.pdf>

[ref2]: <https://github.com/s-h-telgerd/kademlia_dht/blob/main/doc/An-example-of-a-Kademlia-topology.png>