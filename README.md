# Overview
It is a

# Kademlia DHT
### A Peer-to-peer Information System based on the XOR Metric.

Kademlia is a distributed hash table for decentralized peer-to-peer computer networks designed by Petar Maymounkov and David Mazières in 2002. It specifies the structure of the network and the exchange of information through node lookups. Kademlia nodes communicate among themselves using UDP. A virtual or overlay network is formed by the participant nodes. Each node is identified by a number or node ID. The node ID serves not only as identification, but the Kademlia algorithm uses the node ID to locate values (usually file hashes or keywords). In fact, the node ID provides a direct map to file hashes and that node stores information on where to obtain the file or resource.
[more information][ref1]

# Protocol Optimisation
### K Index
- To cover more nodes on a bucket with logical distance 1, k_index leverages with Exponential growth rate, k_rate based on domain lenght.
```sh
if domain is root=[], k_index is the result
else if last member of domain is 0, k_index is the result
else last member of domain is 1:
    K_index * ( ( K_rate - length(Domain) +1 ) / K_rate )
```

# Build
1. Build-in release - visit ' Deploy and run '

2. Build manually
- to build manually ' rebar3 ' is required
```sh
    cd directory/to/kademlia_dht/
    rebar3 as prod tar
```

# Deploy and run

- To deploy kademlia_dht release
```sh
    mkdir kademlia_dht
    git clone https://github.com/s-h-telgerd/kademlia_dht.git
    cd kademlia_dht
    cd releases
    mkdir kademlia_dht-0.1.0
    tar -zxvf kademlia_dht-0.1.0.tar.gz  -C ./kademlia_dht-0.1.0
    cd kademlia_dht-0.1.0
    /bin/kademlia_dht console
```

# Simulation

%%%%%%%%%




[ref1]: <https://github.com/s-h-telgerd/kademlia_dht/blob/main/doc/maymounkov-kademlia-lncs.pdf>