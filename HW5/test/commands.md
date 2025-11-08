# 1. Check if keys have been added with probe

Keys = test:keys(1000).
timer:sleep(1000).
test:add(Keys, pear1).
timer:sleep(2000).
ring:probe(pear1).

# 1.1 Lookup with one node
On the shell:
./start.sh

single:apple(node2).

On another shell:
./client.sh

client:start_all().


# 1.2 Lookup with multiple nodes
On the shell:
./ring.sh node2

On the other shell:
./client.sh

client:start_all().

# 1.3 Send a probe command to a node
On the ring shell:
ring:probe(pear1).


# 1.4 Show the handover functionality
On the ring shell:
ring:grape(node4).






