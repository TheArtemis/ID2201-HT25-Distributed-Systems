# First implementation

Implemented a simple recv:
```erlang
recv(_Name, Local, Remote) ->
    merge(Local, Remote) + 1.
```


What a mess! The logging is all in the wrong order!

Sleeping for 5 seconds...
log: 2 george {received,{hello,83}}
log: 1 paul {sending,{hello,83}}
log: 4 john {received,{hello,15}}
log: 2 paul {sending,{hello,93}}
log: 5 john {received,{hello,53}}
log: 3 george {sending,{hello,15}}
log: 4 george {received,{hello,93}}
log: 1 ringo {sending,{hello,53}}
log: 6 john {received,{hello,30}}
log: 8 george {received,{hello,81}}
log: 10 ringo {received,{hello,12}}
log: 3 paul {sending,{hello,30}}
log: 7 john {sending,{hello,81}}
log: 9 george {sending,{hello,12}}
log: 11 ringo {received,{hello,35}}
log: 11 john {received,{hello,29}}
log: 4 paul {sending,{hello,35}}
log: 13 john {received,{hello,74}}
log: 10 george {sending,{hello,29}}
log: 15 paul {received,{hello,24}}
log: 11 george {sending,{hello,69}}
log: 12 ringo {sending,{hello,74}}
log: 14 john {sending,{hello,24}}
log: 15 john {received,{hello,69}}
log: 16 john {received,{hello,25}}


Let's check if out Lamport Clock implementation works:

| Message     | Sender  | Send Time | Receiver | Receive Time | Valid? |
|-------------|---------|-----------|----------|--------------|--------|
| {hello,83}  | paul    | 1         | george   | 2            | Yes    |
| {hello,15}  | george  | 3         | john     | 4            | Yes    |
| {hello,93}  | paul    | 2         | george   | 4            | Yes    |
| {hello,53}  | ringo   | 1         | john     | 5            | Yes    |
| {hello,30}  | paul    | 3         | john     | 6            | Yes    |
| {hello,81}  | john    | 7         | george   | 8            | Yes    |
| {hello,12}  | george  | 9         | ringo    | 10           | Yes    |
| {hello,35}  | paul    | 4         | ringo    | 11           | Yes    |
| {hello,74}  | ringo   | 12        | john     | 13           | Yes    |
| {hello,29}  | george  | 10        | john     | 11           | No     |
| {hello,69}  | george  | 11        | john     | 15           | Yes    |
| {hello,24}  | john    | 14        | paul     | 15           | Yes    |


There is one message in the wrong order, but that's ok. For each pair {hello, rand} the Receive Time is strictly greater than the Receive Time, hence causality between events is preserved. 

If A -> B, then L(A) < L(B)

# Handling the tricky part
(loggy@DORORO)1> Setting peers for workers...
Sleeping for 5 seconds...
log: 1 ringo {sending,{hello,24}}
log: 1 paul {sending,{hello,9}}
log: 2 ringo {sending,{hello,92}}
log: 2 george {received,{hello,9}}
log: 3 george {received,{hello,24}}
log: 4 george {sending,{hello,11}}
log: 5 george {sending,{hello,38}}
log: 5 john {received,{hello,11}}
log: 6 john {sending,{hello,6}}
log: 7 john {sending,{hello,72}}
log: 7 paul {received,{hello,6}}
log: 8 john {received,{hello,38}}
log: 8 paul {received,{hello,72}}
log: 9 paul {received,{hello,92}}
log: 10 paul {sending,{hello,69}}
log: 11 paul {sending,{hello,79}}
log: 11 ringo {received,{hello,69}}
log: 12 paul {sending,{hello,54}}
log: 12 george {received,{hello,79}}
log: 12 ringo {sending,{hello,33}}
log: 13 george {sending,{hello,97}}
log: 13 ringo {sending,{hello,35}}
log: 13 john {received,{hello,33}}
log: 14 george {received,{hello,54}}
log: 14 john {received,{hello,35}}
log: 15 john {sending,{hello,41}}
log: 16 john {received,{hello,97}}
log: 16 ringo {received,{hello,41}}
log: 17 john {sending,{hello,52}}
log: 17 ringo {sending,{hello,92}}
log: 18 ringo {sending,{hello,71}}
log: 18 paul {received,{hello,92}}
log: 19 ringo {sending,{hello,55}}
log: 19 george {received,{hello,71}}
log: 19 paul {received,{hello,52}}
log: 20 ringo {sending,{hello,5}}
log: 20 paul {sending,{hello,89}}
log: 21 john {received,{hello,89}}
log: 22 john {sending,{hello,22}}
log: 23 john {sending,{hello,83}}
log: 23 paul {received,{hello,22}}
log: 24 john {sending,{hello,85}}
log: 24 paul {sending,{hello,1}}
log: 25 john {received,{hello,5}}
log: 25 paul {sending,{hello,38}}
log: 25 george {received,{hello,1}}
log: 26 john {sending,{hello,77}}
log: 26 george {received,{hello,83}}
Stopping loggy and workers...
log: 27 ringo {received,{hello,77}}
log: 27 george {received,{hello,38}}
log: 28 george {sending,{hello,45}}
log: 29 george {received,{hello,55}}
log: 29 paul {received,{hello,45}}
log: 30 george {received,{hello,85}}
log: 31 george {sending,{hello,66}}
log: 32 george {sending,{hello,44}}
log: 32 paul {received,{hello,66}}
log: 33 paul {received,{hello,44}}
log: 35 ringo {received,{hello,82}}

A message is safe to Log if its time stamp is less than or equal for every other node's clock.

Implementing the HoldBack queue we are able to log the messages in order. Whenever we receive a new message, if it's already safe to print we log it, otherwise we add it to the holdback queue. At the beginning of each iteration we go through all messages to see which ones are safe and log them.

Once a stop message is received, we flush all the messages in the queue.

Let's see how big our queue can get. 
Three runs, with 10, and 100 Nodes

The queue can get quite big, the worst case scenario happens when a node with a very high clock sends a message to the rest of the nodes, which have a very low clock. All those messages will be queued until the node with the high clock sends messages to all the other nodes, allowing them to increase their clocks and making the messages safe to log.

Lookin at the plots, with 10 nodes the queue length can get up to 75 messagesm but usually staying around 35.

With 100 nodes the queue length peaks at 1400 messages, but usually stays around 700.

# Vector Clocks

