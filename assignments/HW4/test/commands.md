# GMS1 - Simple group communication

W1 = test:first(1, gms1, 1000).

test:add(2, gms1, W1, 1000).

# GMS2 -

W1 = test:first(1, gms2, 1000).

W2 = test:add(2, gms2, W1, 1000).

test:add(3, gms2, W1, 1000).

**Kill the leader**

test:add(10, gms2, W2, 1000).

# GMS2p5 -

W1 = test:first(1, gms2p5, 1000).

W2 = test:add(2, gms2p5, W1, 1000).

test:add(3, gms2p5, W1, 1000).




# GMS3 -