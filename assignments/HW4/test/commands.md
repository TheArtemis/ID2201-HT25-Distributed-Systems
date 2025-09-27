# GMS1 - Simple group communication

W1 = test:first(1, gms1, 1000).

test:add(2, gms1, W1, 1000).

# GMS2

W1 = test:first(1, gms2, 1000).

W2 = test:add(2, gms2, W1, 1000).

test:add(3, gms2, W1, 1000).

**Kill the leader**

test:add(12, gms2, W10, 1000).

# GMS2p5

W1 = test:first(1, gms2p5, 1000).

W2 = test:add(2, gms2p5, W1, 1000).

W3 = test:add(3, gms2p5, W2, 1000).

W4 = test:add(4, gms2p5, W3, 1000).

W5 = test:add(5, gms2p5, W4, 1000).

W6 = test:add(6, gms2p5, W5, 1000).

W7 = test:add(7, gms2p5, W6, 1000).

W8 = test:add(8, gms2p5, W7, 1000).

W9 = test:add(9, gms2p5, W8, 1000).

W10 = test:add(10, gms2p5, W9, 1000).

W11 = test:add(11, gms2p5, W10, 1000).

test:add(12, gms2p5, W11, 1000).

# GMS3

W1 = test:first(1, gms3, 1000).

W2 = test:add(2, gms3, W1, 1000).

W3 = test:add(3, gms3, W2, 1000).

W4 = test:add(4, gms3, W3, 1000).

W5 = test:add(5, gms3, W4, 1000).

W6 = test:add(6, gms3, W5, 1000).

W7 = test:add(7, gms3, W6, 1000).

W8 = test:add(8, gms3, W7, 1000).

W9 = test:add(9, gms3, W8, 1000).

W10 = test:add(10, gms3, W9, 1000).

W11 = test:add(11, gms3, W10, 1000).

test:add(12, gms3, W11, 1000).


# GMS3p5

W1 = test:first(1, gms3p5, 1000).

W2 = test:add(2, gms3p5, W1, 1000).

W3 = test:add(3, gms3p5, W2, 1000).

W4 = test:add(4, gms3p5, W3, 1000).

W5 = test:add(5, gms3p5, W4, 1000).

W6 = test:add(6, gms3p5, W5, 1000).

W7 = test:add(7, gms3p5, W6, 1000).

W8 = test:add(8, gms3p5, W7, 1000).

W9 = test:add(9, gms3p5, W8, 1000).

W10 = test:add(10, gms3p5, W9, 1000).

W11 = test:add(11, gms3p5, W10, 1000).

test:add(12, gms3p5, W11, 1000).

# GMSX

W1 = test:first(1, gmsX, 1000).

W2 = test:add(2, gmsX, W1, 1000).

W3 = test:add(3, gmsX, W2, 1000).

W4 = test:add(4, gmsX, W3, 1000).

W5 = test:add(5, gmsX, W4, 1000).

test:add(6, gmsX, W5, 1000).