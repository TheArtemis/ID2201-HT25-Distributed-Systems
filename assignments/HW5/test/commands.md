# 1. Check if keys have been added with probe

Keys = test:keys(1000).
timer:sleep(1000).
test:add(Keys, pear1).
timer:sleep(2000).
ring:probe(pear1).

# 1.1 Use functions to benchmark lookup time


# 1.2 Check if keys have moved once a node has joined



