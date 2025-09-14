# The network map

## Defining the areas
italy:
- rome 
- milan
- turin

spain:
- barcelona
- madrid

## defining the connections
rome -> milan, turin
milan -> barcellona
barcellona -> madrid
turin -> madrid
madrid -> barcellona

## Creating the routers

//Italy
routy:start(r1, rome)
routy:start(r2, milan)
routy:start(r3, turin)

//Spain
routy:start(r4, barcelona)
routy:start(r5, madrid)


## creating the connections
// rome -> milan, turin
test:add(r1, milan, {'r2', 'italy@DORORO'}).
test:add(r1, turin, {'r3', 'italy@DORORO'}).

// milan -> barcellona
test:add(r2, barcelona, {'r4', 'spain@DORORO'}).

// barcellona -> madrid
test:add(r4, madrid, {'r5', 'spain@DORORO'}).

// madrid -> turin
test:add(r4, madrid, {'r3', 'italy@DORORO'}).

// madrid -> barcellona
test:add(r5, barcelona, {'r4', 'spain@DORORO'}).

