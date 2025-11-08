-module(client).

-export([init_red/0, init_blue/0, init_green/0, init_yellow/0, start_all/0]).

-define(NUM_KEYS, 1000).
-define(NODES, [apple1, apple2, pear1, pear2, banana1, banana2, cherry1, cherry2]).
-define(SAME_NODE, false).

init_red() ->
    Pid = spawn(fun() ->
                   receive
                       start ->
                           io:format("Client RED started~n"),
                           client_fun()
                   end,
                   loop()
                end),
    register(red, Pid),
    io:format("Client RED initiated~n"),
    Pid.

init_blue() ->
    Pid = spawn(fun() ->
                   receive
                       start ->
                           io:format("Client BLUE started~n"),
                           client_fun()
                   end,
                   loop()
                end),
    register(blue, Pid),
    io:format("Client BLUE initiated~n"),
    Pid.

init_green() ->
    Pid = spawn(fun() ->
                   receive
                       start ->
                           io:format("Client GREEN started~n"),
                           client_fun()
                   end,
                   loop()
                end),
    register(green, Pid),
    io:format("Client GREEN initiated~n"),
    Pid.

init_yellow() ->
    Pid = spawn(fun() ->
                   receive
                       start ->
                           io:format("Client YELLOW started~n"),
                           client_fun()
                   end,
                   loop()
                end),
    register(yellow, Pid),
    io:format("Client YELLOW initiated~n"),
    Pid.

start_all() ->
    {red, netw:node_addr(red)} ! start,
    {blue, netw:node_addr(blue)} ! start,
    {green, netw:node_addr(green)} ! start,
    {yellow, netw:node_addr(yellow)} ! start,
    ok.

client_fun() ->
    io:format("~p: Generating ~p keys...~n", [self(), ?NUM_KEYS]),
    Keys = test:keys(?NUM_KEYS),
    io:format("~p: Keys generated, waiting 1 second...~n", [self()]),
    timer:sleep(1000),

    Node =
        case ?SAME_NODE of
            true ->
                apple1;
            false ->
                Nodes = ?NODES,
                RandomIndex = rand:uniform(length(Nodes)),
                lists:nth(RandomIndex, Nodes)
        end,

    io:format("~p: Selected node: ~p~n", [self(), Node]),

    io:format("~p: Adding keys to node ~p...~n", [self(), Node]),
    test:add(Keys, Node),
    io:format("~p: Keys added successfully~n", [self()]),

    io:format("~p: Waiting 2 seconds before checking...~n", [self()]),
    timer:sleep(2000),

    io:format("~p: Checking keys on node ~p...~n", [self(), Node]),
    test:check(Keys, Node),
    io:format("~p: Check completed~n", [self()]),

    ok.

loop() ->
    receive
        stop ->
            ok;
        _ ->
            loop()
    end.
