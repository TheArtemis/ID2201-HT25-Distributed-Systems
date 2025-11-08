-module(test_abc).

-export([start/0]).

-define(TIMEOUT, 2000).

%% Nodes A and B (A < B) store keys P and Q respectively, and have each other replicated.
%% Node C gets added as a successor to B. After synchronization, node B dies.
%% Result: A should store P and Q (and replicate Q), C should store Q (and replicate P).

start() ->
    io:format("~n=== Test Case 4: A < B < C, B dies scenario ===~n"),

    %% Setup: Create ring with nodes A(100) and B(200)
    io:format("~nStep 1: Creating initial ring with A(100) and B(200)...~n"),
    NodeA = node4:start(100),
    timer:sleep(100),
    NodeB = node4:start(200, NodeA),
    timer:sleep(500),

    %% Add keys P and Q
    io:format("~nStep 2: Adding keys P(150) and Q(250)...~n"),
    Qref1 = make_ref(),
    NodeA ! {add, 150, "P", Qref1, self()},
    receive
        {Qref1, ok} ->
            io:format("  Key P added~n")
    after ?TIMEOUT ->
        io:format("  Key P add timeout~n")
    end,

    Qref2 = make_ref(),
    NodeA ! {add, 250, "Q", Qref2, self()},
    receive
        {Qref2, ok} ->
            io:format("  Key Q added~n")
    after ?TIMEOUT ->
        io:format("  Key Q add timeout~n")
    end,

    timer:sleep(300),

    io:format("~nStep 3: Initial state~n"),
    io:format("  - A should store P and replicate Q~n"),
    io:format("  - B should store Q and replicate P~n"),

    %% Add node C(300) as successor to B
    io:format("~nStep 4: Adding node C(300)...~n"),
    NodeC = node4:start(300, NodeA),
    timer:sleep(1000),

    NodeA ! probe,
    timer:sleep(300),

    io:format("~nStep 5: Ring after C joins - checking all keys...~n"),
    Qref3 = make_ref(),
    NodeA ! {lookup, 150, Qref3, self()},
    receive
        {Qref3, {150, "P"}} ->
            io:format("  Key P found~n");
        {Qref3, Other} ->
            io:format("  Key P lookup returned: ~p~n", [Other])
    after ?TIMEOUT ->
        io:format("  Key P lookup timeout~n")
    end,

    Qref4 = make_ref(),
    NodeA ! {lookup, 250, Qref4, self()},
    receive
        {Qref4, {250, "Q"}} ->
            io:format("  Key Q found~n");
        {Qref4, Other2} ->
            io:format("  Key Q lookup returned: ~p~n", [Other2])
    after ?TIMEOUT ->
        io:format("  Key Q lookup timeout~n")
    end,

    %% Kill node B
    io:format("~nStep 6: Killing node B...~n"),
    exit(NodeB, kill),
    timer:sleep(1000),

    %% Verify both keys are still accessible
    io:format("~nStep 7: Checking keys after B dies...~n"),
    Qref5 = make_ref(),
    NodeA ! {lookup, 150, Qref5, self()},
    receive
        {Qref5, {150, "P"}} ->
            io:format("  Key P still accessible (on A)~n");
        {Qref5, Other3} ->
            io:format("  Key P lookup returned: ~p~n", [Other3])
    after ?TIMEOUT ->
        io:format("  Key P lookup timeout~n")
    end,

    Qref6 = make_ref(),
    NodeA ! {lookup, 250, Qref6, self()},
    receive
        {Qref6, {250, "Q"}} ->
            io:format("  Key Q still accessible (recovered from replica)~n");
        {Qref6, Other4} ->
            io:format("  Key Q lookup returned: ~p~n", [Other4])
    after ?TIMEOUT ->
        io:format("  Key Q lookup timeout~n")
    end,

    %% Now verify that keys are properly replicated again
    io:format("~nStep 8: Checking replication after recovery...~n"),
    io:format("  Testing by killing A and checking if C can still find Q...~n"),
    exit(NodeA, kill),
    timer:sleep(500),

    Qref7 = make_ref(),
    NodeC ! {lookup, 250, Qref7, self()},
    receive
        {Qref7, {250, "Q"}} ->
            io:format("  Key Q found on C (was re-replicated from A)~n");
        {Qref7, Other5} ->
            io:format("  Key Q lookup on C returned: ~p~n", [Other5])
    after ?TIMEOUT ->
        io:format("  Key Q lookup on C timeout~n")
    end,

    exit(NodeC, kill),
    io:format("~n=== Test complete ===~n"),
    ok.
