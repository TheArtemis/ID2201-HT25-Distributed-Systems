-module(test_stabilization).

-export([watch_ring_formation/0]).

watch_ring_formation() ->
    io:format("~n=== Watching Ring Formation ===~n"),
    io:format("Creating Node 1 (ID: 100)...~n"),
    N1 = node1:start(100),
    timer:sleep(500),

    io:format("~nNode 1 after creation:~n"),
    N1 ! info,
    timer:sleep(200),

    io:format("~n---~nCreating Node 2 (ID: 200)...~n"),
    N2 = node1:start(200, N1),
    timer:sleep(200),

    io:format("~nState after 1 second:~n"),
    timer:sleep(1000),
    N1 ! info,
    N2 ! info,
    timer:sleep(200),

    io:format("~nState after 2 seconds:~n"),
    timer:sleep(1000),
    N1 ! info,
    N2 ! info,
    timer:sleep(200),

    io:format("~n---~nCreating Node 3 (ID: 300)...~n"),
    N3 = node1:start(300, N2),
    timer:sleep(200),

    io:format("~nState after 2 seconds:~n"),
    timer:sleep(2000),
    N1 ! info,
    N2 ! info,
    N3 ! info,
    timer:sleep(200),

    io:format("~nState after 4 seconds:~n"),
    timer:sleep(2000),
    N1 ! info,
    N2 ! info,
    N3 ! info,
    timer:sleep(200),

    io:format("~nState after 6 seconds:~n"),
    timer:sleep(2000),
    N1 ! info,
    N2 ! info,
    N3 ! info,
    timer:sleep(200),

    io:format("~n---~nTesting probe:~n"),
    N1 ! probe,
    timer:sleep(1000),

    % Cleanup
    N1 ! die,
    N2 ! die,
    N3 ! die,
    ok.
