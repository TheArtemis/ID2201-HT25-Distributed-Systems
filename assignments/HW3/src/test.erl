-module(test).
-export([run/2]).

run(Sleep, Jitter) ->
    io:format("Starting loggy and workers...~n"),
    Log = loggy:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),

    io:format("Setting peers for workers...~n"),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),

    io:format("Sleeping for 5 seconds...~n"),
    timer:sleep(5000),

    io:format("Stopping loggy and workers...~n"),
    loggy:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).
