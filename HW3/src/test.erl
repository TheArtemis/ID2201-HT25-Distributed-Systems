-module(test).
-export([run/2, run/3]).

run(Sleep, Jitter, N) ->
    case N > 1000 of
        true ->
            io:format("Maximum 1000 nodes supported. Exiting.~n"),
            ok;
        false ->
            ok
    end,
    Nodes = lists:sublist(names:all(), N),
    io:format("Starting loggy and ~p workers...~n", [N]),
    Log = loggy:start(Nodes),
    NamesSeeds = lists:zip(Nodes, lists:seq(1, N)),
    Workers = [worker:start(Name, Log, Seed, Sleep, Jitter) || {Name, Seed} <- NamesSeeds],

    io:format("Setting peers for workers...~n"),
    lists:foreach(
        fun(W) ->
            Peers = lists:delete(W, Workers),
            worker:peers(W, Peers)
        end,
        Workers
    ),
    RunSleep = 120000,
    io:format("Sleeping for ~p milliseconds...~n", [RunSleep]),
    timer:sleep(RunSleep),

    io:format("Stopping loggy and workers...~n"),
    loggy:stop(Log),
    lists:foreach(fun worker:stop/1, Workers).

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
