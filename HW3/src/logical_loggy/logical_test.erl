-module(logical_test).
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
    io:format("Starting loggy and ~p logical_workers...~n", [N]),
    Log = logical_loggy:start(Nodes),
    NamesSeeds = lists:zip(Nodes, lists:seq(1, N)),
    Logical_workers = [
        logical_worker:start(Name, Log, Seed, Sleep, Jitter)
     || {Name, Seed} <- NamesSeeds
    ],

    io:format("Setting peers for logical_workers...~n"),
    lists:foreach(
        fun(W) ->
            Peers = lists:delete(W, Logical_workers),
            logical_worker:peers(W, Peers)
        end,
        Logical_workers
    ),
    RunSleep = 120000,
    io:format("Sleeping for ~p milliseconds...~n", [RunSleep]),
    timer:sleep(RunSleep),

    io:format("Stopping loggy and logical_workers...~n"),
    logical_loggy:stop(Log),
    lists:foreach(fun logical_worker:stop/1, Logical_workers).

run(Sleep, Jitter) ->
    io:format("Starting loggy and logical_workers...~n"),
    Log = logical_loggy:start([john, paul, ringo, george]),
    A = logical_worker:start(john, Log, 13, Sleep, Jitter),
    B = logical_worker:start(paul, Log, 23, Sleep, Jitter),
    C = logical_worker:start(ringo, Log, 36, Sleep, Jitter),
    D = logical_worker:start(george, Log, 49, Sleep, Jitter),

    io:format("Setting peers for logical_workers...~n"),
    logical_worker:peers(A, [B, C, D]),
    logical_worker:peers(B, [A, C, D]),
    logical_worker:peers(C, [A, B, D]),
    logical_worker:peers(D, [A, B, C]),

    io:format("Sleeping for 5 seconds...~n"),
    timer:sleep(5000),

    io:format("Stopping loggy and logical_workers...~n"),
    logical_loggy:stop(Log),
    logical_worker:stop(A),
    logical_worker:stop(B),
    logical_worker:stop(C),
    logical_worker:stop(D).
