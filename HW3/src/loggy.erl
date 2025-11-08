-module(loggy).

-export([start/1, stop/1]).

-define(OUTPUT, "./dumps/").
-define(TEST, "test_ordering_vec").
-define(LOGS_OUTPUT, ?OUTPUT ++ "logs/").
-define(DUMP_QUEUE, false).
-define(DUMP_LOGS, false).
-define(LOG, true).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(hb_queue:new(), vect:clock(Nodes)).

loop(Queue, Clock) ->
    dump(Queue),
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = vect:update(From, Time, Clock),
            Queue1 = hb_queue:add(From, Time, Msg, Queue),
            {Safe, Unsafe} = hb_queue:partition(Queue1, UpdatedClock),

            % Log all safe messages
            log(Safe),

            loop(Unsafe, UpdatedClock);
        stop ->
            Sorted = hb_queue:sort(Queue),
            dump(Sorted),
            log(Sorted),
            ok
    end.

log(From, Time, Msg) ->
    if ?LOG ->
           io:format("log: ~w ~w ~p~n", [Time, From, Msg]);
       true ->
           ok
    end,

    if ?DUMP_LOGS ->
           write_log({From, Time, Msg});
       true ->
           ok
    end.

log(Queue) ->
    lists:foreach(fun({F, T, M}) -> log(F, T, M) end, Queue).

dump(Queue) ->
    if ?DUMP_QUEUE ->
           misc:append_to_csv(?OUTPUT ++ ?TEST ++ ".csv", {hb_queue:size(Queue)});
       true ->
           ok
    end.

write_log(Log) ->
    misc:append_to_csv(?LOGS_OUTPUT ++ ?TEST ++ "logs.log", Log).
