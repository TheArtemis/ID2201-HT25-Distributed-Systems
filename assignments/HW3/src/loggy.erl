-module(loggy).
-export([start/1, stop/1]).

-define(OUTPUT, "./dumps/").
-define(TEST, "test_random_").

-define(DUMP_QUEUE, true).
-define(LOG, true).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).
stop(Logger) ->
    Logger ! stop.
init(Nodes) ->
    loop(hb_queue:new(), logical_time:clock(Nodes)).

loop(Queue, Clock) ->
    % At each iteration log all the safe messages
    {Safe, Unsafe} = hb_queue:partition(Queue, Clock),
    log(Safe),
    receive
        {log, From, Time, Msg} ->
            %io:format("Clock before: ~w~n", [Clock]),
            %io:format("Message time: ~w~n", [Time]),
            %io:format("Safe check: ~w~n", [vect:safe(Time, Clock)]),
            case logical_time:safe(Time, Clock) of
                true ->
                    log(From, Time, Msg),
                    UpdatedClock = logical_time:update(From, Time, Clock),
                    loop(Unsafe, UpdatedClock);
                false ->
                    %io:format("queuing.. ~w ~w ~p~n", [Time, From, Msg]),
                    Queue1 = hb_queue:add(From, Time, Msg, Unsafe),
                    UpdatedClock = logical_time:update(From, Time, Clock),
                    if
                        ?DUMP_QUEUE ->
                            dump(Queue1)
                    end,
                    loop(Queue1, UpdatedClock)
            end;
        stop ->
            % Flush remaining queue in order
            Sorted = hb_queue:sort(Unsafe),
            if
                ?DUMP_QUEUE ->
                    dump(Sorted)
            end,
            log(Sorted),
            ok
    end.

log(From, Time, Msg) ->
    if
        ?LOG ->
            io:format("log: ~w ~w ~p~n", [Time, From, Msg]);
        true ->
            ok
    end.

log(Queue) ->
    lists:foreach(
        fun({F, T, M}) -> log(F, T, M) end,
        Queue
    ).

dump(Queue) ->
    misc:append_to_csv(?OUTPUT ++ ?TEST ++ "queue_len.csv", {hb_queue:size(Queue)}).
