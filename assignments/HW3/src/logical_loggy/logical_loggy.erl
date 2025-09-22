-module(logical_loggy).
-export([start/1, stop/1]).

-define(OUTPUT, "./dumps/").
-define(TEST, "test_logical_random").
-define(LOGS_OUTPUT, ?OUTPUT ++ "logs/").

-define(DUMP_QUEUE, false).
-define(DUMP_LOGS, false).
-define(LOG, true).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).
stop(Logger) ->
    Logger ! stop.
init(Nodes) ->
    loop(logical_hb_queue:new(), logical_time:clock(Nodes)).

loop(Queue, Clock) ->
    % At each iteration log all the safe messages
    dump(Queue),
    {Safe, Unsafe} = logical_hb_queue:partition(Queue, Clock),
    log(Safe),
    receive
        {log, From, Time, Msg} ->
            %io:format("Clock before: ~w~n", [Clock]),
            %io:format("Message time: ~w~n", [Time]),
            %io:format("Safe check: ~w~n", [logical_time:safe(Time, Clock)]),
            case logical_time:safe(Time, Clock) of
                true ->
                    log(From, Time, Msg),
                    UpdatedClock = logical_time:update(From, Time, Clock),
                    loop(Unsafe, UpdatedClock);
                false ->
                    %io:format("queuing.. ~w ~w ~p~n", [Time, From, Msg]),
                    Queue1 = logical_hb_queue:add(From, Time, Msg, Unsafe),
                    UpdatedClock = logical_time:update(From, Time, Clock),
                    loop(Queue1, UpdatedClock)
            end;
        stop ->
            % Flush remaining queue in order (use the full stored Queue)
            Sorted = logical_hb_queue:sort(Queue),
            dump(Sorted),
            log(Sorted),
            ok
    end.

log(From, Time, Msg) ->
    if
        ?LOG ->
            io:format("log: ~w ~w ~p~n", [Time, From, Msg]);
        true ->
            ok
    end,

    if
        ?DUMP_LOGS ->
            write_log({From, Time, Msg});
        true ->
            ok
    end.

log(Queue) ->
    lists:foreach(
        fun({F, T, M}) -> log(F, T, M) end,
        Queue
    ).

dump(Queue) ->
    if
        ?DUMP_QUEUE ->
            misc:append_to_csv(?OUTPUT ++ ?TEST ++ ".csv", {logical_hb_queue:size(Queue)});
        true ->
            ok
    end.

write_log(Log) ->
    misc:append_to_csv(?LOGS_OUTPUT ++ ?TEST ++ "logs.log", Log).
