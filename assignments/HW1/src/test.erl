-module(test).
-export([bench/2, bench_many/3, bench_live/2]).
-import(misc, [dump_to_csv/2]).

-define(N_BENCH, 100).
-define(N_REQUESTS, 100).
-define(DUMP_PATH, "../HW1/dumps/bench/presentation").

bench_many(Nodes, Host, Port) ->
    Pids = initialize_many(Nodes, Host, Port),
    start_many(Pids).

initialize_many(Nodes, Host, Port) ->
    [spawn(fun() -> wait_for_signal(Host, Port) end) || _ <- lists:seq(1, Nodes)].

start_many(Pids) ->
    lists:foreach(fun(P) -> P ! start end, Pids),
    io:format("Started all processes ~n").

wait_for_signal(Host, Port) ->
    receive
        start ->
            Start = Start = erlang:system_time(micro_seconds),
            Result = run_live(?N_REQUESTS, Host, Port),
            %Finish = erlang:system_time(micro_seconds),
            %io:format("Total Time Elapsed ~.3f ms~n", [(Finish - Start) / 1000.0]),
            FileName = integer_to_list(Start),
            DumpFile = filename:join(?DUMP_PATH, "tm_" ++ FileName ++ ".csv"),
            dump_to_csv(DumpFile, Result)
    end.

bench(Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    run(?N_REQUESTS, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Result = Finish - Start,
    io:format("Time Elapsed ~.3f ms~n", [Result / 1000.0]),
    Result.

bench_live(Host, Port) ->
    Start = Start = erlang:system_time(micro_seconds),
    Result = run_live(?N_REQUESTS, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    io:format("Total Time Elapsed ~.3f ms~n", [(Finish - Start) / 1000.0]),
    FileName = integer_to_list(Start),
    DumpFile = filename:join(?DUMP_PATH, "concurrent_bench_" ++ FileName ++ ".csv"),
    dump_to_csv(DumpFile, Result).

run_live(0, _Host, _Port) ->
    [];
run_live(N, Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    request(Host, Port),
    End = erlang:system_time(micro_seconds),
    Delta = End - Start,
    [{Start, Delta} | run_live(N - 1, Host, Port)].

run(N, Host, Port) ->
    if
        N == 0 ->
            ok;
        true ->
            request(Host, Port),
            run(N - 1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:connect(Host, Port, Opt) of
        {ok, Server} ->
            case gen_tcp:send(Server, http:get("foo")) of
                ok -> ok;
                {error, SendErr} -> io:format("test: send error: ~p~n", [SendErr])
            end,
            case gen_tcp:recv(Server, 0) of
                {ok, _} ->
                    gen_tcp:close(Server),
                    ok;
                {error, Error} ->
                    io:format("test: recv error: ~p~n", [Error]),
                    gen_tcp:close(Server),
                    {error, Error}
            end;
        {error, Error} ->
            io:format("test: connect error: ~p~n", [Error]),
            {error, Error}
    end.
