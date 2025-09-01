-module(test).
-export([bench/2, bench_many/3, start_bench/2, bench_live/2]).
-import(misc, [dump_to_csv/2]).

-define(N_BENCH, 100).
-define(N_REQUESTS, 100).
-define(DUMP_PATH, "../HW1/dumps").

start_bench(Host, Port) ->
    Result = bench_many(Host, Port, ?N_BENCH),
    Max = lists:max(Result),
    Min = lists:min(Result),
    Avg = lists:sum(Result) / length(Result),
    io:format("Minimum RTT ~.3f ms~n", [Min / 1000.0]),
    io:format("Maximum RTT ~.3f ms~n", [Max / 1000.0]),
    io:format("Average RTT ~.3f ms~n", [Avg / 1000.0]),

    TotalRequests = ?N_BENCH * ?N_REQUESTS,
    TotalTimeMs = lists:sum(Result),
    RequestsPerSecond = (TotalRequests * 1000000) / TotalTimeMs,
    io:format("Average requests per second ~B~n", [trunc(RequestsPerSecond)]),

    DumpFile = filename:join(?DUMP_PATH, "bench_results.csv"),
    dump_to_csv(DumpFile, Result).

bench_many(_Host, _Port, 0) ->
    [];
bench_many(Host, Port, N) ->
    Result = bench(Host, Port),
    [Result | bench_many(Host, Port, N - 1)].

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
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    %io:format("Sent message. ~n"),

    Recv = gen_tcp:recv(Server, 0),
    %io:format("Server responded. ~n"),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
