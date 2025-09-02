-module(multi_rudy).
-export([init/2, request/1]).
-export([start/1, stop/0]).

-define(HANDLING_TIME, 40).
-define(CONCURRENCY, 3).

init(Port, Concurrency) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Client} ->
            io:format("Listening on port ~p with a pool of ~p workers ~n", [Port, Concurrency]),
            Workers = spawn_handlers(Concurrency, Client),
            wait(Client, Workers);
        {error, _} ->
            error
    end.

wait(Listen, _Workers) ->
    receive
        stop ->
            io:format("Shutting down gracefully...~n"),
            gen_tcp:close(Listen),
            ok
    after infinity ->
        ok
    end.

spawn_handlers(0, _Client) ->
    [];
spawn_handlers(N, Client) ->
    Process = spawn(fun() -> handler(Client) end),
    [Process | spawn_handlers(N - 1, Client)].

handler(Listen) ->
    io:format("Spawned worker: ~w~n", [self()]),
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            io:format("Accepting connection from ~p~n", [Listen]),
            request(Client),
            handler(Listen);
        {error, _} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    io:format("Request being handled ~w~n", [self()]),
    case Recv of
        {ok, Str} ->
            {Request, Headers, Body} = http:parse_request(Str),
            %io:format("=== INCOMING HTTP REQUEST ===~n"),
            %io:format("Received Message from ~p~n", [Client]),
            io:format("Raw request string: ~p~n", [Str]),
            %io:format("Received Request: ~p~n", [Request]),
            %io:format("Received Headers: ~p~n", [Headers]),
            %io:format("Received Body: ~p~n", [Body]),

            %io:format("=== END OF HTTP REQUEST ===~n"),
            Response = reply_handle({Request, Headers, Body}),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply_handle({{get, URI, _}, _, _}) ->
    timer:sleep(?HANDLING_TIME),
    http:ok("You requested the URI: " ++ URI).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port, ?CONCURRENCY) end)).
stop() ->
    Pid = whereis(rudy),
    Pid ! stop,
    timer:sleep(10),
    exit(Pid, "time to die").
