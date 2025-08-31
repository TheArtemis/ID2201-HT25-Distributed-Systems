-module(rudy).
-export([init/1, request/1]).
-export([start/1, stop/0]).

-define(HANDLING_TIME, 40).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, _} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, _} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            {Request, Headers, Body} = http:parse_request(Str),
            % io:format("=== INCOMING HTTP REQUEST ===~n"),

            %io:format("Received Request: ~p~n", [Request]),
            %io:format("Received Headers: ~p~n", [Headers]),
            %io:format("Received Body: ~p~n", [Body]),

            %io:format("=== END OF HTTP REQUEST ===~n"),
            Response = reply({Request, Headers, Body}),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    http:ok("You requested the URI: " ++ URI).

reply_handle({{get, URI, _}, _, _}) ->
    timer:sleep(?HANDLING_TIME),
    http:ok("You requested the URI: " ++ URI).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
    exit(whereis(rudy), "time to die").
