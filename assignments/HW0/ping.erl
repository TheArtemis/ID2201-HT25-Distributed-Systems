-module(ping).
-export([receive_ping/0]).
-export([send_ping/1]).
-export([rpc/2]).

% we will send the message P ! {self(), ping}
receive_ping() ->
    receive
        {From, ping} ->
            io:format("Received ping from: ~p~n", [From]),
            io:format("Sending back pong to: ~p~n", [From]),
            From ! {self(), ping}
    end.

% remote procedure call
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response ->
            Response
    end.

send_ping(To) ->
    Response = rpc(To, ping),
    {From, ping} = Response,
    io:format("Received ping back from: ~p~n", [From]).

% Pid = spawn(ping, receive_ping, [])

% register(ping, Pid).

% ping:send_ping({ping, server@DORORO}). 

% for a distributed application we need to specify the {process, node}
