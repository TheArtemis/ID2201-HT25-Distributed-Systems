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
            From ! {self(), pong}
    end.

% remote procedure call
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        % Should be Pid to make sure that the source is the same as the one where we sent it
        {_, Response} ->
            Response
    end.

send_ping(To) ->
    io:format("Sent ping! ~n"),
    Response = rpc(To, ping),
    io:format("Received ~p back from: ~p~n", [Response, To]).

% register(server, spawn(ping, receive_ping, [])).

% ping:send_ping({server, server@DORORO}). 

% for a distributed application we need to specify the {process, node}
