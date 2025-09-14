-module(msg).
-export([status/2]).

status(Pid, Timeout) ->
    Pid ! {status, self()},
    receive
        {status, Status} ->
            Status
    after Timeout ->
        io:format("Timeout waiting for status from ~p~n", [Pid]),
        timeout
    end.
