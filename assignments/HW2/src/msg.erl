-module(msg).
-export([status/2]).

status(Pid, Timeout) ->
    Pid ! {status, self()},
    receive
        {status, {Name, N, Hist, Intf, Table, Map}} ->
            io:format(
                "Status received from ~p:~n  Name: ~p~n  N: ~p~n  Hist: ~p~n  Intf: ~p~n  Table: ~p~n  Map: ~p~n",
                [Pid, Name, N, Hist, Intf, Table, Map]
            );
        %{Name, N, Hist, Intf, Table, Map};
        {status, Status} ->
            io:format("Status received from ~p: ~p~n", [Pid, Status]),
            Status
    after Timeout ->
        io:format("Timeout waiting for status from ~p~n", [Pid]),
        timeout
    end.
