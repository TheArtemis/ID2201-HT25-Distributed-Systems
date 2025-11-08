-module(logical_time).
-export([zero/0, inc/2, merge/2, leq/2, recv/3, clock/1, update/3, safe/2]).

% Initial lamport value is 0

zero() ->
    0.

inc(_Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    erlang:max(Ti, Tj).

leq(Ti, Tj) ->
    Ti =< Tj.

recv(Name, Local, Remote) ->
    inc(Name, merge(Local, Remote)).

% Nodes is list of Nodes
% Clock is a list of tuples.
% Clock: [{Node, Time}, ...]

clock(Nodes) ->
    [{Node, 0} || Node <- Nodes].

update(Node, Time, Clock) ->
    OldTime =
        case lists:keyfind(Node, 1, Clock) of
            {Node, T} -> T;
            false -> 0
        end,
    [{Node, merge(OldTime, Time)} | lists:keydelete(Node, 1, Clock)].

% When is it safe to print?
safe(Time, Clock) ->
    lists:all(fun({_Node, T}) -> Time =< T end, Clock).
