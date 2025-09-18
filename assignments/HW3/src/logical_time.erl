-module(logical_time).
-export([zero/0, inc/2, merge/2, leq/2]).

% Initial lamport value is 0

zero() ->
    0.

inc(_Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    erlang:max(Ti, Tj).

leq(Ti, Tj) ->
    Ti =< Tj.
