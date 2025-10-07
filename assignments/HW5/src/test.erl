-module(test).

-export([start/1, start/2, start/3, add/3, add/2, lookup/2, keys/1, check/2, check/4]).

-define(Timeout, 1000).
-define(LOG, true).

%% Starting up a set of nodes is made easier using this function.

start(Module) ->
    Id = key:generate(),
    apply(Module, start, [Id]).

start(Module, P) ->
    Id = key:generate(),
    apply(Module, start, [Id, P]).

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N - 1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value, P) ->
    Q = make_ref(),
    ?LOG andalso io:format("Sending add request: Key=~p, Value=~p, P=~p~n", [Key, Value, P]),
    Node =
        netw:node_addr(
            netw:node_base(P)),
    {P, Node} ! {add, Key, Value, Q, self()},
    receive
        {Q, ok} ->
            ?LOG andalso io:format("Add succeeded for Key=~p~n", [Key]),
            ok
    after ?Timeout ->
        ?LOG andalso io:format("Request Timed out for Key=~p~n", [Key]),
        {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    NodeAddr =
        netw:node_addr(
            netw:node_base(Node)),
    {Node, NodeAddr} ! {lookup, Key, Q, self()},
    receive
        {Q, Value} ->
            Value
    after ?Timeout ->
        {error, "timeout"}
    end.

%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1, N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

check(Keys, P) ->
    T1 = erlang:monotonic_time(millisecond),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = erlang:monotonic_time(millisecond),
    Done = T2 - T1,
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).

check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key | Keys], P, Failed, Timeout) ->
    case lookup(Key, P) of
        {Key, _} ->
            check(Keys, P, Failed, Timeout);
        {error, _} ->
            check(Keys, P, Failed, Timeout + 1);
        false ->
            check(Keys, P, Failed + 1, Timeout)
    end.
