-module(storage).

-export([create/0, add/3, add_idm/3, lookup/2, split/3, merge/2, size/1, foreach/2]).

%% Store is a list of tuple {Key, Value}

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

add_idm(Key, Value, Store) ->
    % Idempotent add: Remove any existing entry with this key, then add the new one
    [{Key, Value} | lists:keydelete(Key, 1, Store)].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    lists:foldl(fun({Key, Value}, {Updated, Rest}) ->
                   case key:between(Key, From, To) of
                       false -> {Updated, [{Key, Value} | Rest]};
                       true -> {[{Key, Value} | Updated], Rest}
                   end
                end,
                {[], []},
                Store).

merge(Entries, Store) ->
    lists:foldl(fun({Key, Value}, Acc) -> add(Key, Value, Acc) end, Store, Entries).

size(Store) ->
    length(Store).

foreach(Fun, Store) ->
    lists:foreach(fun({Key, Value}) -> Fun(Key, Value) end, Store).
