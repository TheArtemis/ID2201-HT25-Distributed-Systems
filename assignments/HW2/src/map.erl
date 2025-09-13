-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% keyfind/3, keydelete/3, map/2 and foldl/3

new() ->
    [].

% map:update(berlin, [london, paris], []).
update(Node, Links, Map) ->
    [{Node, Links} | lists:keydelete(Node, 1, Map)].

% map:reachable(berlin, [{berlin, [london, paris]}]).
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, Links} -> Links;
        false -> []
    end.

all_nodes(Map) ->
    lists:usort([N || {Node, Links} <- Map, N <- [Node | Links]]).
