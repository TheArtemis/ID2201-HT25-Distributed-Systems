-module(dijkstra).
-export([table/2, route/2]).

% {berlin, 2, paris}

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0;
        {Node, Hops, _} -> Hops
    end.

replace(Node, N, Gateway, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        false ->
            Sorted;
        _ ->
            lists:keysort(2, [
                {Node, N, Gateway} | lists:keydelete(Node, 1, Sorted)
            ])
    end.

update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        Hops when N < Hops ->
            replace(Node, N, Gateway, Sorted);
        _ ->
            Sorted
    end.

%                                 \ DUMMY ENTRY \
% Sorted: [{paris, 0, paris}, {berlin, inf, unknown}]
% Map: [{paris, [berlin]}] -> Nodes that are directly Reachable
% Table: []

iterate([], _Map, _Gateways, Table) ->
    lists:reverse(Table);
iterate([{_, inf, _} | _], _Map, _Gateways, Table) ->
    lists:reverse(Table);
iterate([H | T], Map, Gateways, Table) ->
    {Node, N, Gateway} = H,

    % Find all the nodes in the sorted map that are reachable from the entry
    Reachable = map:reachable(Node, Map),

    Sorted = lists:foldl(
        fun(ReachNode, AccSorted) ->
            NewGateway =
                case lists:member(Node, Gateways) of
                    true -> Node;
                    false -> Gateway
                end,
            update(ReachNode, N + 1, NewGateway, AccSorted)
        end,
        T,
        Reachable
    ),

    %io:format("Current Sorted: ~p~nMap: ~p~nTable: ~p~n", [Sorted, Map, Table]),

    % The entry is then added to the Table
    iterate(Sorted, Map, Gateways, [{Node, Gateway} | Table]).

% Gateways: [paris, madrid],
% Map: [{madrid,[berlin]}, {paris, [rome,madrid]}]

table(Gateways, Map) ->
    % REMEMBER this was a issue: AllNodes = map:all_nodes(Map),
    AllNodes = lists:usort(Gateways ++ map:all_nodes(Map)),

    Initial = [
        {Node,
            case lists:member(Node, Gateways) of
                true -> 0;
                false -> inf
            end,
            case lists:member(Node, Gateways) of
                true -> Node;
                false -> unknown
            end}
     || Node <- AllNodes
    ],

    Sorted = lists:keysort(2, Initial),
    iterate(Sorted, Map, Gateways, []).

route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} -> {ok, Gateway};
        false -> notfound
    end.
