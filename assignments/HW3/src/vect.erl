-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, recv/3, clock/1, update/3, safe/2]).

% Vectors are in the shape of
% [{john, 3}, {ringo, 2}, {paul, 4}, {george, 1}]

zero() ->
    [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, T} ->
            lists:keyreplace(Name, 1, Time, {Name, T + 1});
        false ->
            [{Name, 1} | Time]
    end.

merge([], Time) ->
    Time;
merge([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, erlang:max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti} | merge(Rest, Time)]
    end.

leq([], _) ->
    true;
leq([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} when Ti =< Tj ->
            leq(Rest, Time);
        _ ->
            false
    end.

recv(Name, Local, Remote) ->
    inc(Name, merge(Local, Remote)).

% Nodes is list of Nodes
% Clock is a list of tuples.
% Clock: [{Node, Time}, ...]

clock(_) ->
    [].

update(Node, Time, Clock) ->
    % Extract the node's timestamp from the vector clock
    NodeTime =
        case lists:keyfind(Node, 1, Time) of
            {Node, T} -> T;
            % If node not in vector, treat as 0
            false -> 0
        end,

    % Update or add the node's timestamp in our clock
    case lists:keyfind(Node, 1, Clock) of
        {Node, OldTime} ->
            % Take the maximum of old and new timestamp for this node
            NewTime = erlang:max(OldTime, NodeTime),
            lists:keyreplace(Node, 1, Clock, {Node, NewTime});
        false ->
            [{Node, NodeTime} | Clock]
    end.

% When is it safe to print?
safe(Time, Clock) ->
    lists:all(
        fun({Name, Ti}) ->
            case lists:keyfind(Name, 1, Clock) of
                {Name, Tj} when Ti =< Tj -> true;
                _ -> false
            end
        end,
        Time
    ).
