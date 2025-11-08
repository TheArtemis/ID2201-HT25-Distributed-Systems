-module(hist).
-export([new/1, update/3]).

% If we know that we have seen message 15 from london, then we know that messages from london with a lower number are old and can be thrown away.

new(Name) ->
    [{Name, inf}].

update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {Node, Highest} when N =< Highest ->
            old;
        {Node, Highest} when N > Highest ->
            History1 = lists:keyreplace(Node, 1, History, {Node, N}),
            {new, History1};
        false ->
            {new, [{Node, N} | History]}
    end.
