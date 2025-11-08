-module(misc).
-export([dump_to_csv/2]).

dump_to_csv(File, Results) ->
    {ok, Io} = file:open(File, [write]),
    lists:foreach(
        fun({Idx, Tuple}) ->
            io:format(Io, "~B", [Idx]),
            lists:foreach(
                fun(Elem) -> io:format(Io, ",~p", [Elem]) end,
                tuple_to_list(Tuple)
            ),
            io:format(Io, "~n", [])
        end,
        lists:zip(lists:seq(1, length(Results)), Results)
    ),
    file:close(Io).
