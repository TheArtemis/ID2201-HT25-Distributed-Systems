-module(misc).
-export([dump_to_csv/2]).

dump_to_csv(File, Results) ->
    {ok, Io} = file:open(File, [write]),
    io:format(Io, "Run,RTT_microseconds~n", []),
    lists:foreach(
        fun({Idx, Val}) ->
            io:format(Io, "~B,~B~n", [Idx, Val])
        end,
        lists:zip(lists:seq(1, length(Results)), Results)
    ),
    file:close(Io).
