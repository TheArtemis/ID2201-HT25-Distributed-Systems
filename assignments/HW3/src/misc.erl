-module(misc).
-export([dump_to_csv/2, append_to_csv/2]).

dump_to_csv(File, Results) ->
    ensure_dir_exists(File),
    {ok, Io} = file:open(File, [write]),
    lists:foreach(
        fun({Idx, Tuple}) ->
            write_csv_row(Io, Idx, Tuple)
        end,
        lists:zip(lists:seq(1, length(Results)), Results)
    ),
    file:close(Io).

append_to_csv(File, Tuple) ->
    ensure_dir_exists(File),
    {ok, Io} = file:open(File, [append]),
    write_csv_row(Io, 0, Tuple),
    file:close(Io).

write_csv_row(Io, Idx, Tuple) ->
    io:format(Io, "~B", [Idx]),
    lists:foreach(
        fun(Elem) -> io:format(Io, ",~p", [Elem]) end,
        tuple_to_list(Tuple)
    ),
    io:format(Io, "~n", []).

ensure_dir_exists(File) ->
    Dir = filename:dirname(File),
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> filelib:ensure_dir(File)
    end.
