-module(key).

-export([generate/0, between/3]).

-define(HASH_MAX_VALUE, 1000000000).

generate() ->
    rand:uniform(?HASH_MAX_VALUE).

between(Key, From, To) when From < To ->
    % Check if Key is in (From, To]
    Key > From andalso Key =< To;
between(Key, From, To) when From =:= To ->
    % Everything except From is between
    Key =/= From;
between(Key, From, To) ->
    % From > To, check if Key is in (From, Max] or [1, To]
    Key > From orelse Key =< To.
