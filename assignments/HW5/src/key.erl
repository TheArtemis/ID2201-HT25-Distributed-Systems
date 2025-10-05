-module(key).

-export([generate/0, between/3]).

-define(HASH_MAX_VALUE, 1000000000).

generate() ->
    rand:uniform(?HASH_MAX_VALUE).

between(Key, From, To) when From < To ->
    Key > From andalso Key =< To;
between(Key, From, To) when From =:= To ->
    Key =/= From;
between(Key, From, To) ->
    Key > To andalso Key < From.
