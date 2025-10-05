-module(key).

-export([generate/0, between/3]).

-define(hash_max, 1000000000).

generate() ->
    rand:uniform(?hash_max).

between(Key, From, To) when From < To ->
    Key > From andalso Key =< To;
between(Key, From, To) when From =:= To ->
    Key =/= From;
between(Key, From, To) ->
    Key > To andalso Key < From.
