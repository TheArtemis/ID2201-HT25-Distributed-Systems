-module(netw).

-export([hostname/0, node_addr/1, node_base/1]).

hostname() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom(Hostname).

node_addr(Name) ->
    list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(hostname())).

node_base(NodeName) when is_atom(NodeName) ->
    list_to_atom(lists:takewhile(fun(C) -> C >= $a andalso C =< $z end,
                                 atom_to_list(NodeName))).
