-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Interface:
% Symbolic Name, Process Reference, Process Identifier

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | lists:keydelete(Name, 1, Intf)].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, _Ref, Pid} ->
            {ok, Pid};
        false ->
            notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, Ref, _Pid} ->
            {ok, Ref};
        false ->
            notfound
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, Ref, _Pid} ->
            {ok, Name};
        false ->
            notfound
    end.

list(Intf) ->
    [Name || {Name, _Ref, _Pid} <- Intf].

broadcast(Message, Intf) ->
    lists:foreach(
        fun
            ({_Name, _Ref, Pid}) when is_pid(Pid) ->
                Pid ! Message;
            ({_Name, _Ref, {RegName, Node}}) ->
                {RegName, Node} ! Message;
            (_) ->
                ok
        end,
        Intf
    ),
    ok.
