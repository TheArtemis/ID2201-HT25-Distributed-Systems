-module(gmsX).

-export([leader/6, slave/7, start/1, start/2, init/3, init/4]).

-define(timeout, 2000).
-define(arghh, 10000). % Once every 10000
-define(drop_rate, 100). % Drop 1 in 100 messages
-define(history_size, 100). % Keep last 100 messages for retransmit

start(Id) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    rand:seed(default, {Rnd, Rnd, Rnd}),
    leader(Id, Master, 1, [], [Master], []).

start(Id, Grp) ->
    Self = self(),
    Rnd = rand:uniform(1000),
    {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
    rand:seed(default, {Rnd, Rnd, Rnd}),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves], Group}, Slaves, Group);
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, 1, undefined, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

% Start a worker given:
% Id: a unique interger, only used for debugging
% Master: rocess identifier of the application layer
% Slaves: an ordered list of the process identifiers of all slaves in the group
% Group: a list of all application layer processes in the group

%                   %WRK   %SLAVES
leader(Id, Master, N, Slaves, Group, History) ->
    receive
        {mcast, Msg} ->
            Term = {msg, N, Msg},
            History2 = add_to_history(History, N, Term),
            bcast(Id, Term, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group, History2);
        % Wrk: Process Identifier of application layer
        % Peer: Process Identifier of the group process
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            Term = {view, N, [self() | Slaves2], Group2},
            History2 = add_to_history(History, N, Term),
            bcast(Id, Term, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2, History2);
        % Handle NACK requests for retransmission
        {nack, FromSeq, Slave} ->
            resend_from(History, FromSeq, Slave),
            leader(Id, Master, N, Slaves, Group, History);
        stop ->
            ok
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    erlang:monitor(process, Leader),
    receive
        % request from its master to multicast a message, the message is forwarded to the leader
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % request from the master to allow a new node to join the group, the message is forwarded to the leader
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % Case of a old message
        {msg, I, _} when I < N ->
            io:format("slave ~w: ignoring message with seq ~w, current ~w~n", [Id, I, N]),
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % Case of a old view message
        {view, I, _, _} when I < N ->
            io:format("slave ~w: ignoring message with seq ~w, current ~w~n", [Id, I, N]),
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % Gap detected in message sequence - request retransmission
        {msg, I, _} when I > N ->
            io:format("slave ~w: gap detected, expected ~w got ~w, sending NACK~n", [Id, N, I]),
            Leader ! {nack, N, self()},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % Gap detected in view sequence - request retransmission
        {view, I, _, _} when I > N ->
            io:format("slave ~w: gap detected, expected ~w got ~w, sending NACK~n", [Id, N, I]),
            Leader ! {nack, N, self()},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        % multicasted message from the leader. A message Msg is sent to the master
        {msg, N, Msg} -> % N: sequence number
            Master ! Msg,
            slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
        % a multicasted view from the leader. A view is delivered to the master process
        {view, N, [Leader | Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id,
                  Master,
                  Leader,
                  N + 1,
                  {view, N, [Leader | Slaves2], Group2},
                  Slaves2,
                  Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            ok
    end.

election(Id, Master, N, Last, Slaves, [_ | Group]) ->
    Self = self(),
    case Slaves of
        [Self | Rest] ->
            case Last of
                undefined ->
                    ok;
                _ ->
                    bcast(Id, Last, Rest)
            end,
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            io:format("leader ~w: I am the new leader!~n", [Id]),
            leader(Id, Master, N + 1, Rest, Group, []);
        [Leader | Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) ->
                     send_with_drop(Node, Msg),
                     crash(Id)
                  end,
                  Nodes).

% drop message with probability 1/?drop_rate
send_with_drop(Node, Msg) ->
    case rand:uniform(?drop_rate) of
        1 ->
            io:format("Message dropped to ~w: ~p~n", [Node, Msg]);
        _ ->
            Node ! Msg
    end.

% History management for NACK-based reliability
add_to_history(History, Seq, Term) ->
    NewHistory = [{Seq, Term} | History],
    case length(NewHistory) > ?history_size of
        true ->
            lists:sublist(NewHistory, ?history_size);
        false ->
            NewHistory
    end.

% Resend all messages with sequence >= FromSeq to requesting slave
resend_from(History, FromSeq, Dest) ->
    ToResend = [Term || {Seq, Term} <- History, Seq >= FromSeq],
    SortedResend = lists:reverse(ToResend), % Send in correct order
    lists:foreach(fun(Term) ->
                     io:format("Retransmitting ~p to ~w~n", [Term, Dest]),
                     Dest ! Term
                  end,
                  SortedResend).

crash(Id) ->
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
