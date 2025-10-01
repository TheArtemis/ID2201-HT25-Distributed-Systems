-module(gmsXPro).

-export([leader/6, slave/8, start/1, start/2, init/3, init/4]).

-define(timeout, 2000).
-define(arghh, 100). % Once every 100
-define(drop_rate, 100). % Drop 1 in 100 messages
-define(history_size, 100). % Keep last 100 messages for retransmit
-define(nack_crash_rate, 10).
-define(slave_buffer_size, 10). % Keep last 10 messages as slave

start(Id) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    rand:seed(default, {Rnd, Rnd, Rnd}),
    % initialize leader history using buffer module
    leader(Id, Master, 1, [], [Master], buffer:new(?history_size)).

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
            ViewMsg = {view, N, [Leader | Slaves], Group},
            SlaveBuffer =
                buffer:add(
                    buffer:new(?slave_buffer_size), {N, ViewMsg}),
            slave(Id, Master, Leader, N + 1, ViewMsg, Slaves, Group, SlaveBuffer);
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, 1, undefined, Slaves, Group, buffer:new(?slave_buffer_size))
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.

leader(Id, Master, N, Slaves, Group, History) ->
    io:format("leader ~w: seq = ~w~n", [Id, N]),
    receive
        {mcast, Msg} ->
            T = {msg, N, Msg},
            History2 = add_to_history(History, N, T),
            bcast(Id, T, Slaves),
            Master ! Msg,
            leader(Id, Master, N + 1, Slaves, Group, History2);
        % Wrk: Process Identifier of application layer
        % Peer: Process Identifier of the group process
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            T = {view, N, [self() | Slaves2], Group2},
            History2 = add_to_history(History, N, T),
            bcast(Id, T, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N + 1, Slaves2, Group2, History2);
        % Handle NACK requests for retransmission
        {nack, FromSeq, Slave} ->
            io:format("leader ~w: received NACK from ~w for seq >= ~w~n", [Id, Slave, FromSeq]),
            crash_on_nack(Id), % Crash before retransmitting
            resend_from(History, FromSeq, Slave),
            leader(Id, Master, N, Slaves, Group, History);
        stop ->
            ok
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer) ->
    erlang:monitor(process, Leader),
    receive
        % request from its master to multicast a message, the message is forwarded to the leader
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % request from the master to allow a new node to join the group, the message is forwarded to the leader
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % New leader asking for our history for recovery
        {history, NewLeader} ->
            NewLeader ! {history, buffer:to_list(Buffer), self()},
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % Case of a old message
        {msg, I, _} when I < N ->
            io:format("slave ~w: ignoring message with seq ~w, current ~w~n", [Id, I, N]),
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % Case of a old view message
        {view, I, _, _} when I < N ->
            io:format("slave ~w: ignoring message with seq ~w, current ~w~n", [Id, I, N]),
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % Gap detected in message sequence - request retransmission
        {msg, I, _} when I > N ->
            io:format("slave ~w: gap detected, expected ~w got ~w, sending NACK~n", [Id, N, I]),
            Leader ! {nack, N, self()},
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % Gap detected in view sequence - request retransmission
        {view, I, _, _} when I > N ->
            io:format("slave ~w: gap detected, expected ~w got ~w, sending NACK~n", [Id, N, I]),
            Leader ! {nack, N, self()},
            slave(Id, Master, Leader, N, Last, Slaves, Group, Buffer);
        % multicasted message from the leader. A message Msg is sent to the master
        {msg, N, Msg} -> % N: sequence number
            io:format("slave ~w: id seq = ~w~n", [Id, N]),
            Master ! Msg,
            MsgTerm = {msg, N, Msg},
            NewBuffer = buffer:add(Buffer, {N, MsgTerm}),
            slave(Id, Master, Leader, N + 1, MsgTerm, Slaves, Group, NewBuffer);
        % a multicasted view from the leader. A view is delivered to the master process
        {view, N, [Leader | Slaves2], Group2} ->
            io:format("slave ~w: id seq = ~w~n", [Id, N]),
            Master ! {view, Group2},
            ViewTerm = {view, N, [Leader | Slaves2], Group2},
            NewBuffer = buffer:add(Buffer, {N, ViewTerm}),
            slave(Id, Master, Leader, N + 1, ViewTerm, Slaves2, Group2, NewBuffer);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group, Buffer);
        stop ->
            ok
    end.

election(Id, Master, N, Last, Slaves, [_ | Group], Buffer) ->
    Self = self(),
    case Slaves of
        [Self | Rest] ->
            io:format("leader ~w: starting history recovery from ~w members~n", [Id, length(Rest)]),
            io:format("leader ~w: my buffer before recovery: ~p~n", [Id, buffer:to_list(Buffer)]),

            RecoveredHistory = collect_history_from_slaves(Rest, Buffer, 2000),

            io:format("leader ~w: final recovered history: ~p~n",
                      [Id, buffer:to_list(RecoveredHistory)]),
            io:format("leader ~w: recovered ~w entries in history~n",
                      [Id, buffer:size(RecoveredHistory)]),

            % Messages that were not in the slave buffer with a seq that's higher than the newest message in the buffer
            MissingMsgs =
                lists:filter(fun({Seq, _Term}) ->
                                buffer:find(Buffer, Seq) =:= undefined
                                andalso Seq > buffer:max_seq(Buffer)
                             end,
                             buffer:to_list(RecoveredHistory)),
            io:format("leader ~w: missing messages to retransmit: ~p~n", [Id, MissingMsgs]),

            % When MissingMsgs is non empty it means that the newly elected node had some lost messages
            lists:foreach(fun({_, T}) ->
                             case T of
                                 {msg, _N, Msg} -> Master ! Msg;
                                 {view, _N, _Leaders, Group2} -> Master ! {view, Group2};
                                 _ -> ok
                             end
                          end,
                          MissingMsgs),

            io:format("leader ~w: I am the new leader!~n", [Id]),
            leader(Id, Master, buffer:max_seq(RecoveredHistory) + 1, Rest, Group, RecoveredHistory);
        [Leader | Rest] ->
            erlang:monitor(process, Leader),
            io:format("slave ~w: last message: ~p~n", [Id, Last]),
            slave(Id, Master, Leader, N, Last, Rest, Group, Buffer)
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

collect_history_from_slaves(Slaves, OwnBuffer, Timeout) ->
    % Request history from all slaves
    lists:foreach(fun(Slave) -> Slave ! {history, self()} end, Slaves),
    % Collect responses and merge with own buffer
    AllEntries = wait_for_history(Slaves, buffer:to_list(OwnBuffer), Timeout),

    % Remove duplicates and sort by sequence number
    UniqueEntries = lists:usort(fun({SeqA, _}, {SeqB, _}) -> SeqA =< SeqB end, AllEntries),
    io:format("Merged and sorted ~w unique entries~n", [length(UniqueEntries)]),

    RecentEntries =
        case length(UniqueEntries) > ?history_size of
            true ->
                lists:nthtail(length(UniqueEntries) - ?history_size, UniqueEntries);
            false ->
                UniqueEntries
        end,

    io:format("Keeping ~w most recent entries~n", [length(RecentEntries)]),

    NewHistory = buffer:new(?history_size),
    lists:foldl(fun(Entry, Acc) -> buffer:add(Acc, Entry) end, NewHistory, RecentEntries).

wait_for_history([], Collected, _Timeout) ->
    Collected;
wait_for_history(Pending, Collected, Timeout) ->
    receive
        {history, SlaveHistory, From} ->
            % Append slave history to collected list
            NewCollected = Collected ++ SlaveHistory,
            wait_for_history(lists:delete(From, Pending), NewCollected, Timeout)
    after Timeout ->
        io:format("History recovery timeout, proceeding with ~w collected entries~n",
                  [length(Collected)]),
        Collected
    end.

add_to_history(History, Seq, T) ->
    buffer:add(History, {Seq, T}).

% Resend all messages with sequence >= FromSeq to requesting slave
resend_from(History, FromSeq, Dest) ->
    ToResend = buffer:to_resend(History, FromSeq),
    lists:foreach(fun(T) ->
                     io:format("Retransmitting ~p to ~w~n", [T, Dest]),
                     Dest ! T
                  end,
                  ToResend).

crash_on_nack(Id) ->
    case rand:uniform(?nack_crash_rate) of
        1 ->
            io:format("leader ~w: CRASH during NACK handling!~n", [Id]),
            exit(nack_crash);
        _ ->
            ok
    end.

crash(Id) ->
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
