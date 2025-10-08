-module(node4).

-export([start/1, start/2]).

-define(STABILIZE, 300).
-define(TIMEOUT, 10000).
-define(LOG, true).
-define(LOG_LOOKUP, true).
-define(LOG_ADD, true).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store = storage:create(),
    Next = nil,
    Replica = storage:create(),
    node(Id, Predecessor, Successor, Next, Store, Replica).

connect(Id, nil) ->
    % We are the first node, so we are our own successor
    {ok, {Id, monitor(self()), self()}};
connect(_Id, Peer) ->
    % Connect to an existing ring
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            % Received the key from a node in the ring
            % That node (or its successor) will be our successor
            {ok, {Skey, monitor(Peer), Peer}}
    after ?TIMEOUT ->
        io:format("Time out: no response~n", []),
        {error, timeout}
    end.

% A Peer will be: {Key, Ref, Pid}

node(Id, Predecessor, Successor, Next, Store, Replica) ->
    receive
        % Peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store, Replica);
        % New node informs us of its existence
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store, Successor),
            % Clear replica since we have a new predecessor who will send us new replicas
            NewReplica = storage:create(),
            node(Id, Pred, Successor, Next, Keep, NewReplica);
        % Predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor, Next),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        % Syccessor informs us about it's predecessor
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            % If successor changed, re-replicate our store
            case Succ of
                Successor ->
                    ok;  % Same successor, no change
                {_, _, NewSpid} ->
                    % New successor - send our entire store as replica
                    NewSpid ! {set_replica, Store}
            end,
            node(Id, Predecessor, Succ, Nxt, Store, Replica);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        info ->
            io:format("Node ~w: Predecessor: ~w, Successor: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        die ->
            io:format("Node ~w: I'm being forced to die~n", [Id]),
            ok;
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {probe, Id, Nodes, T, KeyCounts, ReplicaCounts} ->
            remove_probe(T, Nodes, KeyCounts, ReplicaCounts, Store, Replica),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {probe, Ref, Nodes, T, KeyCounts, ReplicaCounts} ->
            forward_probe(Ref, T, Nodes, KeyCounts, ReplicaCounts, Successor, Store, Replica),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        % Qref is used to tag the return message to the Client.
        % Client will be able to identify the reply message
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added, Replica);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store, Replica);
        {handover, Elements} ->
            % Merge received elements into our store (Entries, Store)
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Next, Merged, Replica);
        {'DOWN', Ref, process, _, _} ->
            ?LOG andalso io:format("Node ~w: Detected node down (Ref=~p)~n", [Id, Ref]),
            {Pred, Succ, Nxt, NewStore, NewReplica} =
                down(Ref, Predecessor, Successor, Next, Store, Replica),
            node(Id, Pred, Succ, Nxt, NewStore, NewReplica);
        {replicate, Key, Value} ->
            ?LOG
            andalso io:format("Node ~w: Received replica: Key=~w, Value=~w~n", [Id, Key, Value]),
            AddedReplica = storage:add_idm(Key, Value, Replica),
            node(Id, Predecessor, Successor, Next, Store, AddedReplica);
        {set_replica, NewReplica} ->
            ?LOG
            andalso io:format("Node ~w: Setting replica (size=~w)~n",
                              [Id, storage:size(NewReplica)]),
            node(Id, Predecessor, Successor, Next, Store, NewReplica);
        {request_replica, Pid} ->
            ?LOG
            andalso io:format("Node ~w: Sending replica (size=~w) to ~w~n",
                              [Id, storage:size(Store), Pid]),
            Pid ! {set_replica, Store},
            node(Id, Predecessor, Successor, Next, Store, Replica);
        _ ->
            io:format("Node ~w: Unexpected message~n", [Id]),
            node(Id, Predecessor, Successor, Next, Store, Replica)
    end.

% Send stabilization message after a timer
schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Nx, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        % Inform him of our existence
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Nx};
        % If it's us we don't do anything
        {Id, _} ->
            {Successor, Nx};
        % If it's himself we notify him of our existence
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            {Successor, Nx};
        % Time to slide in
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    % Xkey is between us and our successor
                    % Adopt X as our new successor
                    % Our old successor becomes our next node
                    % De-monitor old successor, monitor new one
                    drop(Sref),
                    Xpid ! {notify, {Id, self()}},
                    {{Xkey, monitor(Xpid), Xpid}, Successor};
                false ->
                    % We should be between Xkey and Skey
                    % Notify our successor of our existence
                    Spid ! {notify, {Id, self()}},
                    {Successor, Nx}
            end
    end.

notify({Nkey, Npid}, Id, Predecessor, Store, Successor) ->
    {_, _, Spid} = Successor,
    case Predecessor of
        nil ->
            % We have no predecessor, so accept the new node
            Keep = handover(Id, Store, Nkey, Npid),
            % Send our remaining keys as replica to our successor
            Spid ! {set_replica, Keep},
            % Request replica from our new predecessor
            Npid ! {request_replica, self()},
            {{Nkey, monitor(Npid), Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % The new node is between our predecessor and us
                    % So it should be our new predecessor
                    % De-monitor old predecessor, monitor new one
                    drop(Pref),
                    Keep = handover(Id, Store, Nkey, Npid),
                    % Send our remaining keys as replica to our successor
                    Spid ! {set_replica, Keep},
                    % Request replica from our new predecessor
                    Npid ! {request_replica, self()},
                    {{Nkey, monitor(Npid), Npid}, Keep};
                false ->
                    % The new node is not between our predecessor and us
                    % So we keep our current predecessor
                    {Predecessor, Store}
            end
    end.

request(Peer, Predecessor, Next) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, Next};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, Next}
    end.

%% Probe functions to check ring connectivity

% Create a new probe and send it to our successor
create_probe(Id, Successor) ->
    {_Skey, _Sref, SPid} = Successor,
    T = erlang:system_time(microsecond),
    SPid
    ! {probe, Id, [], T, [], []}.  % Start with empty node, key count, and replica count lists

% Remove (complete) a probe that came back to us
remove_probe(T, Nodes, KeyCounts, ReplicaCounts, Store, Replica) ->
    Time = erlang:system_time(microsecond) - T,
    % Nodes list already contains all nodes except us, so just add ourselves
    AllNodes = [self() | Nodes],
    AllKeyCounts = [storage:size(Store) | KeyCounts],
    AllReplicaCounts = [storage:size(Replica) | ReplicaCounts],
    TotalKeys = lists:sum(AllKeyCounts),
    TotalReplicas = lists:sum(AllReplicaCounts),
    io:format("Probe completed in ~w microseconds. Nodes in ring: ~w~n",
              [Time, lists:reverse(AllNodes)]),
    io:format("Number of nodes: ~w~n", [length(AllNodes)]),
    io:format("Total number of keys in ring: ~w~n", [TotalKeys]),
    io:format("Total number of replica keys in ring: ~w~n", [TotalReplicas]),
    lists:foreach(fun({Node, KeyCount, ReplicaCount}) ->
                     io:format("Node: ~p, Keys: ~p, Replica: ~p~n", [Node, KeyCount, ReplicaCount])
                  end,
                  lists:reverse(
                      lists:zip3(AllNodes, AllKeyCounts, AllReplicaCounts))).

% Forward a probe that is not ours to our successor
forward_probe(Ref, T, Nodes, KeyCounts, ReplicaCounts, Successor, Store, Replica) ->
    {_, _, Spid} = Successor,
    ?LOG andalso io:format("Node ~w: Got a probe from ~w~n", [self(), Ref]),
    Spid
    ! {probe,
       Ref,
       [self() | Nodes],
       T,
       [storage:size(Store) | KeyCounts],
       [storage:size(Replica) | ReplicaCounts]}.

%% Add and Lookup functions

add(Key, Value, Qref, Client, Id, Predecessor, {_, _, Spid}, Store) ->
    case Predecessor of
        nil ->
            % We are the only node or have no predecessor yet
            Client ! {Qref, ok},
            ?LOG_ADD andalso io:format("Node ~w: Adding key ~w with value ~w~n", [Id, Key, Value]),
            Added = storage:add(Key, Value, Store),
            % Replicate to successor
            Spid ! {replicate, Key, Value},
            Added;
        {Pkey, _, _} ->
            case key:between(Key, Pkey, Id) of
                true ->
                    Client ! {Qref, ok},
                    ?LOG_ADD
                    andalso io:format("Node ~w: Adding key ~w with value ~w~n", [Id, Key, Value]),
                    Added = storage:add(Key, Value, Store),
                    % Replicate to successor
                    Spid ! {replicate, Key, Value},
                    Added;
                false ->
                    ?LOG_ADD
                    andalso io:format("Node ~w: Forwarding add request for key ~w to client ~w~n",
                                      [Id, Key, Client]),
                    Spid ! {add, Key, Value, Qref, Client},
                    Store
            end
    end.

lookup(Key, Qref, Client, Id, Predecessor, Successor, Store) ->
    ?LOG_LOOKUP
    andalso io:format("Node ~w: Lookup called for key ~w, Store size=~w, Pred=~p~n",
                      [Id, Key, storage:size(Store), Predecessor]),
    case Predecessor of
        nil ->
            % We are the only node or have no predecessor yet
            Result = storage:lookup(Key, Store),
            ?LOG_LOOKUP andalso io:format("Node ~w: looking up key ~w -> ~p~n", [Id, Key, Result]),
            Client ! {Qref, Result};
        {Pkey, _, _} ->
            case key:between(Key, Pkey, Id) of
                true ->
                    Result = storage:lookup(Key, Store),
                    ?LOG_LOOKUP
                    andalso io:format("Node ~w: looking up key ~w -> ~p~n", [Id, Key, Result]),
                    Client ! {Qref, Result};
                false ->
                    {_, _, Spid} = Successor,
                    ?LOG_LOOKUP
                    andalso io:format("Node ~w: Forwarding lookup request for key ~w to client ~w~n",
                                      [Id, Key, Client]),
                    Spid ! {lookup, Key, Qref, Client}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    % Split the store: Keep keys in range (Nkey, Id], send rest to new predecessor
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Ref) ->
    try
        erlang:demonitor(Ref, [flush])
    catch
        error:badarg ->
            % Monitor reference already invalid/cleaned up - that's ok
            ok
    end.

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
    % Predecessor died - we now own the replica data
    ?LOG
    andalso io:format("Node: Predecessor died, merging replica (size=~w) into store "
                      "(size=~w)~n",
                      [storage:size(Replica), storage:size(Store)]),
    MergedStore = storage:merge(Replica, Store),
    ?LOG andalso io:format("Node: After merge, store size=~w~n", [storage:size(MergedStore)]),
    NewReplica = storage:create(),

    % Re-replicate the merged store to our successor to maintain replication invariant
    {_, _, Spid} = Successor,
    ?LOG andalso io:format("Node: Re-replicating merged store to successor~n"),
    Spid ! {set_replica, MergedStore},

    {nil, Successor, Next, MergedStore, NewReplica};
down(Ref, nil, {_, Ref, _}, nil, Store, Replica) ->
    % Successor died, we have no predecessor or next - we're alone
    % Point to ourselves
    Sref = monitor(self()),
    MyId = self(),
    {nil, {MyId, Sref, self()}, nil, Store, Replica};
down(Ref, nil, {_, Ref, _}, {Nkey, Nref, Npid}, Store, Replica) ->
    % Successor died, we have no predecessor but we have Next
    case Nref of
        Ref ->
            % Next is also the dead node - we're alone now
            Sref = monitor(self()),
            MyId = self(),
            {nil, {MyId, Sref, self()}, nil, Store, Replica};
        _ ->
            % Next is alive - adopt it as new successor
            Npid ! {set_replica, Store},
            self() ! stabilize,
            {nil, {Nkey, Nref, Npid}, nil, Store, Replica}
    end;
down(Ref, Predecessor, {_, Ref, _}, nil, Store, Replica) ->
    % Successor died but we have no Next - we're now alone in the ring
    % Point to ourselves
    {Pkey, _, _} = Predecessor,
    Sref = monitor(self()),
    {Predecessor, {Pkey, Sref, self()}, nil, Store, Replica};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Nref, Npid}, Store, Replica) ->
    % Check if Next is also down (same Ref) - this can happen in small rings
    case Nref of
        Ref ->
            % Next is also the dead node - point to ourselves as successor
            {Pkey, _, _} = Predecessor,
            Sref = monitor(self()),
            {Predecessor, {Pkey, Sref, self()}, nil, Store, Replica};
        _ ->
            % Next is alive - adopt it as new successor
            % Send all our store entries to new successor for replication
            Npid ! {set_replica, Store},
            self() ! stabilize,
            {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}
    end.
