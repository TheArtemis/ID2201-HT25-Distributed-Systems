-module(node3).

-export([start/1, start/2]).

-define(STABILIZE, 300).
-define(TIMEOUT, 10000).
-define(LOG, true).

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
    node(Id, Predecessor, Successor, Next, Store).

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

node(Id, Predecessor, Successor, Next, Store) ->
    receive
        % Peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Next, Store);
        % New node informs us of its existence
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Next, Keep);
        % Predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor, Next),
            node(Id, Predecessor, Successor, Next, Store);
        % Syccessor informs us about it's predecessor
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Nxt, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Next, Store);
        info ->
            io:format("Node ~w: Predecessor: ~w, Successor: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Next, Store);
        die ->
            io:format("Node ~w: I'm being forced to die~n", [Id]),
            ok;
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Id, Nodes, T, KeyCounts} ->
            remove_probe(T, Nodes, KeyCounts, Store),
            node(Id, Predecessor, Successor, Next, Store);
        {probe, Ref, Nodes, T, KeyCounts} ->
            forward_probe(Ref, T, Nodes, KeyCounts, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store);
        % Qref is used to tag the return message to the Client.
        % Client will be able to identify the reply message
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Next, Store);
        {handover, Elements} ->
            % Merge received elements into our store (Entries, Store)
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Next, Merged);
        {'DOWN', Ref, process, _, _} ->
            ?LOG andalso io:format("Node ~w: Detected node down (Ref=~p)~n", [Id, Ref]),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Nxt, Store);
        _ ->
            io:format("Node ~w: Unexpected message~n", [Id]),
            node(Id, Predecessor, Successor, Next, Store)
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

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % We have no predecessor, so accept the new node
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, monitor(Npid), Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % The new node is between our predecessor and us
                    % So it should be our new predecessor
                    % De-monitor old predecessor, monitor new one
                    drop(Pref),
                    Keep = handover(Id, Store, Nkey, Npid),
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
    SPid ! {probe, Id, [], T, []}.  % Start with empty node and key count lists

% Remove (complete) a probe that came back to us
remove_probe(T, Nodes, KeyCounts, Store) ->
    Time = erlang:system_time(microsecond) - T,
    % Nodes list already contains all nodes except us, so just add ourselves
    AllNodes = [self() | Nodes],
    AllKeyCounts = [storage:size(Store) | KeyCounts],
    TotalKeys = lists:sum(AllKeyCounts),
    io:format("Probe completed in ~w microseconds. Nodes in ring: ~w~n",
              [Time, lists:reverse(AllNodes)]),
    io:format("Number of nodes: ~w~n", [length(AllNodes)]),
    io:format("Total number of keys in ring: ~w~n", [TotalKeys]).

% Forward a probe that is not ours to our successor
forward_probe(Ref, T, Nodes, KeyCounts, Successor, Store) ->
    {_, _, Spid} = Successor,
    ?LOG andalso io:format("Node ~w: Got a probe from ~w~n", [self(), Ref]),
    Spid ! {probe, Ref, [self() | Nodes], T, [storage:size(Store) | KeyCounts]}.

%% Add and Lookup functions

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            ?LOG andalso io:format("Node ~w: Adding key ~w with value ~w~n", [Id, Key, Value]),
            storage:add(Key, Value, Store);
        false ->
            ?LOG
            andalso io:format("Node ~w: Forwarding add request for key ~w to client ~w~n",
                              [Id, Key, Client]),
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            ?LOG andalso io:format("Node ~w: looking up key ~w -> ~p~n", [Id, Key, Result]),
            Client ! {Qref, Result};
        false ->
            {_, _, Spid} = Successor,
            ?LOG
            andalso io:format("Node ~w: Forwarding lookup request for key ~w to client ~w~n",
                              [Id, Key, Client]),
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).

% If our predecessor died we set it to nil and eventually we will become someone else's successor
down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
% If our successor dies and we have a Next node, adopt it as our new successor
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    Nref = monitor(Npid),
    self() ! stabilize,
    {Predecessor, {Nkey, Nref, Npid}, nil}.
