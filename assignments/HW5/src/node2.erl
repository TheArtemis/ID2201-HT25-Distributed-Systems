-module(node2).

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
    % Create the store once and thread it through the node loop
    Store = storage:create(),
    node(Id, Predecessor, Successor, Store).

connect(Id, nil) ->
    % We are the first node, so we are our own successor
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    % Connect to an existing ring
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            % Received the key from a node in the ring
            % That node (or its successor) will be our successor
            {ok, {Skey, Peer}}
    after ?TIMEOUT ->
        io:format("Time out: no response~n", []),
        {error, timeout}
    end.

% A Peer will be: {Key, Pid}

node(Id, Predecessor, Successor, Store) ->
    receive
        % Peer needs to know our key
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        % New node informs us of its existence
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Keep);
        % Predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        % Syccessor informs us about it's predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        info ->
            io:format("Node ~w: Predecessor: ~w, Successor: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor, Store);
        die ->
            io:format("Node ~w: I'm being forced to die~n", [Id]),
            ok;
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Successor),
            node(Id, Predecessor, Successor, Store);
        % Qref is used to tag the return message to the Client.
        % Client will be able to identify the reply message
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            % Merge received elements into our store (Entries, Store)
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Merged);
        _ ->
            io:format("Node ~w: Unexpected message~n", [Id]),
            node(Id, Predecessor, Successor, Store)
    end.

% Send stabilization message after a timer
schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        % Inform him of our existence
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        % If it's us we don't do anything
        {Id, _} ->
            Successor;
        % If it's himself we notify him of our existence
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        % Time to slide in
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    % Xkey is between us and our successor
                    % Adopt it as our new successor and stabilize again
                    stabilize(Pred, Id, {Xkey, Xpid});
                false ->
                    % We should be between Xkey and Skey
                    % Notify our successor of our existence
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % We have no predecessor, so accept the new node
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % The new node is between our predecessor and us
                    % So it should be our new predecessor
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    % The new node is not between our predecessor and us
                    % So we keep our current predecessor
                    {Predecessor, Store}
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

%% Probe functions to check ring connectivity

% Create a new probe and send it to our successor
create_probe(Id, Successor) ->
    {_Skey, SPid} = Successor,
    T = erlang:system_time(microsecond),
    SPid ! {probe, Id, [], T}.  % Start with empty list

% Remove (complete) a probe that came back to us
remove_probe(T, Nodes) ->
    Time = erlang:system_time(microsecond) - T,
    % Nodes list already contains all nodes except us, so just add ourselves
    AllNodes = [self() | Nodes],
    io:format("Probe completed in ~w microseconds. Nodes in ring: ~w~n",
              [Time, lists:reverse(AllNodes)]),
    io:format("Number of nodes: ~w~n", [length(AllNodes)]).

% Forward a probe that is not ours to our successor
forward_probe(Ref, T, Nodes, Successor) ->
    {_, Spid} = Successor,
    ?LOG andalso io:format("Node ~w: Got a probe from ~w~n", [self(), Ref]),
    Spid ! {probe, Ref, [self() | Nodes], T}.

%% Add and Lookup functions

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
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

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            ?LOG andalso io:format("Node ~w: looking up key ~w -> ~p~n", [Id, Key, Result]),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            ?LOG
            andalso io:format("Node ~w: Forwarding lookup request for key ~w to client ~w~n",
                              [Id, Key, Client]),
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.
