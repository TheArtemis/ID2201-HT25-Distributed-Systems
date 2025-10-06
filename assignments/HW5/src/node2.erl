-module(node2).

-export([start/1, start/2]).

-define(STABILIZE, 300).
-define(TIMEOUT, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        % Peer needs to know our jey
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % New node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % Predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % Syccessor informs us about it's predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        info ->
            io:format("Node ~w: Predecessor: ~w, Successor: ~w~n", [Id, Predecessor, Successor]),
            node(Id, Predecessor, Successor);
        die ->
            io:format("Node ~w: I'm being forced to die~n", [Id]),
            ok;
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Successor),
            node(Id, Predecessor, Successor);
        _ ->
            io:format("Node ~w: Unexpected message~n", [Id]),
            node(Id, Predecessor, Successor)
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

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            % We have no predecessor, so accept the new node
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % The new node is between our predecessor and us
                    % So it should be our new predecessor
                    {Nkey, Npid};
                false ->
                    % The new node is not between our predecessor and us
                    % So we keep our current predecessor
                    Predecessor
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
    Spid ! {probe, Ref, [self() | Nodes], T}.
