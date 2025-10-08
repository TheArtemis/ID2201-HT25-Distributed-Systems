-module(ring).

-export([apple/1, banana/1, cherry/1, pear/1]).
-export([info/0, probe/1, kill/1]).



apple(NodeModule) ->
    io:format("Starting apple/1 with NodeModule=~p~n", [NodeModule]),

    Id1 = key:generate(),
    Id2 = key:generate(),
    Pid1 = NodeModule:start(Id1),
    register(apple1, Pid1),
    io:format("🍎 Apple1 node started with ID ~w~n", [Id1]),
    
    Pid2 = NodeModule:start(Id2, Pid1),
    register(apple2, Pid2),
    io:format("🍎 Apple2 node started with ID ~w~n", [Id2]),
    [Id1, Id2].

banana(NodeModule) ->
    io:format("Starting banana/1 with NodeModule=~p~n", [NodeModule]),
    Id1 = key:generate(),
    Id2 = key:generate(),
    ApplePid = rpc:call(netw:node_addr(apple), erlang, whereis, [apple1]),
    
    Pid1 = NodeModule:start(Id1, ApplePid),
    register(banana1, Pid1),
    io:format("🍌 Banana1 node started with ID ~w~n", [Id1]),
    
    Pid2 = NodeModule:start(Id2, Pid1),
    register(banana2, Pid2),
    io:format("🍌 Banana2 node started with ID ~w~n", [Id2]),
    [Id1, Id2].

cherry(NodeModule) ->
    io:format("Starting cherry/1 with NodeModule=~p~n", [NodeModule]),
    Id1 = key:generate(),
    Id2 = key:generate(),
    ApplePid = rpc:call(netw:node_addr(apple), erlang, whereis, [apple1]),
    
    Pid1 = NodeModule:start(Id1, ApplePid),
    register(cherry1, Pid1),
    io:format("🍒 Cherry1 node started with ID ~w~n", [Id1]),
    
    Pid2 = NodeModule:start(Id2, Pid1),
    register(cherry2, Pid2),
    io:format("🍒 Cherry2 node started with ID ~w~n", [Id2]),
    [Id1, Id2].

pear(NodeModule) ->
    io:format("Starting pear/1 with NodeModule=~p~n", [NodeModule]),
    Id1 = key:generate(),
    Id2 = key:generate(),
    ApplePid = rpc:call(netw:node_addr(apple), erlang, whereis, [apple1]),
    
    Pid1 = NodeModule:start(Id1, ApplePid),
    register(pear1, Pid1),
    io:format("🍐 Pear1 node started with ID ~w~n", [Id1]),
    
    Pid2 = NodeModule:start(Id2, Pid1),
    register(pear2, Pid2),
    io:format("🍐 Pear2 node started with ID ~w~n", [Id2]),
    [Id1, Id2].

info() ->
    io:format("~nRing Information:~n"),
    rpc:call(netw:node_addr(apple), erlang, send, [apple1, info]),
    rpc:call(netw:node_addr(apple), erlang, send, [apple2, info]),
    rpc:call(netw:node_addr(banana), erlang, send, [banana1, info]),
    rpc:call(netw:node_addr(banana), erlang, send, [banana2, info]),
    rpc:call(netw:node_addr(cherry), erlang, send, [cherry1, info]),
    rpc:call(netw:node_addr(cherry), erlang, send, [cherry2, info]),
    rpc:call(netw:node_addr(pear), erlang, send, [pear1, info]),
    rpc:call(netw:node_addr(pear), erlang, send, [pear2, info]),
    ok.

probe(NodeName) ->
    io:format("~nSending probe from ~p...~n", [NodeName]),
    NodeAtom = netw:node_addr(netw:node_base(NodeName)),
    rpc:call(NodeAtom, erlang, send, [NodeName, probe]),
    ok.

kill(Node) ->
    NodeAddr =
        netw:node_addr(
            netw:node_base(Node)),
    {Node, NodeAddr} ! die.


