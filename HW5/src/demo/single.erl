-module(single).
-export([apple/1, probe/1, kill/1]).

apple(NodeModule) ->
    io:format("Starting apple/1 with NodeModule=~p~n", [NodeModule]),
    Id1 = key:generate(),
    Pid1 = NodeModule:start(Id1),
    register(apple1, Pid1),
    io:format("🍎 Apple1 node started with ID ~w~n", [Id1]).

probe(NodeName) ->
    io:format("~nSending probe from ~p...~n", [NodeName]),
    NodeAtom =
        netw:node_addr(
            netw:node_base(NodeName)),
    rpc:call(NodeAtom, erlang, send, [NodeName, probe]),
    ok.

kill(Node) ->
    NodeAddr =
        netw:node_addr(
            netw:node_base(Node)),
    {Node, NodeAddr} ! die.
