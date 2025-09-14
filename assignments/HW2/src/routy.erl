-module(routy).
-export([start/2, stop/1, init/1]).

% Name will be a unique identifier for the running router on the node

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    io:format("Router ~w started~n", [Name]),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} ->
            % Monitor
            % A monitors B:

            % B dies and A gets exit signal
            % A dies, B won't get exit signal

            Ref = erlang:monitor(process, Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    %Table1 = dijkstra:table(intf:list(Intf), Map1),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        % Message is ours
        {route, Name, _From, Message} ->
            io:format("~w: received message ~p ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        % Message should be forwarded
        {route, To, From, Message} ->
            io:format("~w: routing message ~p ~n", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    io:format("~w: gateway resolved to ~p~n", [Name, Gw]),
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            Pid ! {route, To, From, Message};
                        notfound ->
                            io:format("~w: gateway ~p not found in interfaces~n", [Name, Gw]),
                            ok
                    end;
                notfound ->
                    io:format("~w: no route to ~p~n", [Name, To]),
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        % Send a message
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N + 1, Hist, Intf, Table, Map);
        stop ->
            ok
    end.
