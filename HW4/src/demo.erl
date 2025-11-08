-module(demo).

-export([start/1]).

-define(INITIAL_NODES, 3).
-define(MAX_NODES, 10).
-define(SLEEP, 2500).
-define(NODE_SPAWN_INTERVAL, 3000).

start(Module) ->
    ensure_module_loaded(Module),
    W1 = test:first(1, Module, ?SLEEP),
    io:format("Started node 1: ~p~n", [W1]),
    timer:sleep(500),
    Pids = build_initial_chain([W1], 2, ?INITIAL_NODES, Module),
    loop(Pids, ?INITIAL_NODES + 1, Module).

%% Build initial chain
build_initial_chain(Pids, Curr, Max, Module) when Curr =< Max ->
    LastNode = lists:last(Pids),
    NewPid = test:add(Curr, Module, LastNode, ?SLEEP),
    io:format("Added node ~p: ~p~n", [Curr, NewPid]),
    timer:sleep(100),
    build_initial_chain(Pids ++ [NewPid], Curr + 1, Max, Module);
build_initial_chain(Pids, _Curr, _Max, _Module) ->
    Pids.

%% Main loop
loop(Pids, NextNodeId, Module) ->
    Monitors = [erlang:monitor(process, Pid) || Pid <- Pids],
    receive
        {'DOWN', _Ref, process, Pid, Reason} ->
            %io:format("Node ~p died (reason: ~p)~n", [Pid, Reason]),
            NewPids = lists:delete(Pid, Pids),
            loop(NewPids, NextNodeId, Module)
    after ?NODE_SPAWN_INTERVAL ->
        [erlang:demonitor(M, [flush]) || M <- Monitors],
        AlivePids = [P || P <- Pids, is_process_alive(P)],
        case {AlivePids, length(AlivePids) < ?MAX_NODES} of
            {[], _} ->
                io:format("All nodes dead~n"),
                ok;
            {_, false} ->
                io:format("At max capacity (~p nodes)~n", [?MAX_NODES]),
                loop(AlivePids, NextNodeId, Module);
            {_, true} ->
                LastNode = lists:last(AlivePids),
                io:format("Adding node ~p (prev: ~p)~n", [NextNodeId, LastNode]),
                NewPid = test:add(NextNodeId, Module, LastNode, ?SLEEP),
                timer:sleep(500),
                case is_process_alive(NewPid) of
                    true ->
                        io:format("Node ~p successfully joined~n", [NextNodeId]),
                        loop(AlivePids ++ [NewPid], NextNodeId + 1, Module);
                    false ->
                        io:format("Node ~p failed to join (died immediately)~n", [NextNodeId]),
                        loop(AlivePids, NextNodeId + 1, Module)
                end
        end
    end.

%% Ensure module is loaded
ensure_module_loaded(Module) ->
    ModuleAtom =
        if is_atom(Module) ->
               Module;
           true ->
               list_to_atom(Module)
        end,
    case code:is_loaded(ModuleAtom) of
        {file, _} ->
            ok;
        false ->
            case code:load_file(ModuleAtom) of
                {module, ModuleAtom} ->
                    ok;
                _ ->
                    io:format("Error: Module ~p could not be loaded~n", [ModuleAtom]),
                    exit(module_not_loaded)
            end
    end.
