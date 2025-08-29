-module(wait).
-export([hello/0]).

hello() ->
    receive
        X -> io:format("This is a message: ~s~n", [X])
    end.

% Pid = spawn(Mod, Func, Args) --- Arg can be [] for no args

% register(identifier, Pid).

% Pid ! "Message to Send"
