-module(logical_worker).
-export([start/5, stop/1, peers/2]).
start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).
stop(Worker) ->
    Worker ! stop.
init(Name, Log, Seed, Sleep, Jitter) ->
    rand:seed(exsplus, {Seed, Seed, Seed}),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, logical_time:zero());
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Clock) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            Clock1 = logical_time:recv(Name, Clock, Time),
            Log ! {log, Name, Clock1, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, Clock1);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = logical_time:inc(Name, Clock),
        % Use more unique message ID: combine timestamp with large random number
        MessageId = rand:uniform(1000000),
        Message = {hello, MessageId},
        Selected ! {msg, Time, Message},
        jitter(Jitter),

        Log ! {log, Name, Time, {sending, Message}},

        loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
