-module(test_world).
-export([
    setup_italy/0,
    setup_spain/0,
    update_italy/0,
    update_spain/0,
    status_all/0,
    update_all/0
]).
-define(HOST, "130.229.168.103").
-define(ITALY, list_to_atom("italy" ++ "@" ++ ?HOST)).
-define(SPAIN, list_to_atom("spain" ++ "@" ++ ?HOST)).
-define(SWEDEN, 'sweden@n128-p22.eduroam.kth.se').

%italy@130.229.168.103
%spain@130.229.168.103

setup_italy() ->
    io:format("Starting routers for Italy...~n"),
    routy:start(r1, rome),
    routy:start(r2, milan),
    routy:start(r3, turin),

    io:format("Sleeping for routers to initialize...~n"),
    timer:sleep(1000),

    io:format("Sending add messages to Italy routers...~n"),
    r1 ! {add, milan, {r2, ?ITALY}},
    r2 ! {add, rome, {r1, ?ITALY}},

    r1 ! {add, turin, {r3, ?ITALY}},
    r3 ! {add, rome, {r1, ?ITALY}},

    % Gateway to Spain
    r2 ! {add, barcelona, {r4, ?SPAIN}},
    %r3 ! {add, madrid, {r5, ?SPAIN}},
    % REMOVED GATEWAY TO MADRID

    io:format("Finished sending add messages to Italy routers.~n"),
    timer:sleep(1500),

    io:format("Sending gateway messages to Sweden from Italy...~n"),
    % Gateway to Sweden
    r3 ! {add, lulea, {r3, ?SWEDEN}},

    io:format("Waiting 1s to ensure messages are delivered...~n"),
    timer:sleep(1500),
    io:format("Broadcasting and updating all Italy routers...~n"),

    update_italy(),

    io:format("Italy setup complete.~n"),
    ok.

setup_spain() ->
    io:format("Starting routers for Spain...~n"),
    routy:start(r4, barcelona),
    routy:start(r5, madrid),

    io:format("Sleeping for routers to initialize...~n"),
    timer:sleep(1000),

    io:format("Sending add messages to Spain routers...~n"),
    r4 ! {add, madrid, {r5, ?SPAIN}},
    r5 ! {add, barcelona, {r4, ?SPAIN}},

    % Gateway to Italy
    r4 ! {add, milan, {r2, ?ITALY}},
    %r5 ! {add, turin, {r3, ?ITALY}},
    %REMOVED GATEWAY TO TURIN

    timer:sleep(1500),

    %Gateway to Sweden
    r5 ! {add, lulea, {r3, ?SWEDEN}},

    io:format("Waiting 1s to ensure messages are delivered...~n"),
    timer:sleep(1000),
    io:format("Broadcasting and updating all Spain routers...~n"),

    update_spain(),
    io:format("Spain setup complete.~n"),
    ok.

update_italy() ->
    lists:foreach(
        fun(R) ->
            R ! broadcast
        end,
        [r1, r2, r3]
    ),
    timer:sleep(1000),
    lists:foreach(
        fun(R) ->
            R ! update
        end,
        [r1, r2, r3]
    ).

update_spain() ->
    lists:foreach(
        fun(R) ->
            R ! broadcast
        end,
        [r4, r5]
    ),

    timer:sleep(1000),

    lists:foreach(
        fun(R) ->
            R ! update
        end,
        [r4, r5]
    ).

update_all() ->
    update_italy(),
    update_spain().

status_all() ->
    lists:foreach(
        fun(R) ->
            _ = msg:status({R, ?ITALY}, 1000)
        %io:format("Status from ~p on Italy: ~p~n", [R, Status])
        end,
        [r1, r2, r3]
    ),
    lists:foreach(
        fun(R) ->
            _ = msg:status({R, ?SPAIN}, 1000)
        %io:format("Status from ~p on Spain: ~p~n", [R, Status])
        end,
        [r4, r5]
    ).
