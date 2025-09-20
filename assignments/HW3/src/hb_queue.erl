-module(hb_queue).
-export([new/0, add/4, get_safe/2, get_unsafe/2, sort/1, partition/2, size/1]).

% Create a new empty queue
new() ->
    [].

% Add a message to the queue in sorted order by timestamp
% Returns the updated queue
add(From, Time, Msg, Queue) ->
    lists:keysort(2, [{From, Time, Msg} | Queue]).

% Partition the queue into safe and unsafe messages
% Returns {Safe, Unsafe} where both are sorted by timestamp
partition(Queue, Clock) ->
    {Safe, Unsafe} = lists:partition(
        fun({_F, T, _M}) -> logical_time:safe(T, Clock) end,
        Queue
    ),
    {lists:keysort(2, Safe), lists:keysort(2, Unsafe)}.

get_safe(Queue, Clock) ->
    {Safe, _Unsafe} = partition(Queue, Clock),
    Safe.

get_unsafe(Queue, Clock) ->
    {_Safe, Unsafe} = partition(Queue, Clock),
    Unsafe.

% Sort the queue by Time
sort(Queue) ->
    lists:keysort(2, Queue).

size(Queue) ->
    length(Queue).
