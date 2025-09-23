-module(hb_queue).
-export([new/0, add/4, get_safe/2, get_unsafe/2, sort/1, partition/2, size/1]).

% Create a new empty queue
new() ->
    [].

% Add a message to the queue in sorted order by timestamp
% Returns the updated queue
add(From, Time, Msg, Queue) ->
    sort([{From, Time, Msg} | Queue]).

% Partition the queue into safe and unsafe messages
% Returns {Safe, Unsafe} where both are sorted by vector time
partition(Queue, Clock) ->
    {Safe, Unsafe} = lists:partition(
        fun({_F, T, _M}) -> vect:safe(T, Clock) end,
        Queue
    ),
    {sort(Safe), sort(Unsafe)}.

% Sort the queue by vector time using happens-before relation
sort(Queue) ->
    lists:sort(
        fun({_F1, T1, _M1}, {_F2, T2, _M2}) ->
            % T1 should come before T2 if T1 happens-before T2
            vect:leq(T1, T2) andalso not vect:leq(T2, T1)
        end,
        Queue
    ).

get_safe(Queue, Clock) ->
    {Safe, _Unsafe} = partition(Queue, Clock),
    Safe.

get_unsafe(Queue, Clock) ->
    {_Safe, Unsafe} = partition(Queue, Clock),
    Unsafe.

size(Queue) ->
    length(Queue).
