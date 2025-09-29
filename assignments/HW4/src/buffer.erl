-module(buffer).

-export([new/1, add/2, remove/2, to_list/1, to_resend/2, has_seq/2, size/1, head/1]).

% Buffer represented as {Max, [{Seq, Term} | ...]} newest-first

new(Max) when is_integer(Max), Max > 0 ->
    {Max, []}.

add({Max, List} = Buf, {Seq, Term}) when is_integer(Seq) ->
    case lists:keymember(Seq, 1, List) of
        true ->
            Buf; % already present, keep as is
        false ->
            New = [{Seq, Term} | List],
            case length(New) > Max of
                true ->
                    {Max, lists:sublist(New, Max)};
                false ->
                    {Max, New}
            end
    end.

remove({Max, List}, Seq) ->
    {Max, [X || X <- List, element(1, X) =/= Seq]}.

to_list({_, List}) ->
    List.

to_resend({_, List}, FromSeq) when is_integer(FromSeq) ->
    Filtered = [{Seq, Term} || {Seq, Term} <- List, Seq >= FromSeq],
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A < B end, Filtered),
    [Term || {_, Term} <- Sorted].

has_seq({_, List}, Seq) ->
    lists:keymember(Seq, 1, List).

size({_, List}) ->
    length(List).

head({_, []}) ->
    undefined;
head({_, [{Seq, Term} | _]}) ->
    {Seq, Term}.
