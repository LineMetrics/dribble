-module(dribble_util).

-export([
    replace/3,
    replace_in_flows/4,
    enum_map/2,
    heads_and_last/1]).

replace(X1, X2, List) ->
    {Pre, Post} = lists:splitwith(fun(X) -> X =/= X1 end, List),
    case Post of
       [] -> Pre; % no replacement
       [X1 | RestPost] -> Pre ++ [X2] ++ RestPost
    end.

%% @doc used in rewiring plugins
replace_in_flows(PipeElem1, PipeElem2, FlowId, Flows) ->
    {FlowId, Vis, Pipe} = Flow = lists:keyfind(FlowId, 1, Flows),
    Pipe2 = dribble_util:replace(PipeElem1, PipeElem2, Pipe),
    Flow2 = {FlowId, Vis, Pipe2},
    dribble_util:replace(Flow, Flow2, Flows).

enum_map(F, List) when is_function(F, 2), is_list(List) ->
    [ F(Ind, X) || {Ind, X} <- lists:zip(lists:seq(1, length(List)), List) ].

heads_and_last([]) -> {[], undefined};
heads_and_last(L) ->
    [Last|T] = lists:reverse(L),
    {lists:reverse(T), Last}.
