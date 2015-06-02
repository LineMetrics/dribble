-module(dribble_util).

-export([
    replace/3,
    replace_in_flows/4,
    enum_map/2,
    enum_filter/2,
    enum_foldl/3,
    heads_and_last/1,
    split/2,
    get_path/2,
    get_path/3,
    set_path/3]).

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

enum_map(F, List) when is_function(F, 1), is_list(List) ->
    lists:map(
        F,
        lists:zip(lists:seq(1, length(List)), List)).

enum_filter(F, List) when is_function(F, 1), is_list(List) ->
    Filtered = lists:filter(
        F,
        lists:zip(lists:seq(1, length(List)), List)),
    {_, Vals} = lists:unzip(Filtered),
    Vals.

enum_foldl(F, Acc, List) when is_function(F, 2), is_list(List) ->
    lists:foldl(F,
        Acc,
        lists:zip(lists:seq(1, length(List)), List)).

heads_and_last([]) -> {[], undefined};
heads_and_last(L) ->
    [Last|T] = lists:reverse(L),
    {lists:reverse(T), Last}.

split(X, List) when is_list(List) ->
    {Pre, Post} = lists:splitwith(fun(Y) -> X =/= Y end, List),
    case Post of
        [] -> {Pre, []};
        [_|T] -> {Pre, T}
    end.

get_path([Key], List) -> proplists:get_value(Key, List);
get_path([Key|T], List) ->
    Child = proplists:get_value(Key, List, []),
    get_path(T, Child).

get_path(Path, List, Default) ->
    case get_path(Path, List) of
        undefined -> Default;
        Other -> Other
    end.

set_path([Key], Val, List) -> lists:keystore(Key, 1, List, {Key, Val});
set_path([Key|T], Val, List) ->
    Child = proplists:get_value(Key, List, []),
    Child2 = set_path(T, Val, Child),
    lists:keystore(Key, 1, List, {Key, Child2}).
