-module(dribble_maps_SUITE).

-compile(export_all).

all() -> [ {group, maps} ].

groups() ->
    [
        {maps, [], [ t_ops ]}
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 1000}].

t_ops(_Config) ->
    [] = dribble_maps:new(),
    [{a,1}] = dribble_maps:from_list([{a,1}]),
    [{a,1}] = dribble_maps:to_list([{a,1}]),
    [{a,1}] = Map1 = dribble_maps:new([{a,1}]),
    1 = dribble_maps:get(a, Map1),
    {'EXIT', {bad_key, _}} = (catch dribble_maps:get(b, Map1)),
    -999 = (catch dribble_maps:get(b, Map1, -999)),
    Map2 = dribble_maps:put(b, 2, Map1),
    2 = dribble_maps:get(b, Map2),
    [a, b] = dribble_maps:keys(Map2),
    [1, 2] = dribble_maps:values(Map2),
    true = dribble_maps:is_key(a, Map2),
    false = dribble_maps:is_key(bogus, Map2),
    Map3 = dribble_maps:put(c, 3, Map2),
    3 = dribble_maps:get(c, Map3),
    3 = dribble_maps:size(Map3),
    Map4 = dribble_maps:put(c, 33, Map3),
    33 = dribble_maps:get(c, Map4),
    3 = dribble_maps:size(Map4),
    [{a,1}, {c,33}] = dribble_maps:with([a,c], Map4),
    [{b,2}] = dribble_maps:without([a,c], Map4).
