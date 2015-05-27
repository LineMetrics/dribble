% Proplists like map implementation.
-module(dribble_maps).

-export([
    new/0,
    new/1,
    put/3,
    keys/1,
    values/1,
    is_key/2,
    size/1,
    from_list/1,
    to_list/1,
    with/2,
    without/2,
    get/2,
    get/3,
    remove/2]).
    
new()                      -> [].
new(Map) when is_list(Map) -> Map.
put(Key, Value, Map)       -> lists:keystore(Key, 1, Map, {Key, Value}).
keys(Map)                  -> [ Key || {Key, _} <- Map ]. 
values(Map)                -> [ Value || {_, Value} <- Map ]. 
is_key(Key, Map)           -> lists:keymember(Key, 1, Map). 
size(Map)                  -> length(Map).
from_list(Map)             -> Map.
to_list(List)              -> List.
with(Keys, Map)            -> [ {K,V} || {K,V} <- Map, Key <- Keys, K =:= Key ].
without(Keys, Map)         -> Map -- with(Keys, Map).

get(Key, Map) when is_list(Map) ->
    case lists:keyfind(Key, 1, Map) of
        {_, Value} -> Value;
        false -> error(bad_key)
    end.

get(Key, Map, Default) when is_list(Map) ->
    case lists:keyfind(Key, 1, Map) of
        {_, Value} -> Value;
        false -> Default
    end.

remove(Key, Map) ->
    case lists:keytake(Key, 1, Map) of
        {_, _, Map2} -> Map2;
        false -> Map
    end.

