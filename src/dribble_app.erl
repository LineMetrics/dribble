-module(dribble_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    dribble_sup:start_link().

stop(_State) ->
    ok.
