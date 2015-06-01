-module(dribble_window_tumbling_time).

-include_lib("eep_app/include/eep_erl.hrl").

-export([
    new/5,
    push/2,
    tick/1]).

new(Size, AggMod, AggSeed, ClockMod, Interval) ->
    eep_window:tumbling({clock, ClockMod, Interval}, Size, AggMod, AggSeed).

push(Win, Event) ->
    case eep_window:decide([{accumulate, Event}], Win) of
        {noop, _}=Noop -> Noop;
        {{emit, _}, _} -> throw({emission,on,push,for,time,window})
    end.

tick(Win) ->
    case eep_window:tick(Win) of
        {noop, _}=Noop ->
            Noop;
        {{emit, Emission}, #eep_win{aggmod=AggMod}=Next} ->
            {{emit, AggMod:emit(Emission)}, eep_window:reset(Next)}
    end.
