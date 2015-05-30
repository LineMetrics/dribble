-module(dribble_window_sliding_event).

-include_lib("eep_app/include/eep_erl.hrl").

-export([
    new/3,
    push/2,
    tick/1]).

new(Size, AggMod, AggSeed) ->
    eep_window:sliding(event, Size, AggMod, AggSeed).

push(Win, Event) ->
    case eep_window:decide([{accumulate, Event}, tick], Win) of
        {{emit, _}, #eep_win{count=C, size=S}=Win2} when C =< S ->
            {noop, Win2};
        {{emit, Emission}, #eep_win{aggmod=AggMod}=Win2} ->
            {{emit, AggMod:emit(Emission)}, eep_window:compensate(Win2)};
        Other -> Other
    end.

tick({_,_}=Win) -> {noop, Win}.
