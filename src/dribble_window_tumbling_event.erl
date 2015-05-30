-module(dribble_window_tumbling_event).

-include_lib("eep_app/include/eep_erl.hrl").

-export([
    new/3,
    push/2,
    tick/1]).

new(Size, AggMod, AggSeed) ->
    eep_window:tumbling(event, Size, AggMod, AggSeed).

push(Win, Event) ->
    case eep_window:decide([{accumulate, Event}, tick], Win) of
        {{emit, Emission}, #eep_win{aggmod=AggMod}=Win2} ->
            {{emit, AggMod:emit(Emission)}, eep_window:reset(Win2)};
        Other -> Other
    end.

tick(Win) -> {noop, Win}.
