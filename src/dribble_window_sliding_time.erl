-module(dribble_window_sliding_time).

-include_lib("eep_app/include/eep_erl.hrl").

-export([
    new/5,
    push/2,
    tick/1]).

new(Size, AggMod, AggSeed, ClockMod, Interval) ->
    W0 = eep_window:sliding({clock, ClockMod, Interval}, Size, AggMod, AggSeed),
    C0 = W0#eep_win.clock,
    L0 = W0#eep_win.log,
    L1 = eep_winlog:tick(eep_clock:at(C0), L0),
    W0#eep_win{log=L1}.

push(Win, Event) ->
    case eep_window:decide([{accumulate, Event}], Win) of
        {noop, _}=Noop -> Noop;
        {{emit, _}, _} -> throw({emission,on,push,for,time,window})
    end.

tick(Win) ->
    case eep_window:tick(Win) of
        {noop, _}=Noop -> Noop;
        {{emit, _Emission}, Next} -> %% TODO Might need to also use Emission here?
            case expire_and_emit(Next) of
                {noop, _}=Noop -> Noop;
                {Emissions, #eep_win{aggmod=AggMod}=WinN} ->
                    Emissions2 = [
                        begin
                            [Res] = AggMod:emit(E),
                            Res
                        end|| {emit, E} <- Emissions
                      ],
                    {{emit, Emissions2}, WinN}
            end
    end.

expire_and_emit(#eep_win{aggmod=AMod, agg=Agg, size=S, log=Log, clock=C}=Win0) ->
    At = eep_clock:at(C),
    case eep_winlog:expire(At - S, Log) of
        {[], Current} -> %% Nothing expiring
            {noop, Win0#eep_win{log=Current}};
        {Expiring, Current} ->
            {Compensated, Emissions} = lists:foldl(fun(E, {A0, Ems}) ->
                                              A1 = AMod:compensate(A0, E),
                                              {A1, Ems++[{emit, A0}]}
                                      end,
                                      {Agg, []}, Expiring),
            {Emissions, Win0#eep_win{agg=Compensated, log=Current}}
    end.
