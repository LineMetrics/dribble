%% Main interface to dribble.
-module(dribble).

-export([
    new/1,
    push/3,
    push/4]).

-include("dribble_int.hrl").

new(Algo) -> dribble_factory:to_beam(Algo).

push(#dribble_ctx{}=DribbleCtx, PipeLabel, Event) ->
    {Sinks, Runtime, []} = push(DribbleCtx, PipeLabel, Event, false),
    {Sinks, Runtime}.

push(#dribble_ctx{public=Public, beam=Beam, runtime=Runtime}=DribbleCtx, PipeLabel, Event, ShouldAudit) ->
    case lists:member(PipeLabel, Public) of
        true ->
            {_Res, #dribble_runtime{sinks=Sinks}=Runtime2, Audit} = beam_flow:push(Beam, PipeLabel, Event, Runtime, ShouldAudit),
            % reset sinks
            Runtime3 = Runtime2#dribble_runtime{sinks=dribble_maps:new()},
            Ctx2 = DribbleCtx#dribble_ctx{runtime=Runtime3},
            {Sinks, Ctx2, Audit};
        false -> throw({non_public_pipe,PipeLabel})
    end.

