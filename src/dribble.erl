%% Main interface to dribble.
-module(dribble).

-include("dribble_int.hrl").

-export([
    new/1,
    push/3,
    push/4,
    tick/2,
    tick/3,
    tick_all/2,
    filter_audit/2,
    filter_audit/3]).

-define(valid_filters,      [filter, transform, branch, sink]).
-define(valid_audit_fields, [type, label, ctx, in, out]).

new(Algo) -> dribble_factory:to_beam(Algo).

push(DribbleCtx, PipeLabel, Event) ->
    {Sinks, Runtime, []} = push(DribbleCtx, PipeLabel, Event, false),
    {Sinks, Runtime}.

push(#dribble_ctx{meta=Meta, beam=Beam, runtime=Runtime}=DribbleCtx, PipeLabel, Event, ShouldAudit) ->
    Public = proplists:get_value(public, Meta),
    case lists:member(PipeLabel, Public) of
        true ->
            {_Res, #dribble_runtime{sinks=Sinks}=Runtime2, Audit} = beam_flow:push(Beam, PipeLabel, Event, Runtime, ShouldAudit),
            % reset sinks
            Runtime3 = Runtime2#dribble_runtime{sinks=[]},
            Ctx2 = DribbleCtx#dribble_ctx{runtime=Runtime3},
            {Sinks, Ctx2, Audit};
        false -> throw({non_public_pipe,PipeLabel})
    end.

tick_all(#dribble_ctx{meta=Meta}=DribbleCtx, ShouldAudit) ->
    Tickables = proplists:get_value(tickables, Meta, []),
    lists:foldl(
        fun(Tickable, {SinkAudits, Ctx}) ->
            {Sinks, Ctx2, Audit} = tick(Ctx, Tickable, ShouldAudit),
            {SinkAudits++{Sinks, Audit}, Ctx2}
        end,
        {[], DribbleCtx},
        Tickables).

tick(DribbleCtx, TickLabel) ->
    {Sinks, Runtime, []} = tick(DribbleCtx, TickLabel, false),
    {Sinks, Runtime}.

tick(#dribble_ctx{beam=Beam, runtime=Runtime}=DribbleCtx, TickLabel, ShouldAudit) ->
      % FIXME: validate it's a window
      {_Res, #dribble_runtime{sinks=Sinks}=Runtime2, Audit} = beam_flow:push(Beam, TickLabel, tick, Runtime, ShouldAudit),
      % reset sinks
      Runtime3 = Runtime2#dribble_runtime{sinks=[]},
      Ctx2 = DribbleCtx#dribble_ctx{runtime=Runtime3},
      {Sinks, Ctx2, Audit}.

filter_audit(Audit, Filters) ->
    % validate params
    case Filters -- ?valid_filters of
        [] -> ignore;
        InvalidFilters -> throw({invalid_audit_filters,InvalidFilters})
    end,
    % convert to sink
    Audit2 = [
        case A of
            {transform, {sink, SinkLabel}, Ctx, In, Out} -> {sink, SinkLabel, Ctx, In, Out};
            Other -> Other
        end
        || A <- Audit ],
    lists:filter(fun(X) -> lists:member(element(1, X), Filters) end, Audit2).

filter_audit(Audit, Filters, FieldFilters) ->
    Audit2 = filter_audit(Audit, Filters),
    % validate params
    case FieldFilters -- ?valid_audit_fields of
        [] -> ignore;
        InvalidFilters -> throw({invalid_audit_filters,InvalidFilters})
    end,
    % collect the audit fields
    [
        begin
            AsTuple = dribble_util:enum_foldl(
                fun({Ind, X}, Acc) ->
                    case lists:member(X, FieldFilters) of
                        true -> Acc ++ [element(Ind, A)];
                        false -> Acc
                    end
                end,
                [],
                ?valid_audit_fields),
            list_to_tuple(AsTuple)
        end
        || A <- Audit2
    ].
