-module(dribble_plugin_window_agg).

-include("dribble_int.hrl").
-include_lib("eep_app/include/eep_erl.hrl").

-behaviour(eep_aggregate).

-export([
    init/0,
    init/1,
    accumulate/2,
    compensate/2,
    emit/1]).

init(#agg_ctx{acc_cb=AccCb, comp_cb=CompCb, emit_cb=EmitCb}=Ctx)
    when (AccCb  == undefined orelse is_function(AccCb, 2)),
         (CompCb == undefined orelse is_function(CompCb, 2)),
         (EmitCb == undefined orelse is_function(EmitCb, 1)) -> Ctx.
init() -> throw(not_supported). % must use seedable aggregate

accumulate(#agg_ctx{acc_cb=undefined, state=State}=Ctx, Event) when is_list(State) ->
    Ctx#agg_ctx{state=State++[Event]};
accumulate(#agg_ctx{acc_cb=AccCb, state=State}=Ctx, Event) ->
    Ctx#agg_ctx{state=AccCb(State, Event)}.

compensate(#agg_ctx{comp_cb=undefined, state=[]}=Ctx, _Event) ->
    Ctx;
compensate(#agg_ctx{comp_cb=undefined, state=[_H|T]}=Ctx, _Event) ->
    Ctx#agg_ctx{state=T};
compensate(#agg_ctx{comp_cb=CompCb, state=State}=Ctx, Event) ->
    Ctx#agg_ctx{state=CompCb(State, Event)}.

emit(#agg_ctx{emit_cb=undefined, state=State}) ->
    [State];
emit(#agg_ctx{emit_cb=EmitCb, state=State}) ->
    EmitCb(State).
