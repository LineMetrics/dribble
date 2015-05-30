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

init({AccCb, CompCb, EmitCb, _InitAggCtx}=Ctx) 
    when is_function(AccCb, 2),
         (CompCb == undefined orelse is_function(CompCb, 2)),
         (EmitCb == undefined orelse is_function(EmitCb, 1)) -> Ctx.
init() -> throw(not_supported). % must use seedable aggregate

accumulate({AccCb, CompCb, EmitCb, AggCtx}, Event) ->
    AggCtx2 = AccCb(AggCtx, Event),
    {AccCb, CompCb, EmitCb, AggCtx2}.

compensate({_, undefined, _, _}=Ctx, _Event) -> Ctx;
compensate({AccCb, CompCb, EmitCb, AggCtx}, Event) ->
    AggCtx2 = CompCb(AggCtx, Event),
    {AccCb, CompCb, EmitCb, AggCtx2}.

emit({_, _, undefined, AggCtx}) -> AggCtx;
emit({_, _, EmitCb, AggCtx}) ->
    EmitCb(AggCtx).

