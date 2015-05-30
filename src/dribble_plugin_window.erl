-module(dribble_plugin_window).

-include("dribble_int.hrl").

-behaviour(dribble_plugin).

-export([filter_audit/1, rewire/4]).

filter_audit(_Audits) ->
    % convert:
    % - FlowId -> {window, {in, pre:in}, {out, post:out}, {ctx: pre:ctx}}
    % - WindowId: {window-tick, {in, pre:tick}, {out, post:out}, {ctx: pre:tick}}
    throw(unimplemented).

%% split up flow into:
%% - FlowId: pre ++ [enqueue_splitter] ++ post
%% - WindowId: [tick_splitter] ++ post
rewire(WindowId, PluginSpec, FlowId, Flows) ->
    Size = get_or_die(size, PluginSpec),
    {fn, AccCb}  = get_or_die(accumulate, PluginSpec),
    {fn, CompCb} = kvlists:get_value(compensate, PluginSpec, {fn, undefined}),  % optional
    {fn, EmitCb} = kvlists:get_value(emit, PluginSpec, {fn, undefined}),        % optional
    InitCtx = get_or_die(init_ctx, PluginSpec),
    Type = get_or_die(type, PluginSpec),
    Axis = get_or_die(axis, PluginSpec),
    {fn, GroupBy} = kvlists:get_value(group_by, PluginSpec),
    AggSeed = {AccCb, CompCb, EmitCb, InitCtx},

    {WinMod, InitWinCtx} = instantiate(Type, Axis, Size, AggSeed, PluginSpec),

    GetCtxPath = fun(Event) ->
        case GroupBy of
            undefined -> [window, WindowId];
            _ -> [window, WindowId, GroupBy(Event)]
        end
    end,
    Push = fun(Event, #dribble_runtime{plugins=P}=R) ->
        CtxPath = GetCtxPath(Event),
        WinCtx = case kvlists:get_path(CtxPath, P) of
            [] -> InitWinCtx;
            Other -> Other
        end,
        {Res, WinCtx2} = WinMod:push(WinCtx, Event),
        P2 = kvlists:set_path(CtxPath, WinCtx2, P),
        {win_results(Res), R#dribble_runtime{plugins=P2}}
    end,
    Tick = fun(_Event, #dribble_runtime{plugins=P}=R) ->
        % iterate through all window instances
        WinCtxes = kvlists:get_path([window, WindowId], P),
        {Results, WinCtxes2} = lists:foldl(
            fun({WinId, WinCtx}, {ResSoFar, WinCtxSoFar}) ->
                {Res, WinCtx2} = WinMod:tick(WinCtx),
                % flatten out the results
                Res2 = ResSoFar ++ win_results(Res),
                WinCtxSoFar2 = WinCtxSoFar ++ [{WinId, WinCtx2}],
                {Res2, WinCtxSoFar2}
            end,
            {[], []},
            WinCtxes),
        P2 = kvlists:set_path([window, WindowId], WinCtxes2, P), 
        {Results, R#dribble_runtime{plugins=P2}}
    end,

    % re-construct the Flows
    {FlowLabel, Vis, Pipe} = lists:keyfind(FlowId, 1, Flows),   % split the flow
    PluginRef = {plugin, ?MODULE, WindowId, PluginSpec},
    {PrePipe, PostPipe} = dribble_util:split(PluginRef, Pipe),
    FlowPipe2 = 
        PrePipe ++
        [{beam_splitter, {window, WindowId, push}, {fn, Push}}] ++
        PostPipe,
    Flow2 = {FlowLabel, Vis, FlowPipe2},
    Flows2 = lists:keystore(FlowId, 1, Flows, Flow2),
    
    % add tick flow for time windows
    Flows3 = case Axis of
        event -> Flows2;
        time ->
            WindowPipe =
                [{beam_splitter, {window, WindowId, tick}, {fn, Tick}}] ++
                PostPipe,
            WindowFlow = {WindowId, public, WindowPipe},
            lists:keystore(WindowId, 1, Flows2, WindowFlow)
    end,
    Flows3.

%% internals
win_results(noop) -> [];
win_results({emit, Val}) -> Val;
win_results(Vals) when is_list(Vals) ->
    [ V || {emit, V} <- Vals ].

instantiate(sliding, event, Size, AggSeed, _PluginSpec) ->
    {dribble_window_sliding_event,
     dribble_window_sliding_event:new(Size, dribble_plugin_window_agg, AggSeed)};
instantiate(tumbling, event, Size, AggSeed, _PluginSpec) ->
    {dribble_window_tumbling_event,
     dribble_window_tumbling_event:new(Size, dribble_plugin_window_agg, AggSeed)};
instantiate(sliding, time, Size, AggSeed, PluginSpec) ->
    ClockMod = get_or_die(clock_mod, PluginSpec),
    ClockInterval = get_or_die(clock_interval, PluginSpec),
    {dribble_window_sliding_time,
     dribble_window_tumbling_event:new(Size, dribble_plugin_window_agg, AggSeed, ClockMod, ClockInterval)};
instantiate(tumbling, time, Size, AggSeed, PluginSpec) ->
    ClockMod = get_or_die(clock_mod, PluginSpec),
    ClockInterval = get_or_die(clock_interval, PluginSpec),
    {dribble_window_sliding_time,
     dribble_window_tumbling_event:new(Size, dribble_plugin_window_agg, AggSeed, ClockMod, ClockInterval)}.

get_or_die(Key, PluginSpec) ->
    case kvlists:get_value(Key, PluginSpec) of
        undefined -> throw({missing_param_in_window_spec, Key});
        Res -> Res
    end.
