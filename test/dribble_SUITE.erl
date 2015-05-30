-module(dribble_SUITE).

-include("../src/dribble_int.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(to_algo(Pipe), {algorithm, {flows, [{a, public, Pipe}]}, {plugin_defs, []}}).

all() -> [ {group, util}, {group, validator}, {group, factory}, {group, window} ].

groups() ->
    [
        {util, [], [
            t_replace,
            t_enum_map,
            t_enum_filter,
            t_enum_foldl
        ]},
        {validator, [], [
            t_validate_implements,
            t_pre_validate
        ]},
        {factory, [], [
            t_resolve,
            t_to_beam
        ]},
        {window, [], [
            t_window_sliding_event,
            t_window_tumbling_event
        ]}
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 1000}].

t_replace(_Config) ->
    [1,c,3] = dribble_util:replace(2, c, [1,2,3]). 

t_enum_map(_Config) ->
    [{1,a},{2,b},{3,c}] = dribble_util:enum_map(fun({Ind, X}) -> {Ind, X} end, [a,b,c]).

t_enum_filter(_Config) ->
    [a,c] = dribble_util:enum_filter(fun({Ind, _}) -> Ind =/= 2 end, [a,b,c]).

t_enum_foldl(_Config) ->
    "1a2b3c" = dribble_util:enum_foldl(fun({Ind, X}, Acc) -> ?format("~s~b~p", [Acc, Ind, X]) end, [], [a,b,c]).

t_validate_implements(_Config) ->
    ok = dribble_validator:validate_implements(user_sup, supervisor_bridge),
    {behaviour_not_implemented,_,_} = (catch dribble_validator:validate_implements(?MODULE, blah)).

t_pre_validate(_Config) ->
    FilterFn = {fn, fun(_,_) -> ok end},
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', FilterFn}])),
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', FilterFn}, {sink, a}])),
    {not_last_in_pipe,{sink,a}} = (catch dribble_validator:pre_validate(?to_algo([{sink, a}, {filter, 'f', FilterFn}]))),
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', FilterFn}, {branch, [a]}])),
    {not_last_in_pipe,{branch,[a]}} = (catch dribble_validator:pre_validate(?to_algo([{branch, [a]}, {filter, 'f', FilterFn}]))),
    Flowless = {algorithm, {flows, []}, {plugin_defs, []}},
    no_public_flows = (catch dribble_validator:pre_validate(Flowless)),
    DanglingBranches = {algorithm, {flows, [{a, public, [{branch, [b]}]}]}, {plugin_defs, []}},
    {dangling_branches,[b]} = (catch dribble_validator:pre_validate(DanglingBranches)),
    InvalidPipe = {algorithm, {flows, [{a, public, [invalid_pipe_elem]}]}, {plugin_defs, []}},
    {unrecognized_pipe_element,invalid_pipe_elem} = (catch dribble_validator:pre_validate(InvalidPipe)),
    InvalidPluginType = {algorithm, {flows, [{a, public, [{plugin,undefined_plugin,'plugin_path'}]}]}, {plugin_defs, []}},
    {undefined_plugin_type,{plugin,undefined_plugin,plugin_path}} = (catch dribble_validator:pre_validate(InvalidPluginType)),
    InvalidPluginPath = {algorithm, {flows, [{a, public, [{plugin,some_plugin,'undefined_plugin_path'}]}]}, {plugin_defs, [{some_plugin,[]}]}},
    {undefined_plugin_path,{plugin,some_plugin,undefined_plugin_path}} = (catch dribble_validator:pre_validate(InvalidPluginPath)),
    DuplicateFlowIds = {algorithm, {flows, [{a, public, []}, {a, internal, []}]}, {plugin_defs, []}},
    {duplicate_ids,[a]} = (catch dribble_validator:pre_validate(DuplicateFlowIds)),
    DuplicateFlowPluginIds = {algorithm, {flows, [{aa, public, [{plugin, aa, bb}]}]}, {plugin_defs, [{aa, [{bb, []}]}]}},
    {duplicate_ids,[aa]} = (catch dribble_validator:pre_validate(DuplicateFlowPluginIds)).

t_resolve(_Config) ->
    IsUptime = ToAlert = IsDowntime = ToAlert = Getter = fun(_) -> dummy_fun end,
    Algo = {algorithm,
        {flows, [                                               %% input endpoints, either public or internal
            {'cep_in', public,
                [{branch, ['check_downtime', 'check_uptime']}]
            },
            {'check_downtime', internal,
                [{filter, 'is_downtime_ap', {fn, IsUptime}},    %% where data.uptime = -1 && data.logical_group = "ap"
                 {transform, 'to_alert',    {fn, ToAlert}},     %% converts to notification payload
                 {branch, ['stabilizer']}]
            },
            {'check_uptime', internal,
                [{filter, 'is_uptime_ap',   {fn, IsDowntime}},  %% where data.uptime = 0 && data.logical_group = "ap"
                 {transform, 'to_alert',    {fn, ToAlert}},     %% converts to notification payload
                 {branch, ['stabilizer']}]
            },
            {'stabilizer', internal, 
                [{plugin, dribble_plugin_window, 'stabilizer_win'},     %% auto re-wires 'stabilizer' to fit in the 'stabilizer_win'
                 {sink, 'output_sink'}]                         %% mandatory sink (must be at least 1)
            }
        ]},

        {plugin_defs, [
            %% For windows, split up the parent flow and insert window flow
            {dribble_plugin_window, [
                {'stabilizer_win', [
                    {type, eep_window_tumbling},
                    {size, 30000},
                    {group_by, {fn, Getter([device_id])}}
                ]}
            ]}
        ]}
    },
    Resolved = dribble_factory:resolve(Algo),
    ct:log("Resolved algo: ~p", [Resolved]).
    %% FIXME: validate the above!!!

t_to_beam(_Config) ->
    IsEven  = fun(X) -> X rem 2 == 0 end,
    Times10 = fun(X) -> 10 * X end,
    Algo = {algorithm,
        {flows, [                                               %% input endpoints, either public or internal
            {in, public, [{filter, is_5, {fn, IsEven}}, {branch, [pass, multiplied]}]},
            {pass, internal, [{sink, out_pass}]},
            {multiplied, internal, [{transform, times10, {fn, Times10}}, {sink, out_multiplied}]}
        ]},
        {plugin_defs, []}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, 5),
    {[{out_pass, [4]}, {out_multiplied, [40]}], _Ctx3} = dribble:push(Ctx2, in, 4).

t_window_sliding_event(_Config) ->
    GroupBy = fun({Group, _Val}) -> Group end,
    Acc   = fun(Ctx, {_Group, Val}) -> Ctx++[Val] end,
    Stats = fun(Ctx) -> [mean(Ctx)] end,  % must get a list for splitter
    Algo = {algorithm,
        {flows, [
            {in, public,
                [{plugin, dribble_plugin_window, 'stabilizer_win'},
                 {sink, 'out'}]
            }
        ]},
        {plugin_defs, [
            %% For windows, split up the parent flow and insert window flow
            {dribble_plugin_window, [
                {'stabilizer_win', [
                    {type, sliding},
                    {axis, event},
                    {size, 2},
                    {accumulate, {fn, Acc}},
                    % {compensate, {fn, Comp}},
                    {emit, {fn, Stats}},
                    {init_ctx, []},
                    {group_by, {fn, GroupBy}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, {id1, 1}), 
    {[], Ctx3} = dribble:push(Ctx2, in, {id2, 10}), 
    {[{out,[1.5]}], Ctx4} = dribble:push(Ctx3, in, {id1, 2}),   % emit id1
    {[{out,[2.0]}], Ctx5} = dribble:push(Ctx4, in, {id1, 3}),   % emit id1
    {[{out,[15.0]}], Ctx6} = dribble:push(Ctx5, in, {id2, 20}), % emit id2
    {[{out,[2.5]}], _Ctx7} = dribble:push(Ctx6, in, {id1, 4}).  % emit id1

t_window_tumbling_event(_Config) ->
    GroupBy = fun({Group, _Val}) -> Group end,
    Acc   = fun(Ctx, {_Group, Val}) -> Ctx++[Val] end,
    Stats = fun(Ctx) -> [mean(Ctx)] end,  % must get a list for splitter
    Algo = {algorithm,
        {flows, [
            {in, public,
                [{plugin, dribble_plugin_window, 'stabilizer_win'},
                 {sink, 'out'}]
            }
        ]},
        {plugin_defs, [
            %% For windows, split up the parent flow and insert window flow
            {dribble_plugin_window, [
                {'stabilizer_win', [
                    {type, tumbling},
                    {axis, event},
                    {size, 2},
                    {accumulate, {fn, Acc}},
                    % {compensate, {fn, Comp}},
                    {emit, {fn, Stats}},
                    {init_ctx, []},
                    {group_by, {fn, GroupBy}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, {id1, 1}), 
    {[], Ctx3} = dribble:push(Ctx2, in, {id2, 10}), 
    {[{out,[1.5]}], Ctx4} = dribble:push(Ctx3, in, {id1, 2}),   % emit id1
    {[], Ctx5} = dribble:push(Ctx4, in, {id1, 3}),   % emit id1
    {[{out,[15.0]}], Ctx6} = dribble:push(Ctx5, in, {id2, 20}), % emit id2
    {[{out,[3.5]}], _Ctx7} = dribble:push(Ctx6, in, {id1, 4}).  % emit id1

%% internal
mean(List) when is_list(List) ->
    Sum = lists:foldl(
        fun(X, Acc) -> Acc + X end,
        0,
        List),
    Sum/length(List).

median(List) when is_list(List) ->
    Ind = length(List) div 2,
    lists:nth(Ind, lists:sort(List)).
