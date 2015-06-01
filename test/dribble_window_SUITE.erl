-module(dribble_window_SUITE).

-include("../src/dribble_int.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [ {group, window} ].

groups() ->
    [
        {window, [], [
            t_window_sliding_event,
            t_window_tumbling_event,
            t_window_sliding_time,
            t_window_tumbling_time,
            t_window_time_counting_clock,
            t_window_defaults
        ]}
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 1000}].

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
    {[{out,[2.5]}], Ctx5} = dribble:push(Ctx4, in, {id1, 3}),   % emit id1
    {[{out,[15.0]}], Ctx6} = dribble:push(Ctx5, in, {id2, 20}), % emit id2
    {[{out,[3.5]}], _Ctx7} = dribble:push(Ctx6, in, {id1, 4}).  % emit id1

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


t_window_sliding_time(_Config) ->
    GroupBy = fun({Group, _Val}) -> Group end,
    Acc  = fun(Ctx, GroupVal) -> Ctx++[GroupVal] end,
    Comp = fun([_H|T], _X) -> T end,
    First = fun([]) -> [];
               ([H|_T]) -> [H]
           end,  % must get a list for splitter
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
                    {axis, time},
                    {size, 2},
                    {accumulate, {fn, Acc}},
                    {clock_interval, 50},
                    {compensate, {fn, Comp}},
                    {emit, {fn, First}},
                    {init_ctx, []},
                    {group_by, {fn, GroupBy}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, {id1, 1}),
    {[], Ctx3} = dribble:push(Ctx2, in, {id1, 2}),
    {[], Ctx4} = dribble:push(Ctx3, in, {id2, 10}),
    {[], Ctx5} = dribble:tick(Ctx4, stabilizer_win),  % too early, no emission
    timer:sleep(50),
    {[{out,[{id1,1},{id1,2},{id2,10}]}], Ctx6} = dribble:tick(Ctx5, stabilizer_win),  % emit
    {[], Ctx7} = dribble:push(Ctx6, in, {id2, 11}),
    {[], Ctx8} = dribble:push(Ctx7, in, {id2, 12}),
    {[], Ctx9} = dribble:tick(Ctx8, stabilizer_win),  % too early, no emission
    timer:sleep(50),
    {[{out,[{id2,11},{id2,12}]}], Ctx10} = dribble:tick(Ctx9, stabilizer_win),  % emit
    timer:sleep(50),
    {[], _Ctx11} = dribble:tick(Ctx10, stabilizer_win).  % emit

t_window_tumbling_time(_Config) ->
    GroupBy = fun({Group, _Val}) -> Group end,
    Acc  = fun(Ctx, {Group, Val}) -> Ctx++[{Group, Val}] end,
    Last = fun([]) -> [];
              (Ctx) -> [lists:last(Ctx)]
           end,  % must get a list for splitter
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
                    {axis, time},
                    {accumulate, {fn, Acc}},
                    {clock_interval, 50},
                    {emit, {fn, Last}},
                    {init_ctx, []},
                    {group_by, {fn, GroupBy}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, {id1, 1}),
    {[], Ctx3} = dribble:push(Ctx2, in, {id1, 2}),
    {[], Ctx4} = dribble:push(Ctx3, in, {id2, 10}),
    {[], Ctx5} = dribble:tick(Ctx4, stabilizer_win),  % too early, no emission
    timer:sleep(50),
    {[{out,[{id1,2},{id2,10}]}], Ctx6} = dribble:tick(Ctx5, stabilizer_win),  % emit
    {[], Ctx7} = dribble:push(Ctx6, in, {id2, 11}),
    {[], Ctx8} = dribble:push(Ctx7, in, {id2, 12}),
    {[], Ctx9} = dribble:tick(Ctx8, stabilizer_win),  % too early, no emission
    timer:sleep(50),
    {[{out,[{id2,12}]}], Ctx10} = dribble:tick(Ctx9, stabilizer_win).  % emit


t_window_defaults(_Config) ->
    Mean = fun(Ctx) -> [mean(Ctx)] end,  % must get a list for splitter
    Algo = {algorithm,
        {flows, [
            {'in', public,
                [{plugin, dribble_plugin_window, 'tumbler'},
                 {plugin, dribble_plugin_window, 'slider'},
                 {sink, 'out'}]
            }
        ]},
        {plugin_defs, [
            {dribble_plugin_window, [
                % minimum defaults required for tumbling event
                {'tumbler', [
                    {type, tumbling},
                    {axis, event},
                    {size, 2},
                    {emit, {fn, Mean}}
                ]},
                % minimum defaults required for sliding time
                {'slider', [
                    {type, tumbling},
                    {axis, time},
                    {clock_interval, 50},
                    {emit, {fn, Mean}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    {[], Ctx2} = dribble:push(Ctx, in, 1),
    {[], Ctx3} = dribble:push(Ctx2, in, 2),
    {[], Ctx4} = dribble:push(Ctx3, in, 10),
    {[], Ctx5} = dribble:push(Ctx4, in, 11),
    {[], Ctx6} = dribble:push(Ctx5, in, 100),  % should not be used...
    timer:sleep(50),
    {[{out,[6.0]}], _Ctx7} = dribble:tick(Ctx6, slider).  % ((1+2)/2 + (10+11)/2)/2 = (1.5+10.5)/2


t_window_time_counting_clock(_Config) ->
    Mean = fun(Ctx) -> [mean(Ctx)] end,  % must get a list for splitter
    Algo = {algorithm,
        {flows, [
            {'in', public,
                [{plugin, dribble_plugin_window, 'tumbler'},
                 {plugin, dribble_plugin_window, 'slider'},
                 {sink, 'out'}]
            }
        ]},
        {plugin_defs, [
            {dribble_plugin_window, [
                % minimum defaults required for tumbling time
                {'tumbler', [
                    {type, tumbling},
                    {axis, time},
                    {clock_interval, 1},
                    {clock_mod, eep_clock_count},
                    {emit, {fn, Mean}}
                ]},
                % minimum defaults required for sliding time
                {'slider', [
                    {type, tumbling},
                    {axis, time},
                    {clock_interval, 1},
                    {clock_mod, eep_clock_count},
                    {emit, {fn, Mean}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),
    % no sleeps required!
    {[], Ctx2} = dribble:push(Ctx, in, 1),
    {[], Ctx3} = dribble:push(Ctx2, in, 2),
    {[], Ctx4} = dribble:push(Ctx3, in, 3),
    {[], Ctx5} = dribble:tick(Ctx4, tumbler),
    {[], Ctx6} = dribble:push(Ctx5, in, 100),
    {[], Ctx7} = dribble:tick(Ctx6, tumbler),
    {[{out,[51.0]}], _Ctx8} = dribble:tick(Ctx7, slider).  % ((1+2+3)/3 + 100)/2 = 51/2


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
