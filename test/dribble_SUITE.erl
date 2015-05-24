-module(dribble_SUITE).

-inslude("../include/dribble.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(to_algo(Pipe), {algorithm, {flows, [{a, public, Pipe}]}, {plugin_defs, []}}).

all() -> [ {group, util}, {group, validator}, {group, factory} ].

groups() ->
    [
        {util, [], [
            t_replace,
            t_enum_map
        ]},
        {validator, [], [
            t_validate_implements,
            t_pre_validate
        ]},
        {factory, [], [
            t_resolve
        ]}
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 1000}].

t_replace(_Config) ->
    [1,c,3] = dribble_util:replace(2, c, [1,2,3]). 

t_enum_map(_Config) ->
    [{1,a},{2,b},{3,c}] = dribble_util:enum_map(fun(Ind, X) -> {Ind, X} end, [a,b,c]).

t_validate_implements(_Config) ->
    ok = dribble_validator:validate_implements(user_sup, supervisor_bridge),
    {behaviour_not_implemented,_,_} = (catch dribble_validator:validate_implements(?MODULE, blah)).

t_pre_validate(_Config) ->
    Filter = fun(_,_) -> ok end,
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', Filter}])),
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', Filter}, {sink, a}])),
    {not_last_in_pipe,{sink,a}} = (catch dribble_validator:pre_validate(?to_algo([{sink, a}, {filter, 'f', Filter}]))),
    ok = dribble_validator:pre_validate(?to_algo([{filter, 'f', Filter}, {branch, [a]}])),
    {not_last_in_pipe,{branch,[a]}} = (catch dribble_validator:pre_validate(?to_algo([{branch, [a]}, {filter, 'f', Filter}]))).

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
                 {box, 'populate_parent', data_in}]
            },
            {'check_downtime-cont', internal,                   %% continuation flow for 'populate_parent'
                [{branch, ['stabilizer']}]
            },
            {'check_uptime', internal,
                [{filter, 'is_uptime_ap',   {fn, IsDowntime}},  %% where data.uptime = 0 && data.logical_group = "ap"
                 {transform, 'to_alert',    {fn, ToAlert}},     %% converts to notification payload
                 {branch, ['stabilizer']}]
            },
            {'stabilizer', internal, 
                [{window, 'stabilizer_win'},                    %% auto re-wires 'stabilizer' to fit in the 'stabilizer_win'
                 {sink, 'output_sink'}]                         %% mandatory sink (must be at least 1)
            }
        ]},

        {plugin_defs, [
            %% define boxes with their implementation module, in/out ports, initial config
            {box, [
                {'populate_parent', [
                    {impl, populate_parent_op},
                    {in,   [data_in]},                              %% input port
                    {out,  [{data_out, 'check_downtime-cont'}]},    %% output port pointing to 'check_downtime-cont' pipe
                    {with, [{evict_every, 3600000}]}
                ]}
            ]},

            %% For windows, split up the parent flow and insert window flow
            {window, [
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
