-module(dribble_proc_SUITE).

-include("../src/dribble_int.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [ {group, proc} ].

groups() ->
    [
        {proc, [], [
            t_proc_creation
        ]}
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 1000}].

t_proc_creation(_Config) ->
    Algo1 = Algo2 = {algorithm,
        {flows, [{in, public, []}]},
        {plugin_defs, []}
    },
    {ok, Sup} = dribble_algo_inst_sup:start_link(),
    {ok, Pid1} = dribble_algo_inst_sup:start_child("algo1", Algo1),
    {ok, Pid2} = dribble_algo_inst_sup:start_child("algo2", Algo2),
    {ok, Pid1} = dribble_algo_inst_sup:child("algo1"),
    {ok, Pid2} = dribble_algo_inst_sup:child("algo2"),
    Children = dribble_algo_inst_sup:all_children(),
    true = lists:member({"algo1", Pid1}, Children),
    true = lists:member({"algo2", Pid2}, Children).

t_proc_window(_Config) ->
    throw(unimplemented).

t_proc_audit(_Config) ->
    throw(unimplemented).
