% Examples of usage
-module(dribble_examples_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [ {group, lib} ].

groups() ->
    [
        {lib, [], [
            t_lib_pipeline,
            t_lib_pipeline_with_branches,
            t_lib_pipeline_with_audit
            % t_lib_window,
            % t_lib_window_with_audit
        ]}
    ].

t_lib_pipeline(_Config) ->
    EventIn = fun(Uptime, LogicalGroup) -> [
        {organization_id, "ubnt"},
        {device_id,       "airMax"},
        {device_ip,       "0.0.0.0"},
        {device_host,     "localhost"},
        {data, [{uptime, Uptime}, {logical_group, LogicalGroup}]}]
    end,
    Ctx = downtime_algo(),
    ExpEventOut = [
        {alert_type,        "APDisconnected"},
        {alert_title,       "AP offline"},
        {alert_description, "An AP device has gone offline"},
        {organization_id,   "ubnt"},
        {device_id,         "airMax"},
        {device_ip,         "0.0.0.0"},
        {device_host,       "localhost"}],
    {[{out,[ExpEventOut]}], Ctx2} = dribble:push(Ctx, in, EventIn(0, "ap")),
    {[], Ctx3} = dribble:push(Ctx2, in, EventIn(10, "ap")),
    {[], _Ctx4} = dribble:push(Ctx3, in, EventIn(0, "cpe")).


%% @doc algo with with 2 main branches, 2 sub-branches on each, each main sinking twice.
t_lib_pipeline_with_branches(_Config) ->
    Ctx = odd_even_algo(),
    % multiple values for the same sinks
    {[{out_even, [4, 0.25]}], Ctx2} = dribble:push(Ctx, in, 4),
    {[{out_odd,  [5, 25]}], _} = dribble:push(Ctx2, in, 5).


%% @doc audit, filtering on branches and sinks, then on filters, transforms and sinks
t_lib_pipeline_with_audit(_Config) ->
    Ctx = odd_even_algo(),
    {_, Ctx2, Audit2} = dribble:push(Ctx, in, 4, true),
    [{branch,odd,{out,drop}},
     {branch,odd_squared,{out,drop}},
     {sink,out_even,{out,{sinked,out_even}}},
     {branch,even,{out,{sinked,out_even}}},
     {sink,out_even,{out,{sinked,out_even}}},
     {branch,even_reciprocal,{out,{sinked,out_even}}}] = dribble:filter_audit(Audit2, [branch, sink], [type, label, out]),

    {_, _, Audit3} = dribble:push(Ctx2, in, 5, true),
    [{filter,is_odd,{out,true}},
     {sink,out_odd,{out,{sinked,out_odd}}},
     {filter,is_odd,{out,true}},
     {transform,odd_squared,{out,25}},
     {sink,out_odd,{out,{sinked,out_odd}}},
     {filter,is_even,{out,false}},
     {filter,is_even,{out,false}}] = dribble:filter_audit(Audit3, [filter, transform, sink], [type, label, out]).


t_lib_window(_Config) -> throw(unimplemented).


t_lib_window_with_audit(_Config) -> throw(unimplemented).


% internals
downtime_algo() ->
    Eq = fun(Path, ExpVal) -> fun(Data) -> kvlists:get_path(Path, Data) =:= ExpVal end end,
    And = fun(Predicates) -> fun(Data) -> lists:usort([ P(Data) || P <- Predicates]) == [true] end end,
    Get = fun kvlists:get_path/2,
    IsDowntime = And([Eq([data, uptime], 0), Eq([data, logical_group], "ap")]),
    ToAlert = fun(Data) ->
        [{alert_type,        "APDisconnected"},
         {alert_title,       "AP offline"},
         {alert_description, "An AP device has gone offline"},
         {organization_id,   Get([organization_id], Data)},
         {device_id,         Get([device_id], Data)},
         {device_ip,         Get([device_ip], Data)},
         {device_host,       Get([device_host], Data)}]
    end,
    Algo = {algorithm,
        {flows, [
            {in, public, [
                {filter, is_downtime, {fn, IsDowntime}},
                {transform, to_alert, {fn, ToAlert}},
                {sink, out}]}
        ]},
        {plugin_defs, []}
    },
    dribble:new(Algo).

odd_even_algo() ->
    IsEven = fun(X) -> X rem 2 == 0 end,
    IsOdd = fun(X) -> X rem 2 =/= 0 end,
    Squared = fun(X) -> X*X end,
    Reciprocal = fun(X) -> 1/X end,
    Algo = {algorithm,
        {flows, [
            {in, public,   [{branch, [odd, odd_squared, even, even_reciprocal]}]},
            {odd, internal, [
                {filter, is_odd, {fn, IsOdd}},
                {sink, out_odd}]},
            {odd_squared, internal, [
                {filter, is_odd, {fn, IsOdd}},
                {transform, odd_squared, {fn, Squared}},
                {sink, out_odd}]},
            {even, internal, [
                {filter, is_even, {fn, IsEven}},
                {sink, out_even}]},
            {even_reciprocal, internal, [
                {filter, is_even, {fn, IsEven}},
                {transform, even_reciprocal, {fn, Reciprocal}},
                {sink, out_even}]}
        ]},
        {plugin_defs, []}
    },
    dribble:new(Algo).
