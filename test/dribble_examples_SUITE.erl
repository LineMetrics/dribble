%% Usage examples.
-module(dribble_examples_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [ {group, lib} ].

groups() ->
    [
        {lib, [], [
            t_lib_pipeline,
            t_lib_window,
            t_lib_audit
        ]}
    ].

%% @doc detection of online/offline of an AP device.
%% utilizes filters, transforms, branches and sinks.
t_lib_pipeline(_Config) ->
    % setup algo
    IsOffline = and_([eq([data, uptime], -1), eq([data, logical_group], "ap")]),
    IsOnline  = and_([eq([data, uptime],  0), eq([data, logical_group], "ap")]),
    ToOfflineAlert = to_alert("APDisconnected", "AP offline", "An AP device has gone offline"),
    ToOnlineAlert  = to_alert("APConnected", "AP online", "An AP device has gone online"),
    Algo = {algorithm,
        {flows, [
            {in, public, [{branch, [offline, online]}]},
            {offline, internal, [
                {filter, is_offline, {fn, IsOffline}},
                {transform, to_alert, {fn, ToOfflineAlert}},
                {branch, [output]}]},
            {online, internal, [
                {filter, is_online,  {fn, IsOnline}},
                {transform, to_alert, {fn, ToOnlineAlert}},
                {branch, [output]}]},
            {output, internal, [
                {sink, out}
            ]}
        ]},
        {plugin_defs, []}
    },
    Ctx = dribble:new(Algo),

    % push in events & validate
    In1 = in_event("#123", 0, "cpe"),  % will be ignored
    In2 = in_event("#456", 0, "ap"),
    In3 = in_event("#123", -1, "ap"),
    Out1 = online_out_event("#456"),
    Out2 = offline_out_event("#123"),
    {[], Ctx2} = dribble:push(Ctx, in, In1),
    {[{out, [Out1]}], Ctx3} = dribble:push(Ctx2, in, In2),
    {[{out, [Out2]}], _Ctx4} = dribble:push(Ctx3, in, In3).


%% @doc detection of online/offline of an AP device, 'stabilized' across a time window.
%% utilizes filters, transforms, branches, sinks and windows.
t_lib_window(_Config) ->
    % setup algo
    IsOffline = and_([eq([data, uptime], -1), eq([data, logical_group], "ap")]),
    IsOnline  = and_([eq([data, uptime],  0), eq([data, logical_group], "ap")]),
    ToOfflineAlert = to_alert("APDisconnected", "AP offline", "An AP device has gone offline"),
    ToOnlineAlert  = to_alert("APConnected", "AP online", "An AP device has gone online"),
    Algo = {algorithm,
        {flows, [
            {in, public, [{branch, [offline, online]}]},
            {offline, internal, [
                {filter, is_offline, {fn, IsOffline}},
                {transform, to_alert, {fn, ToOfflineAlert}},
                {branch, [output]}]},
            {online, internal, [
                {filter, is_online, {fn, IsOnline}},
                {transform, to_alert, {fn, ToOnlineAlert}},
                {branch, [output]}]},
            {output, internal, [
                {plugin, dribble_plugin_window, stabilizer_win},  %% stabilizing window before output
                {sink, out}
            ]}
        ]},
        {plugin_defs, [
            {dribble_plugin_window, [
                {stabilizer_win, [  % stabilizer picks up the last event for group id
                    {type, tumbling},
                    {axis, time},
                    {clock_interval, 50},
                    {emit, {fn, fun last/1}},
                    {group_by, {fn, fun group_by/1}}
                ]}
            ]}
        ]}
    },
    Ctx = dribble:new(Algo),

    % push in events & validate
    Ins = [
        in_event("#456", 0, "ap"),
        in_event("#456", 0, "ap"),
        in_event("#456", 0, "ap"),
        in_event("#456", 0, "ap"),
        in_event("#456", 0, "ap"),
        in_event("#456", -1, "ap"),  % last for #456
        in_event("#123", 0, "cpe")   % ignored, not an offline/online
    ],
    % no sinks activated till the tick after 50ms
    Ctx2 = lists:foldl(
        fun(In, C) ->
            {[], C2} = dribble:push(C, in, In),
            C2
        end,
        Ctx,
        Ins),
    timer:sleep(50),
    Out = offline_out_event("#456"),
    {[{out, [Out]}], _Ctx3} = dribble:tick(Ctx2, stabilizer_win).


%% @doc example of audit, filtered by entity type and reported fields.
t_lib_audit(_Config) ->
    % setup algo
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
    Ctx = dribble:new(Algo),

    % run algo
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


% internals
eq(Path, ExpVal) ->
    fun(Data) ->
        kvlists:get_path(Path, Data) =:= ExpVal
    end.

and_(Predicates) ->
    fun(Data) ->
        lists:usort([ P(Data) || P <- Predicates]) == [true]
    end.

is_downtime() ->
    and_([eq([data, uptime], 0), eq([data, logical_group], "ap")]).

to_alert(Type, Title, Description) ->
    fun(Data) ->
        [{alert_type,        Type},
         {alert_title,       Title},
         {alert_description, Description},
         {organization_id,   kvlists:get_value(organization_id, Data)},
         {device_id,         kvlists:get_value(device_id, Data)},
         {device_ip,         kvlists:get_value(device_ip, Data)},
         {device_host,       kvlists:get_value(device_host, Data)}]
    end.

group_by(Data) -> kvlists:get_value(device_id, Data).

last([]) -> [];
last(Ctx) -> [ lists:last(Ctx) ].  % must get a list for splitter

online_out_event(DevId) ->
    [{alert_type,        "APConnected"},
     {alert_title,       "AP online"},
     {alert_description, "An AP device has gone online"},
     {organization_id,   "ubnt"},
     {device_id,         DevId},
     {device_ip,         "0.0.0.0"},
     {device_host,       "localhost"}].

offline_out_event(DevId) ->
    [{alert_type,        "APDisconnected"},
     {alert_title,       "AP offline"},
     {alert_description, "An AP device has gone offline"},
     {organization_id,   "ubnt"},
     {device_id,         DevId},
     {device_ip,         "0.0.0.0"},
     {device_host,       "localhost"}].

in_event(DevId, Uptime, LogicalGroup) ->
    [{organization_id, "ubnt"},
     {device_id,       DevId},
     {device_ip,       "0.0.0.0"},
     {device_host,     "localhost"},
     {data, [{uptime, Uptime}, {logical_group, LogicalGroup}]}].
