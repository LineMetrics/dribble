-record(dribble_runtime, {
    generic = dribble_maps:new(),
    plugins = dribble_maps:new(),
    sinks   = dribble_maps:new()
}).

-record(dribble_ctx, {
    public = [],
    beam,
    runtime = #dribble_runtime{}
}).
