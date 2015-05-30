-record(dribble_runtime, {
    generic = [],
    plugins = [],
    sinks   = []
}).

-record(dribble_ctx, {
    public = [],
    beam,
    runtime = #dribble_runtime{}
}).
