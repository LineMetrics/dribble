-record(dribble_runtime, {
    generic = [],
    plugins = [],
    sinks   = []
}).

-record(dribble_ctx, {
    meta = [],
    beam,
    runtime = #dribble_runtime{}
}).
