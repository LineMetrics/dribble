% Examples of usage
-module(dribble_examples_SUITE).

-compile(export_all).

all() -> [ {group, maps} ].

groups() ->
    [
        {maps, [], [ 
            t_lib_pipeline,
            t_lib_pipeline_with_branches,
            t_lib_window,
            t_lib_pipeline_with_audit,
            t_lib_window_with_audit
        ]}
    ].

t_lib_pipeline(_Config) -> throw(unimplemented).

t_lib_pipeline_with_branches(_Config) -> throw(unimplemented).

t_lib_window(_Config) -> throw(unimplemented).

t_lib_pipeline_with_audit(_Config) -> throw(unimplemented).

t_lib_window_with_audit(_Config) -> throw(unimplemented).
