-include("dribble.hrl").

-define(format(S, P), lists:flatten(io_lib:format(S, P))).
-define(format_bin(S, P), list_to_binary(lists:flatten(io_lib:format(S, P)))).
-define(format_atom(S, P), list_to_atom(lists:flatten(io_lib:format(S, P)))).
-define(stack_trace, try throw(stack_trace) catch throw:stack_trace -> erlang:get_stacktrace() end).

-type plugin_type() :: atom().
-type flow()        :: any().  %% for now...
-type label()       :: any().
-type flow_id()     :: any().
-type plugin_impl() :: atom().

-type plugin()      :: {plugin_type(), label()} | {plugin_type(), label(), any()}.
-type plugin_def()  :: any().
-type algo()        :: {algorithm, [flow()]}.
