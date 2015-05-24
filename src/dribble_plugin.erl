-module(dribble_plugin).

-include("dribble_int.hrl").

-callback rewire(plugin(), plugin_impl(), plugin_def(), flow_id(), [flow()]) -> [flow()].
