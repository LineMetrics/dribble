-module(dribble_plugin).

-include("dribble_int.hrl").

-callback filter_audit([audit()]) -> [audit()].
-callback rewire(plugin_id(), plugin_spec(), flow_id(), [flow()]) -> [flow()].
