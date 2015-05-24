-module(dribble_plugin_box).

-behaviour(dribble_plugin).

-export([rewire/5]).

rewire({box, _Label, _Port}, _PluginImpl, _PluginSpec, _FlowId, _Flows) ->
    throw(unimplemented).
