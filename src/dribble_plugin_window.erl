-module(dribble_plugin_window).

-behaviour(dribble_plugin).

-export([rewire/4]).

rewire({window, _Label}, _PluginSpec, _FlowId, _Flows) ->
    throw(unimplemented).
