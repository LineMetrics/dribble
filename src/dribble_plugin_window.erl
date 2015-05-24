-module(dribble_plugin_window).

-behaviour(dribble_plugin).

-export([rewire/5]).

rewire({window, _Label}, _PluginImpl, _PluginSpec, _FlowId, _Flows) ->
    throw(unimplemented).
