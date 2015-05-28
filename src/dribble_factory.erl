-module(dribble_factory).

-export([
    to_beam/1,
    resolve/1,   %% FIXME: remove
    rewire/1]).  %% FIXME: remove

-include("dribble_int.hrl").

to_beam(Algo) ->
    % pre-validate
    dribble_validator:pre_validate(Algo),
    % map: plugin references -> {ref, impl, init_spec}
    Flows = resolve(Algo),
    % foldl: change every instance of {ref, impl, init_spec} to filter|transform|branch
    Flows2 = rewire(Flows),
    % build actual beam_flow network
    dribble_beam:build(Flows2).

resolve({algorithm, {flows,Flows}, {plugin_defs,PluginDefs}}) ->
    % map flow components to rewirables
    [ resolve_flow(F, PluginDefs) || F <- Flows ].
    

rewire(Flows) ->
    case find_plugin_in_flows(Flows) of
        none ->
            Flows;
        {FlowId, [{ref, Ref},
                  {impl, ImplMod},
                  {init_spec, InitSpec}]} ->
            ImplMod:rewire(Ref, InitSpec, FlowId, Flows)
    end.

%% internal
resolve_flow({FlowId, Visibility, Pipe}, PluginDefs) when Visibility == public; Visibility == internal ->
    Pipe2 = [ resolve_pipe_element(Elem, PluginDefs) || Elem <- Pipe],
    {FlowId, Visibility, Pipe2}.

resolve_pipe_element({filter, Label, Exec}, _PluginDefs) ->
    {beam_filter, Label, wrap_ctx(Exec)};
resolve_pipe_element({transform, Label, Exec}, _PluginDefs) ->
    {beam_transform, Label, wrap_ctx(Exec)};
resolve_pipe_element({sink, SinkId}, _PluginDefs) ->
    {beam_transform, {sink, SinkId}, {fn, sink_transform(SinkId)}};
resolve_pipe_element({branch, Branches}, _PluginDefs) when is_list(Branches) -> {beam_branch, Branches};
resolve_pipe_element(Plugin, PluginDefs) ->
    {Impl, InitSpec} = resolve_plugin(Plugin, PluginDefs),
    {plugin, [{ref, Plugin}, {impl, Impl}, {init_spec, InitSpec}]}.

resolve_plugin(Plugin, PluginDefs) ->
    Type = element(1, Plugin),
    Id   = element(2, Plugin),
    Impl = ?format_atom("dribble_plugin_~p", [Type]),
    case (catch Impl:module_info()) of
        {'EXIT', {undef, _}} -> throw({plugin_unknown, Impl});
        _ ->
            dribble_validator:validate_implements(Impl, dribble_plugin),
            Def = proplists:get_value(Type, PluginDefs, []),
            case proplists:get_value(Id, Def) of
                undefined -> throw({plugin_undefined,Type,Id});
                InitSpec -> {Impl, InitSpec}
            end
    end.

sink_transform(SinkId) ->
    fun(X, #dribble_runtime{sinks=Sinks}=Runtime) ->
        Sinked = dribble_maps:get(SinkId, Sinks, []),
        Sinked2 = Sinked ++ [X],
        Sinks2 = dribble_maps:put(SinkId, Sinked2, Sinks),
        Res = {sinked,SinkId},
        {Res, Runtime#dribble_runtime{sinks=Sinks2}}
    end.

wrap_ctx({fn, Fun}) -> 
    {arity, N} = erlang:fun_info(Fun, arity),
    case N of
        1 -> {fn, Fun};  % no ctx passing
        2 ->
            Fun2 = fun(X, #dribble_runtime{generic=GenericCtx}=Runtime) ->
                {X2, GenericCtx2} = Fun(X, GenericCtx),
                {X2, Runtime#dribble_runtime{generic=GenericCtx2}}
            end,
            {fn, Fun2}
    end;
wrap_ctx({mfa, _M,_F,_A}) ->
    throw('unimplemented, needs a beam_flow conversion func for ctx param'). 

find_plugin_in_flows([]) -> none;
find_plugin_in_flows([{_Label, _Vis, Pipe} | Rest]) ->
    case find_plugin_in_pipe(Pipe) of
        none -> find_plugin_in_flows(Rest);
        Plugin -> Plugin
    end.

find_plugin_in_pipe([]) -> none;
find_plugin_in_pipe([{plugin, _}=Plugin | _]) -> Plugin;
find_plugin_in_pipe([_ | Rest]) -> find_plugin_in_pipe(Rest).
