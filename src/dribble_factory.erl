-module(dribble_factory).

-export([to_beam/1]).
-export([resolve/1]).
-export([rewire/1]).
-export([build/1]).

-include("dribble_int.hrl").

to_beam(Algo) ->
    % pre-validate
    dribble_validator:pre_validate(Algo),
    % map: plugin references -> {ref, impl, init_spec}
    Algo2 = resolve(Algo),
    % foldl: change every instance of {ref, impl, init_spec} to filter|transform|branch
    Algo3 = rewire(Algo2),
    % build actual beam_flow network
    build(Algo3).

resolve({algorithm, {flows,Flows}, {plugin_defs,PluginDefs}}) ->
    % map flow components to rewirables
    Flows2 = [ resolve_flow(F, PluginDefs) || F <- Flows ],
    {algorithm, Flows2}.

rewire({algorithm, {flows,_Flows}, {plugin_defs,_}}) ->
    % rewire till no change
    throw(unimplemented).

build({algorithm, {flows,_Flows}, {plugin_defs,_}}) ->
    % build beam network
    throw(unimplemented).

%% internal
resolve_flow({FlowId, Visibility, Pipe}, PluginDefs) when Visibility == public; Visibility == internal ->
    Pipe2 = [ resolve_pipe_element(Elem, PluginDefs) || Elem <- Pipe],
    {FlowId, Visibility, Pipe2}.

resolve_pipe_element({filter, Label, Exec}, _PluginDefs) ->
    {beam_filter, Label, wrap_ctx(Exec)};
resolve_pipe_element({transform, Label, Exec}, _PluginDefs) ->
    {beam_transform, Label, wrap_ctx(Exec)};
resolve_pipe_element({sink, SinkId}, _PluginDefs) ->
    {beam_transform, {sink, SinkId}, sink_transform(SinkId)};
resolve_pipe_element({branch, Branches}=Branch, _PluginDefs) when is_list(Branches) -> Branch;
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
    fun(X, #dribble_ctx{sinks=Sinks}=Ctx) ->
        Sinks2 = dict:store(SinkId, X, Sinks),
        Res = {sinked,SinkId},
        {Res, Ctx#dribble_ctx{sinks=Sinks2}}
    end.

wrap_ctx({fn, Fun}) -> 
    {arity, N} = erlang:fun_info(Fun, arity),
    case N of
        1 -> {fn, Fun};  % no ctx passing
        2 ->
            Fun2 = fun(X, #dribble_ctx{generic=GenericCtx}=Ctx) ->
                {X2, GenericCtx2} = Fun(X, GenericCtx),
                {X2, Ctx#dribble_ctx{generic=GenericCtx2}}
            end,
            {fn, Fun2}
    end;
wrap_ctx({mfa, _M,_F,_A}) ->
    throw('unimplemented, needs a beam_flow conversion func for ctx param'). 

