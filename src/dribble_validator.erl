-module(dribble_validator).

-export([
    validate_implements/2,
    pre_validate/1]).

-define(generic_pipe_elements, [filter, transform, branch, sink, splitter]).

validate_implements(Mod, Behaviour) when is_atom(Mod), is_atom(Behaviour) ->
    case (catch proplists:get_value(behaviour, Mod:module_info(attributes), [])) of
        {'EXIT',{undef,_}} -> throw({undefined_module, Mod});
        Behaviours when is_list(Behaviours) ->
            case lists:member(Behaviour, Behaviours) of
                false -> throw({behaviour_not_implemented, Mod, Behaviour});
                true -> ok
            end
    end.

pre_validate({algorithm, {flows, Flows}, {plugin_defs, PluginDefs}}) ->
    % ensure valid elements
    [
        [ validate_pipe_elem(Elem, PluginDefs) || Elem <- Pipe ]
        || {_Label, _Vis, Pipe} <- Flows
    ],
    % make sure sinks and branches are last pipe elements
    [
        begin
            Length = length(Pipe),
            dribble_util:enum_map(
                fun({Ind, {branch, _}=B}) when Ind =/= Length -> throw({not_last_in_pipe,B});
                   ({Ind, {sink, _}=S}) when Ind =/= Length -> throw({not_last_in_pipe,S});
                   ({_, _}) -> ok
                end,
                Pipe)
        end
        || {_Label, _Vis, Pipe} <- Flows
    ],
    % ensure no dangling branches
    PipeNames = [ Label || {Label, _Vis, _Pipe} <- Flows ],
    BranchNames = lists:flatten([
        [ Branches || {branch, Branches} <- Pipe ]
        || {_Label, _Vis, Pipe} <- Flows
    ]),
    case BranchNames -- PipeNames of
        [] -> ok;
        DanglingBranches -> throw({dangling_branches,DanglingBranches})
    end,
    % ensure Visibility is public or internal, with at least 1 public
    Vises = lists:usort([ Vis || {_Label, Vis, _Pipe} <- Flows ]),
    case Vises of
        [ internal, public ] -> ok;
        [ public ]           -> ok;
        []                   -> throw(no_public_flows);
        [ internal ]         -> throw(no_public_flows);
        Other                -> throw({invalid_visibilities, Other})
    end,
    % ensure that flow/plugin ids are unique
    AllIds = lists:foldl(
        fun({FlowId, _, Pipe}, Acc) ->
            Plugins = lists:filter(
                fun(PipeElem) ->
                    not lists:member(element(1, PipeElem), ?generic_pipe_elements) 
                end,
                Pipe),
            PluginIds = [ element(2, P) || P <- Plugins ],
            Acc ++ [FlowId | PluginIds]
        end,
        [],
        Flows),
    case AllIds -- lists:usort(AllIds) of
        [] -> ok;
        DuplicateIds -> throw({duplicate_ids,DuplicateIds})
    end,
    % FIXME: validate plugin defs
    ok;
pre_validate(AlgoWannabe) -> throw({invalid_algo,AlgoWannabe}).


validate_pipe_elem({filter, _Label, {fn, Fun}}, _) when is_function(Fun, 1); is_function(Fun, 2) -> ok;
validate_pipe_elem({filter, _Label, {mfa, M,F,A}}, _) when is_atom(M); is_atom(F); is_list(A) -> validate_mfa(M, F, A);
validate_pipe_elem({transform, _Label, {fn, Fun}}, _) when is_function(Fun, 1); is_function(Fun, 2) -> ok;
validate_pipe_elem({transform, _Label, {mfa, M,F,A}}, _) when is_atom(M); is_atom(F); is_list(A) -> validate_mfa(M, F, A);
validate_pipe_elem({splitter, _Label, {fn, Fun}}, _) when is_function(Fun, 1); is_function(Fun, 2) -> ok;
validate_pipe_elem({splitter, _Label, {mfa, M,F,A}}, _) when is_atom(M); is_atom(F); is_list(A) -> validate_mfa(M, F, A);
validate_pipe_elem({branch, List}, _) when is_list(List) -> ok;
validate_pipe_elem({sink, _Sink}, _) -> ok;
validate_pipe_elem({plugin, Mod, Id}=PluginRef, PluginDefs) when is_tuple(PluginRef) ->
    case proplists:get_value(Mod, PluginDefs) of
        undefined -> throw({undefined_plugin_type,PluginRef});
        Defs ->
            case proplists:get_value(Id, Defs) of
                undefined -> throw({undefined_plugin_path,PluginRef});
                _ -> ok
            end
    end;
validate_pipe_elem(Other, _) -> throw({unrecognized_pipe_element, Other}).

validate_mfa(M, F, A) ->
    Airity = length(A),
    try
        Exports = M:module_info(exports),
        lists:member({F, Airity}, Exports)
    catch error:undef -> false
    end.
