-module(dribble_validator).

-export([validate_implements/2]).
-export([pre_validate/1]).

validate_implements(Mod, Behaviour) when is_atom(Mod), is_atom(Behaviour) ->
    case (catch proplists:get_value(behaviour, Mod:module_info(attributes), [])) of
        {'EXIT',{undef,_}} -> throw({undefined_module, Mod});
        Behaviours when is_list(Behaviours) ->
            case lists:member(Behaviour, Behaviours) of
                false -> throw({behaviour_not_implemented, Mod, Behaviour});
                true -> ok
            end
    end.

pre_validate({algorithm, {flows, Flows}, _}) ->
    % make sure sinks and branches are last pipe elements
    [
        begin
            Length = length(Pipe),
            dribble_util:enum_map(
                fun(Ind, {branch, _}=B) when Ind =/= Length -> throw({not_last_in_pipe,B});
                   (Ind, {sink, _}=S) when Ind =/= Length -> throw({not_last_in_pipe,S});
                   (_, _) -> ok
                end,
                Pipe)
        end
        || {_Label, _Vis, Pipe} <- Flows
    ],
    ok.
