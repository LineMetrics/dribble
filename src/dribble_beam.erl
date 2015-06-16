% Constructs beam flows from dribble intermediate form.
-module(dribble_beam).

-export([build/1]).

-include("dribble_int.hrl").

build(Flows) ->
    % create placeholders for all pipes
    Beam = lists:foldl(
        fun({PipeName, _Vis, _Pipe}, B) ->
            beam_flow:pipe(B, PipeName, []) 
        end,
        beam_flow:new(),
        Flows),
    
    % populate the pipes
    Beam2 = lists:foldl(
        fun({Label, _Vis, Pipe}, B) ->
            build_pipe(Label, Pipe, B)
        end,
        Beam,
        Flows),
    % build up metadata
    Meta = lists:foldl(
        fun({Name, Tags, _Pipe}, Meta0) ->
            update_meta(Name, Tags, Meta0)
        end,
        [],
        Flows),
    #dribble_ctx{meta=Meta, beam=Beam2}.

build_pipe(Label, Pipe, Beam) ->
    {PipeHeads, PipeLast} = dribble_util:heads_and_last(Pipe),
    case PipeLast of
        {beam_branch, Branches} ->
            BeamPipe = [ beam_pipe_elem(Elem) || Elem <- PipeHeads ],
            Beam2 = beam_flow:pipe(Beam, Label, BeamPipe),
            lists:foldl(
                fun(As, B) ->
                    beam_flow:branch(B, Label, As)
                end,
                Beam2,
                Branches);
        _ ->
            BeamPipe = [ beam_pipe_elem(Elem) || Elem <- Pipe ],
            beam_flow:pipe(Beam, Label, BeamPipe)
    end.

beam_pipe_elem({beam_filter, As, {fn, Fun}}) -> beam_flow:filter(Fun, As);
beam_pipe_elem({beam_filter, As, {mfa, M,F,A}}) -> beam_flow:filter(M, F, A, As);
beam_pipe_elem({beam_transform, As, {fn, Fun}}) -> beam_flow:transform(Fun, As);
beam_pipe_elem({beam_transform, As, {mfa, M,F,A}}) -> beam_flow:transform(M, F, A, As);
beam_pipe_elem({beam_splitter, As, {fn, Fun}}) -> beam_flow:splitter(Fun, As);
beam_pipe_elem({beam_splitter, As, {mfa, M,F,A}}) -> beam_flow:splitter(M, F, A, As).

%% internals
update_meta(_, [], Meta) -> Meta;
update_meta(Name, [HTag|TTags], Meta) ->
    Meta2 = update_meta(Name, HTag, Meta),
    update_meta(Name, TTags, Meta2);
update_meta(Name, Tag, Meta) ->
    Classification = proplists:get_value(Tag, Meta, []),
    lists:keystore(Tag, 1, Meta, {Tag, Classification++[Name]}).
