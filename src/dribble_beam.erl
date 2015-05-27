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
    Public = [ Name || {Name, public, _Pipe} <- Flows ],
    #dribble_ctx{public=Public, beam=Beam2}.

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
beam_pipe_elem({beam_transform, As, {mfa, M,F,A}}) -> beam_flow:transform(M, F, A, As).
