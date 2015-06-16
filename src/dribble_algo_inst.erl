-module (dribble_algo_inst).

-behaviour(gen_fsm).

-include("dribble_int.hrl").

-export([
    start_link/3]).

%% public api
-export([
    push/3,
    tick/1,
    id/1,
    start/1,
    stop/1]).

%% gen_fsm callbacks
-export([
    started/3,
    stopped/3,
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-record(dribble_algo_instance_state, {
    id,
    ctx,
    should_audit
}).

%% Public API
start_link(AlgoId, AlgoDsl, ShouldAudit) ->
    gen_fsm:start_link(?MODULE, {AlgoId, AlgoDsl, ShouldAudit}, []).

id(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, id). 

push(Pid, Pipe, Event) ->
    gen_fsm:sync_send_event(Pid, {push, Pipe, Event}).

tick(Pid) ->
    gen_fsm:sync_send_event(Pid, tick).

start(Pid) ->
    gen_fsm:sync_send_event(Pid, start).

stop(Pid) ->
    gen_fsm:sync_send_event(Pid, stop).

%% Callbacks
init({AlgoId, AlgoDsl, ShouldAudit}) ->
    % to register:
    % gproc:reg({p, l, dribble_algo}, AlgoId),
    
    % to lookup:
    % PidIds = gproc:lookup_values({p, l, algo_reg}),
    % Pids = [Pid || {Pid, P} <- PidsIds],
    State = #dribble_algo_instance_state{id=AlgoId, ctx=dribble:new(AlgoDsl), should_audit=ShouldAudit},
    {ok, stopped, State}.

handle_sync_event(id, _, StateName, #dribble_algo_instance_state{id=Id}=State) ->
    {reply, Id, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

stopped(start, _, State) ->
    {reply, ok, started, State};
stopped(stop, _, State) ->
    {reply, ok, stopped, State};
stopped(Req, _, State) ->
    {reply, {error, {invalid_req_in_stopped, Req}}, stopped, State}.

started(start, _, State) ->
    {reply, ok, started, State};
started(stop, _, State) ->
    {reply, ok, stopped, State};
started(tick, _, #dribble_algo_instance_state{ctx=Ctx, should_audit=ShouldAudit}=State) ->
    Ctx2 = dribble:tick_all(Ctx, ShouldAudit),
    State2 = State#dribble_algo_instance_state{ctx=Ctx2},
    {reply, ok, started, State2};
started({push, Pipe, Event}, _, #dribble_algo_instance_state{ctx=Ctx, should_audit=ShouldAudit}=State) ->
    Ctx2 = dribble:push(Ctx, Pipe, Event, ShouldAudit),
    State2 = State#dribble_algo_instance_state{ctx=Ctx2},
    {reply, ok, started, State2};
started(Req, _, State) ->
    {reply, {error, {invalid_req_in_started, Req}}, started, State}.

handle_event(_Event, StateName, State) -> {next_state, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
