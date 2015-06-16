-module(dribble_clock).
-behaviour(gen_server).

-export([start_link/1]).

% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(dribble_clock_state, {
    clock_freq
}).

start_link(ClockFreq) ->
    gen_server:start_link(?MODULE, [ClockFreq], []).

init([ClockFreq]) ->
    erlang:send_after(ClockFreq, self(), tick),
    {ok, #dribble_clock_state{clock_freq=ClockFreq}}.

handle_info(tick, #dribble_clock_state{clock_freq=ClockFreq}=State) ->
    [ dribble_algo_inst:tick(Child) || Child <- dribble_algo_inst_sup:all_children() ],
    erlang:send_after(ClockFreq, self(), tick),
    {noreply,State}.

handle_call(_, _From, State) -> {reply, ok, State}.
handle_cast(_, State) -> {noreply, State}.
terminate(_, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
