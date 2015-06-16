-module(dribble_algo_inst_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    start_child/2,
    all_children/0,
    child/1,
    init/1]).

-define(SUPERVISOR, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_child(AlgoId, AlgoDsl) ->
    case child(AlgoId) of
        {ok, _Pid} -> {error, {already_started, AlgoId}};
        not_found -> supervisor:start_child(?MODULE, [AlgoId, AlgoDsl])
    end.

all_children() ->
    Children = supervisor:which_children(?SUPERVISOR),
    [ {dribble_algo_inst:id(Pid), Pid} || {_,Pid,_,_} <- Children ].

child(Id) ->
    Pids = [ Pid || {Id0, Pid} <- all_children(), Id0==Id ],
    case Pids of
       [Pid] -> {ok, Pid};
       [] -> not_found
    end.

init([]) ->
    {ok, {
        {simple_one_for_one, 5, 10},
        [{dribble_algo_inst, {dribble_algo_inst, start_link, []}, transient, 1000, worker, [dribble_algo_inst]}]
    }}.
