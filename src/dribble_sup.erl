-module(dribble_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1]).

-define(SUPERVISOR, ?MODULE).
-define(CHILD(Id, Type, Mod, Params), {Id, {Mod, start_link, Params}, transient, 1000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    Freq = application:get_env(dribble, clock_freq, 100), 
    {ok, {
        {one_for_one, 5, 10},
        [?CHILD(dribble_algo_inst_sup, supervisor, dribble_algo_inst_sup, []),
         ?CHILD(dribble_clock, worker, dribble_clock, [Freq])
        ]
    }}.
