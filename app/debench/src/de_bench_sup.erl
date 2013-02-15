%% DEbench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_sup.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, workers/0, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).

workers() ->
    [Pid || {_Id, Pid, worker, [de_bench_worker]} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Sleep_time=de_bench_config:get(sleep_time_before_ping, 0),
	timer:sleep(timer:seconds(Sleep_time)),
    Workers = worker_specs(de_bench_config:get(concurrent), []),
    {ok, {{one_for_one, 5, 1},
		[?CHILD(de_bench_stats, worker)] ++
        Workers
    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, Acc) ->
    Acc;
worker_specs(Count, Acc) ->
    Id = list_to_atom(lists:concat(['worker_', Count])),
    Spec = {Id, {de_bench_worker, start_link, [Id, Count]},
            permanent, 5000, worker, [de_bench_worker]},
	worker_specs(Count-1, [Spec | Acc]).
