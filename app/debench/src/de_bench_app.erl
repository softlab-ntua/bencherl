%% DEbench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_app.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_app).

-behaviour(application).

%% API
-export([start/0,
         stop/0,
         is_running/0]).

%% Application callbacks
-export([start/2, stop/1]).


start() ->
    %% Start up our application -- mark it as permanent so that the node
    %% will be killed if we go down
    application:start(de_bench, permanent).

stop() ->
    application:stop(de_bench).

is_running() ->
    application:get_env(de_bench_app, is_running) == {ok, true}.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = de_bench_sup:start_link(),
    application:set_env(de_bench_app, is_running, true),
    Sleep_time=de_bench_config:get(sleep_time_after_ping, 0),
    timer:sleep(timer:seconds(Sleep_time)),
    ok = de_bench_stats:run(),
    ok = de_bench_worker:run(de_bench_sup:workers()),
	{ok, Pid}.

stop(_State) ->
    ok.
