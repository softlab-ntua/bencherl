%% DEbench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench).

-export([main/1, main/4]).

-include("de_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

%  make clean
%  make 
% ./de_bench bench.config
%  make results

main([]) ->
    io:format("Config file is needed  ..~n");

main(Configs) ->
	initial(Configs),
	start_bench().

main(Cores, Node, Config,NumberOfOperation) ->
	initial([Config]),
	de_bench_config:set(max_num_operarions, NumberOfOperation),
	de_bench_config:set(erlange_nodes, [Node]),
	de_bench_config:set(concurrent, Cores+5),
	start_bench().

start_bench() ->
	%% Spin up the application
    ok = de_bench_app:start(),
    
     %% Pull the runtime duration from the config and sleep until that's passed OR
    %% the supervisor process exits
    Mref = erlang:monitor(process, whereis(de_bench_sup)),   
	DurationMins = de_bench_config:get(duration),
	wait_for_stop(Mref, DurationMins),
	io:format("Benchmark ends at ~p ~n",[erlang:time()]).

initial(Configs) ->
	io:format("Benchmark starts at ~p ~n",[erlang:time()]),
	de_helper:file_exist(Configs),
	%% Load the config files
	de_bench_config:load(Configs),
	ok = application:load(de_bench),
	register(de_bench, self()),
    
    %% Setup working directory for this test. All logs, stats, and config
    %% info will be placed here
    %% Define these dirs before Lager starts
    {ok, Cwd} = file:get_cwd(),
    TestId = de_helper:id(),
    TestDir = filename:join([Cwd, de_bench_config:get(test_dir), TestId]),

    ok = filelib:ensure_dir(filename:join(TestDir, "foobar")),

	de_bench_config:set(test_dir_path, TestDir),

    %% Start Lager
    application:load(lager),

    %% Fileoutput
    ConsoleLagerLevel = de_bench_config:get(lager_level, debug),
    filelib:ensure_dir(TestDir),
    ErrorLog = filename:join([TestDir, "error.log"]),
    ConsoleLog = filename:join([TestDir, "console.log"]),
    CrashLog = filename:join([TestDir, "crash.log"]),
    application:set_env(lager,
                        handlers,
                        [{lager_console_backend, ConsoleLagerLevel},
                         {lager_file_backend,
                          [ {ErrorLog, error, 10485760, "$D0", 5},
                            {ConsoleLog, debug, 10485760, "$D0", 5} ]} ]),
    application:set_env(lager, crash_log, CrashLog),

    lager:start(),

    io:format("TestDir = ~p ~n",[filename:join([Cwd, de_bench_config:get(test_dir), "current"])]),
    %% Create a link to the test dir for convenience
    TestLink = filename:join([Cwd, de_bench_config:get(test_dir), "current"]),
    [] = os:cmd(?FMT("rm -f ~s; ln -sf ~s ~s", [TestLink, TestDir, TestLink])),

    %% Copy the config into the test dir for posterity
    [ begin {ok, _} = file:copy(Config, filename:join(TestDir, filename:basename(Config))) end
      || Config <- Configs ].



%% ====================================================================
%% Internal functions
%% ====================================================================

wait_for_stop(Mref, infinity) ->
    receive
        {'DOWN', Mref, _, _, Info} ->
            ?CONSOLE("Test stopped: ~p\n", [Info])
    end;


wait_for_stop(Mref, DurationMins) ->
    Duration = timer:minutes(DurationMins) + timer:seconds(1),
    receive
        {'DOWN', Mref, _, _, Info} ->
            ?CONSOLE("Test stopped: ~p\n", [Info]);
        {shutdown, Reason, Exit} ->
            de_bench_app:stop(),
            ?CONSOLE("Test shutdown: ~s~n", [Reason]),
            halt(Exit)

    after Duration ->
            de_bench_app:stop(),
            ?CONSOLE("Test completed after ~p mins.\n", [DurationMins])
    end.

