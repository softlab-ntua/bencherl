-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(_Version, _Conf) ->
  [[]].

run(_, _, Conf) ->
  %% Setup a coordinator to know when the benchmark finished. This is done by
  %% counting the number of loggers that have finished.
  register(coordinator, self()),
  launcher:start(2, 1, 2, 4, [greedy], 1),
  {_,DataDir} = lists:keyfind(datadir, 1, Conf),
  logger:launch_latency("Bencherl_test", 1, 2, 50, 4, 1, greedy, DataDir ++ "/"),
  timer:sleep(60000),
  toxic_client:launch(50, 4, greedy),
  timer:sleep(60000),
  toxic_client:launch_traffic(50, 4, greedy),
  loop(4),
  ok.

loop(0) -> ok;
loop(N_Loggers) ->
  receive
    logger_stopped ->
      loop(N_Loggers - 1)
  end.
