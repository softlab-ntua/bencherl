-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(_Version, _Conf) ->
  [[]].

run(_, Slaves, Conf) ->
  %% Setup a coordinator to know when the benchmark finished. This is done by
  %% counting the number of loggers that have finished.
  register(coordinator, self()),
  %% Get the data dir in order to store the .csv output files there.
  {_,DataDir} = lists:keyfind(datadir, 1, Conf),
  %% Find the domain of the slave nodes (currently the same for everyone).
  Slave1 = hd(Slaves),
  [_SlaveName, SlaveHost] = string:tokens(atom_to_list(Slave1), "@"),
  Domain = list_to_atom(SlaveHost),
  launcher:start(3, 1, 3, 10, [Domain], 1),
  logger:launch_latency("Bencherl_test", 1, 3, 8000, 10, 1, Domain, DataDir ++ "/"),
  timer:sleep(60000), %XXX: Just to make sure that all clients have logged in.
  toxic_client:launch(8000, 10, Domain),
  timer:sleep(60000),
  toxic_client:launch_traffic(8000, 10, Domain),
  loop(10).

%% loop/1 is a helper function that "prevents" run/3 from finishing until all
%% loggers have halted. Without this function the benchmark would finish after
%% launching the traffic generators and, thus, bencherl would kill all spawned
%% nodes, i.e. routers, servers, etc.
loop(0) -> ok;
loop(N_Loggers) ->
  receive
    logger_stopped -> loop(N_Loggers - 1)
  end.
