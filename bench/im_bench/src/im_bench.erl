-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(short, _Conf) ->
  [[8000]];
bench_args(intermediate, _Conf) ->
  [[20000]]; %XXX: Find a value that makes sense!
bench_args(long, _Conf) ->
  [[80000]]. %XXX: Find a value that makes sense!

run([Clients], Slaves, Conf) ->
  % Setup a coordinator to know when the benchmark finished. This is done by
  % counting the number of loggers that have finished.
  global:register_name(coordinator, self()),
  % Get the data dir in order to store the .csv output files there.
  {_,DataDir} = lists:keyfind(datadir, 1, Conf),
  % Get the benchmark arguments from the configuration.
  ClientNodes = filter_nodes(Slaves, "client"),
  ServerNodes = filter_nodes(Slaves, "server"),
  RouterNodes = filter_nodes(Slaves, "router"),
  % Start the benchmark on the different client domains.
  launcher:start_bencherl(length(ServerNodes) div length(RouterNodes), 1,
    length(ServerNodes), length(ClientNodes), Slaves),
  logger:launch_latency("Bencherl_test", length(RouterNodes), length(ServerNodes),
    Clients, 1, ClientNodes, DataDir ++ "/"),
  timer:sleep(60000), %XXX: Just to make sure that all clients have logged in.
  toxic_client:launch(Clients, ClientNodes),
  timer:sleep(60000),
  toxic_client:launch_traffic(Clients, ClientNodes),
  loop(length(ClientNodes)).

%% filter_nodes/2 returns the nodes in the given list whose name starts with
%% the given prefix.
filter_nodes(Nodes, Prefix) ->
  lists:filter(fun(N) ->
      string:sub_string(atom_to_list(N), 1, string:len(Prefix)) == Prefix
    end, Nodes).

%% loop/1 is a helper function that "prevents" run/3 from finishing until all
%% loggers have halted. Without this function the benchmark would finish after
%% launching the traffic generators and, thus, bencherl would kill all spawned
%% nodes, i.e. routers, servers, etc.
loop(0) -> ok;
loop(N_Loggers) ->
  receive
    logger_stopped -> loop(N_Loggers - 1)
  end.
