-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(short, _Conf) ->
  [[8000]];
bench_args(intermediate, _Conf) ->
  [[8000]]; %XXX: Find a value that makes sense!
bench_args(long, _Conf) ->
  [[8000]]. %XXX: Find a value that makes sense!

run([Clients], Slaves, Conf) ->
  % Setup a coordinator to know when the benchmark finished. This is done by
  % counting the number of loggers that have finished.
  register(coordinator, self()),
  % Get the data dir in order to store the .csv output files there.
  {_,DataDir} = lists:keyfind(datadir, 1, Conf),
  % Get the benchmark arguments from the configuration.
  ClientNodes = filter_nodes(Slaves, "client"),
  ServerNodes = filter_nodes(Slaves, "server"),
  RouterNodes = filter_nodes(Slaves, "router"),
  Domains = find_domains(Slaves),
  % Start the benchmark on the different client domains.
  launcher:start(length(ServerNodes) div length(RouterNodes), 1,
    length(ServerNodes), length(ClientNodes), Domains, length(Domains)),
  ClientDomains = find_domains(ClientNodes),
  lists:foreach(fun(D) ->
      logger:launch_latency("Bencherl_test", length(RouterNodes),
        length(ServerNodes), Clients, length(ClientNodes), 1, D, DataDir ++ "/"),
      timer:sleep(60000), %XXX: Just to make sure that all clients have logged in.
      toxic_client:launch(Clients, length(ClientNodes), D),
      timer:sleep(60000),
      toxic_client:launch_traffic(Clients, length(ClientNodes), D)
    end, ClientDomains),
  loop(length(ClientNodes)).

%% filter_nodes/2 is a helper function that returns the nodes in the given list
%% whose name starts with the given prefix.
filter_nodes(Nodes, Prefix) ->
  lists:filter(fun(N) ->
      string:sub_string(atom_to_list(N), 1, string:len(Prefix)) == Prefix
    end, Nodes).

%% find_domains/1 is a helper function that returns the different domains of
%% the given nodes.
find_domains(Nodes) ->
  lists:usort(lists:map(fun(N) ->
      [_N, H] = string:tokens(atom_to_list(N), "@"),
      list_to_atom(H)
    end, Nodes)).

%% loop/1 is a helper function that "prevents" run/3 from finishing until all
%% loggers have halted. Without this function the benchmark would finish after
%% launching the traffic generators and, thus, bencherl would kill all spawned
%% nodes, i.e. routers, servers, etc.
loop(0) -> ok;
loop(N_Loggers) ->
  receive
    logger_stopped -> loop(N_Loggers - 1)
  end.
