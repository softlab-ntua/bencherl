-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(Vsn, Conf) ->
  { _, Slaves } = lists:keyfind(slaves, 1, Conf),
  ClientNodes = filter_nodes(Slaves, "client"),
  Ratio =
    case Vsn of
      short        ->  5000;
      intermediate -> 10000;
      long         -> 15000
    end,
  ClientProcs =
    case os:getenv("CLIENT_PROCS") of
       false -> Ratio * length(ClientNodes);
       P     -> erlang:list_to_integer(P)
    end,
  %io:format("Number of client processes: ~p~n", [ClientProcs]),
  [[ ClientProcs, Ratio ]].

run([Clients, ClientsPerNode], Slaves, Conf) ->
  %% Setup a coordinator to know when the benchmark finished. This is done by
  %% counting the number of loggers that have finished.
  global:register_name(coordinator, self()),
  %% Get the data dir in order to store the .csv output files there.
  { _, DataDir } = lists:keyfind(datadir, 1, Conf),
  %% Get the benchmark arguments from the configuration.
  ClientNodes = filter_nodes(Slaves, "client"),
  ServerNodes = filter_nodes(Slaves, "server"),
  RouterNodes = filter_nodes(Slaves, "router"),
  NC = length(ClientNodes),
  % Start the benchmark on the different client domains.
  launcher:start_bencherl(length(ServerNodes) div length(RouterNodes),
                          1,
                          length(ServerNodes),
                          NC,
                          Slaves),
%  timer:sleep(10000),
  logger:launch_latency("Bencherl_test",
                        length(RouterNodes),
                        length(ServerNodes),
                        Clients,
                        1,
                        ClientNodes,
                        DataDir ++ "/"),
  timer:sleep(60000), %XXX: Just to make sure that the IM has been deployed.
  %% Open the statistics file.
  {ok, Fd} = file:open(DataDir ++ "/statistics.txt", [append]),
  %% Log the configuration of the experiment.
  io:fwrite(Fd, "# Conf: ~w scheduler(s), ~w server(s), ~w router(s), "
        "~w client(s), ~w client processes~n",
    [element(2, lists:keyfind(schedulers, 1, Conf)), length(ServerNodes),
     length(RouterNodes), NC, Clients]),
  io:fwrite(Fd, "# <Node> <Messages> <Average Latency> <Median Latency>~n", []),
  %% Launch and login the clients
  toxic_client_lite:launch(ClientsPerNode, ClientNodes),
  InitialInfo = dict:from_list([{N, dict:new()} || N <- ClientNodes]),
  ClientsInfo = wait_for_lite_clients(Clients, InitialInfo),
  StartTime = os:timestamp(),
  %% Start the traffic generators
  toxic_client_lite:launch_traffic(ClientNodes, ClientsInfo),
  timer:sleep(250000),  % Benchmark duration.
  %% Stop the loggers
  stop_loggers(ClientNodes),
  EndTime = os:timestamp(),
  wait_results_from_loggers(NC, Fd),
  %% Log the execution time.
  io:fwrite(Fd, "* Execution time: ~w secs.~n",
    [timer:now_diff(EndTime, StartTime) / 1000000]),
  %% Close the statistics file.
  ok = file:close(Fd).

%% filter_nodes/2 returns the nodes in the given list whose name starts with
%% the given prefix.
filter_nodes(Nodes, Prefix) ->
  lists:filter(fun(N) ->
      string:sub_string(atom_to_list(N), 1, string:len(Prefix)) == Prefix
    end, Nodes).

%% Stops the loggers at the client nodes.
stop_loggers(Nodes) ->
  lists:foreach(fun (N) ->
                  {latency_logger, N} ! {stop_latency, 0}
                end, Nodes).

%% wait_results_from_loggers/2 is a helper function that "prevents" run/3 from
%% finishing until all loggers have halted. Without this function the benchmark
%% would finish after launching the traffic generators and, thus, bencherl
%% would kill all spawned nodes, i.e. routers, servers, etc.
wait_results_from_loggers(0, _) -> ok;
wait_results_from_loggers(Loggers, Fd) ->
  receive
    {logger_stopped, Node, L, Avg, Mean, Stats} ->
        io:fwrite(Fd, "~p ~w ~.2f microsecs ~w microsecs ( ",
                  [Node, L, Avg, Mean]),
        lists:foreach(fun (I) -> io:fwrite(Fd, "~w ", [I]) end, Stats),
        io:fwrite(Fd, ")~n", []),
        wait_results_from_loggers(Loggers - 1, Fd)
  end.

%% wait_for_lite_clients/2 waits for all the client processes to be deployed
%% and logged in and it returns a dictionary of their names and pids per node.
%% Info :: dict:dict(node(), dict:dict(string(), pid()))
wait_for_lite_clients(0, Info) -> Info;
wait_for_lite_clients(Clients, Info) ->
  receive
    {client_setup_ok, Node, Name, Pid} ->
        UpdatedInfo =
            dict:update(Node,
                        fun(Registry) -> dict:store(Name, Pid, Registry) end,
                        Info),
        wait_for_lite_clients(Clients - 1, UpdatedInfo)
  end.
