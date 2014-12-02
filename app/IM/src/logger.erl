%%%--------------------------------------------------------------------
%%% LOGGER MODULE
%%%
%%% @author: Mario Moro Hernandez
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%     Logger module is a recorder of messages sent between clients 
%%%     logged in the IM system. 
%%%
%%%	This module allows to run throughput and latency experiments,
%%%     by recording these measurementes.
%%% @end 
%%%
%%% Created: 30 Mar 2014 by Mario Moro Hernandez
%%% Modified: 25 Aug 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------

-module(logger).
-export([start/2, start_throughput/5, start_latency/4, launch/9, launch/10,
	 launch_latency/7, launch_latency/8]).

%%---------------------------------------------------------------------
%% @doc
%%     start/2 starts a generic logger, which records the latency of
%%     successfully delivered messages on a text file named as specified
%%     in the argument passed to the function.
%%
%%     Argument:
%%       - Dir: (String) the directory of the .csv output files.
%%       - FileName: (String) name of the file.
%% 
%% @spec start(FileName) -> File.csv | {error, io_error_create_file}
%% @end
%%----------------------------------------------------------------------
start(Dir, FileName) ->
    case whereis(logger) of
        undefined->
	    {_Status, Fd} = create_file(Dir, FileName),
	    case Fd of
		unable_to_open_file ->
		    io:format("ERROR: generic logger cannot start.~n"),
		    {error, io_error_create_file};
		Pid ->
		    register(logger, spawn(fun() -> logger:loop({recording,
		                                                 Pid},
		                                                Dir) end))
	    end;
        _ ->
	    io:format("ERROR: The logger process has already started.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch/9 initiates a throughput logger (see below) on each of the
%%     client nodes deployed in the architecture.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) T, is the time in seconds in which the logger must
%%                 be active.
%%       - Threshold: (int) R, is the time in microseconds which is 
%%                    considered acceptable for a good service.
%%       - Nodes: (List) List containingh the deployed client nodes.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec launch(Technology, Routers, Servers, Clients, Num_Nodes,
%%              Trials, Timer, Threshold, Domain) -> ok;
%% @end
%%---------------------------------------------------------------------
launch(Technology, Routers, Servers, Clients, Trials, Timer, Threshold, Nodes,
       Dir) ->
    case Nodes of
	[] ->
	    io:format("All loggers are launched now.~n"),
	    ok;
	[Node|New_Nodes] ->
	    [Node_Name|_Domain] = string:tokens(atom_to_list(Node),"@"),
	    case string:sub_word(Node_Name, 2, $_) of
		[] ->
		    Num_Node = list_to_integer(string:sub_word(Node_Name, 2, $t));
		Val ->
		    Num_Node = list_to_integer(Val)
	    end,
	    spawn(Node, fun() -> start_throughput(Technology,
						  Routers,
						  Servers,
						  Clients,
						  Num_Node,
						  Trials,
						  Timer,
						  Threshold,
						  n,
					          Dir)
			end),
	    launch(Technology, Routers, Servers, Clients, Trials, Timer,
	           Threshold, New_Nodes, Dir)
    end.    

%%---------------------------------------------------------------------
%% @doc
%%     launch/10 initiates a throughput logger (see below) on each of the
%%     client nodes deployed in the architecture.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Num_Node: (int) M, number of client nodes deployed.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) T, is the time in seconds in which the logger must
%%                 be active.
%%       - Threshold: (int) R, is the time in microseconds which is 
%%                    considered acceptable for a good service.
%%       - Domain: (Atom) Domain or host where the client nodes are deployed.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec launch(Technology, Routers, Servers, Clients, Num_Nodes,
%%              Trials, Timer, Threshold, Domain) -> ok;
%% @end
%%---------------------------------------------------------------------
launch(Technology, Routers, Servers, Clients, Num_Nodes, Trials, Timer,
       Threshold, Domain, Dir) ->
    case Num_Nodes == 0 of
	true ->
	    io:format("All loggers are launched now.~n"), 
	    ok;
	false ->
	    Node = client_node_name(Num_Nodes, Domain),
	    spawn(Node, fun() -> start_throughput(Technology,
						  Routers,
						  Servers, 
						  Clients,
						  Num_Nodes,
						  Trials,
						  Timer,
						  Threshold,
						  n,
					          Dir)
			end),
	    launch(Technology, Routers, Servers, Clients, Num_Nodes - 1, Trials,
	           Timer, Threshold, Domain, Dir)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     start_throughput/10 starts a throughput logger, which records the
%%     number of sent messages, received messages, successfully delivered,
%%     messages delivered below an arbitrary time threshold, and the
%%     quality of the service provided, during specified time-window.
%%     The results are written in two text files named after the arguments
%%     passed to the function. One of these files is a .csv file ready for
%%     being process on a data spreadsheet, or statistical analysis software,
%%     whereas the other one is a .txt file that offers a summary in text.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Num_Node: (int) M, number of client nodes deployed.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) T, is the time in seconds in which the logger must
%%                be active.
%%       - Threshold: (int) R, is the time in microseconds which is considered
%%                    acceptable for a good service.
%%       - Dump_to_file: (Atom) y, 'Yes', _Other. Not in use.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec start_throughput(Technology, Routers, Servers, Clients,
%%                        Num_Node, Num_Trials, Timer, Threshold,
%%                        Dump_Table_To_File) -> File.txt; File.csv
%% @end
%%--------------------------------------------------------------------
start_throughput(Technology, Routers, Servers, Clients, Num_Node, Num_Trials,
                 Timer, Threshold, Dump_Table_To_File, Dir) ->
    case whereis(throughput_logger) of
	undefined ->
	    Table_Name = string:join([Technology, "Throughput", 
				      integer_to_list(Routers),
				      integer_to_list(Servers),
				      integer_to_list(Clients),
				      "Node",
				      integer_to_list(Num_Node)], "_"),
	    record_throughput({ets_based, Table_Name, Num_Trials, Timer,
	                       Threshold, Dump_Table_To_File}, Dir);
	_Other ->
	    io:format("ERROR: The logger process has already started.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     record_throughput/2 starts a throughput logger, which records the
%%     number of sent, received, successfully delivered, and delivered
%%     below a time-threshold, messages for an specified time-window.
%%     The results are output on two text files named after the parameters
%%     passed as arguments to the function.
%%
%%     Arguments:
%%       - Table_Name: (String) S, name of the ets table to record the results.
%%                     Not in use.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) T, is the time in seconds in which the logger must
%%                be active.
%%       - Threshold: (int) R, is the time in microseconds which is considered
%%                    acceptable for a good service.
%%       - Dump_to_file: (Atom) y, 'Yes', _Other. Not in use.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec record_throughput({ets_based, Table_Name, Num_Trials, Timer,
%%                          Threshold, Dump_Table_To_File}) ->
%%                   File.txt; File.csv
%% @end
%%---------------------------------------------------------------------
record_throughput({ets_based, Table_Name, Num_Trials, Timer, Threshold,
                   Dump_Table_To_File}, Dir) ->
    case Num_Trials > 0 of
	true ->
	    case whereis(throughput_logger) of
		undefined ->
		    register(throughput_logger,
			     spawn(fun() -> loop({initial_state,
						  Table_Name,
						  Num_Trials,
						  Threshold}, Dir)
				   end));
		_Other ->
		    ok
	    end,
	    timer:sleep(Timer * 1000),
	    whereis(throughput_logger) ! {new_trial, Num_Trials - 1},
	    record_throughput({ets_based, Table_Name, Num_Trials - 1, Timer,
	                       Threshold, Dump_Table_To_File}, Dir);
	false ->
	    whereis(throughput_logger) ! {stop_throughput},
	    io:format("Throughput benchmarks finished.~n"),
	    case Dump_Table_To_File of
		'Yes' ->
		    File = string_to_atom(Table_Name ++ ".csv"),
		    R = ets:tab2file(string_to_atom(Table_Name), File),
		    io:format("R = ~p~n", [R]),
		    io:format("File ~p has been created.~n", [File]);
		_Other ->
		    ok
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%%     start_throughput/5 starts a throughput logger, which records the
%%     number of successfully delivered messages for an specified time-
%%     window on a text file named after the parameters passed as
%%     arguments to the function.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Condition: (String) "i_routers_j_servers_k_clients" where i,
%%                     j, and k are integers denoting the number of
%%                     routers, servers and clients composing the
%%                     benchmarked system.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) M, is the time in seconds in which the logger
%%                must be active.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec start(Technology, Condition, Num_of_Trials, Timer) -> File.csv
%% @end
%%---------------------------------------------------------------------
start_throughput(Technology, Condition, Num_of_trials, Timer, Dir) ->
    case whereis(throughput_logger) of
	undefined ->
	    {_Status, Fd} = create_file(Dir, Technology, "Throughput",
	                                Condition, Num_of_trials),
	    case Fd of
		unable_to_open_file ->
		    io:format("ERROR: latency logger cannot start.~n"),
		    exit(io_error_create_file);
		_ ->
		    record_throughput(Technology, Condition, Num_of_trials,
		                      Timer, Fd, Dir)
	    end;
	_ ->
	    io:format("ERROR: The logger process has already started.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     record_throughput/6 starts a throughput logger, which records the
%%     number of successfully delivered messages for an specified time-
%%     window on a text file named after the parameters passed as
%%     arguments to the function.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Condition: (String) "i_routers_j_servers_k_clients" where i,
%%                    j, and K are integers denoting the number of
%%                    routers, servers and clients composing the
%%                    benchmarked system.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Timer: (int) M, is the time in seconds in which the logger must
%%                be active.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec record_throughput(Technology, Condition, Num_of_Trials, Timer, Fd)
%%                   -> File.csv
%% @end
%%---------------------------------------------------------------------
record_throughput(Technology, Condition, Num_of_trials, Timer, Fd, Dir) ->
    case Num_of_trials >= 1 of
	true ->
	    register(throughput_logger, spawn(fun() -> loop({recording, Fd},
	                                                    Dir) end)),
	    timer:sleep(Timer * 1000),
	    stop(Fd, throughput_logger),
	    start_throughput(Technology, Condition, Num_of_trials - 1, Timer,
	                     Dir);
	false ->
	    io:format("Throughput benchmarks finished.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch_latency/7 initiates a latency logger (see below) on each
%%     of the client nodes deployed in the architecture.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Series: (int) N, is the number of series to be recorded.
%%       - Nodes: (List) List containing the deployed client nodes.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec launch(Technology, Routers, Servers, Clients, Num_Nodes,
%%              Trials, Timer, Threshold, Domain) -> ok;
%% @end
%%---------------------------------------------------------------------
launch_latency(Technology, Routers, Servers, Clients, Series, Nodes, Dir) ->
    case Nodes of
	[] ->
	    io:format("All loggers are launched now.~n"),
	    ok;
	[Node|New_Nodes] ->
	    [Node_Name|_Domain] = string:tokens(atom_to_list(Node),"@"),
	    case string:sub_word(Node_Name, 2, $_) of
		[] ->
		    Num_Node = list_to_integer(string:sub_word(Node_Name, 2, $t));
		Val ->
		    Num_Node = list_to_integer(Val)
	    end,
	    spawn(Node, fun() -> start_latency(Technology,
					       Routers,
					       Servers,
					       Clients,
					       Series,
					       Num_Node,
				               Dir)
			end),
	    launch_latency(Technology, Routers, Servers, Clients, Series,
	                   New_Nodes, Dir)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     launch_latency/8 initiates a latency logger (see below) on each
%%     of the client nodes deployed in the architecture.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Num_Node: (int) M, number of client nodes deployed.
%%       - Series: (int) N, is the number of series to be recorded.
%%       - Domain: (Atom) Domain or host where the client nodes are deployed.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec launch(Technology, Routers, Servers, Clients, Num_Nodes,
%%              Trials, Timer, Threshold, Domain) -> ok;
%% @end
%%---------------------------------------------------------------------
launch_latency(Technology, Routers, Servers, Clients, Num_Nodes, Series,
               Domain, Dir) ->
    case Num_Nodes == 0 of
	true ->
	    io:format("All loggers are launched now.~n"), 
	    ok;
	false ->
	    Node = client_node_name(Num_Nodes, Domain),
	    spawn(Node, fun() -> start_latency(Technology,
					       Routers,
					       Servers, 
					       Clients,
					       Series,
					       Num_Nodes,
				               Dir)
			end),
	    launch_latency(Technology, Routers, Servers, Clients, Num_Nodes - 1,
	                   Series, Domain, Dir)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     start_latency/7 starts a latency logger, which records the latency
%%     of 20000 successfully delivered messages on a text file named after
%%     the parameters passed as arguments to the function.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Routers: (int) J, number of router nodes.
%%       - Servers: (int) K, number of server nodes.
%%       - Clients: (int) L, number of client processes.
%%       - Num_Node: (int) M, number of client nodes deployed.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec start_latency(Technology, Routers, Servers,
%%                     Clients, Num_Node, Series) -> File.csv
%% @end
%%---------------------------------------------------------------------
start_latency(Technology, Routers, Servers, Clients, Series, Num_Node, Dir) ->
    case whereis(latency_logger) of
	undefined ->
	    Condition = string:join([integer_to_list(Routers),
				     integer_to_list(Servers),
				     integer_to_list(Clients),
				     "Node",
				     integer_to_list(Num_Node)], "_"),
	    register(latency_logger, spawn(fun() ->
	                                           put(stats, []),
						   loop(element(2, create_file(Dir,
						                               Technology,
									       "Latency",
									       Condition,
									       Series)),
							0,
							Technology,
							Condition,
							Series,
						        Dir)
					   end));
	_Other ->
	    io:format("ERROR: The logger process has already started.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     start_latency/4 starts a latency logger, which records the latency
%%     of 20000 successfully delivered messages on a text file named after
%%     the parameters passed as arguments to the function.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Condition: (String) "i_routers_j_servers_k_clients" where i,
%%                     j, and k are integers denoting the number of
%%                     routers, servers and clients composing the
%%                     benchmarked system.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec start_latency(Technology, Condition, Num_of_Trials) -> File.csv
%% @end
%%---------------------------------------------------------------------
start_latency(Technology, Condition, Num_of_trials, Dir) ->
    case whereis(latency_logger) of
	undefined ->
	    {_Status, Fd} = create_file(Dir, Technology, "Latency", Condition,
	                                Num_of_trials),
	    case Fd of
		unable_to_open_file ->
		    io:format("ERROR: latency logger cannot start.~n"),
		    exit(io_error_create_file);
		_ ->
		    register(latency_logger, spawn(fun() -> loop(Fd,
								 0,
								 Technology,
								 Condition,
								 Num_of_trials,
							         Dir)
						   end))
	    end;
	_ ->
	    io:format("ERROR: The logger process has already started.~n")
    end.



%%---------------------------------------------------------------------
%% @doc
%%     create_file/2 opens a file with name FileName to hold the recorded
%%     information during the benchmarking of the system. If the file
%%     specified by the FileName argument does not exist, then the
%%     function creates the file.
%%
%%     Argument:
%%       - FileName: (String) name of the file to be opened or created.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec create_file(Filename) -> {ok, Fd} |
%%                                {error, unable_to_open_file}
%% @end 
%%---------------------------------------------------------------------
create_file(_Dir, _FileName) ->
%    case file:open(Dir ++ FileName,[read,write]) of
%	{ok, Fd} ->
%	    {ok, Eof} = file:position(Fd,eof),
%	    file:position(Fd,Eof),
%	    io:format("INFO: The logger process has been started.~n"),
%	    {ok, Fd};
%	_ ->
%	    io:format("ERROR: The file cannot be opened.~n"),
%	    {error, unable_to_open_file}
%    end.
    io:format("INFO: The logger process has been started.~n"),
    {ok, fd}.

%%---------------------------------------------------------------------
%% @doc
%%     create_file/5 opens a file with name "Technology_Condition_Trial.csv"
%%     to hold the recorded information during the benchmarking of the
%%     system. If the file specified does not exist, then the function
%%     creates the file.
%%
%%     Arguments:
%%       - Techology: (String) "DErlang" or "SDErlang".
%%       - Benchmark: (String) "Latecy" or "Throughput".
%%       - Condition: (String) "i_routers_j_servers_k_clients" where i,
%%                    j, and k are integers denoting the number of
%%                    routers, servers and clients composing the
%%                    benchmarked system.
%%       - Num_of_trials: (int) N, is the number of series to be recorded.
%%       - Dir: (String) the directory of the .csv output files.
%%
%% @spec create_file(Technology, Benchmark, Condition, Trial) ->
%%                  {file_descriptor, Fd} | {error, unable_to_open_file}
%% @end
%%---------------------------------------------------------------------
create_file(_Dir, _Technology, _Benchmark, _Condition, _Trial) ->
%    Tr = integer_to_list(Trial),
%    FileName =
%      Dir ++ string:join([Technology, Tr, Benchmark, Condition],"_") ++ ".csv",
%    case file:open(FileName,[read,write]) of
%	{ok, Fd} ->
%	    {ok, Eof} = file:position(Fd,eof),
%	    file:position(Fd,Eof),
%	    io:format("INFO: The log file has been created.~n"),
%	    {file_descriptor, Fd};
%	_ ->
%	    io:format("ERROR: The file cannot be opened.~n"),
%	    {error, unable_to_open_file}
%    end.
    io:format("INFO: The log file has been created.~n"),
    {file_descriptor, fd}.

%%---------------------------------------------------------------------
%% @doc
%%     stop/2 close the file described by file descriptor Fd and unregisters
%%     the logger from the global processes registry.
%%   
%%     Arguments:
%%       - Fd: (IoDevide) Reference to the active file.
%%       - Logger: (String) name of the logger process to be stopped.
%%
%% @spec stop(Fd, Logger) -> Message.
%% @end
%%---------------------------------------------------------------------
stop(_Fd, Logger) ->
    case whereis(Logger) of
        undefined ->
            io:format("ERROR: The logger process cannot be stopped because it"
                      ++ "has not started.~n");
        _ ->
%            file:close(Fd),
            unregister(Logger),
            io:format("INFO: The logger process has been stopped.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     loop/2 is a recursive function to record the throughput and the
%%     latency of the messages into the file and stop the active logger
%%     process.
%%
%%     Note that this function is to be called by the generic logger and
%%     the throughput_logger processes.
%%
%% @spec loop(Fd, Dir) -> term()
%% @end
%%---------------------------------------------------------------------
loop({initial_state, Table_Name, Num_Trials, Threshold}, Dir) ->
    TN = string_to_atom(Table_Name),
    %%ets:new(TN, [named_table, set]),
    {_Status, Fd1} = create_file(Dir, Table_Name ++ ".csv"),
    case Fd1 of
	unable_to_open_file ->
	    io:format("ERROR: throughput logger cannot start.~n"),
	    exit(io_error_create_file);
	_Other ->
	    ok
    end,
    {_Status, Fd2} = create_file(Dir, Table_Name ++ "_Summary.txt"),
    case Fd2 of
	unable_to_open_file ->
	    io:format("ERROR: throughput logger cannot start.~n"),
	    exit(io_error_create_file);
	_Another ->
%            io:fwrite(Fd2, "=============== Beginning of Benchmarks ========="
%	        ++ "======~n~n",[])
	    ok
    end,
    Statistics = {0,0,0,0},
    loop({recording, Fd1, Fd2, TN, Num_Trials, Threshold, Statistics}, Dir);

loop({recording, Fd1, Fd2, Table_Name, Num_Trials, Threshold, Statistics},
     Dir) ->
    {Sent, Received, Delivered, Dlv_Blw_Thr} = Statistics,
    receive
	{s, _Metadata, not_delivered} ->
	    %% {Unique_ID, Session_Name, Sender, Receiver, Timestamp} = Metadata,
	    %% {{Y,M,D},{Hour,Min,Sec}} = calendar:now_to_local_time(Timestamp),
	    %% Time = string:join([integer_to_list(Hour), integer_to_list(Min), integer_to_list(Sec)], ":"),
	    %% Date = string:join([integer_to_list(D), integer_to_list(M), integer_to_list(Y)], "/"),
	    %% ets:insert(Table_Name, {Unique_ID, Time, Date, Session_Name, Sender, Receiver, lost}),
	    New_Sent = Sent + 1,
	    New_Statistics = {New_Sent, Received, Delivered, Dlv_Blw_Thr},
	    loop({recording, Fd1, Fd2, Table_Name, Num_Trials, Threshold,
	          New_Statistics}, Dir);
	{r, _Metadata, received} ->
	    %% {_Unique_ID, _Session_Name, _Sender, _Receiver, Timestamp} = Metadata,
	    %% {{Y,M,D},{Hour,Min,Sec}} = calendar:now_to_local_time(Timestamp),
	    %% Time = string:join([integer_to_list(Hour), integer_to_list(Min), integer_to_list(Sec)], ":"),
	    %% Date = string:join([integer_to_list(D), integer_to_list(M), integer_to_list(Y)], "/"),
	    %% ets:insert(Table_Name, {Unique_ID, Time, Date, Session_Name, Sender, Receiver, lost}),
	    New_Received = Received + 1,
	    New_Statistics = {Sent, New_Received, Delivered, Dlv_Blw_Thr},
	    loop({recording, Fd1, Fd2, Table_Name, Num_Trials, Threshold,
	          New_Statistics}, Dir);
	{d, _Metadata, Latency} ->
	    %% {Unique_ID, Session_Name, Sender, Receiver, Timestamp} = Metadata,
	    %% {{Y,M,D},{Hour,Min,Sec}} = calendar:now_to_local_time(Timestamp),
	    %% Time = string:join([integer_to_list(Hour), integer_to_list(Min), integer_to_list(Sec)], ":"),
	    %% Date = string:join([integer_to_list(D), integer_to_list(M), integer_to_list(Y)], "/"),
	    %% ets:insert(Table_Name, {Unique_ID, Time, Date, Session_Name, Sender, Receiver, Latency}),
	    New_Delivered = Delivered + 1,
	    case Latency < Threshold of
		true ->
		    New_Dlv_Blw_Thr = Dlv_Blw_Thr + 1,
		    New_Statistics = {Sent, Received, New_Delivered, New_Dlv_Blw_Thr};
		false ->
		    New_Statistics = {Sent, Received, New_Delivered, Dlv_Blw_Thr}
	    end,
	    loop({recording, Fd1, Fd2, Table_Name, Num_Trials, Threshold,
	          New_Statistics}, Dir);
	{new_trial, New_Num_Trials} ->
	    {{Y, M, D},{Hour,Min,Sec}} = calendar:now_to_local_time(os:timestamp()),
	    _Time = string:join([integer_to_list(Hour), integer_to_list(Min),
	                        integer_to_list(Sec)], ":"),
	    _Date = string:join([integer_to_list(D), integer_to_list(M),
	                        integer_to_list(Y)], "/"),
	    _ = case Sent /= 0 of
		true ->
		    _QoS = (Dlv_Blw_Thr / Sent) * 100;
		false ->
		    _QoS = "N/A"
	    end,
	%    io:fwrite(Fd1,"~p,~p,~p,~p,~p,~p,~p,~p,~p~n",  [Num_Trials,
	%						    Time,
	%						    Date,
	%						    Threshold,
	%						    Sent,
	%						    Received,
	%						    Delivered,
	%						    Dlv_Blw_Thr,
	%						    QoS]),
	%    io:fwrite(Fd2, "Trial ~p: ~p ~p~nThreshold:~p~nMessages Sent: ~p~n"
	%        ++ "Messages Received: ~p~nMessages Delivered: ~p~nMessages"
	%        ++ "Delivered below threshold: ~p~nQuality of service:~p \%"
	%        ++ "~n~n", [Num_Trials, Time, Date, Threshold, Sent, Received,
	%                    Delivered, Dlv_Blw_Thr, QoS]),
	    loop({recording, Fd1, Fd2, Table_Name, New_Num_Trials, Threshold,
	         {0,0,0,0}}, Dir);
	{stop_throughput} ->
	%    io:fwrite(Fd2, "=============== End of Benchmarks ==============="
	%              ++ "~n~n", []),
	%    file:close(Fd1),
	%    file:close(Fd2),
	    ok		
    end;

loop({recording, Fd}, Dir) ->
    receive
	{d, Metadata, _Latency} ->
	    {_Unique_ID, _Session_Name, _Client_A, _Client_B, Timestamp} = Metadata,
	    {{_Y,_M,_D},{_Hour,_Min,_Sec}} = calendar:now_to_local_time(Timestamp),
	%    io:fwrite(Fd,"~p, ~p:~p:~p ~p/~p/~p,~p,~p,~p,~p~n", [Unique_ID,
	%							 Hour,
	%							 Min,
	%							 Sec,
	%							 D, M, Y,
	%							 Session_Name,
	%							 Client_A,
	%							 Client_B,
	%							 Latency]),
	%    file:position(Fd,eof),
	    loop({recording, Fd}, Dir);
	stop ->
	    stop(Fd, logger);
	stop_throughput ->
	    stop(Fd, throughput_logger);
	_Other ->
	    loop({recording, Fd}, Dir)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     loop/6 is a recursive function to record the latency of the messages
%%     into the file and stop the active latency_logger process.
%%
%% @spec loop(Fd, Record, Technology, Condition, Trial, Dir) -> term() | ok
%% @end
%%---------------------------------------------------------------------
loop(Fd, Record, Technology, Condition, Trial, Dir) ->
    receive
	{d, Metadata, Latency} ->
	    {_Unique_ID, Session_Name, Client_A, Client_B, Timestamp} = Metadata,
	    {{_Y,_M,_D},{_Hour,_Min,_Sec}} = calendar:now_to_local_time(Timestamp),
	    Stats = get(stats),
           put(stats, [{Session_Name, Client_A, Client_B, Latency} | Stats]),
	%    io:fwrite(Fd,"~p,~p:~p:~p ~p/~p/~p,~p,~p,~p,~p~n", [Unique_ID,
	%							Hour,
	%							Min,
	%							Sec,
	%							D, M, Y,
	%							Session_Name,
	%							Client_A,
	%							Client_B,
	%							Latency]),
	%    file:position(Fd,eof),
	    case Record =< 20000 of
		true ->
		    %%io:format("Trial: ~p, Record: ~p~n", [Trial, Record]),
		    loop(Fd, Record + 1, Technology, Condition, Trial, Dir);
		false ->
		    self() ! {stop_latency, Trial},
		    loop(Fd, Record + 1, Technology, Condition, Trial, Dir)
	    end;
	{stop_latency, Trial} ->
	    case Trial > 1 of
		true ->
		    %%io:format("stop_latency received; case Trial > 0 of true."
		    %% ++ "Trial = ~p~n", [Trial]),
%		    file:close(Fd),
		    loop(element(2,create_file(Dir, Technology, "Latency", Condition,
		         Trial - 1)), 0, Technology, Condition, Trial - 1, Dir);
		false ->
		    %%io:format("stop_latency received; case Trial > 0 of false."
		    %% ++ "Trial = ~p~n", [Trial]),
		    stop(Fd, latency_logger),
		    %%XXX My super stats!
                    Stats = [Lat || {_, _, _, Lat} <- get(stats)],
		    L = length(Stats),
		    Avg = lists:sum(Stats)/L,
                    Median = lists:nth(L div 2, lists:sort(Stats)),
		    %%XXX: Notify the coordinator that a logger has finished.
                    case global:whereis_name(coordinator) of
                        undefined -> ok;
                        CoordinatorPid ->
                          CoordinatorPid !
                            {logger_stopped, node(), L, Avg, Median, Stats}
                    end,
                    ok
	    end
    end.

%%===============================================================================
%% AUXILIARY FUNCTIONS
%%===============================================================================
%%--------------------------------------------------------------------
%% @doc
%%     string_to_atom/1 takes a string and returns an atom, or an
%%     existing atom if that is the case.
%%
%% @spec string_to_atom(String) -> atom() | existing_atom()
%% @end
%%--------------------------------------------------------------------
string_to_atom(String) ->
    try list_to_existing_atom(String) of
	Val ->
	    Val
    catch
	error:_ ->
	    list_to_atom(String)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     client_node_name/2 builds an atom corresponding to the name of
%%     a target client node.
%%     
%% @spec client_node_name(Node_Number, Domain) -> atom() | existing_atom()
%% @end
%%--------------------------------------------------------------------
client_node_name(Node_Number, Domain) ->
    case is_atom(Domain) of
	true ->
	    D = atom_to_list(Domain);
	false ->
	    D = Domain
    end,
    Str = "client_" ++ integer_to_list(Node_Number) ++ "@" ++ D,
    string_to_atom(Str).
