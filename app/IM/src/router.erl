%%%-------------------------------------------------------------------
%%% ROUTER MODULE
%%%
%%% @author Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Router module for the Distributed Erlang instant messenger (IM)
%%%	application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the functionality for router nodes in a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end
%%% Created : 25 Jul 2014 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(router).
-export([router_supervisor/5, router_process/1, chaos_on/0, chaos_on/1, chaos_on/2]).

-import(launcher, [launch_router_processes/6, launch_server_supervisors/4]).
-import(server, [restart_server_supervisor/1]).

%%===============================================================================
%% ROUTER PROCESSES CODE
%%===============================================================================

%%---------------------------------------------------------------------
%% @doc
%%     router_supervisor_monitor/1 is a process that monitors the router
%%     supervisor process, and re-starts it in the case of failure.
%%
%%     This process has three states. The initial state stablishes the
%%     link between router_supervisor and router_supervisor_monitor
%%     processes. The final state just listens for any change in the
%%     router supervisor information, and traps the router supervisor
%%     failure triggering the recovery strategy. Finally, the supervisor
%%     recovery state is in charge of the recovery of the router supervisor
%%     process.
%%
%% @spec router_supervisor_monitor/1.
%% @end
%%---------------------------------------------------------------------
router_supervisor_monitor({initial_state, R_Sup_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids}) ->
    erlang:monitor(process, R_Sup_Pid),
    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids});

router_supervisor_monitor({supervisor_recovery_state, R_Sup_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids}) ->
    erlang:monitor(process, R_Sup_Pid),
    R_Sup_Pid ! {recover_router, self(), Monitored_Routers, Routers_List},
    router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids});

router_supervisor_monitor({final_state, R_Sup_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids}) ->
    process_flag(trap_exit, true),
    R_Sup_Mon_Pid = self(),
    %%io:format("router_supervisor_monitor pid = ~p~n",[R_Sup_Mon_Pid]),
    receive
	{'DOWN', _Ref, process, Pid, Reason} ->
	    case Reason of
		killed ->
		    io:format("R_Sup_Mon received {'DOWN', _Ref, process, ~p, ~p}.~nSpawning new router supervisor~n", [Pid, Reason]),
		    New_R_S_Pid = spawn(fun() -> router_supervisor(R_Sup_Mon_Pid,
								   Monitored_Routers,
								   Routers_List, 
								   Routers_Info, 
								   Routers_DBs_Pids) end),
		    router_supervisor_monitor({supervisor_recovery_state, New_R_S_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids});
		Other->
		    io:format("Reason = ~p~n", [Other])
	    end;
	{R_Sup_Pid, New_Monitored_Routers, New_Routers_List, New_Routers_Info, New_Routers_DBs_Pids} ->
	    router_supervisor_monitor({final_state, R_Sup_Pid, New_Monitored_Routers, New_Routers_List, New_Routers_Info, New_Routers_DBs_Pids})
    end.

%%--------------------------------------------------------------------
%% @doc
%%     router_supervisor/5 constitutes the router supervisor process.
%%     This process is in charge of spawning the router processes
%%     in the router node, during the deployment of the system. Once
%%     the system is deployed, the router supervisor process monitors
%%     the router processes and re-starts them if they fail.
%%
%% @spec router_supervisor(R_Sup_Mon_Pid, Monitored_Routers,
%%           Routers_List, Routers_Info, Routers_DBs_Pids) ->
%%                   router_process/1
%% @end
%%--------------------------------------------------------------------
router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids) ->
    process_flag(trap_exit, true),
    R_Sup_Pid = self(),
    receive
	%% Setup actions
	{Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid, launch_router_processes} ->
	    io:format("Launching router processes.~n"),
	    launch_router_processes(self(), Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid),
	    {New_R_Sup_Mon_Pid, _Ref} = spawn_monitor(fun() -> router_supervisor_monitor({initial_state,
											  R_Sup_Pid,
											  Monitored_Routers,
											  Routers_List,
											  Routers_Info,
											  Routers_DBs_Pids}) end),
	    router_supervisor(New_R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids);
	{list_routers, Received_Routers_List, Received_Routers_DBs_Pids} ->
	    timer:sleep(250),
	    %%New_Routers_Info = get_routers_info(Received_Routers_List, Routers_Info),
	    spawn(fun() -> get_routers_info(R_Sup_Pid, Received_Routers_List, Routers_Info) end),
	    monitor_routers(Monitored_Routers, Received_Routers_List),
	    %%router_supervisor(Monitored_Routers, Received_Routers_List, New_Routers_Info, Received_Routers_DBs_Pids);
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Received_Routers_List, Routers_Info, Received_Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Received_Routers_List, Routers_Info, Received_Routers_DBs_Pids);
	%% Recover router actions
	{recover_router, Router_Sup_Mon_Pid, Monitored_Routers, Received_Routers_List} ->
	    spawn(fun() -> update_routers({router_supervisor, Received_Routers_List, R_Sup_Pid}) end),
	    monitor_routers(Monitored_Routers, Received_Routers_List),
	    erlang:monitor(process, Router_Sup_Mon_Pid),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Received_Routers_List, Routers_Info, Routers_DBs_Pids);
	%% Update monitored routers information
	{update_routers_info, Received_Routers_List} ->
	    %%io:format("Received {update_routers_info, ~p}~n", [Received_Routers_List]),
	    %%New_Routers_Info = get_routers_info(Received_Routers_List, Routers_Info),
	    spawn(fun() -> get_routers_info(R_Sup_Pid, Received_Routers_List, Routers_Info) end),
	    %%router_supervisor(Monitored_Routers, Received_Routers_List, New_Routers_Info, Routers_DBs_Pids),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Received_Routers_List, Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Received_Routers_List, Routers_Info, Routers_DBs_Pids);
	{new_routers_info, New_Routers_Info} ->
	    %%io:format("New_Routers_Info = ~p~n", [New_Routers_Info]),
	    R_Sup_Mon_Pid ! {R_Sup_Pid, Monitored_Routers, Routers_List, New_Routers_Info, Routers_DBs_Pids},
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, New_Routers_Info, Routers_DBs_Pids);
	%% Termination logic
	{'EXIT', normal} ->
	    io:format("router_supervisor() terminated normally.~n");
	%% ==== Uncomment these two lines for bencherl ====
	{'EXIT', _Pid, _Reason} ->
	     router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids);
	%% Reliability control.
	{'DOWN', _Ref, process, Pid, Reason} ->
	    case Pid of
		R_Sup_Mon_Pid ->
		    io:format("Router Supervisor Monitor with pid ~p is down.~n", [Pid]),
		    {New_R_Sup_Mon_Pid, _} = spawn_monitor(fun() -> router_supervisor_monitor({initial_state,
											       R_Sup_Pid,
											       Monitored_Routers,
											       Routers_List,
											       Routers_Info,
											       Routers_DBs_Pids}) end),
		    router_supervisor(New_R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids);
		_Other_Pid ->
		    {Router_Str, _Router_Pid} = lists:keyfind(Pid, 2, Routers_List),
		    io:format("========================================================~nRouter ~p is down with reason ~p~nRecovering router ~p~n========================================================~n", [Router_Str, Reason,Router_Str]),
		    %%io:format("Routers_Info = ~p~n", [Routers_Info]),
		    %% Get info to spawn a new router
		    {_R_N,_R_P, List_Monitored_Servers, List_Servers, Server_Nodes} = lists:keyfind(Router_Str, 1, Routers_Info), 
		    %%io:format("Info retrieved: {~p,~p,~p,~p,~p}~n", [R_N, R_P, List_Monitored_Servers, List_Servers, Server_Nodes]), 
		    {New_Router_Pid, _Ref_N_R} = spawn_monitor(
						   fun() -> router_process({recovery_state,
									    R_Sup_Pid,%%self(), %% <== this might be problematic
									    Routers_List,
									    List_Monitored_Servers,
									    List_Servers, Server_Nodes}) end),
		    %io:format("Router_Pid of the new router process: ~p~n", [New_Router_Pid]),
		    New_Monitored_Routers = lists:keyreplace(Pid, 2, Monitored_Routers, {Router_Str, New_Router_Pid}),
		    New_List_Routers = lists:keyreplace(Pid, 2, Routers_List, {Router_Str, New_Router_Pid}),
		    %%io:format("New_Monitored_Routers: ~p~n",[New_Monitored_Routers]),
		    %%io:format("New_List_Routers: ~p~n", [New_List_Routers]),
		    spawn(fun () -> update_routers({router_list, New_List_Routers, New_List_Routers}) end),
		    spawn(fun () -> update_router_dbs(Routers_DBs_Pids, New_List_Routers) end),
		    %%New_Routers_Info = get_routers_info(New_List_Routers, Routers_Info),
		    spawn(fun() -> get_routers_info(R_Sup_Pid, New_List_Routers, Routers_Info) end),
		    %%io:format("New_Routers_Info: ~p~n", [New_Routers_Info]),
		    %%router_supervisor(New_Monitored_Routers, New_List_Routers, New_Routers_Info, Routers_DBs_Pids);
		    R_Sup_Mon_Pid ! {R_Sup_Pid, New_Monitored_Routers, New_List_Routers, Routers_Info, Routers_DBs_Pids},
		    router_supervisor(R_Sup_Mon_Pid, New_Monitored_Routers, New_List_Routers, Routers_Info, Routers_DBs_Pids)
	    end;
	%% Chaos Generation logic
	rhesus_solves_conflict_router ->
	    Router_Pids = extract_pids([self(), R_Sup_Mon_Pid], Routers_List),
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    exit(lists:nth(random:uniform(length(Router_Pids)),Router_Pids),kill),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids);
	rhesus_solves_conflict_server ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    {_, Router_Pid} = lists:nth(random:uniform(length(Routers_List)),Routers_List),
	    Router_Pid ! kill_server_process,
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids);
	%% Trap for unexpected messages
	Other ->
	    io:format("router_supervisor received: ~p~n", [Other]),
	    router_supervisor(R_Sup_Mon_Pid, Monitored_Routers, Routers_List, Routers_Info, Routers_DBs_Pids)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     router_process/1 has basically two missions: to spawn the server
%%     supervisor processes and forward clients messages.
%%
%%     The process has three states. During the initial state, the router
%%     process spawns the server supervisors and informs the router
%%     supervisor process. When the server supervisors are spawned and
%%     the information is passed to the router_supervisor, the router
%%     process changes of state.
%%     
%%     In the final state, the router process listens for client messages
%%     and handles the server supervisors failures.
%%
%%     There is a third state (recovery_state) that serves as the initial
%%     state when the recovery strategy has been triggered.
%%
%% @spec router_process/1 -> server_supervisor_loop/4
%% @end
%%---------------------------------------------------------------------
router_process({initial_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, Server_Nodes}) ->
    process_flag(trap_exit, true),
    receive
	%% Setup Actions
	{Router_Name, Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid, launch_server} ->
	    io:format("~p spawning servers.~nServer_Nodes:~p~n", [Router_Name, Received_Server_Nodes]),
	    launch_server_supervisors(Received_Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid),
	    New_List_Monitored_Servers = monitored_servers(Received_Server_Nodes, List_Monitored_Servers),
	    router_process({initial_state, R_Sup_Pid, List_Routers, New_List_Monitored_Servers, Received_Server_Nodes});
	{list_routers, Received_List_Routers} ->
	    io:format("router_process({initial_state}) received list of routers: ~p~n", [List_Routers]),
	    router_process({initial_state, R_Sup_Pid, Received_List_Routers, List_Monitored_Servers, Server_Nodes});
	{list_servers, List_Servers}->
	    io:format("router_process({initial_state}) received list of servers: ~p~n", [List_Servers]),
	    monitor_servers(List_Monitored_Servers, List_Servers),
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Trap for unexpected messages.
	Other ->
	    io:format("Something failed at router_process({initial_state}). It received: ~p~n", [Other]),
	    router_process({initial_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, Server_Nodes})
    end;

router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}) ->
    receive
	%% Client login logic
	{Client_Name, Client_Pid, login} ->
	    %%io:format("forward client login request~n"),
	    {_, Target_Server_Pid} = lists:nth((compression_function(length(List_Servers), Client_Name) + 1), List_Servers),
	    Target_Server_Pid ! {Client_Name, Client_Pid, login},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Chat session request logic
	{Sender, Receiver, start_chat_session}  ->
	    %%io:format("forward start chat session request~n"),
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    {_, Target_Server_Pid} = lists:nth(random:uniform(length(List_Servers)),List_Servers),
	    Target_Server_Pid ! {Sender, Receiver, start_chat_session},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Chaos kill request forward
	kill_server_process ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    {_, Target_Server_Pid} = lists:nth(random:uniform(length(List_Servers)),List_Servers),
	    Target_Server_Pid ! kill_server_process,
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% Error Handling
	%% ==== Uncomment these two lines for bencherl ====
	{'EXIT', _Pid, _Reason} ->
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% monitored process finished normally
	{'DOWN', _Ref, process, Pid, normal} ->
	    New_Monitored_Servers = lists:keydelete(Pid, 2, List_Monitored_Servers),
	    New_List_Servers = lists:keydelete(Pid, 2, List_Servers),
	    spawn(fun () -> update_routers({server_list, List_Routers, New_List_Servers}) end),
	    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
	%% monitored process finished abnormally.	
	{'DOWN', _Ref, process, Pid, Reason} ->
	    {Server_Str, _Server_Pid} = lists:keyfind(Pid, 2, List_Servers),
	    io:format("========================================================~nServer ~p is down with reason ~p~n========================================================~n", [Server_Str, Reason]),
	    Node = node_name(Server_Str, Server_Nodes),
	    Server = string_to_atom(Server_Str),
	    case Reason of
		noproc ->
		    New_Monitored_Servers = lists:keydelete(Pid, 2, List_Monitored_Servers),
		    New_List_Servers = lists:keydelete(Pid, 2, List_Servers),
		    spawn(fun () -> update_routers({server_list, List_Routers, New_List_Servers}) end),
		    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
		noconnection ->
		    io:format("Fatal error. Node ~p is down, and thus server ~p cannot be restarted.~n", [Node, Server]),
		    New_Monitored_Servers = lists:keydelete(Pid, 2, List_Monitored_Servers),
		    New_List_Servers = lists:keydelete(Pid, 2, List_Servers),
		    spawn(fun () -> update_routers({server_list, List_Routers, New_List_Servers}) end),
		    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes});
		_Other ->
		    Server_Sup_Pid = spawn_link(Node, fun() -> restart_server_supervisor({first_stage, Server}) end),
		    erlang:monitor(process, Server_Sup_Pid),
		    unlink(Server_Sup_Pid),
		    New_Monitored_Servers = lists:keyreplace(Pid, 2, List_Monitored_Servers, {Server_Str, Server_Sup_Pid}),
		    New_List_Servers = lists:keyreplace(Pid, 2, List_Servers, {Server_Str, Server_Sup_Pid}),
		    spawn(fun () -> update_routers({server_list, List_Routers, New_List_Servers}) end),
		    router_process({final_state, R_Sup_Pid, List_Routers, New_Monitored_Servers, New_List_Servers, Server_Nodes})
	    end;
	%% Termination logic
	{'EXIT', normal} ->
	    io:format("router_process() terminated normally.~n");
	{request_router_info, Dest_Pid} ->
	    Dest_Pid ! [self(), List_Monitored_Servers, List_Servers, Server_Nodes],
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	%% update information
	{list_routers, Received_List_Routers} ->
	    %%io:format("received list of routers (Router/final_stage): ~p~n", [Received_List_Routers]),
	    router_process({final_state, R_Sup_Pid, Received_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	{update_servers_list, Updated_List_Servers} ->
	    %%io:format("received list of servers (update_servers_list): ~p~n", [Updated_List_Servers]),
	    %%io:format("List_Routers = ~p~n", [List_Routers]),
	    R_Sup_Pid ! {update_routers_info, List_Routers},
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, Updated_List_Servers, Server_Nodes});
	{update_routers_list, Updated_List_Routers} ->
	    %%io:format("received list of routers (update_routers_list): ~p~n", [Updated_List_Routers]),
	    R_Sup_Pid ! {update_routers_info, Updated_List_Routers},
	    router_process({final_state, R_Sup_Pid, Updated_List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	{update_router_supervisor, New_R_Sup_Pid} ->
	    %%io:format("received list of routers (update_router_supervisor): ~p~n", [New_R_Sup_Pid]),
	    router_process({final_state, New_R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes});
	Other ->
	    {Router_Str, _Router_Pid} = lists:keyfind(self(), 2, List_Routers),
	    io:format("~p router_Process received: ~p~n", [Router_Str, Other]),
	    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes})
    end;

router_process({recovery_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}) ->
    process_flag(trap_exit, true),
    monitor_servers(List_Monitored_Servers, List_Servers),
    router_process({final_state, R_Sup_Pid, List_Routers, List_Monitored_Servers, List_Servers, Server_Nodes}).

%%===============================================================================
%% CHAOS GENERATION LOGIC
%%===============================================================================
chaos_on() ->
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    Timer = (random:uniform(3595) + 5) * 1000,
    io:format("A random process will be killed every ~p seconds.~n", [Timer div 1000]),
    chaos_on(Timer, undefined).

chaos_on(Timer) ->
    chaos_on(Timer, undefined).

chaos_on(Timer, Router_Sup_Pid) ->
    case Router_Sup_Pid of
	undefined ->
	    rhesus_gets_nervous(Timer);
	Pid ->
	    case is_process_alive(Pid) of
		true ->
		    rhesus_attack(Timer, Pid);
		false ->
		    rhesus_gets_nervous(Timer)
	    end
    end.

rhesus_gets_nervous(Timer) ->
    case find_router_sup_pid() of
	not_found ->
	    io:format("Router supervisor process not found on this node.~n");
	R_Sup_Pid ->
	    rhesus_attack(Timer, R_Sup_Pid)
    end.

rhesus_attack(Timer, Router_Sup_Pid)->
    timer:sleep(Timer),
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    case random:uniform(4) of
	1 ->
	    Router_Sup_Pid ! rhesus_solves_conflict_router,
	    chaos_on(Timer, Router_Sup_Pid);
	_Other ->
	    Router_Sup_Pid ! rhesus_solves_conflict_server,
	    chaos_on(Timer, Router_Sup_Pid)
    end.

find_router_sup_pid() ->
    find_router_sup_pid(erlang:processes()).

find_router_sup_pid(List) ->
    case List of
	[] ->
	    not_found;
	[H|T] ->
	    {Name,Tuple} = hd(erlang:process_info(H)),
	    case Name of
		current_function ->
		    {_,F,_} = Tuple,
		    case F of
			router_supervisor ->
			    H;
			_Other ->
			    find_router_sup_pid(T)
		    end;
		_Any_other ->
		    find_router_sup_pid(T)
	    end
    end.

extract_pids(List_Pids, List_Routers) ->
    case List_Routers of
	[] ->
	    List_Pids;
	[H|T] ->
	    {_Name, Pid} = H,
	    extract_pids([Pid|List_Pids],T)
    end.

%%===============================================================================
%% AUXILIARY FUNCTIONS
%%===============================================================================

%%---------------------------------------------------------------------
%% @doc
%%     monitored_servers/2 builds a list of the servers monitored by one
%%     router after the list of server nodes received during deployment.
%%
%% @spec monitored_servers(Server_Nodes, Monitored_Servers) -> list()
%% @end
%%---------------------------------------------------------------------
monitored_servers(Server_Nodes, Monitored_Servers) ->
    case Server_Nodes of
	[] ->
	    Monitored_Servers;
	[Server|Tail_Servers] ->
	    S = atom_to_list(Server),
	    Server_Name = string:left(S, string:chr(S,$@) - 1),
	    New_Monitored_Servers = Monitored_Servers ++ [Server_Name],
	    monitored_servers(Tail_Servers, New_Monitored_Servers)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     monitor_servers/2 traverses a list of servers and establishes a
%%     monitor-monitored processes relationship between a router process
%%     and a server supervisor process.
%%
%% @spec monitor_servers(Monitored_Servers, Servers_List) -> ok
%% @end
%%---------------------------------------------------------------------
monitor_servers(Monitored_Servers, Servers_List) ->
    case Monitored_Servers of
	[] ->
	    ok;
	[Server|Tail_Mon_Serv] ->
	    {_Serv, Server_Pid} = lists:keyfind(Server, 1, Servers_List),
	    erlang:monitor(process, Server_Pid),
	    monitor_servers(Tail_Mon_Serv, Servers_List)
    end.

%%---------------------------------------------------------------------
%% @doc
%%      monitor_routers/2 is similar to monitor_servers/2, but for the
%%      router_supervisor - router_process processes.
%%
%% @spec monitor_routers(Monitored_Routers, Routers_List -> ok
%% @end
%%---------------------------------------------------------------------
monitor_routers(Monitored_Routers, Routers_List) ->
    case Monitored_Routers of
	[] ->
	    ok;
	[Router|Tail] ->
	    {_Router, Router_Pid} = lists:keyfind(atom_to_list(Router), 1, Routers_List),
	    erlang:monitor(process, Router_Pid),
	    monitor_routers(Tail, Routers_List)
    end.

%%---------------------------------------------------------------------
%% @doc
%%     node_name/2 finds the server node corresponding to a server
%%     supervisor.
%%
%% @spec node_name(Server, Server_Nodes) -> node name | {error, Reason}
%% @end
%%---------------------------------------------------------------------
node_name(Server, Server_Nodes) ->
    case Server_Nodes of
	[] ->
	    io:format("Error. There is not server node corresponding to the server process.~n"),
	    {error, no_server_node_found};
	[Target_Node|Tail] ->
	    T = atom_to_list(Target_Node),
	    case Server ==  string:left(T, string:chr(T,$@) - 1) of
		true ->
		    Target_Node;
		false ->
		    node_name(Server, Tail)
	    end
    end.    

%%---------------------------------------------------------------------
%% @doc
%%     update_routers/1 updates the information stored in a router process.
%%     This can be:
%%          - The list of server supervisor processes monitored by the
%%            router process
%%          - The list of router processes in the router monitor.
%%          - The list of router processes monitored by the router
%%            supervisor process.
%%
%% @spec update_routers({Update, Routers, List}) -> ok | {error, Reason}
%% @end
%%---------------------------------------------------------------------
update_routers({Update, Routers, List}) ->
    case Routers of
	[] ->
	    ok;
	[{_R_Name, Router_Pid}|T] ->
	    case Update of
		server_list ->
		    Router_Pid ! {update_servers_list, List};
		router_list ->
		    Router_Pid ! {update_routers_list, List};
		router_supervisor ->
		    Router_Pid ! {update_router_supervisor, List}
	    end,	    
	    update_routers({Update, T, List})
    end.

%%---------------------------------------------------------------------
%% @doc
%%     update_router_dbs/2 updates the information of the routers_db
%%     processes when a router process has changed.
%%
%% @spec update_router_dbs(Routers_DBs, List) -> ok.
%% @end
%%---------------------------------------------------------------------
update_router_dbs(Routers_DBs, List) ->
    case Routers_DBs of
	[] ->
	    ok;
	[R_DB_Pid|Tail] ->
	    R_DB_Pid ! {List, receive_router_list},
	    update_router_dbs(Tail, List)
    end.			    

%%---------------------------------------------------------------------
%% @doc
%%     get_routers_info/3 sends the information of the routers supervised
%%     by a router supervisor process to the said router supervisor.
%%
%%     This funcion also updates the routers information in the router
%%     supervisor if this information has changed.
%%
%% @spec get_routers_info(R_Sup_Pid, Routers_List, Routers_Info) -> list()
%% @end
%%---------------------------------------------------------------------
get_routers_info(R_Sup_Pid, Routers_List, Routers_Info) ->
    case Routers_List of
	[] ->
	    R_Sup_Pid ! {new_routers_info, Routers_Info};
	[{Router_Name, Router_Pid}|Tail] ->
	    Router_Pid ! {request_router_info, self()},
	    receive
		[Received_Router_Pid, List_Mon_S, List_S, S_Nds] ->
		    case lists:keyfind(Router_Name, 1, Routers_Info) of
			false ->
			    New_Routers_Info = lists:append(Routers_Info,
							    [{Router_Name,
							      Router_Pid,
							      List_Mon_S,
							      List_S,
							      S_Nds}]);				    
			_ ->
			    New_Routers_Info = lists:keyreplace(Router_Name,
								1,
								Routers_Info,
								{Router_Name,
								 Received_Router_Pid,
								 List_Mon_S,
								 List_S,
								 S_Nds})
		    end,
		    get_routers_info(R_Sup_Pid, Tail, New_Routers_Info);
		_Other ->
		    get_routers_info(R_Sup_Pid, Routers_List, Routers_Info)
	    end
    end.

%% ------------------
%% Hashing functions.
%% ------------------

%%---------------------------------------------------------------------
%% @doc
%%     compression_function/2 returns the hash value within the interval
%%     [1,Number of Servers], for a client name.
%%     
%%     The compression function is:
%%     (((A * I) + B) rem P) rem Num_Servers
%%
%%     where:
%%          A is randomly generated parameter.
%%          B is randomly generated parameter.
%%          I is the hash code of the client name.
%%          P is a prime number greater than the number of buckets
%%            (servers).
%%     
%%     However, A and B are hard-coded random values to avoid
%%     inconsistencies.
%%
%% @spec compression_function(Num_Servers, Client_Name) -> integer()
%% @end
%%---------------------------------------------------------------------
compression_function(Num_Servers, Client_Name) ->
    case is_atom(Client_Name) of
	true ->
	    I = hash_code(atom_to_list(Client_Name), 0);
	false ->
	    I = hash_code(Client_Name, 0)
    end,
    P = 4294967291, %%highest 32-bit prime number
    (((33 * I) + 429496) rem P) rem Num_Servers.

%%---------------------------------------------------------------------
%% @doc
%%     hash_code/2 calculates a hash code for a given string.
%%
%% @spec hash_code(String, Hash_Code) -> integer()
%% @end
%%---------------------------------------------------------------------
hash_code(String, Hash_Code) ->
    case length(String) > 0 of
	true ->
	    Hash_Shift = Hash_Code bsl 5,%% or Hash_Code bsr 27,
	    [H|T] = String,
	    New_Hash_Code =  Hash_Shift + H,
	    hash_code(T, New_Hash_Code);
	false ->
	    Hash_Code
    end.

%% -------------------------
%% Other auxiliary functions
%% -------------------------

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
