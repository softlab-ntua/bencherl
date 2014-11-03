%%%-------------------------------------------------------------------
%%% LAUNCHER MODULE
%%%
%%% @author Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Launcher module for the Distributed Erlang instant messenger
%%%     (IM) application developed as a real benchmark for the
%%%     Scalable Distributed Erlang extension of the Erlang/OTP
%%%     language.
%%%
%%%	This module implements the logic to deploy a system similar
%%%     to the system described in the Section 2 of the document
%%%     "Instant Messenger Architectures Design Proposal" given a
%%%     set of virtual machines to host the system.
%%% @end
%%% Created : 25 Jul 2014 by Mario Moro Hernandez
%%%-------------------------------------------------------------------
-module(launcher).
-export([start/5, start/6, start_bencherl/5, stop/5, stop/6, launch_router_processes/6, launch_server_supervisors/4]).
%%-compile(export_all).

-import(router,[router_supervisor/5, router_process/1, compression_function/2, hash_code/2]).

%%============================================================================
%% APPLICATION DEPLOYMENT LOGIC
%%============================================================================

%%--------------------------------------------------------------------
%% @doc
%%     start/6 launches the sequence to set up the architecture, and
%%     run the IM application.
%%
%%     Arguments:
%%          Servers_Per_Router_Node: (int) The number of server nodes
%%                                   that are children of a router node.
%%          Server_Per_Router_Process: (int) The number of server
%%                                     supervisor processes that are
%%                                     monitored by a router process.
%%          Servers_Total: (int) Final number of servers in the system.
%%          Clients_Total: (int) Final number of client nodes.
%%          List_Domains: (list) List containing all the domains of
%%                        the hosts in which the system is deployed.
%%                        This is usually one, but can be more if the
%%                        application is hosted in a cluster with
%%                        different domains. 
%%          Num_of_Hosts: (int) Number of hosts in which the system is
%%                        deployed.
%%
%%     Example: start(2,1,2,4,['domain.do'], 1).
%%              start(2,1,2,4,['domain'], 1).
%%
%%              These launch a system comprising 1 router, 2 servers, and
%%              4 client nodes on 1 host, with domain domain.do or domain.
%%              In this case, there are 2 router processes, each of them
%%              monitoring one server supervisor process.
%%
%% @spec start(Servers_Per_Router_Node, Servers_Per_Router_Process,
%%             Servers_Total, Clients_Total, List_Domains, Num_of_Hosts) ->
%%                  Status Messages | {error, reason}
%% @end
%%---------------------------------------------------------------------
start(Servers_Per_Router_Node, Servers_Per_Router_Process, Servers_Total, Clients_Total, List_Domains, Num_of_Hosts) ->
    Num_TS = Servers_Total * Num_of_Hosts,
    Architecture_Info = {Servers_Per_Router_Node,
			 Servers_Per_Router_Process,
			 Servers_Total,
			 Num_TS,
			 Clients_Total},
    case List_Domains == [] of
	true ->
	    io:format("start/4 is finished.~n");
	false ->
	    [Domain|New_List_Domains] = List_Domains,
	    case whereis(routers_listener) of
	    	undefined ->
	    	    io:format("~n=============================================~n"),
	    	    io:format("Initiating the Distributed Instant Messenger.~n"),
	    	    io:format("=============================================~n"),
		    Routers_Listener_Pid = spawn(fun() ->
							 routers_listener(Num_TS,
									  [],
									  [],
									  [],
									  [])
						 end),
		    register(routers_listener, Routers_Listener_Pid);
	    	_ ->
	    	    ok
	    end,
	    start_host(Num_of_Hosts,
		       Architecture_Info,
		       Domain,
		       whereis(routers_listener)),
	    start(Servers_Per_Router_Node,
		  Servers_Per_Router_Process,
		  Servers_Total,
		  Clients_Total,
		  New_List_Domains,
		  Num_of_Hosts - 1)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     starts the sequence of that launch each of the components that
%%     will be deployed in a particular hosts from the list specified
%%     in start/6.
%%
%% @spec
%%     start_host(Num_of_Host, Architecture_Info, Domain,
%%                Routers_Listener_Pid) -> Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_host(Num_of_Host, Architecture_Info, Domain, Routers_Listener_Pid) ->
    {Servers_Per_Router_Node,
     Servers_Per_Router_Process,
     Servers_Total,
     Num_TS,
     Clients_Total} = Architecture_Info,
    case Servers_Total rem Servers_Per_Router_Node of
	0 ->
	    Routers = Servers_Total div Servers_Per_Router_Node;
	_Greater_than_0 ->
	    Routers = (Servers_Total div Servers_Per_Router_Node) + 1
    end,
    case Servers_Per_Router_Process > Servers_Per_Router_Node of
	true ->
	    S_R_Pr = Servers_Per_Router_Node;
	false ->
	    S_R_Pr = Servers_Per_Router_Process
    end,

    Router_Nodes = node_name_generator(Num_of_Host, router, Domain, Routers, 0),
    Server_Nodes = node_name_generator(Num_of_Host, server, Domain, Servers_Total, 0),
    Client_Nodes = node_name_generator(Num_of_Host, client, Domain, Clients_Total, 0),
    Router_Processes = router_names_generator(Num_of_Host, Servers_Total, Servers_Per_Router_Node, S_R_Pr, 0),

    io:format("~n=============================================~n"),
    io:format("            Launching Host: ~p               ~n", [Num_of_Host]),
    io:format("=============================================~n"),
    io:format("Router nodes: ~p~n", [Router_Nodes]),
    io:format("Server nodes: ~p~n", [Server_Nodes]),
    io:format("Client nodes: ~p~n", [Client_Nodes]),    
    io:format("Router processes: ~p~n", [Router_Processes]),

    Routers_Listener_Pid ! {Client_Nodes, add_client_node},
    case length(Router_Processes) rem Routers of
	0 ->
	    Router_Pr_per_R_Nd = length(Router_Processes) div Routers;
	_Other ->
	    Router_Pr_per_R_Nd = length(Router_Processes) div Routers + 1
    end,
    New_Architecture_Info = {Servers_Per_Router_Node,
			     Servers_Per_Router_Process,
			     Num_TS,
			     Servers_Total,
			     Routers,
			     Router_Pr_per_R_Nd},
    Routers_Servers_Lists = {Router_Nodes, Server_Nodes, Router_Processes},

    launch_router_supervisors(Routers_Servers_Lists, New_Architecture_Info, Routers_Listener_Pid).

%%---------------------------------------------------------------------
%% @doc
%%     start/5 launches the sequence to set up the architecture, and
%%     run the IM application.
%%
%%     Arguments:
%%          S_RN: (int) The number of server nodes that are children of
%%                 a router node.
%%          S_RP: (int) The number of server supervisor processes that are
%%                monitored by a router process.
%%          S_T: (int) Final number of servers in the system.
%%          Cl_T: (int) Final number of client nodes.
%%          List_Domains: (list) List containing all the domains of
%%                        the hosts in which the system is deployed.
%%                        This is usually one, but can be more if the
%%                        application is hosted in a cluster with
%%                        different domains. 
%%
%%     Example:
%%          start(2,1,2,4,['domain_1.do','domain_2.do], ..., 'domain_4.do ]).
%%
%%          This launches a system comprising 1 router, 2 servers, and 4 client
%%          nodes, deploying the router on host with domain 'domain_1.do',
%%          the two servers on the hosts with domain 'domain_2.do and 'domain_3.do',
%%          and four client nodes on a host 'domain_4.do'.
%%
%%          In this case, there are 2 router processes, each of them monitoring
%%          one server supervisor process.
%%
%% @spec start(S_RN, S_RP, S_T, Cl_T, List_Domains) ->
%%                   Status Messages | {error, reason}
%% @end
%%---------------------------------------------------------------------
start(S_RN, S_RP, S_T, Cl_T, List_Domains) ->
    case S_T rem S_RP of
	Rem when Rem == 0 ->
	    R_T = S_T div S_RN;
	_Rem ->
	    R_T = (S_T div S_RN) + 1
    end,
    case (R_T + S_T) + 1 =< length(List_Domains) of
	true ->
	    Architecture_Info = {S_RN, S_RP, S_T, S_T, Cl_T},
	    Nodes = nodes_list(R_T, S_RN, S_T, Cl_T, List_Domains),
	    case whereis(routers_listener) of
		undefined ->
		    io:format("~n=============================================~n"),
	    	    io:format("Initiating the Distributed Instant Messenger.~n"),
	    	    io:format("=============================================~n"),
		    Routers_Listener_Pid = spawn(fun() ->
							 routers_listener(S_T,
									  [],
									  [],
									  [],
									  [])
						 end),
		    register(routers_listener, Routers_Listener_Pid);
	    	Pid ->
	    	    Routers_Listener_Pid = Pid
	    end,
	    start_distributed(Architecture_Info, Nodes, Routers_Listener_Pid);
	false ->
	    io:format("ERROR: There are not enough hosts to deploy the system. Aborting.~n")
    end.

%%---------------------------------------------------------------------
%% @doc
%%     start_bencherl/5 is similar to start/5, but in this case the last
%%     argument is a list containing the nodes where the aplication must
%%     be executed. This function is intended to be used with bencherl
%%     only.
%%
%%     Arguments:
%%          S_RN: (int) The number of server nodes that are children of
%%                 a router node.
%%          S_RP: (int) The number of server supervisor processes that are
%%                monitored by a router process.
%%          S_T: (int) Final number of servers in the system.
%%          Cl_T: (int) Final number of client nodes.
%%          Nodes: (list) List containing all the nodes comprising the
%%                 architecture of the application. 
%%
%%     Example:
%%          start(2,1,2,4,['router_1@domain_1.do','router_2@domain_2.do'],
%%                          ..., 'client_4@domain_4.do ]).
%%
%% @spec start_bencherl(Servers_Per_Router_Node, Servers_Per_Router_Process,
%%             Servers_Total, Clients_Total, List_Domains, Num_of_Hosts) ->
%%                 Status Messages | {error, reason}
%% @end
%%---------------------------------------------------------------------
start_bencherl(S_RN, S_RP, S_T, Cl_T, Nodes) ->
    Architecture_Info = {S_RN, S_RP, S_T, S_T, Cl_T},
    case whereis(routers_listener) of
	undefined ->
	    io:format("~n=============================================~n"),
	    io:format("Initiating the Distributed Instant Messenger.~n"),
	    io:format("=============================================~n"),
	    Routers_Listener_Pid = spawn(fun() ->
						 routers_listener(S_T,
								  [],
								  [],
								  [],
								  [])
					 end),
	    register(routers_listener, Routers_Listener_Pid);
	Pid ->
	    Routers_Listener_Pid = Pid
    end,
    start_distributed(Architecture_Info, Nodes, Routers_Listener_Pid).

%%--------------------------------------------------------------------
%% @doc
%%     starts the sequence of that launch each of the components that
%%     will be deployed in the hosts from the list specified in start/5.
%%
%%     It is equivalent to start_host/4, yet in a distributed environment
%%     where each host executes one node.
%%
%% @spec
%%     start_Distributed(Architecture_Info, Nodes,
%%          Routers_Listener_Pid) -> Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_distributed(Architecture_Info, Nodes, Routers_Listener_Pid) ->
    {S_RN, S_RP, S_T, Num_TS, _Cl_T} = Architecture_Info,
    case S_RP > S_RN of
	true ->
	    S_R_Pr = S_RN;
	false ->
	    S_R_Pr = S_RP
    end,
    {Router_Nodes, Server_Nodes, Client_Nodes} = extractor(Nodes),
    Router_Processes = router_names_generator(1, S_T, S_RN, S_R_Pr, 0),
    io:format("Router processes: ~p~n", [Router_Processes]),

    Routers_Listener_Pid ! {Client_Nodes, add_client_node},
    case length(Router_Processes) rem length(Router_Nodes) of
	0 ->
	    R_Pr_RN = length(Router_Processes) div length(Router_Nodes);
	_Other ->
	    R_Pr_RN = length(Router_Processes) div length(Router_Nodes) + 1
    end,
    New_Architecture_Info = {S_RN, S_RP, Num_TS, S_T, length(Router_Nodes), R_Pr_RN},
    Routers_Servers_List = [Router_Nodes, Server_Nodes, Router_Processes],
    launch_router_supervisors(Routers_Servers_List, New_Architecture_Info, Routers_Listener_Pid).

%%--------------------------------------------------------------------
%% @doc
%%     This function determines the router nodes where the router
%%     supervisors must be spawned and gathers all the information
%%     required for that purpose.
%%
%% @spec launch_router_supervisors(Routers_Servers_Lists,
%%                     Architecture_Info, Routers_Listener_Pid) ->
%%                  Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
launch_router_supervisors(Routers_Servers_Lists, Architecture_Info, Routers_Listener_Pid) ->
    [Router_Nodes, Server_Nodes, Router_Processes] = Routers_Servers_Lists,
    {Server_Router_Nd, Servers_Router_Pr, Num_Total_Servers, _, _, Router_Pr_Per_R_Nd} = Architecture_Info,
    [Router_Nd | New_Router_Nd] = Router_Nodes,
    io:format("Router_Pr_per_R_Nd = ~p; Router_Processes = ~p~n", [Router_Pr_Per_R_Nd, Router_Processes]),
    case length(Server_Nodes) > Server_Router_Nd of
	true ->
	    {Servers, New_Server_Nd} = lists:split(Server_Router_Nd, Server_Nodes), 
	    {Router_Prcs, New_Router_Processes} = lists:split(Router_Pr_Per_R_Nd, Router_Processes), 
	    io:format("launch_router_supervisors.~nRouter_Nd: ~p~nServers in ~p:~p~nRouter Processes in ~p:~p~n",
		      [Router_Nd, Router_Nd, Servers,Router_Nd, Router_Prcs]),
	    New_Rs_Ss_Lists = {New_Router_Nd, New_Server_Nd, New_Router_Processes},
	    start_router_supervisor(Router_Nd,
				    Servers,
				    Servers_Router_Pr,
				    Router_Prcs,
				    Num_Total_Servers,
				    Routers_Listener_Pid),
	    launch_router_supervisors(New_Rs_Ss_Lists, Architecture_Info, Routers_Listener_Pid);
	false ->
	    start_router_supervisor(Router_Nd,
				    Server_Nodes,
				    Servers_Router_Pr,
				    Router_Processes,
				    Num_Total_Servers,
				    Routers_Listener_Pid),
	    io:format("launch_router_supervisors.~nRouter_Nd: ~p~nServers in ~p:~p~nRouter Processes in ~p:~p~n",
		      [Router_Nd, Router_Nd, Server_Nodes,Router_Nd, Router_Processes])
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function spawns a router supervisor on the target node and
%%     flags the start of the sequence to deploy the router processes.
%% @spec start_router_supervisor(Router_Node, Server_Nodes, Servers_Router_Pr,
%%           Router_Processes, Num_Total_Servers, Routers_Listener_Pid) ->
%%                  ok | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_router_supervisor(Router_Node, Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid) ->
    R_Sup_Pid = spawn_link(Router_Node, fun() ->
						router_supervisor(undefined, Router_Processes, [], [], [])
					end),
    Routers_Listener_Pid ! {R_Sup_Pid, add_router_sup},
    R_Sup_Pid ! {Server_Nodes,
		 Servers_Router_Pr,
		 Router_Processes,
		 Num_Total_Servers,
		 Routers_Listener_Pid,
		 launch_router_processes},
    ok.

%%--------------------------------------------------------------------
%% @doc
%%     This function determines the number of router processes that are
%%     going to be spawned on a router node and gathers all the information
%%     required for that purpose.
%%
%% @spec launch_router_processes(R_Sup_Pid, Server_Nodes, Servers_Router_Pr,
%%           Router_Processes, Num_Total_Servers, Routers_Listener_Pid) ->
%%                  Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
launch_router_processes(R_Sup_Pid, Server_Nodes, Servers_Router_Pr, Router_Processes, Num_Total_Servers, Routers_Listener_Pid) ->
    case Router_Processes of
	[] ->
	    io:format("Router processes start sequence, finished.~n");
	[Router_Process|New_Router_Processes] ->
	    io:format("launch_router_processes/4 Router_Processes: ~p~n", [Router_Processes]),
	    case length(Server_Nodes) > Servers_Router_Pr of
		true ->
		    {Servers, New_Server_Nodes} = lists:split(Servers_Router_Pr, Server_Nodes),
		    start_router_process(Router_Process,
					 Servers, Servers_Router_Pr,
					 Num_Total_Servers,
					 R_Sup_Pid,
					 Routers_Listener_Pid),
		    launch_router_processes(R_Sup_Pid,
					    New_Server_Nodes,
					    Servers_Router_Pr,
					    New_Router_Processes,
					    Num_Total_Servers,
					    Routers_Listener_Pid);
		false ->
		    start_router_process(Router_Process,
					 Server_Nodes,
					 Servers_Router_Pr,
					 Num_Total_Servers,
					 R_Sup_Pid,
					 Routers_Listener_Pid)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function spawns a router process on the target node and
%%     flags the start of the sequence to deploy the server supervisor
%%     process on the target node.
%%
%% @spec start_router_process(Router_Name, Server_Nodes, Servers_Router_Pr,
%%           Num_Total_Servers, R_Sup_Pid, Routers_Listener_Pid) ->
%%                  yes | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_router_process(Router_Name, Server_Nodes, Servers_Router_Pr, Num_Total_Servers, R_Sup_Pid, Routers_Listener_Pid) ->
    %% global:register_name(Router_Name,
    %% 			 R_Pid = spawn_link(fun() -> 
    %%						    router_process({initial_state, R_sup_Pid, [], [], []})
    %% 					    end)),
    R_Pid = spawn_link(fun() ->
			       router_process({initial_state, R_Sup_Pid, [], [], []})
		       end),
    Routers_Listener_Pid ! {Router_Name, R_Pid, add_router},
    R_Pid ! {Router_Name,
	     Server_Nodes,
	     Servers_Router_Pr,
	     Num_Total_Servers,
             Routers_Listener_Pid,
	     launch_server},
    yes.

%%--------------------------------------------------------------------
%% @doc
%%     This function determines the target nodes to spawn the corresponding
%%     server supervisor processes.
%%
%% @spec launch_server_supervisors(Server_Nodes, Servers_Router_Pr,
%%           Num_Total_Servers, Routers_Listener_Pid) ->
%%                  Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
launch_server_supervisors(Server_Nodes, Servers_Router_Pr, Num_Total_Servers, Routers_Listener_Pid) ->
    case length(Server_Nodes) > Servers_Router_Pr of
	true ->
	    {Servers, New_Server_Nodes} = lists:split(Servers_Router_Pr, Server_Nodes),
	    io:format("launch_server_supervisors.~nServers:~p~n", [Server_Nodes]),
	    start_server_supervisors(Servers, Num_Total_Servers, Routers_Listener_Pid),
	    launch_server_supervisors(New_Server_Nodes,
				      Servers_Router_Pr,
				      Num_Total_Servers,
				      Routers_Listener_Pid);
	false ->
	    io:format("launch_server_supervisors.~nServers:~p~n", [Server_Nodes]),
	    start_server_supervisors(Server_Nodes,
				     Num_Total_Servers,
				     Routers_Listener_Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function spawns the server supervisor processes on the target
%%     nodes. Then, it notifies the name and pid of the server supervisor
%%     to allow the routing of the requests made by the clients.
%%
%% @spec launch_server_supervisors(Server_Nodes, Servers_Router_Pr,
%%           Num_Total_Servers, Routers_Listener_Pid) ->
%%                  Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_server_supervisors(Server_Nodes, Num_Total_Servers, Routers_Listener_Pid) ->
    case Server_Nodes == [] of
	true ->
	    io:format("Server supervisors start sequence is finished.~n");
	false ->
	    [Server|New_Server_Nodes] = Server_Nodes,
	    S = atom_to_list(Server),
	    Server_Name = string:left(S, string:chr(S,$@) - 1),
	    Server_Sup_Pid = spawn_link(Server,
					fun() -> server:start_server_supervisor(
						   Server, string_to_atom(Server_Name), Num_Total_Servers) end),
	    Routers_Listener_Pid ! {Server_Name, Server_Sup_Pid, add_server},
	    start_server_supervisors(New_Server_Nodes, Num_Total_Servers, Routers_Listener_Pid)
    end.

stop(S_RN, S_RP, S_T, Cl_T, List_Domains) ->
   case S_T rem S_RP of
	Rem when Rem == 0 ->
	    R_T = S_T div S_RN;
	_Rem ->
	    R_T = (S_T div S_RN) + 1
    end,
    stop_nodes(nodes_list(R_T, S_RN, S_T, Cl_T, List_Domains)),
    init:stop().

stop(Servers_Per_Router_Node, Servers_Per_Router_Process, Servers_Total, Clients_Total, List_Domains, Num_of_Hosts) ->
    Num_TS = Servers_Total * Num_of_Hosts,
    Architecture_Info = {Servers_Per_Router_Node,
			 Servers_Per_Router_Process,
			 Servers_Total,
			 Num_TS,
			 Clients_Total},
    case List_Domains == [] of
	true ->
	    init:stop();
	false ->
	    [Domain|New_List_Domains] = List_Domains,
	    stop_host(Num_of_Hosts,
		      Architecture_Info,
		      Domain),
	    stop(Servers_Per_Router_Node,
		 Servers_Per_Router_Process,
		 Servers_Total,
		 Clients_Total,
		 New_List_Domains,
		 Num_of_Hosts - 1)
    end.

stop_host(Num_of_Host, Architecture_Info, Domain) ->
    {Servers_Per_Router_Node,
     _Servers_Per_Router_Process,
     Servers_Total,
     _Num_TS,
     Clients_Total} = Architecture_Info,

    case Servers_Total rem Servers_Per_Router_Node of
	0 ->
	    Routers = Servers_Total div Servers_Per_Router_Node;
	_Greater_than_0 ->
	    Routers = (Servers_Total div Servers_Per_Router_Node) + 1
    end,

    Router_Nodes = node_name_generator(Num_of_Host, router, Domain, Routers, 0),
    Server_Nodes = node_name_generator(Num_of_Host, server, Domain, Servers_Total, 0),
    Client_Nodes = node_name_generator(Num_of_Host, client, Domain, Clients_Total, 0),

    stop_nodes(Client_Nodes),
    stop_nodes(Server_Nodes),
    stop_nodes(Router_Nodes).

stop_nodes(Nodes_List) ->
    case Nodes_List of
	[] ->
	    ok;
	[Node|Rest_of_Nodes] ->
	    rpc:call(Node, init, stop, []),
	    stop_nodes(Rest_of_Nodes)
    end.

%%%========================================================================
%%% AUXILIARY FUNCTIONS FOR ROUTERS INFORMATION AND RELIABILITY
%%%========================================================================

%%--------------------------------------------------------------------
%% @doc
%%     routers_listener/5 is an auxiliary process central to the start
%%     sequence of the system. It is responsible for receive the pids
%%     of router supervisors, router processes and server supervisors
%%     to pass this information to the relevant processes, once these
%%     processes are all spawned.
%%
%% @spec routers_listener(Servers_Total, List_Servers, List_Routers,
%%           List_Router_Sups, Client_Nodes) ->
%%                  Status Messages | {error, reason}
%% @end
%%--------------------------------------------------------------------
routers_listener(Servers_Total, List_Servers, List_Routers, List_Router_Sups, Client_Nodes) ->
    receive
	{Server_Name, Server_Sup_Pid, add_server} ->
	    New_List_Servers = lists:append(List_Servers, [{Server_Name, Server_Sup_Pid}]),
	    io:format("List of servers (routers_listener/4): ~p~n", [New_List_Servers]),
	    case length(New_List_Servers) == Servers_Total of
		true ->
		    io:format("launching start_routers_db/2 having Client_Nodes: ~p~n", [Client_Nodes]),
		    Routers_DBs_Pids = launch_routers_db(Client_Nodes, List_Routers, []),
		    feed_router_sup(List_Router_Sups, List_Routers, Routers_DBs_Pids),
		    feed_routers(New_List_Servers, List_Routers, List_Routers);
		false ->
		    routers_listener(Servers_Total, New_List_Servers, List_Routers, List_Router_Sups, Client_Nodes)
	    end;
	{Router_Name, Router_Pid, add_router} ->
	    New_List_Routers = lists:append(List_Routers, [{atom_to_list(Router_Name), Router_Pid}]),
	    routers_listener(Servers_Total, List_Servers, New_List_Routers, List_Router_Sups, Client_Nodes);
	{Router_Sup_Pid, add_router_sup} ->
	    New_List_Router_Sups = lists:append(List_Router_Sups, [Router_Sup_Pid]),
	    routers_listener(Servers_Total, List_Servers, List_Routers, New_List_Router_Sups, Client_Nodes);
	{New_Clients, add_client_node} ->
	    New_Client_Nodes = lists:append(Client_Nodes, New_Clients),
	    routers_listener(Servers_Total, List_Servers, List_Routers, List_Router_Sups, New_Client_Nodes);
	Other ->
	    io:format("Something failed here. routers_listener/4 received: ~p~n", [Other]),
	    routers_listener(Servers_Total, List_Servers, List_Routers, List_Router_Sups, Client_Nodes)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     launch_routers_db/3 spawns a routers_db process on each of the
%%     client nodes started, and returns a list with the pids of all
%%     the routers_db processes spawned.
%%
%% @spec launch_routers_db(Client_Nodes, List_Routers, Routers_DBs) ->
%%                  Routers_DBs (list) | {error, reason}
%% @end
%%--------------------------------------------------------------------
launch_routers_db(Client_Nodes, List_Routers, Routers_DBs) ->
    case Client_Nodes of 
	[] ->
	    io:format("All routers_db processes started.~n"),
	    Routers_DBs;
	[Client|New_Client_Nodes] ->
	    New_Routers_DBs = Routers_DBs ++ start_routers_db(Client, List_Routers),
	    launch_routers_db(New_Client_Nodes, List_Routers, New_Routers_DBs)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     start_routers_db/2 spawns a routers_db process on the target
%%     client node passed as the argument. It returns a list of length
%%     1 containing the pid of the routers_db process spawned. It also
%%     flags the process to globally register itself.
%%
%% @spec launch_routers_db(Client_Node, List_Routers) ->
%%                  [Routers_DB_Pid] | {error, reason}
%% @end
%%--------------------------------------------------------------------
start_routers_db(Client_Node, List_Routers) ->
    io:format("Executing start_routers_db/2~n"),
    R_DB_Pid = spawn(Client_Node, fun() -> routers_db(List_Routers) end),
    io:format("routers_db Pid = ~p~n", [R_DB_Pid]),
    R_DB_Pid ! {register_globally},
    [R_DB_Pid].

%%--------------------------------------------------------------------
%% @doc
%%     This process is spawned in all client nodes and it stores the
%%     pids of all the routers processes spawned in the system. It is
%%     in charge of providing the clients these pids, so the clients
%%     can direct their requests to the routers.
%%
%% @spec routers_db(Routers_List) -> Status Messages | Routers_pids (list)
%% @end
%%--------------------------------------------------------------------
routers_db(Routers_List) ->
    receive
	{register_globally} ->
	    io:format("routers_db/1 received {register_globally}~n"),
	    global:register_name(routers_db, self()),
	    routers_db(Routers_List);
	{register_locally} ->
	    io:format("routers_db/1 received {register_locally}~n"),
	    Pid = self(),
	    register(routers_db, Pid),
	    routers_db(Routers_List);
	{retrieve_routers, Requester_Pid} ->
	    Requester_Pid ! {Routers_List, router_list},
	    routers_db(Routers_List);
	{New_Routers_List, receive_router_list}->
	    routers_db(New_Routers_List);
	Other ->
	    io:format("Something failed at routers_db/1. Received: ~p~n", [Other]),
	    routers_db(Routers_List)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     feed_router_sup/3 passes the list of routers spawned in the system
%%     to the router supervisors.
%% @spec feed_router_sup(List_Router_Sups, List_Routers, Router_DBs_Pids) ->
%%                  ok | {error, reason}
%% @end
%%--------------------------------------------------------------------
feed_router_sup(List_Router_Sups, List_Routers, Router_DBs_Pids) ->
    io:format("feed_router_sup/2~n"),
    case List_Router_Sups of
	[] ->
	    ok;
	[R_Sup_Pid|T] ->
	    R_Sup_Pid ! {list_routers, List_Routers, Router_DBs_Pids},
	    feed_router_sup(T, List_Routers, Router_DBs_Pids)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     feed_routers/3 passes the list of routers and servers spawned in
%%     the system to the router processes. This is needed for reliability
%% @spec feed_router_sup(List_Servers, List_Routers, List_Routers_2) ->
%%                  Status Message | {error, reason}
%% @end
%%--------------------------------------------------------------------    
feed_routers(List_Servers, List_Routers, List_Routers_2) ->
    case List_Routers_2 of
	[] ->
	    io:format("Host is deployed.~n");
	%%init:stop();
	[{_, Router_Pid}|New_List_Routers] ->
	    Router_Pid ! {list_routers, List_Routers},
	    Router_Pid ! {list_servers, List_Servers},
	    feed_routers(List_Servers, List_Routers, New_List_Routers)
    end.

%%=========================================================================
%% AUXILIARY FUNCTIONS
%%=========================================================================

%%--------------------------------------------------------------------
%% @doc
%%     This function builds a list containing the names of the target
%%     nodes given all the details of the architecture of the Instant
%%     Messenger application. This is used to launch the application
%%     on a distributed environment with one node per host.
%%
%% @spec nodes_list(R_T, S_RN, S_T, Cl_Total, Domains) -> list()
%% @end
%%--------------------------------------------------------------------
nodes_list(R_T, S_RN, S_T, Cl_T, Domains) ->
    {R_S_Dom, Cl_Dom} = lists:split(R_T + S_T, Domains),
    R_S_Nodes = router_server_nodes(1,1,S_RN, R_S_Dom, []),
    Cl_Nodes = client_nodes(Cl_T, 1, hd(Cl_Dom), []),
    lists:reverse(lists:flatten([Cl_Nodes|R_S_Nodes])).

%%--------------------------------------------------------------------
%% @doc
%%     router_server_nodes/5 builds the list containing the names of
%%     the router and server nodes where the Instant Messenger
%%     application will be deployed.
%%
%% @spec router_server_nodes(R_Acc, S_Acc, S_RN,
%%                           Domains, Nodes_List) -> list()
%% @end
%%--------------------------------------------------------------------
router_server_nodes(R_Acc, S_Acc, S_RN, Domains, Nodes_List) ->
    case Domains of
	[] ->
	    lists:flatten(Nodes_List);
	[R_Dom|O_Doms] ->
	    Router = [string_to_atom(string:join(["router", integer_to_list(R_Acc)], "_") ++ "@" ++ atom_to_list(R_Dom))],
	    case S_RN =< length(O_Doms) of
		true ->
		    {S_Doms, New_Domains} = lists:split(S_RN, O_Doms),
		    {Servers, New_S_Acc} = server_nodes(S_Acc, S_Doms, []),
		    New_Nodes_List = [[Servers|Router]|Nodes_List],
		    router_server_nodes(R_Acc + 1, New_S_Acc, S_RN, New_Domains, New_Nodes_List);
		false ->
		    {Servers, New_S_Acc} = server_nodes(S_Acc, O_Doms, []),
		    New_Nodes_List = [[Servers|Router]|Nodes_List],
		    router_server_nodes(R_Acc + 1, New_S_Acc, S_RN, [], New_Nodes_List)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%%     server_nodes/3 is an auxiliary function to router_server_nodes/5.
%%     It generates the names of the server nodes.
%%
%% @spec server_nodes(S_Acc, S_RN, Domains, Nodes) -> {list(), Integer()}
%% @end
%%--------------------------------------------------------------------
server_nodes(S_Acc, Domains, Nodes) ->
    case Domains of
	[] ->
	    {Nodes, S_Acc};
	[H|T] ->
	    New_Nodes = [string_to_atom(string:join(["server", integer_to_list(S_Acc)], "_")
					++ "@" ++ atom_to_list(H))|Nodes],
	    server_nodes(S_Acc + 1, T, New_Nodes)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     client_nodes/4 generates the names of the client nodes.
%%
%% @spec client_nodes(Cl_T, Cl_Acc, Domain, Nodes) -> list()
%% @end
%%--------------------------------------------------------------------
client_nodes(Cl_T, Cl_Acc, Domain, Nodes) ->
    case Cl_Acc of
	Cl_T ->
	    New_Nodes = [string_to_atom(string:join(["client", integer_to_list(Cl_Acc)], "_")
					++ "@" ++ atom_to_list(Domain))|Nodes],
	    lists:flatten(New_Nodes);
	_Other ->
	    New_Nodes = [string_to_atom(string:join(["client", integer_to_list(Cl_Acc)], "_")
					++ "@" ++ atom_to_list(Domain))|Nodes],
	    client_nodes(Cl_T, Cl_Acc + 1, Domain, New_Nodes)
    end.

%--------------------------------------------------------------------
%% @doc
%%     Given a list of nodes, extractor/1 returns a tuple containing
%%     three lists: one containing the router nodes, a second that
%%     holds the server nodes, and a third with the client nodes. If
%%     the input list contains invalid names, these are dismissed.
%%
%% @spec extractor(Nodes_List) -> {list(), list(), list()}.
%% @end
%%--------------------------------------------------------------------
extractor(Nodes_List) ->
    extractor(Nodes_List, [],[],[]).

%--------------------------------------------------------------------
%% @doc
%%     Given a list of nodes, and three recipient lists for the names
%%     of the router nodes, server nodes and client nodes, extractor/4
%%     behaves in the same way as extractor/1.
%%
%% @spec extractor(Nodes_List, R_Nodes, S_Nodes, Cl_Nodes) ->
%%                  {list(), list(), list()}.
%% @end
%%--------------------------------------------------------------------
extractor(Nodes_List, R_Nodes, S_Nodes, Cl_Nodes) ->
    case Nodes_List of
	[] ->
	    {lists:reverse(R_Nodes),
	     lists:reverse(S_Nodes),
	     lists:reverse(Cl_Nodes)};
	[H|T] ->
	    [Token|_Domain] = string:tokens(atom_to_list(H), "@"),
	    case hd(string:tokens(Token, "_")) of
		"router" ->
		    New_R_Nodes = [H|R_Nodes],
		    extractor(T, New_R_Nodes, S_Nodes, Cl_Nodes);
		"server" ->
		    New_S_Nodes = [H|S_Nodes],
		    extractor(T, R_Nodes, New_S_Nodes, Cl_Nodes);
		"client" ->
		    New_Cl_Nodes = [H|Cl_Nodes],
		    extractor(T, R_Nodes, S_Nodes, New_Cl_Nodes);
		_Other ->
		    extractor(T, R_Nodes, S_Nodes, Cl_Nodes)
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function builds the name of a target node given all the
%%     details of the architecture of the system.
%%
%% @spec node_name_generator(Host, Node_Type, Domain,
%%          Total_Nodes, Children_Nodes_Per_Node) -> atom()
%% @end
%%--------------------------------------------------------------------
node_name_generator(Host, Node_Type, Domain, Total_Nodes, Children_Nodes_Per_Node) ->
    case Children_Nodes_Per_Node of
	0 ->
	    [string_to_atom(
	       string:join([atom_to_list(Node_Type),
			    integer_to_list(X)], "_") ++
		   "@" ++ atom_to_list(Domain)) ||
		X <- lists:seq(((Host * Total_Nodes) - Total_Nodes + 1),
			       Host * Total_Nodes)];
	_Other ->
	    [string_to_atom(
	       string:join([atom_to_list(Node_Type),
			    integer_to_list(X),
			    integer_to_list(Y)], "_") ++
		   "@" ++ atom_to_list(Domain)) ||
		X <- lists:seq(((Host * Total_Nodes) - Total_Nodes + 1),
			       Host * Total_Nodes),
		Y <- lists:seq(1, Children_Nodes_Per_Node)]
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function builds a list containing the name of all the router 
%%     processes that will be used in the system.
%%
%% @spec node_name_generator(Host, Node_Type, Domain,
%%          Total_Nodes, Children_Nodes_Per_Node) -> list of atom()
%% @end
%%--------------------------------------------------------------------
router_names_generator(Num_of_Host, Servers_Total, Servers_Router_Node,
		       Servers_Router_Process, Num_of_Sub_Pr) ->
    B =  Servers_Total div Servers_Router_Node,
    C =  Servers_Total div Servers_Router_Process,
    io:format("Servers_Router_Node: ~p, Servers_Router_Process: ~p~n",
	      [Servers_Router_Node, Servers_Router_Process]),
    io:format("B = ~p, C = ~p~n", [B, C]),
    case Servers_Total of
	Servers_Router_Node ->
	    io:format("true~n"),
	    D = (Servers_Total div Servers_Router_Process);
	_Other ->
	    io:format("false~n"),
	    case Servers_Router_Node rem Servers_Router_Process == 0 of
		true ->
		    D = Servers_Total div Servers_Router_Process;
		false ->
		    D = (Servers_Total div Servers_Router_Node)
			+ (Servers_Total div Servers_Router_Process)
	    end
    end,
    io:format("D: ~p~n",[D]),
    case D * Servers_Router_Process < Servers_Total of
	true ->
	    A = D + 1;
	false ->
	    A = D
    end,
    io:format("A: ~p~n", [A]),
    Lower_Limit = (Num_of_Host-1) * A + 1,
    Upper_Limit = Lower_Limit + A - 1,
    case Num_of_Sub_Pr of
	0 ->
	    [string_to_atom(
	       string:join(["router",
			    integer_to_list(X)], "_")) ||
		X <- lists:seq(Lower_Limit, Upper_Limit)];
	_Another ->
	    [string_to_atom(
	       string:join(["router",
			    integer_to_list(X),
			    integer_to_list(Y)], "_")) ||
		X <- lists:seq(Lower_Limit, Upper_Limit),
		Y <- lists:seq(1, Num_of_Sub_Pr)]
    end.

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
