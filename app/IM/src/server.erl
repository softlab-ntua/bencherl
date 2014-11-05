%%%--------------------------------------------------------------------
%%% SERVER MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Server module for the Reliable Distributed Erlang instant
%%%     messenger (RD-IM) application developed as a real benchmark for
%%%     the Scalable Distributed Erlang extension of the Erlang/OTP
%%%     language.
%%%
%%%	This module implementes the functionality for server nodes in a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 1 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------

-module(server).
-export([start_server_supervisor/3, stop_server_supervisor/1, restart_server_supervisor/1]).

%%---------------------------------------------------------------------
%% @doc
%%     Starts the server by spawing a server supervisor process.
%%
%% @spec start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
%%          Pid | {error, Error}
%% @end
%%---------------------------------------------------------------------
start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
    process_flag(trap_exit, true),
    Mon_Process_Table = monitored_processes_1,
    case ets:info(Mon_Process_Table) of
	undefined ->
	    ets:new(Mon_Process_Table, [set, named_table]);
	_Other ->
	    ok
    end,
    DBs = [monitored_processes_2] ++ client_db_name(Server_Name, Num_Total_Servers) ++ chat_db_name(Server_Name, Num_Total_Servers),
    Monitored_DBs = spawn_databases(Node, DBs, Mon_Process_Table, []),
    register_dbs(Monitored_DBs),
    recover_monitor_db(monitored_processes_2, monitored_processes_1),
    {Client_DBs, Chat_DBs} = server_dbs(Server_Name),
    Monitored_Processes = {monitored_processes_1, monitored_processes_2},
    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes).

%%---------------------------------------------------------------------
%% @doc
%%     This function triggers the recovery strategy for the server
%%     supervisor process. It runs in two stages. The first one sets up
%%     the monitored_db process if it is not alive, and the ets table
%%     that holds the information about the processes monitored by the
%%     server_supervisor process. Then, it triggers the recovery strategy
%%     for the monitored_db process.
%%     The second stage, feeds the information received into the ets
%%     table of the server_supervisor process.     
%%
%% @spec start_server_supervisor(Node, Server_Name, Num_Total_Servers) ->
%%          Pid | {error, Reason}
%% @end
%%---------------------------------------------------------------------
restart_server_supervisor({first_stage, Server_Name}) ->
    process_flag(trap_exit, true),
    Mon_Process_Table = monitored_processes_1,
    Mon_Process_DB_Pid = whereis(monitored_processes_2),
    case Mon_Process_DB_Pid of
     	undefined ->
	    {New_Mon_Process_DB_Pid, _Ref} = spawn_monitor(fun() -> monitored_db:monitored_db(monitored_processes_2) end),
	    register(monitored_processes_2, New_Mon_Process_DB_Pid),
	    New_Mon_Process_DB_Pid ! {undefined, add, New_Mon_Process_DB_Pid, "Monitor_DB", monitored_processes_2},
     	    ok;
     	_ ->
	    case ets:info(Mon_Process_Table) of
		undefined ->
		    ets:new(Mon_Process_Table, [set, named_table]);
		_Other ->
		    ok
	    end,
	    Mon_Process_DB_Pid ! {self(), recover_server, Mon_Process_Table, monitored_processes_2}
    end,
    restart_server_supervisor({second_stage, Server_Name, {Mon_Process_Table, monitored_processes_2}});

restart_server_supervisor({second_stage, Server_Name, Monitored_Processes}) ->
    {Mon_Process_Table, _Mon_Process_DB} = Monitored_Processes,
    receive
	{undefined, add, F1, F2, F3} ->
	    ets:insert(Mon_Process_Table, {F1, F2, F3}),
	    N_Mon_Pr = mon_proc(F1, F2, F3, Monitored_Processes),
	    restart_server_supervisor({second_stage, Server_Name, N_Mon_Pr});
	true ->
	    {Client_DBs, Chat_DBs} = server_dbs(Server_Name),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     Stops the server with the same pid as the Pid parameter.
%%
%% @spec stop_server_supervisor(Pid) -> {'EXIT', normal}
%% @end
%%--------------------------------------------------------------------
stop_server_supervisor(Pid) ->
    Pid!{'EXIT', normal}.

%%--------------------------------------------------------------------
%% @doc
%%     This function constitutes the server supervisor process. It
%%     handles the requests made to the server, and also monitors all
%%     the server processes.
%%
%% @spec server_supervisor_loop(Client_DB_Name, Chat_DB_Name, Monitored_Dbs)
%% @end
%%--------------------------------------------------------------------
server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    receive
	%%client login request.
	{Client_Name, Client_Pid, login} ->
	    Server_Supervisor_Pid = self(),
	    {Client_Monitor_Pid, _Cl_Mon_Ref} = spawn_monitor(fun() -> client_monitor(Client_Name,
										      Client_Pid,
										      [],
										      Client_DBs,
										      Server_Supervisor_Pid)
							      end),
	    Client_Monitor_Pid ! {start_client_monitor},
	    ets:insert(Monitored_Proc_Table, {Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    whereis(Monitored_Proc_DB) ! {undefined, add, Client_Monitor_Pid, "Client_Monitor", Client_Name},
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% chat session request.
	{Sender, Receiver, start_chat_session} ->
	    {Chat_Session_Pid, _Ch_Ses_Ref} = spawn_monitor(fun() -> chat_session(Chat_DBs,
										  [{Sender, undefined},
										   {Receiver, undefined}])
							    end),
	    Chat_Session_Pid ! {self(), Sender, Receiver, start_chat_session},
	    ets:insert(Monitored_Proc_Table, {Chat_Session_Pid, "Chat_Session", chat_session_name(Sender, Receiver)}),
	    whereis(Monitored_Proc_DB) ! {undefined, add, Chat_Session_Pid, "Chat_Session", chat_session_name(Sender, Receiver)},
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% finish server_supervisor.
	{error, chat_session_already_started} ->
	    io:format("Session already exists: chat_session process aborted.~n"),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% confirmation server supervisor is down without any problems.
       	{'EXIT', normal} ->
	    io:format("server_supervisor is down.~n");
	%% confirmation client is logged in.
	{login_success, Client_Monitor_Pid} ->
	    io:format("Client logged in. Client Monitor Pid: ~p~n", [pid_to_list(Client_Monitor_Pid)]),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% confirmation chat session started. (This is never executed).
	{session_added, ok} ->
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% reliability control.
	%% ==== Uncomment these two lines for bencherl ====
	%%{'EXIT', Pid, Reason} ->
	%%    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% monitored process finished normally
	{'DOWN', _Ref, process, Pid, normal} ->
	    ets:delete(Monitored_Proc_Table, Pid),
	    whereis(Monitored_Proc_DB) ! {undefined, remove, Pid},
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% monitored process finished abnormally.	
	{'DOWN', Ref, process, Pid, Reason} ->
	    io:format("Process with pid ~p and ref ~p is down with reason ~p~n", [Pid, Ref, Reason]),
	    [_Pid, Type, Name] = look_up_pid(Monitored_Proc_Table, Pid),
	    case Type of
		"Monitor_DB" ->
		    {New_Monitored_Proc_DB_Pid, _Ref} = spawn_monitor(fun() -> monitored_db:monitored_db(Monitored_Proc_DB) end),
		    register(Monitored_Proc_DB, New_Monitored_Proc_DB_Pid),
		    ets:delete(Monitored_Proc_Table, Pid),
		    ets:insert(Monitored_Proc_Table, {New_Monitored_Proc_DB_Pid, "Monitor_DB", Monitored_Proc_DB}),
		    spawn(fun() -> recover_monitor_db(Monitored_Proc_DB, Monitored_Proc_Table)end),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
		"Client_DB" ->
		    Tokens = string:tokens(atom_to_list(Name), "_"),
	    	    {New_Client_DBs, New_Chat_DBs} = replicate_db(Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes),
		    db_recovery(Tokens),
		    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes);
		"Chat_DB" ->
		    Tokens = string:tokens(atom_to_list(Name), "_"),
	    	    {New_Client_DBs, New_Chat_DBs} = replicate_db(Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes),
	    	    db_recovery(Tokens),
		    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes);
		"Chat_Session" ->
		    chat_session_recovery(Pid, Chat_DBs, Monitored_Processes),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
		"Client_Monitor" ->
		    client_monitor_recovery(Pid, Client_DBs, self(), Monitored_Processes),
		    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes)
	    end;
	{recovered, ok} ->
	    New_Client_DBs = db_names_swap(Client_DBs),
	    New_Chat_DBs = db_names_swap(Chat_DBs),
	    server_supervisor_loop(Server_Name, New_Client_DBs, New_Chat_DBs, Monitored_Processes);
	kill_server_process ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3),
	    case random:uniform(60) of
		Number when Number =< 12 ->
		    %% Processes_To_Kill = ets:select(Monitored_Proc_Table,
		    %% 				   [{{'$0', '$1', '$2'},
		    %% 				     [{'==', '$1', "Monitored_DB"}],
		    %% 				     ['$0']}]),
		    Pid_To_Kill = whereis(monitored_processes_2);%%hd(Processes_To_Kill);
		Number when Number > 12, Number =< 24 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Chat_DB"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 24, Number =< 36 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Client_DB"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 36, Number =< 48 ->
		    Pid_To_Kill = self();
		Number when Number > 48, Number =< 54 ->
		    Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Client_Monitor"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill);
		Number when Number > 54 ->
		     Processes_To_Kill = ets:select(Monitored_Proc_Table,
						   [{{'$0', '$1', '$2'},
						     [{'==', '$1', "Chat_Session"}],
						     ['$0']}]),
		    Pid_To_Kill = lists:nth(random:uniform(length(Processes_To_Kill)),Processes_To_Kill)
	    end,
	    exit(Pid_To_Kill, kill),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes);
	%% Trap for any other messages.
	Other ->
	    io:format("Server supervisor received: ~p~n", [Other]),
	    server_supervisor_loop(Server_Name, Client_DBs, Chat_DBs, Monitored_Processes)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This function constitutes the chat_session process. It forwards
%%     the messages received from Client A to the Client B. It is also
%%     responsible of handling the chat session termiation and notifying
%%     the clients when this happens.
%%
%% @spec chat_session(Chat_DBs, Clients_info).
%% @end
%%--------------------------------------------------------------------
chat_session(Chat_DBs, Clients_Info) ->
    {Chat_DB_Name, Chat_DB_Replica} = Chat_DBs,
    [{Client_A, Client_A_Pid}, {Client_B, Client_B_Pid}] = Clients_Info,
    Session_Name = chat_session_name(Client_A, Client_B),
    receive
	%% Start Chat Session.
	{_, Sender, Receiver, start_chat_session} ->
	    Session_Name = chat_session_name(Sender, Receiver),
	    global:whereis_name(Chat_DB_Name) ! {self(), peak, Session_Name},
	    chat_session(Chat_DBs, Clients_Info);
	%% Session setup.
	[] ->
	    {Client_A, New_Client_A_Pid} = {Client_A, find_client_pid(Client_A, 1)},
	    {Client_B, New_Client_B_Pid} = {Client_B, find_client_pid(Client_B, 1)},
	    case New_Client_B_Pid of
		client_not_found ->
		    New_Client_A_Pid ! {receiver_not_found, Client_B};
		_ ->
		    New_Clients_Info = [{Client_A, New_Client_A_Pid},{Client_B, New_Client_B_Pid}],
		    global:whereis_name(Chat_DB_Name) ! {self(), 
							 add,
							 Session_Name,
							 self(), 
							 Client_A,
							 New_Client_A_Pid, 
							 Client_B,
							 New_Client_B_Pid},
		    global:whereis_name(Chat_DB_Replica) ! {undefined,
							    add,
							    Session_Name,
							    self(),
							    Client_A,
							    New_Client_A_Pid,
							    Client_B,
							    New_Client_B_Pid},
		    chat_session(Chat_DBs, New_Clients_Info)
	    end;
	%% Session already exists.
	[_,_,_,_,_,_] ->
	    chat_session(Chat_DBs, Clients_Info);
	{session_added, ok} ->	   
	    Client_A_Pid ! {chat_session_success_sender, Session_Name, self()},
	    Client_B_Pid ! {chat_session_success_receiver, Session_Name, self()},
	    chat_session(Chat_DBs, Clients_Info);
	%% Message delivery confirmation
	%% (Normal client_
	{From, Timestamp, message_delivered_ok} ->
	    Metadata = {Session_Name, Client_A, Client_B, Timestamp},
	    case From of
		Client_A ->
		    Client_A_Pid ! {Metadata, message_delivered_ok};
		Client_B ->
		    Client_B_Pid ! {Metadata, message_delivered_ok}
	    end,
	    chat_session(Chat_DBs, Clients_Info);
	%% Traffic generator (Toxic client).
	{From, Unique_ID, Timestamp, message_delivered_ok} ->
	    Metadata = {Unique_ID, Session_Name, Client_A, Client_B, Timestamp},
	    case Client_A of
		From ->
		    Client_A_Pid ! {Metadata, message_delivered_ok};
		_To ->
		    Client_B_Pid ! {Metadata, message_delivered_ok}
	    end,
	    chat_session(Chat_DBs, Clients_Info);
	%% Session termination logic and confirmation to clients.
	%% Normal client
	{_, _, _, finish_chat_session} ->
	    global:whereis_name(Chat_DB_Name) ! {self(), remove, Session_Name},
	    global:whereis_name(Chat_DB_Replica) ! {undefined, remove, Session_Name},
	    chat_session(Chat_DBs, Clients_Info);
	%% Traffic generator (Toxic client).
	{_, _, _, _, finish_chat_session} ->
	    case global:whereis_name(Chat_DB_Name) of
		undefined ->
		    ok;
		Chat_DB_Main_Pid ->
		    Chat_DB_Main_Pid ! {self(), remove, Session_Name}
	    end,
	    case global:whereis_name(Chat_DB_Replica) of
		undefined ->
		    ok;
		Chat_DB_Replica_Pid ->
		    Chat_DB_Replica_Pid ! {undefined, remove, Session_Name}
	    end,
	    chat_session(Chat_DBs, Clients_Info);
	{session_removed, ok} ->
	    %%io:format("received {session_removed, ok}~n"),
	    Client_A_Pid ! {Session_Name, {'EXIT', ok}},
	    Client_B_Pid ! {Session_Name, {'EXIT', ok}};
	%% Messages delivery action.
	%% Normal client.
	{From, To, Timestamp, Message} ->
	    case Client_A == To of
		true ->
		    Client_A_Pid ! {From, Message, self(), Timestamp, receive_message};
		false ->
		    Client_B_Pid ! {From, Message, self(), Timestamp, receive_message}
	    end,
	    chat_session(Chat_DBs, Clients_Info);
	%% Traffic generator (Toxic client).
	{Unique_ID, From, To, Timestamp, Message} ->
	    case Client_A == To of
		true ->
		    Client_A_Pid ! {Unique_ID, From, Message, self(), Timestamp, receive_message};
		false ->
		    Client_B_Pid ! {Unique_ID, From, Message, self(), Timestamp, receive_message}
	    end,
	    chat_session(Chat_DBs, Clients_Info);
	%% Trap for any other messages.
	Other ->
	    io:format("Something failed at chat_session({final_state, Chat_DBs, Clients_Info}).~nReceived: ~p~n", [Other]),
	    chat_session(Chat_DBs, Clients_Info)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     This is the client monitor process. It is responsible for pre-
%%     venting a client from logging in twice. It also handles the
%%     client's logout logic and finishes all the pending chat sessions
%%     a client has opened when it terminates (either normally or
%%     abnormally).
%%
%% @spec client_monitor(Client_Name, Client_Pid,
%%                      Client_DB_Name, Server_Supervisor_Pid).
%% @end
%%--------------------------------------------------------------------
client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid) ->
    process_flag(trap_exit, true),
    {Client_DB_Name, Client_DB_Replica} = Client_DBs,
    receive
	%% Login logic.
	{start_client_monitor} ->
	    global:whereis_name(Client_DB_Name) ! {self(), peak, Client_Name},
	    receive
		[] ->
		    erlang:monitor(process, Client_Pid),
		    global:whereis_name(Client_DB_Name) ! {self(), add, Client_Name, Client_Pid, self()},
		    global:whereis_name(Client_DB_Replica) ! {undefined, add, Client_Name, Client_Pid, self()},
		    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
		Response ->
		    Client_Pid ! {error, client_already_logged_in},
		    io:format("Client_DB response: ~p~n", [Response])
	    end;
	{client_added, ok} ->	    
	    Client_Pid ! {login_success, self()},
	    Server_Supervisor_Pid ! {login_success, self()},
	    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	%% chat sessions the client is involved.
	{add_chat_session, Chat_Session} ->
	    New_Chat_Sessions = Opened_Chat_Sessions ++ [Chat_Session],
	    client_monitor(Client_Name, Client_Pid, New_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{remove_chat_session, Chat_Session_Name} ->
	    New_Chat_Sessions = lists:keydelete(Chat_Session_Name, 1, Opened_Chat_Sessions),
	    client_monitor(Client_Name, Client_Pid, New_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{'EXIT', Pid, Reason} ->
	    {'EXIT', Pid, Reason};
	%% monitored client finished normally.
	{'DOWN', _Ref, process, _Pid, normal} ->
	    ok;
	%% monitored client finished abnormally (behave as if client logged out).	
	{'DOWN', _Ref, process, _Pid, _Reason} ->
	    global:whereis_name(Client_DB_Name) ! {self(), peak_by_client_monitor, self()},
	    receive
		[Client_Name, Client_Pid, _Client_Monitor_Pid] ->
		    global:whereis_name(Client_DB_Name) ! {self(), remove, Client_Name},
		    global:whereis_name(Client_DB_Replica) ! {undefined, remove, Client_Name},
		    finish_session(Client_Name, Opened_Chat_Sessions);
		Other ->
		    io:format("Something failed while handling abnormal client termination.~nReceived ~p~n", [Other])
	    end;
	%% Logout logic.
	{Client_Name, Client_Pid, logout} ->
	    global:whereis_name(Client_DB_Name) ! {self(), remove, Client_Name},
	    global:whereis_name(Client_DB_Replica) ! {undefined, remove, Client_Name},
	    client_monitor(Client_Name, Client_Pid, Opened_Chat_Sessions, Client_DBs, Server_Supervisor_Pid);
	{client_removed, ok} ->
	    Client_Pid ! {'EXIT', ok},
	    finish_session(Client_Name, Opened_Chat_Sessions)
    end.

%%===============================================================================
%% AUXILIARY FUNCTIONS
%%===============================================================================

%% -------------------------
%% Names building functions.
%% -------------------------

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of chat sessions.
%%
%% @spec chat_session_name(Sender, Receiver) -> atom()
%% @end
%%--------------------------------------------------------------------
chat_session_name(Sender, Receiver) ->
    case is_atom(Sender) of
	true ->
	    S = atom_to_list(Sender);
	false ->
	    S = Sender
    end,
    case is_atom(Receiver) of
	true ->
	    R = atom_to_list(Receiver);
	false ->
	    R = Receiver
    end,
    case S < R of
	true ->
	    {Client_A, Client_B} = {S,R};
	false ->
	    {Client_A, Client_B} = {R,S}
    end,
    string:join(["chat_session", Client_A, Client_B], "_").

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of client database.
%%
%% @spec client_db_name(Server_Number) -> atom()
%% @end
%%--------------------------------------------------------------------
client_db_name(Server_Number) ->
    Target_DB = string:join(["server", integer_to_list(Server_Number), "1","Client_DB"], "_"),
    string_to_atom (Target_DB).

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of client databases.
%%
%% @spec client_db_name(Server_Name, Num_Total_Servers) -> [atom(), atom()]
%% @end
%%--------------------------------------------------------------------
client_db_name(Server_Name, Num_Total_Servers) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    N1 = "server_" ++ Server_Num ++ "_1_Client_DB",
    case list_to_integer(Server_Num) == Num_Total_Servers of
	true ->
	    N2 = "server_1_2_Client_DB";
	false ->
	    N2 = "server_" ++ integer_to_list(list_to_integer(Server_Num) + 1) ++ "_2_Client_DB"
    end,
    Client_DB_Name = string_to_atom(N1),
    Client_DB_Replica = string_to_atom(N2),
    [Client_DB_Name, Client_DB_Replica].

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of chat databases.
%%
%% @spec chat_db_name(Server_Name, Num_Total_Servers) -> [atom(), atom()]
%% @end
%%--------------------------------------------------------------------
chat_db_name(Server_Name, Num_Total_Servers) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    N1 = "server_" ++ Server_Num ++ "_1_Chat_DB",
    case list_to_integer(Server_Num) == Num_Total_Servers of
	true ->
	    N2 = "server_1_2_Chat_DB";
	false ->
	    N2 = "server_" ++ integer_to_list(list_to_integer(Server_Num) + 1) ++ "_2_Chat_DB"
    end,
    Chat_DB_Name = string_to_atom(N1),
    Chat_DB_Replica = string_to_atom(N2),
    [Chat_DB_Name, Chat_DB_Replica].

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary functions to build the name of the dabases of a server.
%%
%% @spec server_dbs(Server_Name) -> {{atom(), atom()}, {atom(), atom()}}
%% @end
%%--------------------------------------------------------------------
server_dbs(Server_Name) ->
    [_, Server_Num] = string:tokens(atom_to_list(Server_Name), "_"),
    Client_DB_Main = "server_" ++ Server_Num ++ "_1_Client_DB",
    Client_DB_Repl = "server_" ++ Server_Num ++ "_2_Client_DB",
    Chat_DB_Main = "server_" ++ Server_Num ++ "_1_Chat_DB",
    Chat_DB_Repl = "server_" ++ Server_Num ++ "_2_Chat_DB",
    Client_DBs = {string_to_atom(Client_DB_Main), string_to_atom(Client_DB_Repl)},
    Chat_DBs = {string_to_atom(Chat_DB_Main), string_to_atom(Chat_DB_Repl)},
    {Client_DBs, Chat_DBs}.

%% ------------------------------------
%% Databases creation and registration.
%% ------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%      Auxiliary function to build up a list of the databases monitored
%%      by a server supervisor. It spawns the databases needed and then
%%      it returns a list containing the names of the DBs monitored by
%%      the server_supervisor process.
%%
%% @spec spawn_databases(Node, DBs, Mon_Process_Table, Monitored_DBs) ->
%%                  list()
%% @end
%%--------------------------------------------------------------------
spawn_databases(Node, DBs, Mon_Process_Table, Monitored_DBs) ->
    case DBs of
    	[] ->
    	    Monitored_DBs;
    	[DB|Tail_DBs] ->
	    case DB of
		monitored_processes_2 ->
		    Type = "Monitor_DB";
		_Other ->
		    [_,_,_,T,_] = string:tokens(atom_to_list(DB), "_"),
		    Type = T ++ "_DB"
	    end,
    	    Spawned_DB = spawn_db(Node, DB, Type, Mon_Process_Table),
    	    New_Monitored_DBs = Monitored_DBs ++ Spawned_DB,
    	    spawn_databases(Node, Tail_DBs, Mon_Process_Table, New_Monitored_DBs)
    end.

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build up a list of the databases monitored
%%     by a server supervisor. It spawns the databases needed and then
%%     it returns a list containing the names of the DBs monitored by
%%     the server_supervisor process.
%%
%%     There may be a race condition here between spawn_link and monitor
%%     operations.
%%
%% @spec replica_node(Node, Server_Name, Num_Total_Servers) -> list()
%% @end
%%--------------------------------------------------------------------
spawn_db(Node, DB, Type, Mon_Process_Table) ->
    case Type of
	"Client_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> client_db:client_db(DB) end);
	"Chat_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> chat_db:chat_db(DB) end);
	"Monitor_DB" ->
	    DB_Pid = spawn_link(Node, fun() -> monitored_db:monitored_db(monitored_processes_2) end)
    end,
    _DB_Ref = erlang:monitor(process, DB_Pid),
    unlink(DB_Pid),
    ets:insert(Mon_Process_Table, {DB_Pid, Type, DB}),
    [{DB, DB_Pid}].

%%--------------------------------------------------------------------
%% @doc
%%      Auxiliary function to register gobally the databases spawned by
%%      a server supervisor.
%%
%% @spec register_dbs(DBs) -> ok
%% @end
%%--------------------------------------------------------------------
register_dbs(DBs) ->
    case DBs of
	[] ->
	    ok;
	[DB|New_DBs] ->
	    {Name, Pid} = DB,
	    case Name of
		monitored_processes_2 -> 
		    register(Name, Pid);
		_Other ->
		    global:register_name(Name, Pid)
	    end,
	    register_dbs(New_DBs)
    end.

%% -------------------------------------
%% Process recovery auxiliary functions.
%% -------------------------------------

%%---------------------------------------------------------------------
%% @doc
%%      Process Identifier function (Process Recovery).
%%
%% @spec look_up_pid(Table_Name, Process_Pid) ->
%%                  [Pid(), process_type, process_name] |
%%                  [undefined, undefined, undefined]
%% @end
%%---------------------------------------------------------------------
look_up_pid(Table_Name, Process_Pid) ->    
    L = ets:select(Table_Name, [{{'$0', '$1', '$2'},
				 [{'==', '$0', Process_Pid}],
				 [['$0', '$1', '$2']]}]),
    case L == [] of
	true ->
	    [undefined, undefined, undefined];
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%%      Monitor processes setter (Used during server supervisor recovery).
%%
%% @spec mon_proc(Pid, Type, Name, Monitored_Processes) -> {atom(), atom()}
%% @end
%%---------------------------------------------------------------------
mon_proc(Pid, Type, Name, Monitored_Processes) ->
    case Type of
	"Monitor_DB" ->
	    erlang:monitor(process, Pid),
	    New_Mon_Processes = setelement(2, Monitored_Processes, Name),
	    New_Mon_Processes;
	_Other ->
	    erlang:monitor(process, Pid),
	    Monitored_Processes
    end.

%% --------------------------------------
%% Monitored Processes database recovery.
%% --------------------------------------

%%---------------------------------------------------------------------
%% @doc
%% Replicates the monitored processes table specified in the Source
%% parameter, in a new table with name Table_Name. Source is the ets
%% table owned by the server_supervisor process.
%%
%% @spec recover_server_table(Destination, Source, Destination_Pid) ->
%%                  true | {error, Error}
%% @end
%%---------------------------------------------------------------------
recover_monitor_db(Monitored_Proc_DB, Monitored_Proc_Table) ->
    ets:safe_fixtable(Monitored_Proc_Table, true),
    transfer_data(Monitored_Proc_Table, Monitored_Proc_DB, ets:first(Monitored_Proc_Table)),
    ets:safe_fixtable(Monitored_Proc_Table, false).

%%---------------------------------------------------------------------
%% @doc
%% Auxiliary function to the recover_server_table/3 function. This
%% function traverses the source table specified in Source, and feeds the
%% data in the destination table.
%%
%% @spec replicate_server_table(Source, Destination, Key, Destination_Pid) ->
%%                  true | {error, Error}
%% @end
%%---------------------------------------------------------------------
transfer_data(Source, Destination, Key) ->
    case Key of
	'$end_of_table' ->
	    true;
	_ ->
	    S = look_up_pid(Source, Key),
	    whereis(Destination) ! {undefined, add,
				    lists:nth(1, S),
				    lists:nth(2, S),
				    lists:nth(3, S)},
	    transfer_data(Source, Destination, ets:next(Source, Key))
    end.

%% -----------------------------
%% Databases recovery functions.
%% -----------------------------

%%--------------------------------------------------------------------
%% @doc
%%     replicate/5 spawns a new db process and swaps the replica and
%%      main databases.
%%
%% @spec replicate_db(Failed_Pid, Tokens, Client_DBs, Chat_DBs,
%%          Monitored_Processes) -> {{atom(), atom()},{atom(), atom()}}
%% @end
%%--------------------------------------------------------------------
replicate_db(Failed_Pid, Tokens, Client_DBs, Chat_DBs, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    [_, Server_Num, Replica_Flag, Type, _] = Tokens,
    case Replica_Flag == "1" of
	true ->
	    DB2_Name_Str = string:join(["server", Server_Num, "2", Type, "DB"], "_");
	false ->
	    DB2_Name_Str = string:join(["server", Server_Num, "1", Type, "DB"], "_")
    end,
    DB_Name = string_to_atom(string:join(Tokens, "_")),
    %% spawn, register and monitor a new Db
    case Type of
	"Client" ->
	    {New_DB_Pid, _Ref} = spawn_monitor(fun() -> client_db:client_db(DB_Name) end),
	    global:register_name(DB_Name, New_DB_Pid),
	    ets:insert(Monitored_Proc_Table, {New_DB_Pid, "Client_DB", DB_Name}),
	    whereis(Monitored_Proc_DB) ! {undefined, add, New_DB_Pid, "Client_DB", DB_Name},
	    ets:delete(Monitored_Proc_Table, Failed_Pid),
	    whereis(Monitored_Proc_DB) ! {undefined, remove, Failed_Pid};
	"Chat" ->
	    {New_DB_Pid, _New_Ref} = spawn_monitor(fun() -> chat_db:chat_db(DB_Name) end),
	    global:register_name(DB_Name, New_DB_Pid),
	    ets:insert(Monitored_Proc_Table, {New_DB_Pid, "Chat_DB", DB_Name}),
	    whereis(Monitored_Proc_DB) ! {undefined, add, New_DB_Pid, "Chat_DB", DB_Name},
	    ets:delete(Monitored_Proc_Table, Failed_Pid),
	    whereis(Monitored_Proc_DB) ! {undefined, remove, Failed_Pid}
    end,
    %% return data
    case Type of
	"Client" when (Replica_Flag == "1") ->
	    New_Client_DBs = {string_to_atom(DB2_Name_Str), DB_Name},
	    {New_Client_DBs, Chat_DBs};
	"Chat" when (Replica_Flag == "1") ->
	    New_Chat_DBs = {string_to_atom(DB2_Name_Str), DB_Name},
	    {Client_DBs, New_Chat_DBs};
	_Other ->
	    {Client_DBs, Chat_DBs}
    end.

%%--------------------------------------------------------------------
%% @doc
%%     db_recovery/1 fires up the data transfer between replica and
%%     main databases.
%%
%% @spec db_recovery(Tokens) -> New DB | {error, reason}
%% @end
%%--------------------------------------------------------------------
db_recovery(Tokens) ->
    [_, Server, Replica_Flag, Type, _] = Tokens,
    case Replica_Flag == "1" of
	true ->
	    Src = string:join(["server", Server, "2", Type, "DB"], "_");
	false ->
	    Src = string:join(["server", Server, "1", Type, "DB"], "_")
    end,
    Destination = string_to_atom(string:join(Tokens, "_")),
    Source = string_to_atom(Src),
    global:whereis_name(Source) ! {self(), recover, Destination, Source}.

%%---------------------------------------------------------------------
%% @doc
%%     db_names_swap/1 swaps main and replica db names to use replica
%%     as main while main is recovering.
%%
%% @spec db_names_swap(DBs) -> {atom(), atom()}
%% @end
%%---------------------------------------------------------------------
db_names_swap(DBs) ->
    {DB_Repl, DB_Main} = DBs,
    [_, _, Rep_Flag, _, _] = string:tokens(atom_to_list(DB_Repl), "_"),
    case Rep_Flag == "2" of
	true ->
	    New_DBs = {DB_Main, DB_Repl};
	false ->
	    New_DBs = {DB_Repl, DB_Main}
    end,
    New_DBs.

%% -------------------------------
%% Chat Session recovery function.
%% -------------------------------

%%---------------------------------------------------------------------
%% @doc
%%     Signals the end of the failed chat session to clients and removes
%%     the session from Chat_DBs.
%%
%% @spec chat_session_recovery(Session_Pid, Chat_DBs,
%%          Monitored_Processes) -> ok.
%% @end
%%---------------------------------------------------------------------
chat_session_recovery(Session_Pid, Chat_DBs, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    {Chat_DB_Name, Chat_DB_Replica} = Chat_DBs,
    case is_process_alive(global:whereis_name(Chat_DB_Name)) of %% <=== CHECK THIS!!!
    	true ->
    	    global:whereis_name(Chat_DB_Name) ! {self(), peak_by_pid, Session_Pid};
    	false ->
    	    global:whereis_name(Chat_DB_Replica) ! {self(), peak_by_pid, Session_Pid}
    end,
    receive
    	[] ->
    	    io:format("chat_session_recovery received []~n"),
    	    ok;
    	[Session_Name, Session_Pid, _Client_A, Client_A_Pid, _Client_B, Client_B_Pid] ->
	    Client_A_Pid ! {Session_Name, {'EXIT', ok}},
	    Client_B_Pid ! {Session_Name, {'EXIT', ok}},
	    global:whereis_name(Chat_DB_Name) ! {undefined, remove, Session_Name},
	    global:whereis_name(Chat_DB_Replica) ! {undefined, remove, Session_Name},
	    ets:delete(Monitored_Proc_Table, Session_Pid),
	    whereis(Monitored_Proc_DB) ! {undefined, remove, Session_Pid}
    end.

%% ---------------------------------
%% Client Monitor recovery function.
%% ---------------------------------

%%---------------------------------------------------------------------
%% @doc
%%     Spawns a new client monitor, signals the new monitor pid to the
%%     client, and updates Client_DBs.
%%
%% @spec client_monitor_recovery(Client_Monitor_Pid, Client_DBs,
%%          Server_Supervisor_Pid, Monitored_Processes) ->
%%                  client_monitor/5 | ok
%% @end
%%---------------------------------------------------------------------
client_monitor_recovery(Client_Monitor_Pid, Client_DBs, Server_Supervisor_Pid, Monitored_Processes) ->
    {Monitored_Proc_Table, Monitored_Proc_DB} = Monitored_Processes,
    {Client_DB_Name, Client_DB_Replica} = Client_DBs,
    case is_process_alive(global:whereis_name(Client_DB_Name)) of
    	true ->
    	    global:whereis_name(Client_DB_Name) ! {self(), peak_by_client_monitor, Client_Monitor_Pid};
    	false ->
    	    global:whereis_name(Client_DB_Replica) ! {self(), peak_by_client_monitor, Client_Monitor_Pid}
    end,
    receive
    	[] ->
    	    io:format("client_monitor_recovery/3 received []~n"),
    	    ok;
    	[Client_Name, Client_Pid, _Client_Monitor_Pid] ->
	    Client_Pid ! {opened_chat_sessions, self()},
	    receive
		{chat_sessions, Chat_Sessions} ->
		    io:format("Chat_Sessions = ~p~n", [Chat_Sessions]),
		    Chat_Sessions
	    end,
	    {New_Client_Monitor_Pid, _Ref} = spawn_monitor(fun() -> client_monitor(Client_Name,
										   Client_Pid,
										   Chat_Sessions,
										   Client_DBs,
										   Server_Supervisor_Pid)
							   end),
	    Client_Pid ! {new_monitor_pid, New_Client_Monitor_Pid},
	    global:whereis_name(Client_DB_Name) ! {undefined, update_monitor_pid, Client_Name, New_Client_Monitor_Pid},
	    global:whereis_name(Client_DB_Replica) ! {undefined, update_monitor_pid, Client_Name, New_Client_Monitor_Pid},
	    ets:insert(Monitored_Proc_Table, {New_Client_Monitor_Pid, "Client_Monitor", Client_Name}),
	    whereis(Monitored_Proc_DB) ! {undefined, add, New_Client_Monitor_Pid, "Client_Monitor", Client_Name},
	    ets:delete(Monitored_Proc_Table, Client_Monitor_Pid),
	    whereis(Monitored_Proc_DB) ! {undefined, remove, Client_Monitor_Pid}
    end.

%% ---------------------------
%% Other processing functions.
%% ---------------------------

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to finish remaining opened sessions when
%%     client logs out.
%%
%% @spec finish_session(Client_Name, Sessions) -> {all_sessions_finished, ok}
%% @end
%%---------------------------------------------------------------------
finish_session(Client_Name, Sessions) ->
    case Sessions == [] of
	true ->
	    {all_sessions_finished, ok};
	false ->
	    {Session_Name, Session_Pid} = hd(Sessions),
	    {Client_A, Client_B} = extract_clients(Session_Name),
	    Session_Pid ! {Client_A, Client_B, undefined, finish_chat_session},
	    finish_session(Client_Name, tl(Sessions))
    end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to find a client pid.
%%
%% @spec find_client_pid(Client_Name, Server_Number) -> pid() | client_not_found
%% @end
%%---------------------------------------------------------------------
find_client_pid(Client_Name, Server_Number) ->
    Client_DB = client_db_name(Server_Number), %% <-This needs to be changed to use the replica as well.
    Client_DB_Pid = global:whereis_name(Client_DB),
    case Client_DB_Pid of
	undefined ->
	    client_not_found;
	_ ->
	    Client_DB_Pid ! {self(), client_pid, Client_Name},
	    receive
		[] ->
		    find_client_pid(Client_Name, Server_Number + 1);
		Client_Pid ->
		    Client_Pid
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%%     Auxiliary function to extract client names given the name of a
%%     chat session.
%%
%% @spec extract_clients(Session_Name) -> {Client_A, Client_B}
%% @end
%%---------------------------------------------------------------------
extract_clients(Session_Name) ->
    Tokens = string:tokens(Session_Name, "_"),
    Client_A = lists:nth(3, Tokens),
    Client_B = lists:nth(4, Tokens),
    {Client_A, Client_B}.

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

%%====================================================================
%% LEGACY CODE
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%     Auxiliary function to build the name of the target node for the
%%     DB replicas (This is legacy code, not in use. Left just in case
%%     it is useful in future).
%%
%% @spec replica_node(Node, Server_Name, Num_Total_Servers) -> atom()
%% @end
%%--------------------------------------------------------------------
%% replica_node(Node, Server_Name, Num_Total_Servers) ->
%%     Domain = string:sub_string(atom_to_list(Node), length(Server_Name) + 2),
%%     Tokens = string:tokens(Server_Name, "_"),
%%     case length(Tokens) == 3 of
%% 	true ->
%% 	    [_, Router_Num, Server_Num] = Tokens,
%% 	    Target_Server_Num = list_to_integer(Server_Num) + 1,
%% 	    S = "server_" ++ Router_Num,
%% 	    case list_to_integer(Server_Num) == Num_Total_Servers of
%% 		true ->
%% 		    To_Return = S ++ "_1@" ++ Domain;
%% 		false ->
%% 		    To_Return = S ++ "_" ++ integer_to_list(Target_Server_Num) ++ "@" ++ Domain
%% 	    end;
%% 	false ->
%% 	    [_, Server_Num] = Tokens,
%% 	    Target_Server_Num = list_to_integer(Server_Num) + 1,
%% 	    case list_to_integer(Server_Num) == Num_Total_Servers of
%% 		true ->
%% 		    To_Return = "server_1@" ++ Domain;
%% 		false ->
%% 		    To_Return = "server_" ++ integer_to_list(Target_Server_Num) ++ "@" ++ Domain
%% 	    end		
%%     end,
%%     string_to_atom(To_Return).
