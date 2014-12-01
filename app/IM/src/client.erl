%%%--------------------------------------------------------------------
%%% CLIENT MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Client module for the Distributed Erlang instant messenger (IM)
%%%	application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the functionality for client processes
%%%     in a system similar to the system described in the Section 2 of
%%%     the document "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 4 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------

-module(client).
-export([login/1,logout/1, message/3, stop_client/1]).

%%---------------------------------------------------------------------
%% @doc
%% Starts the client by spawing and registering a client process.
%%
%% @spec start(Client_Name) -> Pid | {error, Reason}
%% @end
%%---------------------------------------------------------------------
start_client(Client_Name) ->
    global:whereis_name(routers_db) ! {retrieve_routers, self()},
    receive
	{Routers_List, router_list} ->
	    Pid = spawn(fun() -> client({Client_Name, Routers_List}) end),
	    Answer = register(Client_Name, Pid),
	    Pid ! Answer;
	Other ->
	    io:format("Unable to start the client. No routers list received.~n"),
	    io:format("start_client/1 received: ~p~n", [Other])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the client specified as a parameter by sending an {'EXIT', ok}
%% message.
%%
%% @spec stop(Client_Name)
%% @end
%%--------------------------------------------------------------------
stop_client(Client_Name) ->
    whereis(Client_Name) ! {'EXIT', ok}.

%%--------------------------------------------------------------------
%% @doc
%% This is the function that constitutes the client process. It has two
%% different states. Initially, it is spawned and registered using the
%% client name, only. Once it has been registered, it waits until it
%% receives the Pid of its monitor.
%%
%% When the client_monitor Pid has been received, the login process is
%% concluded.
%%
%% @spec client({Client_Name, Client_Monitor_Pid, Chat_Pids})
%% @end
%%--------------------------------------------------------------------
client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List}) ->
    receive
	%% Chat Logic
	{Sender, Receiver, Message, send_message} ->
	    Session_Name = chat_session_name(Sender, Receiver),
	    case retrieve(Session_Name, Chat_Pids) of
		[] ->
		    Router_Pid = pick_router(Routers_List),
		    case rpc:pinfo(Router_Pid) of
			undefined ->
			    global:whereis_name(routers_db) ! {retrieve_routers, self()},
			    receive
			    	{New_Routers_List, router_list} ->
				    New_Router_Pid = pick_router(Routers_List),
				    New_Router_Pid ! {Sender, Receiver, Message, send_message},
				    client({Client_Name, Client_Monitor_Pid, Chat_Pids, New_Routers_List})
			    end;
			_Other ->
			    Router_Pid ! {Sender, Receiver, start_chat_session},
			    receive
				{chat_session_success_sender, Session_Name, Chat_Session_Pid} ->
				    io:format("Client_A received {chat_session_success, Chat_Session_Pid}.~n"),
				    New_Chat_Pids = Chat_Pids ++ [{Session_Name, Chat_Session_Pid}],
				    Chat_Session_Pid ! {Sender, Receiver, os:timestamp(), Message},
				    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
				{receiver_not_found, Receiver} ->
				    io:format("Sorry, but ~p is not logged in.~n", [Receiver]),
				    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
				Other ->
				    io:format("Client received ~p when trying to start a chat session.~n", [Other]),
				    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
			    end
		    end;
		Chat_Session_Pid ->
		    Chat_Session_Pid ! {Sender, Receiver, os:timestamp(), Message},
		    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
	    end;
	{chat_session_success_receiver, Session_Name, Chat_Session_Pid} ->
	    io:format("Client_B received {chat_session_success, Chat_Session_Pid}.~n"),
	    New_Chat_Pids = Chat_Pids ++ [{Session_Name, Chat_Session_Pid}],
	    Client_Monitor_Pid ! {add_chat_session, {Session_Name, Chat_Session_Pid}},
	    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
	{Metadata, message_delivered_ok} ->
	    Timestamp_2 = os:timestamp(),
	    {_, _, _, Timestamp_1} = Metadata,
	    Latency = timer:now_diff(Timestamp_2, Timestamp_1),
	    {{_Y, _M, _D}, {H, M, S}} = calendar:now_to_local_time(Timestamp_2),
	    io:format("ok. ~p:~p:~p Latency = ~p microseconds~n", [H, M, S, Latency]),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{Sender, Message, Chat_Session_Pid, Timestamp, receive_message} ->
	    io:format("~p wrote: ~p~n", [Sender, Message]),
	    Chat_Session_Pid ! {Sender, Timestamp, message_delivered_ok},
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{Chat_Session_Name, {'EXIT', ok}} ->
	    New_Chat_Pids = lists:keydelete(Chat_Session_Name, 1, Chat_Pids),
	    io:format("~p removed from Chat_Pids.~n", [Chat_Session_Name]),
	    Client_Monitor_Pid ! {remove_chat_session, Chat_Session_Name},
	    client({Client_Name, Client_Monitor_Pid, New_Chat_Pids, Routers_List});
	%% Recovery logic
	{opened_chat_sessions, Client_Monitor_Recover_Pid} ->
	    Client_Monitor_Recover_Pid ! {chat_sessions, Chat_Pids},
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{new_monitor_pid, New_Client_Monitor_Pid} ->
	    client({Client_Name, New_Client_Monitor_Pid, Chat_Pids, Routers_List});
	%% Logout Logic
	{logout} ->
	    Client_Monitor_Pid ! {Client_Name, self(), logout},
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	%% Client Termination Logic
	{error, client_already_logged_in} ->
	    io:format("You are logged in already.~n"),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List});
	{'EXIT', ok} ->
	    unregister(Client_Name),
	    io:format("~p is stopped now.~n", [Client_Name]);
	%% Trap for unexpected messages
	Other ->
	    io:format("Something is failing at client(/4).~n"),
	    io:format("Received: ~p~n", [Other]),
	    client({Client_Name, Client_Monitor_Pid, Chat_Pids, Routers_List})
    end;

%%--------------------------------------------------------------------
%% @doc
%% client(Client_Name) is the client process when the client is not
%% logged in yet. When the client_monitor Pid has been received, it
%% changes the state and changes to the full logged-in client; i.e.,
%% the client({Client_Name, Client_Pid, Client_Monitor_Pid}) function
%% defined above.
%%
%% @spec client(Client_Name)
%% @end
%%--------------------------------------------------------------------
client({Client_Name, Routers_List}) ->
    receive
	true -> %% This allows to have a client up, but not logged-in
	    client({Client_Name, Routers_List});
	false ->
	    {error, client_already_logged_in};
	{login_success, Client_Monitor_Pid} ->
	    io:format("Client is successfuly logged in.~n"),
	    client({Client_Name, Client_Monitor_Pid, [], Routers_List});
	{error, client_already_logged_in} ->
	    io:format("You are logged in already.~n"),
	    unregister(Client_Name);
	Other ->
	    io:format("This is failing at client(/2).~n"),
	    io:format("Received: ~p~n", [Other]),
	    client({Client_Name, Routers_List})
    end.

%%---------------------------------------------------------------------
%% @doc
%% login function starts a client named after the atom passed as
%% parameter and registers it in the system. If the client is logged in
%% already, then returns an error message.
%%
%% @spec login(Client_Name) -> yes | {error, Reason}
%% @end
%%---------------------------------------------------------------------
login(Client_Name) ->
    Client_Pid = whereis(Client_Name),
    case Client_Pid of
	undefined->
	    start_client(Client_Name),
	    global:whereis_name(routers_db) ! {retrieve_routers, self()},
	    receive
		{Routers_List, router_list} ->
		    Router_Pid = pick_router(Routers_List),
		    Router_Pid ! {Client_Name, whereis(Client_Name), login};
		Other ->
		    io:format("Login failed at retrieving routers list.~nReceived: ~p~n", [Other])
	    end,
	    yes;
	_ ->
	    io:format("You are logged in already.~n")
	    %%{error, client_already_logged_in}
    end.

%%---------------------------------------------------------------------
%% @doc
%% logout unegisters the client from the system.
%%
%% @spec logout(Client_Name) -> ok | {error, Reason}
%% @end
%%---------------------------------------------------------------------
logout(Client_Name) ->
    Client_Pid = whereis(Client_Name),
    case Client_Pid of
	undefined ->
	    io:format("Client ~p is not logged in.~n",[Client_Name]),
	    {error, client_not_logged_in};
	_ ->
	    Client_Pid ! {logout},
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% message/3 enables the communication between two clients, sending
%% the message passed as the parameter Message to the client specified
%% as the To parameter.
%%
%% @spec message(From, To, Message) -> yes | {error, Reason}
%% @end
%%--------------------------------------------------------------------
message(From, To, Message) ->
    whereis(From) ! {From, To, Message, send_message},
    yes.

%%--------------------------------------------------------------------
%% @doc
%% This function returns the pid of a router, from a routers list that
%% is passed as parameter.
%%
%% @spec pick_router(Routers_List) -> Pid | {error, Reason}
%% @end
%%--------------------------------------------------------------------
pick_router(Routers_List) ->
    {A1, A2, A3} = os:timestamp(),
    random:seed(A1, A2, A3),
    Router = lists:nth(random:uniform(length(Routers_List)),Routers_List),
    {_, Router_Pid} = Router,
    Router_Pid.

%%--------------------------------------------------------------------
%% @doc
%% chat_session_name/2 returns the name of a chat session, built after
%% the names of the clients passed as parameters.
%%
%% @spec chat_session_name(Sender, Receiver) ->
%%          string() | {error, Reason}
%% @end
%%--------------------------------------------------------------------
chat_session_name(Sender, Receiver) ->
    case atom_to_list(Sender) < atom_to_list(Receiver) of
	true ->
	    {Client1, Client2} = {Sender, Receiver};
	false ->
	    {Client1, Client2} = {Receiver, Sender}
    end,
    string:join(["chat_session",
		 atom_to_list(Client1),
		 atom_to_list(Client2)], "_").

%%--------------------------------------------------------------------
%% @doc
%% This function returns the pid of the chat session specified as the
%% Session_Name argument (if any).
%%
%% @spec chat_session_name(Sender, Receiver) ->
%%          Pid | []
%% @end
%%--------------------------------------------------------------------
retrieve(Session_Name, Chat_Pids) ->
    case lists:keyfind(Session_Name, 1, Chat_Pids) of
	false ->
	    [];
	{Chat_Session_Name, Chat_Session_Pid} ->
	    element(2,{Chat_Session_Name, Chat_Session_Pid})
    end.

