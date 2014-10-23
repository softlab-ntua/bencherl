%%%--------------------------------------------------------------------
%%% CHAT_DB MODULE
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Chat_DB module for the Distributed Erlang instant messenger (IM)
%%%	application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the functionality for the database that
%%%	stores the chat sessions happening at a given time in a system
%%%     similar to the system described in the Section 2 of the document
%%%     "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 1 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------

-module(chat_db).
-export([start/1, stop/1, chat_db/1]).


%%%====================================================================
%%% API
%%%====================================================================

%%---------------------------------------------------------------------
%% @doc
%% Starts the chat sessions database.
%%
%% @spec start(DB_Name)
%% @end
%%---------------------------------------------------------------------
start(DB_Name) ->
    global:register_name(DB_Name, spawn(fun() -> chat_db(DB_Name) end)).

%%--------------------------------------------------------------------
%% @doc
%% Stops the chat sessions database.
%%
%% @spec stop(DB_Name)
%% @end
%%--------------------------------------------------------------------
stop(DB_Name) ->
    destroy(DB_Name),
    global:unregister_name(DB_Name).

%%--------------------------------------------------------------------
%% @doc
%% chat_db is first stage of the database process. It creates an ets
%% table after the atom specified as the argument DB_Name.
%%
%% @spec chat_db(DB_Name)
%% @end
%%--------------------------------------------------------------------
chat_db(DB_Name) ->
    case ets:info(DB_Name) of
	undefined ->
	    create(DB_Name),
	    chat_db_loop(DB_Name);
	_ ->
	    chat_db_loop(DB_Name)
    end.

%%--------------------------------------------------------------------
%% @doc
%% chat_db_loop constitutes the database process, and offers an inter-
%% face to interact with the ets table, allowing the input or retrieval
%% of information concerning the chat sessions in the system. 
%%
%% @spec chat_db_local(DB_Name)
%% @end
%%--------------------------------------------------------------------
chat_db_loop(DB_Name) ->
    receive
    	{From, add, Session_Name, Session_Pid, C1, C1_Pid, C2, C2_Pid} -> 
	    add_session(DB_Name, Session_Name, Session_Pid, C1, C1_Pid, C2, C2_Pid),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {session_added, ok}
	    end,
	    chat_db_loop(DB_Name);
	{From, remove, Session_Name} ->
	    remove_session(DB_Name, Session_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {session_removed, ok}
	    end,
	    chat_db_loop(DB_Name);
	{From, full_db} ->
	    A = retrieve_db(DB_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! A
	    end,
	    chat_db_loop(DB_Name);
	{From, peak, Session_Name} ->
	    S = peak_session(DB_Name, Session_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! S
	    end,
	    chat_db_loop(DB_Name);
	{From, peak_by_pid, Session_Pid} ->
	    S = peak_session_pid(DB_Name, Session_Pid),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! S
	    end,
	    chat_db_loop(DB_Name);
	{From, session_pid, Session_Name} ->
	    Pid = session_pid(DB_Name, Session_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! Pid
	    end,
	    chat_db_loop(DB_Name);
	{From, opened_sessions, Client} ->
	    S = client_sessions(DB_Name, Client),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! S
	    end,
	    chat_db_loop(DB_Name);
	{From, recover, Target_DB_Name, Source_DB_Name} ->
	    recover_db(Target_DB_Name, Source_DB_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {recovered, ok}
	    end,
	    chat_db_loop(DB_Name);
	{From, stop} ->
	    stop(DB_Name),
	    case From of
		undefined ->
		    ok;
		_ ->
		    From ! {chat_session_db_destroyed, ok}
	    end;
	Other ->
		io:format("Something failed at Chat_DB. Received: ~p", [Other]),
		chat_db_loop(DB_Name)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Creates a new ets table -named Table_Name- to store the different
%% chat sessions active in the system.
%%
%% @spec create(Table_Name) -> Table_Name | {error, Error}
%% @end
%%---------------------------------------------------------------------
create(Table_Name) ->
    ets:new(Table_Name, [set, named_table]).

%%---------------------------------------------------------------------
%% @doc
%% Destroys ets table named Table_Name.
%%
%% @spec destroy(Table_Name) -> Table_Name | {error, Error}
%% @end
%%---------------------------------------------------------------------
destroy(Table_Name) ->
    ets:delete(Table_Name).

%%---------------------------------------------------------------------
%% @doc
%% Adds a chat session to the chat sessions database.
%%
%% @spec add_session(Table_Name, Chat_Session_Pid,
%%   	 	     Client_A, Client_B) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
add_session(Table_Name, Chat_Session_Name, Chat_Session_Pid, Client_A, Client_A_Pid, Client_B, Client_B_Pid) ->
    ets:insert(Table_Name, {Chat_Session_Name, Chat_Session_Pid, Client_A, Client_A_Pid, Client_B, Client_B_Pid}).

%%---------------------------------------------------------------------
%% @doc
%% Removes a chat session from the chat sessions database.
%%
%% @spec remove_session(Table_Name, Chat_Session_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
remove_session(Table_Name, Chat_Session_Name) ->
    ets:delete(Table_Name, Chat_Session_Name).

%%---------------------------------------------------------------------
%% @doc
%% Returns the contents of the whole database as a list of lists, where
%% each of the nested lists is one session. (Not in use, only for debug
%% purposes).
%%
%% @spec retrieve_db(Table_Name) ->
%%                  [[session_1], ..., [session_n]] | {error, Error}
%% @end
%%---------------------------------------------------------------------
retrieve_db(Table_Name) ->
    ets:match(Table_Name, {'$0', '$1', '$2', '$3', '$4', '$5'}).

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing the data of the session passed as argument.
%%
%% @spec peak_session(Table_Name, Session_Name) ->
%%         [Session_Name, Session_Pid,
%%          Client_A, Client_A_Pid, ClientB, Client_B_Pid] | {error, Error}
%% @end
%%---------------------------------------------------------------------
peak_session(Table_Name, Session_Name) ->
    L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$0', Session_Name}],
					 [['$0', '$1', '$2', '$3', '$4', '$5']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing the data of the session corresponding to
%% the pid passed as argument.
%%
%% @spec peak_session(Table_Name, Session_Pid) ->
%%          [Session_Name, Session_Pid,
%%          Client_A, Client_A_Pid, ClientB, Client_B_Pid] | {error, Error}
%% @end
%%---------------------------------------------------------------------
peak_session_pid(Table_Name, Session_Pid) ->
     L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$1', Session_Pid}],
					 [['$0', '$1', '$2', '$3', '$4', '$5']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(L)
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns the Pid of a given session passed as argument.
%%
%% @spec session_pid(Table_Name, Session_Name) -> Session_Pid | []
%% @end
%%---------------------------------------------------------------------
session_pid(Table_Name, Session_Name) ->
    L = ets:select(Table_Name, [{{'$0', '$1', '$2', '$3', '$4', '$5'},
					 [{'==', '$0', Session_Name}],
					 [['$1']]}]),
    case L == [] of
	true ->
	    L;
	false ->
	    lists:last(lists:last(L))
    end.

%%---------------------------------------------------------------------
%% @doc
%% Returns a list containing all the sessions -and the information of
%% these sessions- in which the client passed as the argument is one of
%% participants. It returns an empty list if the client is not involved
%% in any chat at the time of invocation.
%%
%% @spec retrieve_server(Table_Name, Session_Pid) ->
%%                  [Session_1, ..., Session_k]  | []
%% @end
%%---------------------------------------------------------------------
client_sessions(Table_Name, Client) ->
    ets:safe_fixtable(Table_Name, true),
    Sessions = opened_sessions(Table_Name, Client, ets:first(Table_Name), []),
    ets:safe_fixtable(Table_Name, false),
    Sessions.

%%---------------------------------------------------------------------
%% @doc
%% Replicates the chat session table specified in the Source argument,
%% in a new table with name Table_Name.
%%
%% @spec retrieve_server(Table_Name, Session_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
recover_db(Destination, Source) ->
    ets:safe_fixtable(Source, true),
    replicate(Source, Destination, ets:first(Source)),
    ets:safe_fixtable(Source, false).

%%---------------------------------------------------------------------
%% @doc
%% Auxiliary function to the function client_sessions(Table_Name, Client).
%% This function traverses the ets table containing all the conversations,
%% and puts in a list all those in which the client is one of the
%% participants.
%%
%% @spec opened_sessions(Table_Name, Client, Cursor, Sessions) ->
%%                  [Session_1, ..., Session_n] | []
%% @end
%%---------------------------------------------------------------------
opened_sessions(Table_Name, Client, Cursor, Sessions) -> 
    case Cursor of
	'$end_of_table' ->
	    Sessions;
	Cursor ->
	    case (element(3, lists:last(ets:lookup(Table_Name, Cursor))) == Client) or
		 (element(4, lists:last(ets:lookup(Table_Name, Cursor))) == Client) of
		true ->
		    opened_sessions(Table_Name, Client, ets:next(Table_Name, Cursor), 
				    (Sessions ++ [lists:last(ets:lookup(Table_Name, Cursor))]));
		false ->
		    opened_sessions(Table_Name, Client, ets:next(Table_Name, Cursor), Sessions)
	    end
    end.

%%---------------------------------------------------------------------
%% @doc
%% Auxiliary function to the function recover_db(Table_Name, Source). This
%% function traverses the source table specified in Source, and feeds the
%% data in the destination table.
%%
%% @spec retrieve_server(Table_Name, Session_Pid) -> true | {error, Error}
%% @end
%%---------------------------------------------------------------------
replicate(Source, Destination, Key) ->
    case Key of
	'$end_of_table' ->
	    true;
	_ ->
	    S = peak_session(Source, Key),
	    	    global:whereis_name(Destination) ! {undefined ,add,
	    	       lists:nth(1, S),
	    	       lists:nth(2, S),
	    	       lists:nth(3, S),
		       lists:nth(4, S),
		       lists:nth(5, S),
		       lists:nth(6, S)},
	    replicate(Source, Destination, ets:next(Source, Key))
    end.
