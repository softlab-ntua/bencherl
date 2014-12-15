%%%--------------------------------------------------------------------
%%% TOXIC CLIENT MODULE (Lite Version)
%%%
%%% @author: Mario Moro Hernandez upon a design by Natalia Chechina
%%% @copyright (C) 2014, RELEASE project
%%% @doc
%%%	Toxic Client module for the Distributed Erlang instant messenger
%%%     (IM) application developed as a real benchmark for the Scalable 
%%%	Distributed Erlang extension of the Erlang/OTP language.
%%%
%%%	This module implementes the traffic generation logic to load a
%%%	system similar to the system described in the Section 2 of the
%%%	document "Instant Messenger Architectures Design Proposal".
%%% @end 
%%% Created: 31 Jul 2014 by Mario Moro Hernandez
%%%--------------------------------------------------------------------

-module(toxic_client_lite).
-export([launch/2, launch_traffic/2]).

-export_type([info/0]).

-define(COORDINATOR, coordinator).
-define(CLIENTS_INFO, clients_info_proc).

-type info() :: dict:dict(node(), dict:dict(string(), pid())).

%%%===================================================================
%%% LAUNCH CLIENTS
%%%===================================================================

%%----------------------------------------------------------------------------
%% @doc
%%     Distributes a number of clients at each client node. This function
%%     assumes that the list Nodes contains only the client nodes.
%%
%% @spec launch(NumOfClientsInNode, Nodes) -> ok
%%          when NumOfClientsInNode :: integer(),
%%               Nodes :: [node()].
%% @end
%%----------------------------------------------------------------------------
-spec launch(integer(), [node()]) -> ok.
launch(_, []) -> ok;
launch(NumOfClientsInNode, [Node|Nodes]) ->
    NumOfNode = extract_node_number_from_name(Node),
    spawn(Node, fun() -> launch_node(NumOfClientsInNode, NumOfNode) end),
    launch(NumOfClientsInNode, Nodes).

%%----------------------------------------------------------------------------
%% @doc
%%     Triggers the logic to set up the clients that will run on an specific
%%     node.
%%
%% @spec launch_node(NumOfClients, NumOfNode) -> ok
%%          when NumOfClients :: integer(),
%%               NumOfNode :: integer().
%% @end
%%----------------------------------------------------------------------------
launch_node(NumOfClients, NumOfNode) ->
    io:format("Setting up the clients at node ~p~n", [node()]),
    setup_clients(NumOfClients, NumOfNode).

%%----------------------------------------------------------------------------
%% @doc
%%     Spawns the clients and logs them in to the system.
%%
%% @spec setup_clients(NumOfClients, NumOfNode) -> ok
%%          when NumOfClients :: integer(),
%%               NumOfNode :: integer().
%% @end
%%----------------------------------------------------------------------------
setup_clients(NumOfClients, NumOfNode)->
    random_seed(),
    F = fun(N) ->
            ClientName = generate_client_name(N, NumOfNode),
            Seed = [random:uniform(100000) || _ <- lists:seq(1, 3)],
            start_and_login_client(ClientName, Seed)
        end,
    lists:foreach(F, lists:seq(1, NumOfClients)).

%%----------------------------------------------------------------------------
%% @doc
%%     Starts a client by spawing and a client process. Then it logs it in to
%%     the system.
%%
%% @spec start_and_login_client(ClientName, Seed) -> pid()
%%          when ClientName :: string(),
%%               Seed = [integer()].
%% @end
%%----------------------------------------------------------------------------
start_and_login_client(ClientName, Seed) ->
    F = fun() ->
            global:whereis_name(routers_db) ! {retrieve_routers, self()},
            receive
                {RoutersList, router_list} ->
                    random_seed(Seed),  % Initial Seed for the client
                    RouterPid = pick_router(RoutersList),
                    RouterPid ! {ClientName, self(), login},
                    client(ClientName, RoutersList);
                Other ->
                    io:format("Unable to start the client ~p at node ~p.~n"
                        "  No routers list received.~n"
                        "  Received ~p~n.", [ClientName, node(), Other])
            end
        end,
    spawn(F).

%%----------------------------------------------------------------------------
%% @doc
%%     Runs he client process when the client is not logged in yet. When the
%%     client_monitor Pid has been received, it changes the state and changes
%%     to the full logged in client.
%%
%% @spec client(ClientName, RoutersList) -> ok
%%          when ClientName :: string(),
%%               RoutersList :: [{string(), pid()}].
%% @end
%%----------------------------------------------------------------------------
client(ClientName, RoutersList) ->
    receive
        {login_success, ClientMonitorPid} ->
            case global:whereis_name(?COORDINATOR) of
                undefined ->
                    io:format("[ERROR] Undefined coordinator!~n");
                CoordPid ->
                    CoordPid ! {client_setup_ok, node(), ClientName, self()},
                    client(ClientName, ClientMonitorPid, [], RoutersList)
            end;
        {error, client_already_logged_in} ->
            io:format("[ERROR]: Client Already logged in!~n");
        Other ->
            io:format("Unexpected message at client ~p at node ~p~n"
                "  Received ~p~n", [ClientName, node(), Other]),
            client(ClientName, RoutersList)
    end.

%%----------------------------------------------------------------------------
%% @doc
%%     The actual client process, i.e. the fully logged in client.
%%
%% @spec client(ClientName, ClientMonitorPid, ChatPids, RoutersList) -> ok
%%          when ClientName :: string(),
%%               ClientMonitorPid :: pid(),
%%               ChatPids :: [{string(), pid()}],
%%               RoutersList :: [{string(), pid()}].
%% @end
%%----------------------------------------------------------------------------
client(ClientName, ClientMonitorPid, ChatPids, RoutersList) ->
    receive
        %% Chat Logic
        {Sender, Receiver, Message, send_message} ->
            SessionName = chat_session_name(Sender, Receiver),
            case retrieve(SessionName, ChatPids) of
                [] ->
                    RouterPid = pick_router(RoutersList),
                    _ = case rpc:pinfo(RouterPid) of
                        undefined ->
                            global:whereis_name(routers_db) ! {retrieve_routers, self()},
                            self() ! {Sender, Receiver, Message, send_message},
                            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
                        _ ->
                            RouterPid ! {Sender, Receiver, start_chat_session},
                            client(ClientName, ClientMonitorPid, ChatPids, RoutersList),
                            self() ! {Sender, Receiver, Message, send_message}
                    end,
                    ok;
                ChatSessionPid ->
                    UniqueID = integer_to_list(erlang:crc32(SessionName)) ++ integer_to_list(sys_time(os:timestamp())),
                    ChatSessionPid ! {UniqueID, Sender, Receiver, os:timestamp(), Message},
                    case Message == finish_chat_session of
                        true  -> ok;
                        false ->
                            Metadata = {UniqueID, SessionName, Sender, Receiver, os:timestamp()},
                            notify_logger(s, Metadata, not_delivered)
                    end,
                    client(ClientName, ClientMonitorPid, ChatPids, RoutersList)
            end;
        %%
        {NewRoutersList, router_list} ->
            client(ClientName, ClientMonitorPid, ChatPids, NewRoutersList);
        %%
        {chat_session_success_sender, SessionName, ChatSessionPid} ->
            NewChatPids = [{SessionName, ChatSessionPid} | ChatPids],
            client(ClientName, ClientMonitorPid, NewChatPids, RoutersList);
        %%
        {receiver_not_found, Receiver} ->
            io:format("[ERROR] Client ~p is not logged in!~n", [Receiver]),
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        %%
        {chat_session_success_receiver, SessionName, ChatSessionPid} ->
            NewChatPids = [{SessionName, ChatSessionPid} | ChatPids],
            ClientMonitorPid ! {add_chat_session, {SessionName, ChatSessionPid}},
            client(ClientName, ClientMonitorPid, NewChatPids, RoutersList);
        %%
        {Metadata, message_delivered_ok} ->
            T2 = os:timestamp(),
            {_, _, _, _, T1} = Metadata,
            Latency = timer:now_diff(T2, T1),
            notify_logger(d, Metadata, Latency),
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        %%
        {UniqueID, Sender, _Message, ChatSessionPid, Timestamp, receive_message} ->
            ChatSessionPid ! {Sender, UniqueID, Timestamp, message_delivered_ok},
            Metadata = {UniqueID, undefined, Sender, ClientName, os:timestamp()},
            notify_logger(r, Metadata, received),
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        %%
        {ChatSessionName, {'EXIT', ok}} ->
            NewChatPids = lists:keydelete(ChatSessionName, 1, ChatPids),
            ClientMonitorPid ! {remove_chat_session, ChatSessionName},
            client(ClientName, ClientMonitorPid, NewChatPids, RoutersList);
        %%
        {opened_chat_sessions, ClientMonitorRecoverPid} ->
            ClientMonitorRecoverPid ! {chat_sessions, ChatPids},
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        %%
        {new_monitor_pid, NewClientMonitorPid} ->
            client(ClientName, NewClientMonitorPid, ChatPids, RoutersList);
        %% Logout Logic
        {logout} ->
            ClientMonitorPid ! {ClientName, self(), logout},
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        %% Client Termination Logic
        {error, client_already_logged_in} ->
            io:format("You are logged in already.~n"),
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList);
        {'EXIT', ok} ->
            %% THIS NEEDS TO BE CHANGED unregister(Client_Name),
            io:format("~p is stopped now.~n", [ClientName]);
        %% Trap for unexpected messages
        Other ->
            io:format("Unexpected Message at client ~p at node ~p.~n"
                "  Received: ~p~n", [ClientName, node(), Other]),
            client(ClientName, ClientMonitorPid, ChatPids, RoutersList)
    end.

%%%===================================================================
%%% LAUNCH TRAFFIC GENERATORS
%%%===================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%     Triggers the traffic generation. It spawns traffic generator processes
%%     half as many as the client processes in each client node. This function
%%     assumes that the list Nodes contains only the client nodes.
%%
%% @spec launch_traffic(Nodes, Info) -> ok
%%          when Nodes :: [node()],
%%               Info :: info().
%% @end
%%-----------------------------------------------------------------------------
-spec launch_traffic([node()], info()) -> ok.
launch_traffic(Nodes, Info) ->
    F = fun(Node) ->
            io:format("Launching traffic generators at node ~p~n", [Node]),
            Clients = dict:fetch(Node, Info),
            NumOfGenerators = dict:size(Clients) div 2,
            spawn(Node, fun() ->
                            start_intranode_traffic(NumOfGenerators, Clients)
                        end)
        end,
    lists:foreach(F, Nodes).

%%-----------------------------------------------------------------------------
%% @doc
%%     Spawns the traffic generators and the process that will own the ets
%%     table with all the client processes' names and their pids.
%%
%% @spec start_intranode_traffic(NumOfGenerators, Clients) -> ok
%%          when NumOfGenerators :: integer(),
%%               Clients :: info().
%% @end
%%-----------------------------------------------------------------------------
start_intranode_traffic(NumOfGenerators, Clients) ->
    Me = self(),
    CInfo = spawn(fun() -> clients_info(Me, Clients) end),
    receive
        {CInfo, info_ready, Tbl} ->
            random_seed(),
            Sz = dict:size(Clients),
            F = fun(Seed) ->
                    spawn(fun() ->
                            random_seed(Seed),
                            traffic_generator(Tbl, Sz)
                          end)
                end,
            lists:foreach(F, [ [random:uniform(100000) || _ <- lists:seq(1, 3)]
                                || _ <- lists:seq(1, NumOfGenerators)])
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%     The process that owns the ets table with the names and pids of all the
%%     client processes in the node. The table is optimized for concurrent
%%     reads.
%%
%% @spec clients_info(Parent, Clients) -> true.
%%          when Parent :: pid(),
%%               Clients :: info().
%% @end
%%-----------------------------------------------------------------------------
clients_info(Parent, Clients) ->
    register(?CLIENTS_INFO, self()),
    Tbl = ets:new(?CLIENTS_INFO,
                  [set, protected, {read_concurrency, true}]),
    L = dict:to_list(Clients),
    Objs = lists:zip(lists:seq(1, length(L)), L),
    ets:insert(Tbl, Objs),
    Parent ! {self(), info_ready, Tbl},
    receive stop -> ets:delete(Tbl) end.

%%-----------------------------------------------------------------------------
%% @doc
%%     Chooses two random clients in the node. Then, it generates random
%%     strings of variable length (between 1 and 255 characters) and tells the
%%     clients to send them from one to the other.
%%
%% @spec traffic_generator(Tbl, NumOfClients) -> ok
%%          when Tbl :: ets:tid(),
%%               NumOfClients :: integer().
%% @end
%%-----------------------------------------------------------------------------
traffic_generator(Tbl, NumOfClients) ->
    receive
        stop -> ok
    after 0 ->
        {Sender, Receiver} = pick_random_clients(Tbl, NumOfClients),
        Interactions = random:uniform(48) + 12,
        generate_interactions(Sender, Receiver, Interactions),
        traffic_generator(Tbl, NumOfClients)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%     Returns a tuple containing the information of two clients randomly
%%     chosen in the node.
%%
%% @spec pick_random_clients(Tbl, NumOfClients) -> {ClientInfo, ClientInfo}
%%          when Tbl :: ets:tid(),
%%               NumOfClients :: integer(),
%%               ClientInfo :: {string(), pid()}.
%% @end
%%-----------------------------------------------------------------------------
pick_random_clients(Tbl, NumOfClients) ->
    {N, SenderInfo} = pick_random_sender(Tbl, NumOfClients),
    ReceiverInfo = pick_random_receiver(Tbl, NumOfClients, N),
    {SenderInfo, ReceiverInfo}.

%%-----------------------------------------------------------------------------
%% @doc
%%     Select a random client in the node.
%%
%% @spec pick_random_sender(Tbl, NumOfClients) -> {integer(), ClientInfo}
%%          when Tbl :: ets:tid(),
%%               NumOfClients :: integer(),
%%               ClientInfo :: {string(), pid()}.
%% @end
%%-----------------------------------------------------------------------------
pick_random_sender(Tbl, NumOfClients) ->
    N = random:uniform(NumOfClients),
    [{N, ClientInfo}] = ets:lookup(Tbl, N),
    {N, ClientInfo}.

%%-----------------------------------------------------------------------------
%% @doc
%%     Select a random client in the node that is not the sender.
%%
%% @spec pick_random_receiver(Tbl, NumOfClients, N) -> ClientInfo
%%          when Tbl :: ets:tid(),
%%               NumOfClients :: integer(),
%%               N :: integer(),
%%               ClientInfo :: {string(), pid()}.
%% @end
%%-----------------------------------------------------------------------------
pick_random_receiver(Tbl, NumOfClients, N0) ->
    case random:uniform(NumOfClients) of
        N0 ->
            pick_random_receiver(Tbl, NumOfClients, N0);
        N1 ->
            [{N1, ClientInfo}] = ets:lookup(Tbl, N1),
            ClientInfo
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%     Simulates a conversation between the two clients specified as Sender
%%     and Receiver.
%%
%% @spec generate_interactions(Sender, Receiver, Interactions) -> ok
%%          when Sender :: {string(), pid()},
%%               Receiver :: {string(), pid()},
%%               Interactions :: integer().
%% @end
%%-----------------------------------------------------------------------------
generate_interactions({SndName, SndPid}, {RcvName, _}, 0) ->
    SndPid ! {SndName, RcvName, finish_chat_session, send_message},
    ok;
generate_interactions({SndName, SndPid}=Snd, {RcvName, _}=Rcv, Interactions) ->
    timer:sleep(random:uniform(20) * 1000),
    Message = [random:uniform(26) + 96 || _ <- lists:seq(1, random:uniform(75))],
    SndPid ! {SndName, RcvName, Message, send_message},
    generate_interactions(Snd, Rcv, Interactions - 1).

%%%===================================================================
%%% AUXILIARY FUNCTIONS
%%%===================================================================

%%-----------------------------------------------------------------------------
%% @doc
%%     Returns the cardinality of the client node. (Assume that the name of the
%%     node is in the format 'client_N@domain').
%%
%% @spec extract_node_number_from_name(Node) -> integer()
%%          when Node :: node().
%% @end
%%-----------------------------------------------------------------------------
extract_node_number_from_name(Node) ->
    [NodeName|_] = string:tokens(atom_to_list(Node), "@"),
    case string:sub_word(NodeName, 2, $_) of
        []  -> list_to_integer(string:sub_word(NodeName, 2, $t));
        Val -> list_to_integer(Val)
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%     Build the name of the client.
%%
%% @spec generate_client_name(N, NumOfNode) -> string()
%%          when N :: integer(),
%%               NumOfNode :: integer().
%% @end
%%-----------------------------------------------------------------------------
generate_client_name(N, NumOfNode) ->
    TC = string:concat("tc", integer_to_list(N)),
    string:join([TC, integer_to_list(NumOfNode)], "_").

%%-----------------------------------------------------------------------------
%% @doc
%%     Seeds random number generation.
%%
%% @spec random_seed() -> undefined.
%% @end
%%-----------------------------------------------------------------------------
random_seed() ->
    {A1, A2, A3} = os:timestamp(),
    %% ensure this is the 1st time that random:seed/3 is called.
    undefined = random:seed(A1, A2, A3).

%%-----------------------------------------------------------------------------
%% @doc
%%     Seeds random number generation.
%%
%% @spec random_seed(Seed) -> undefined
%%          when Seed :: [integer()].
%% @end
%%-----------------------------------------------------------------------------
random_seed([A1, A2, A3]) ->
    %% ensure this is the 1st time that random:seed/3 is called.
    undefined = random:seed(A1, A2, A3).

%%-----------------------------------------------------------------------------
%% @doc
%%     Returns the pid of a randomly selected router from a list of routers.
%%
%% @spec pick_router(RoutersList) -> pid()
%%          when RoutersList :: [{string(), pid()}].
%% @end
%%-----------------------------------------------------------------------------
pick_router(RoutersList) ->
    Router = lists:nth(random:uniform(length(RoutersList)), RoutersList),
    {_, RouterPid} = Router,
    RouterPid.

%%-----------------------------------------------------------------------------
%% @doc
%%     Returns the name of a chat session, built from the names of the
%%     participating clients.
%%
%% @spec chat_session_name(Sender, Receiver) -> string()
%%          when Sender :: [{string(), pid()}],
%%               Receiver :: [{string(), pid()}].
%% @end
%%-----------------------------------------------------------------------------
chat_session_name(Sender, Receiver) ->
    {Client1, Client2} =
        case Sender < Receiver of
            true  -> {Sender, Receiver};
            false -> {Receiver, Sender}
        end,
    string:join(["chat_session", Client1, Client2], "_").

%%-----------------------------------------------------------------------------
%% @doc
%%     Abstracts the action of notifying the logger when a message has been
%%     sent, received or aknowledged.
%%
%% @spec notify_logger(Action, Metadata, Status) -> ok.
%% @end
%%-----------------------------------------------------------------------------
notify_logger(Action, Metadata, Status) ->
    _ = case whereis(throughput_logger) of
        undefined -> ok;
        ThroughputLoggerPid -> ThroughputLoggerPid ! {Action, Metadata, Status}
    end,
    _ = case whereis(latency_logger) of
        undefined -> ok;
        LatencyLoggerPid -> LatencyLoggerPid ! {Action, Metadata, Status}
    end,
    ok.

%%-----------------------------------------------------------------------------
%% @doc
%%     Retrieves a chat session pid from a clients' sessions' registry.
%%
%% @spec retrieve(SessionName, ChatPids) -> pid() | []
%%          when SessionName :: string(),
%%               ChatPids :: [{string(), pid()}].
%% @end
%%-----------------------------------------------------------------------------
retrieve(SessionName, ChatPids) ->
    case lists:keyfind(SessionName, 1, ChatPids) of
        false -> [];
        {_ChatSessionName, ChatSessionPid} -> ChatSessionPid
    end.

%%-----------------------------------------------------------------------------
%% @doc
%%     Returns the timestamp as an integer.
%%
%% @spec sys_time({integer(), integer(), integer()}) -> integer().
%% @end
%%-----------------------------------------------------------------------------
sys_time({A1, A2, A3}) ->
    ((A1 * 1000000) + A2) * 1000000 + A3.
