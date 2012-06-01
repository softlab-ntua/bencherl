%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

%% Author  : Björn-Egil Dahlberg
%% Created : 17 Dec 2009 by Björn-Egil Dahlberg

-module(netio).

-include_lib("kernel/include/inet.hrl").

-export([bench_args/1, run/3]).

-record(state, {
	protocol = tcp,
	port = 8888,
	nodelay = true,
	delay_send = false,
	backlog = 64,
	producers = 4,
	connections = 100,
	packet_size = 2000,
	packets = 100,
	ack = true,
	%% internals
	host,
	ctrl_pid
	}).

bench_args(short) ->
	[[	[], 
		[	{connections, Nc}, 
			{packet_size, Psz}, 
			{ack, Ack}, 
			{packets, Np}]	] || 	Nc <- [200], 
									Psz <- [900], 
									Np <- [1000],
									Ack <- [false]
	];
bench_args(intermediate) ->
    [[  [],
        [   {connections, Nc},
            {packet_size, Psz},
            {ack, Ack},
            {packets, Np}]  ] ||    Nc <- [200],
                                    Psz <- [10000],
                                    Np <- [1000],
                                    Ack <- [false]
    ];
bench_args(long) ->
    [[  [],
        [   {connections, Nc},
            {packet_size, Psz},
            {ack, Ack},
            {packets, Np}]  ] ||    Nc <- [200],
                                    Psz <- [10000],
                                    Np <- [3000],
                                    Ack <- [false]
    ].

run([Copts, Popts|_], _, _) ->
	Pid = consumer(Copts),
	producer(Popts),
	unlink(Pid),
	exit(Pid, kill),
	ok.

consumer(Opts) ->
	S = #state{	port       = proplists:get_value(port, Opts, 8888),
				backlog    = proplists:get_value(backlog, Opts, 64),
				delay_send = proplists:get_value(delay_send, Opts, false),
				nodelay    = proplists:get_value(nodelay, Opts, true)	},
	spawn_link(fun() -> consumer_setup(S) end).

consumer_setup(S) ->
	{ok, Listen} = gen_tcp:listen(S#state.port, [
		binary, 
		{packet, 0}, 
		{active, false}, 
		{reuseaddr, true}, 
		{backlog, S#state.backlog}, 
		{nodelay, S#state.nodelay}, 	
		{delay_send, S#state.delay_send}
	]),
	consumer_accept_loop(Listen).

consumer_accept_loop(Listen) ->
	case gen_tcp:accept(Listen) of 
		{ok, Socket} -> 
			_Pid = spawn_link(fun() -> consumer_handler(Socket) end),
			consumer_accept_loop(Listen)
	end.

consumer_handler(Socket) ->
	ok = gen_tcp:send(Socket, <<"connection confirm">>), 
	%% get config
	{ok, Binary} = gen_tcp:recv(Socket, 0),
	{config, Configs} = erlang:binary_to_term(Binary),
	Ack      = proplists:get_value(ack,         Configs),
	Packets  = proplists:get_value(packets,     Configs),
	PacketSz = proplists:get_value(packet_size, Configs),
	ok       = gen_tcp:send(Socket, <<"config confirm">>),
	consumer_handler(Socket, Ack, Packets, PacketSz).

consumer_handler(Socket, _, 0, _) ->
	ok              = gen_tcp:send(Socket, <<"ack packets">>),
	{error, closed} = gen_tcp:recv(Socket, 0),
	ok;
consumer_handler(Socket, true, N, PacketSz) ->
	{ok, B} = gen_tcp:recv(Socket, PacketSz),
	<<N:32,_/binary>> = B,
	ok = gen_tcp:send(Socket, <<"ack">>),
	consumer_handler(Socket, true, N - 1, PacketSz);
consumer_handler(Socket, false, N, PacketSz) ->
	{ok, B} = gen_tcp:recv(Socket, PacketSz),
	<<N:32,_/binary>> = B,
	consumer_handler(Socket, false, N - 1, PacketSz).

%% producer

producer(Opts) ->
	Hostname = proplists:get_value(hostname, Opts, "localhost"),
	Host     = producer_gethostbyname(Hostname),
	producer_setup(#state{
		ctrl_pid = self(),
		host = Host,
		connections = proplists:get_value(connections, Opts, 1000),
		producers   = proplists:get_value(producers, Opts, 4),
		packet_size = proplists:get_value(packet_size, Opts, 300),
		packets     = proplists:get_value(packets, Opts, 4000),
		ack         = proplists:get_value(ack, Opts, true),
		nodelay     = proplists:get_value(nodelay, Opts, false),
		delay_send  = proplists:get_value(delay_send, Opts, false)
    }).

producer_setup(S) ->
	%% spawn producers
	Pids = [spawn_link(fun() -> producer_init(S) end) || _ <- lists:seq(1, S#state.producers)],
	%% receive finishers
	[receive {Pid, done} -> ok end || Pid <- Pids],
	ok.
 
producer_init(S) -> producer_loop(S, S#state.connections).

producer_loop(S, 0) ->  S#state.ctrl_pid ! {self(), done};
producer_loop(S, Nc) ->
	Socket  = producer_connect(S),
	Payload = binary(S#state.packet_size - 4), 
	producer_send(S, Socket, S#state.ack, S#state.packets, Payload),
	{ok, <<"ack packets">>} = gen_tcp:recv(Socket, 11),
	ok = gen_tcp:close(Socket),
	producer_loop(S, Nc - 1).
	
producer_connect(State) ->
	{ok, Socket} = gen_tcp:connect(State#state.host, State#state.port, [
		binary, 
		{packet,     0}, 
		{reuseaddr,  true}, 
		{active,     false}, 
		{nodelay,    State#state.nodelay},
		{delay_send, State#state.delay_send}
    ]),
	%% handshake
	{ok, <<"connection confirm">>} = gen_tcp:recv(Socket, 18),
	ok = gen_tcp:send(Socket, erlang:term_to_binary({config, [{ack, State#state.ack}, {packets, State#state.packets},{packet_size, State#state.packet_size}]})),
	{ok, <<"config confirm">>} = gen_tcp:recv(Socket, 14),
	Socket.

producer_send(_S, _Socket, _, 0, _) -> ok;
producer_send(S, Socket, true, N, Payload) ->
	Tag = <<N:32>>,
	ok = gen_tcp:send(Socket, [Tag, Payload]),
	{ok, <<"ack">>} = gen_tcp:recv(Socket, 3),
	producer_send(S, Socket, true, N - 1, Payload);
producer_send(S, Socket, false, N, Payload) ->
	Tag = <<N:32>>,
	ok = gen_tcp:send(Socket, [Tag, Payload]),
	producer_send(S, Socket, false, N - 1, Payload).

producer_gethostbyname(Host) ->
	{ok, Hostent} = inet:gethostbyname(Host),
	[Ip | _ ] = Hostent#hostent.h_addr_list, 
	Ip.

binary(N) ->
	erlang:list_to_binary(lists:duplicate(N, 77)).

