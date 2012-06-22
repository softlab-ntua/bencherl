% Spawns P producers and P consumers. Creates P queues on the RabbitMQ server.
% The i-th producer sends M messages to the i-th queue and the i-th consumer
% receives M messages from the same queue. Uses native Erlang messaging.

-module(rabbitmq_onetoone_direct).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([bench_args/1, run/3]).

bench_args(short) ->
	[[P,M] || P <- [10], M <- [10]];
bench_args(intermediate) ->
    [[P,M] || P <- [100], M <- [100]];
bench_args(long) ->
    [[P,M] || P <- [1000], M <- [1000]].

run([P,M|_], _, _) ->
        
	% Spawn the producers.
	Producers = spawn_producers(P, M),
	
	% Spawn the consumers.
	Consumers = spawn_consumers(P, M),
	
	% Wait for all the producers and the consumers to finish.
	Pids = lists:concat([Producers, Consumers]),
	wait_for_them(Pids),
	ok.

spawn_producers(N, M) ->
	Me = self(),
	lists:map(fun(I) -> spawn(fun() -> produce(Me, M, I) end) end, lists:seq(1, N)).

produce(Parent, M, I) ->
	% Connect to the RabbitMQ server.
	{ok, Host} = inet:gethostname(),
	Node = "rabbit@" ++ Host,
        {ok, Connection} = amqp_connection:start(#amqp_params_direct{node = list_to_atom(Node)}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	% Create a new queue.
	Queue = list_to_binary("queue" ++ integer_to_list(I)),
	amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),
	% Send M messages to it (to the default exchange).
	Payload = list_to_binary("ping"),	
	lists:foreach(fun(_) -> amqp_channel:cast(Channel, #'basic.publish'{exchange = <<"">>,routing_key = Queue},#amqp_msg{payload = Payload}) end, lists:seq(1, M)),
	% Close the connection to the RabbitMQ server.
	ok = amqp_channel:close(Channel),
	ok = amqp_connection:close(Connection),
	Parent ! {done, self()}.

spawn_consumers(N, M) ->
	Me = self(),
	lists:map(fun(I) -> spawn(fun() -> consume(Me, M, I) end) end, lists:seq(1, N)).

consume(Parent, M, I) ->
	% Connect to the RabbitMQ server.	
	{ok, Host} = inet:gethostname(),
	Node = "rabbit@" ++ Host,
	{ok, Connection} = amqp_connection:start(#amqp_params_direct{node = list_to_atom(Node)}),
	{ok, Channel} = amqp_connection:open_channel(Connection),
	% Make sure the corresponding queue exists.
	Queue = list_to_binary("queue" ++ integer_to_list(I)),
	amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),
	% Subscribe to it.
	amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue, no_ack = true}, self()),
	receive
		#'basic.consume_ok'{} -> ok
	end,	
	% Receive M messages.
	lists:foreach(fun(_) -> receive {#'basic.deliver'{}, #amqp_msg{payload = _}} -> ok end end, lists:seq(1, M)),
	% Close the connection to the RabbitMQ server.
	ok = amqp_channel:close(Channel),
	ok = amqp_connection:close(Connection),
	Parent ! {done, self()}.

wait_for_them([]) ->
	ok;
wait_for_them(Pids) ->
	receive {done, Pid} ->
		wait_for_them(lists:subtract(Pids, [Pid]))
	end.


