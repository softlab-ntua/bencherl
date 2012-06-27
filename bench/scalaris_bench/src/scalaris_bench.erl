-module(scalaris_bench).

-include_lib("kernel/include/inet.hrl").

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[F1, F2, F3] = case Version of
		short -> [1, 1, 0.5];
		intermediate -> [1, 8, 0.5];
		long -> [1, 16, 0.5]
	end,
	[[T,I,V] || T <- [F1 * Cores], I <- [F2 * Cores], V <- [trunc(F3 * Cores)]].

run([T,I,V|_], _, _) ->
	{ok, N} = inet:gethostname(),
	{ok, #hostent{h_name=H}}=inet:gethostbyname(N),
	Node = "firstnode@" ++ H,
	rpc:block_call(list_to_atom(Node), api_vm, add_nodes, [V]),
	io:format("~p~n", [rpc:block_call(list_to_atom(Node), bench, quorum_read, [T,I])]),
	ok.

