-module(scalaris_bench).

-include_lib("kernel/include/inet.hrl").

-export([bench_args/0, run/3]).

bench_args() ->
	[[T,I,V] || T <- [10], I <- [500], V <- [1,2,4]].

run([T,I,V|_], _, _) ->
	{ok, N} = inet:gethostname(),
	{ok, #hostent{h_name=H}}=inet:gethostbyname(N),
	Node = "firstnode@" ++ H,
	rpc:block_call(list_to_atom(Node), api_vm, add_nodes, [V]),
	io:format("~p~n", [rpc:block_call(list_to_atom(Node), bench, increment, [T,I])]),
	ok.

