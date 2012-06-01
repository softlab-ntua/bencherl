-module(scalaris_bench).

-include_lib("kernel/include/inet.hrl").

-export([bench_args/1, run/3]).

bench_args(short) ->
	[[T,I,V] || T <- [64], I <- [50], V <- [31]];
bench_args(intermediate) ->
    [[T,I,V] || T <- [64], I <- [500], V <- [31]];
bench_args(long) ->
    [[T,I,V] || T <- [64], I <- [1000], V <- [31]].

run([T,I,V|_], _, _) ->
	{ok, N} = inet:gethostname(),
	{ok, #hostent{h_name=H}}=inet:gethostbyname(N),
	Node = "firstnode@" ++ H,
	rpc:block_call(list_to_atom(Node), api_vm, add_nodes, [V]),
	io:format("~p~n", [rpc:block_call(list_to_atom(Node), bench, quorum_read, [T,I])]),
	ok.

