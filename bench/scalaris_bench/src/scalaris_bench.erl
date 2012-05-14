-module(scalaris_bench).

-include_lib("kernel/include/inet.hrl").

-export([bench_args/0, run/3]).

bench_args() ->
	[[T,I] || T <- [10], I <- [50000]].

run([T,I|_], _, _) ->
	{ok, N} = inet:gethostname(),
	{ok, #hostent{h_name=H}}=inet:gethostbyname(N),
	Node = "firstnode@" ++ H,
	io:format("~p~n", [rpc:block_call(list_to_atom(Node), bench, quorum_read, [T,I])]),
	ok.

