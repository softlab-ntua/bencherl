-module(orbit_seq).

-export([bench_args/0, run/3]).

bench_args() ->
	[[G,N] || G <- [fun bench:g124/1], N <- [10000]].

run([G,N|_], _, _) ->
	io:format("~p~n", [bench:seq(G,N)]),
	ok.

