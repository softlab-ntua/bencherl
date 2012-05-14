-module(orbit_dist_bench).

-export([bench_args/0, run/3]).

bench_args() ->
	[[F,G,N,W] || F <- [dist, dist_seq], G <- [fun bench:g124/1], N <- [10000], W <- [8]].

run([F,G,N,W|_], Nodes, _) ->
	io:format("~p~n", [apply(bench, F, [G,N,W,Nodes])]).

