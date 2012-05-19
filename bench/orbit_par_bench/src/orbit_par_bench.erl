-module(orbit_par_bench).

-export([bench_args/0, run/3]).

bench_args() ->
	[[F,G,N,W] || F <- [par, par_seq], G <- [fun bench:g124/1], N <- [10000], W <- [8]].

run([F,G,N,W|_], _, _) ->
	io:format("~p~n", [apply(bench, F, [G,N,W])]).
