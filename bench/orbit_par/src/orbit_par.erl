-module(orbit_par).

-export([bench_args/0, run/3]).

bench_args() ->
	[[F,G,N,P] || F <- [par, par_seq], G <- [fun bench:g124/1], N <- [10000], P <- [8]].

run([F,G,N,P|_], _, _) ->
	io:format("~p~n", [apply(bench, F, [G,N,P])]).
