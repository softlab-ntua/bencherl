-module(orbit_int).

-export([bench_args/1, run/3]).

bench_args(short) ->
	[[IWP,G,N,W] || IWP <- [true,false], G <- [fun bench:g13/1], N <- [700], W <- [100]];
bench_args(intermediate) ->
    [[IWP,G,N,W] || IWP <- [true,false], G <- [fun bench:g124/1], N <- [10000], W <- [100]];
bench_args(long) ->
    [[IWP,G,N,W] || IWP <- [true,false], G <- [fun bench:g1245/1], N <- [10000], W <- [100]].


run([true,G,N,W|_], [], _) ->
	io:format("~p~n", [apply(bench, par, [G,N,W])]);

run([false,G,N,W|_], [], _) ->
    io:format("~p~n", [apply(bench, par_seq, [G,N,W])]);

run([true,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [apply(bench, dist, [G,N,W,Slaves])]);

run([false,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [apply(bench, dist_seq, [G,N,W,Slaves])]).

