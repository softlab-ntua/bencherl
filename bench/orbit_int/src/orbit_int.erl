-module(orbit_int).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
	{_,Schedulers} = lists:keyfind(number_of_schedulers, 1, Conf),
	[F1, F2, F3] = case Version of 
		short -> [fun bench:g13/1, 11, 2];
		intermediate -> [fun bench:g124/1, 157, 2];
		long -> [fun bench:g1245/1, 157, 2]
	end,
    [[IWP,G,N,W] || IWP <- [true,false], G <- [F1], N <- [F2 * Schedulers], W <- [F3 * Schedulers]].

run([true,G,N,W|_], [], _) ->
	io:format("~p~n", [apply(bench, par, [G,N,W])]);

run([false,G,N,W|_], [], _) ->
    io:format("~p~n", [apply(bench, par_seq, [G,N,W])]);

run([true,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [apply(bench, dist, [G,N,W,Slaves])]);

run([false,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [apply(bench, dist_seq, [G,N,W,Slaves])]).

