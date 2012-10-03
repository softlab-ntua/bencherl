-module(orbit_int).

-export([bench_args/2, run/3]).

bench_args(Version, Conf) ->
    {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
    [G0,N0,W0] = bench_args_aux(Version),
    [[IWP,G,N,W] || IWP <- [true,false], G <- [G0],
        N <- [N0 * Cores], W <- [W0 * Cores]].

bench_args_aux(Version) ->
    case Version of
        short -> [fun bench:g13/1, 11, 2];
        intermediate -> [fun bench:g124/1, 157, 2];
        long -> [fun bench:g1245/1, 157, 2]
    end.

run([true,G,N,W|_], [], _) ->
    io:format("~p~n", [bench:par(G,N,W)]);
run([false,G,N,W|_], [], _) ->
    io:format("~p~n", [bench:par_seq(G,N,W)]);
run([true,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [bench:dist(G,N,W,Slaves)]);
run([false,G,N,W|_], Slaves, _) ->
    io:format("~p~n", [bench:dist_seq(G,N,W,Slaves)]).

