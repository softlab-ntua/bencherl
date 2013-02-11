-module(debench).

-export([bench_args/2, run/3]).

bench_args(_Version, Conf) -> 
	{_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
	[{100000,Cores}].

run({NumOps, Cores}, _, Conf) ->
	{_,DataDir} = lists:keyfind(datadir, 1, Conf),
	{_,NodeName} = lists:keyfind(master, 1, Conf),
	de_bench:main(Cores, NodeName,DataDir ++ "/bench.config",NumOps),
		ok.
