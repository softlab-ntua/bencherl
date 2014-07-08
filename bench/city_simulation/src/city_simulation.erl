-module(city_simulation).

-export([bench_args/2, run/3]).

bench_args(Version, _Conf) ->
	case Version of
		short -> [[tiny, brief]];
		intermediate -> [[small, short]];
		long -> [[medium, medium]]
	end.

run([S,D], _, _) ->
	city_benchmarking_test:run(S, D),
	ok.
