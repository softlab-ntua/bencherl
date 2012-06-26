-module(dialyzer_bench).

-export([bench_args/2, run/3]).

bench_args(_, _) -> 
	[[plt], [otp]].

run([plt], _, Conf) ->

	{_,DataDir} = lists:keyfind(datadir, 1, Conf),

	[] = dialyzer:run([{analysis_type, plt_build},
		{report_mode, normal},
		{files_rec, [DataDir ++ "/plt"]},
		{timing, true},
		{output_plt, DataDir ++ "/the.plt"}]),
		ok;

run([otp], _, Conf) ->

    {_,DataDir} = lists:keyfind(datadir, 1, Conf),

    RawWarns = dialyzer:run([{files_rec, [F || F <- [DataDir ++ "/plt", DataDir ++ "/otp"]]},
		{report_mode, normal},
		{init_plt, DataDir ++ "/the.plt"},
		{timing, true}]),
	Warns = lists:sort([dialyzer:format_warning(W) || W <- RawWarns]),
	io:format("~s", [Warns]),
	ok.

