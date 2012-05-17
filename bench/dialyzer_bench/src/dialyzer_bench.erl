-module(dialyzer_bench).

-export([bench_args/0, run/3]).

bench_args() -> 
	[[plt], [otp]].

run([plt], _, Opts) ->

	DataDir = lists:keyfind(datadir, 1, Opts),

	[] = dialyzer:run([{analysis_type, plt_build},
		{report_mode, normal},
		{files_rec, [DataDir ++ "/plt"]},
		{timing, true},
		{output_plt, DataDir ++ "/the.plt"}]),
		ok;

run([otp], _, Opts) ->

    DataDir = lists:keyfind(datadir, 1, Opts),

    RawWarns = dialyzer:run([{files_rec, [F || F <- [DataDir ++ "/plt", DataDir ++ "/otp"]]},
		{report_mode, normal},
		{init_plt, DataDir ++ "/the.plt"},
		{timing, true}]),
	Warns = lists:sort([dialyzer:format_warning(W) || W <- RawWarns]),
	io:format("~s", [Warns]),
	ok.

