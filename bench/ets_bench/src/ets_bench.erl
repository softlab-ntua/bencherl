-module(ets_bench).

-export([bench_args/2, run/3]).

-define(BASE, 2).
-define(RAND_MAX, 65535).
-define(TABLE_PROCESS, table_process).
-define(SAMPLING, 10000).

bench_args(Version, _) ->
	%% positive numbers: this times number of schedulers worker processes
	%% negative number: negative of exactly this many worker processes
	Processes = 1,
	%% percentage of MixedOps that are Updates. Use 0 to measure only lookups.
	MixedOpsUpdates = [0, 0.01, 0.1],
	%% KeyRange, Inserts/Deletes and Lookups/MixedOps as powers of ?BASE
	%% MixedOps are Lookups: (1-MixedOpsUpdates), Inserts: (MixedOpsUpdates/2) and Deletes (MixedOpsUpdates/2)
	[KeyRange, InsDels, MixedOps] = case Version of
		short -> [14, 15, 17];
		intermediate -> [18, 18, 21];
		long -> [21, 20, 22]
	end,
	TableTypes = case Version of
		short -> [set, ordered_set];
		intermediate -> [set, ordered_set ];
		long -> [set, ordered_set ]
	end,
	%% use deterministic seed for reproducable results
	Seed = {0,0,0},
	%% use random seed for varying input
	%Seed = now(), % this currently breaks graph creation
	ConcurrencyOptions = [no, rw], % options are: no, r, w, rw
	[[TT,KeyRange,InsDels,MixedOps,M,C,Processes,Seed] || TT <- TableTypes, C <- ConcurrencyOptions, M <- MixedOpsUpdates ].

run([TableType, _K, _W, _R, _U, C, _Processes, Seed], _, _) ->
	% this is a setup
	{RC, WC} = case C of
		no -> {false, false};
		r -> {true, false};
		w -> {false, true};
		rw -> {true, true}
	end,
	Options = [{read_concurrency, RC}, {write_concurrency, WC}],
	Self = self(),
	spawn(fun() -> table_process(Self, [TableType, public | Options]) end),
	Table = receive {table, T} -> T end,
	
	{{continue, ignore}, [insert, Table, 0, Seed]};
run([[run | State ] | Config], _, _) ->
	run_bench(State ++ Config);
run(State, _, _) ->
	setup(State).

table_process(Pid, Opts) ->
	register(?TABLE_PROCESS, self()),
	Table = ets:new(?MODULE, Opts),
	Pid ! {table, Table},
	receive
		finish -> ok
	end,
	ok.

do(_, {_, []}) -> ok;
do(Action, {T,[X|Xs]}) ->
	ets:Action(T, X),
	do(Action, {T, Xs}).

insert({_, []}) -> ok;
insert({T,[X|Xs]}) ->
	ets:insert(T, {X}),
	insert({T, Xs}).


mixed({_, []}) -> ok;
mixed({T,[{A,X}|Xs]}) ->
	ets:A(T, X),
	mixed({T, Xs}).

setup([[insert, Table, 1, Seed], _T, _K, _W, _R, _U, _C, _Procs, _S]) ->
	{{continue, ignore}, [delete, Table, 0, Seed]};
setup([[insert, Table, P, Seed], _T, K, W, _R, _U, _C, Procs | _]) ->
	random:seed(Seed),
	WOps = round(math:pow(?BASE, W)),
	KeyRange = round(math:pow(?BASE, K)),
	Init = fun(Idx, Max) ->
		Remain = WOps rem Max,
		C = if
			(Idx < Remain) -> 1;
			true -> 0
		end,
		Amount = WOps div Max + C,
		Randoms = make_randoms(Amount, KeyRange),
		 {Table, Randoms}
	end,
	Workers = make_workers(Init, fun(X) -> insert(X) end, Procs),
	NextSeed = make_seed(),
	Name = lists:flatten(["insert ", integer_to_list(WOps)]),
	{{continue, ignore}, [run, insert, Name, Workers, Table, P, NextSeed]};
setup([[lookup, Table, P, Seed], _T, K, _W, R, UpdatePercentage, _C, Procs | _]) ->
	%erlang:display([ets:info(Table, size), _T, [K, _W, R]]),
	random:seed(Seed),
	ROps = round(math:pow(?BASE, R)),
	KeyRange = round(math:pow(?BASE, K)),
	
	case UpdatePercentage of
		0 ->
			Init = fun(Idx, Max) ->
				Remain = ROps rem Max,
				C = if
					(Idx < Remain) -> 1;
					true -> 0
				end,
				Amount = ROps div Max + C,
				Randoms = make_randoms(Amount, KeyRange),
				{Table, Randoms}
			end,
			Workers = make_workers(Init, fun(X) -> do(lookup, X) end, Procs),
			Name = lists:flatten(["lookup ", integer_to_list(ROps)]);
		_ ->
			Init = fun(Idx, Max) ->
				Remain = ROps rem Max,
				C = if
					(Idx < Remain) -> 1;
					true -> 0
				end,
				Amount = ROps div Max + C,
				Randoms = make_randoms(Amount, KeyRange),
				Actions = make_randoms(Amount, ?SAMPLING),
				Combine = fun(ANr, RNr) ->
						if
							ANr =< ?SAMPLING*UpdatePercentage/2 -> {insert, {RNr}};
							ANr =< ?SAMPLING*UpdatePercentage -> {delete, RNr};
							true -> {lookup, RNr}
						end
				end,
				RandomsActions = lists:zipwith(Combine, Actions, Randoms),
				{Table, RandomsActions}
			end,
			Workers = make_workers(Init, fun(X) -> mixed(X) end, Procs),
			Name = lists:flatten([io_lib:format("mixed l:~.2f%, u:~.2f% ", [(1-UpdatePercentage)*100, UpdatePercentage*100]), integer_to_list(ROps)])
	end,
	NextSeed = make_seed(),
	{{continue, ignore}, [run, lookup, Name, Workers, Table, P, NextSeed]};
setup([[delete, Table, 1, _Seed], _T, _K, _W, _R, _U, _C, _Procs, _S]) ->
	ets:delete(Table),
	?TABLE_PROCESS ! finish,
	{{done,ignore}, ok};
setup([[delete, Table, P, Seed], _T, K, W, _R, _U, _C, Procs | _ ]) ->
	random:seed(Seed),
	WOps = round(math:pow(?BASE, W)),
	KeyRange = round(math:pow(?BASE, K)),
	Init = fun(Idx, Max) ->
		Remain = WOps rem Max,
		C = if
			(Idx < Remain) -> 1;
			true -> 0
		end,
		Amount = WOps div Max + C,
		Randoms = make_randoms(Amount, KeyRange),
		{Table, Randoms}
	end,
	
	Workers = make_workers(Init, fun(X) -> do(delete, X) end, Procs),
	NextSeed = make_seed(),
	Name = lists:flatten(["delete ", integer_to_list(WOps)]),
	{{continue, ignore}, [run, delete, Name, Workers, Table, P, NextSeed]}.

run_bench([insert | State]) ->
	run_insert(State);
run_bench([lookup | State]) ->
	run_lookup(State);
run_bench([delete | State]) ->
	run_delete(State).

run_insert([Name, Workers, Table, Part, Seed | _]) ->
	start(Workers),
	wait_for(Workers),
	{{continue, Name}, [lookup, Table, Part, Seed]}.

run_lookup([Name, Workers, Table, Part, Seed | _]) ->
	start(Workers),
	wait_for(Workers),
	{{continue, Name}, [insert, Table, Part+1, Seed]}.

run_delete([Name, Workers, Table, Part, Seed | _]) ->
	start(Workers),
	wait_for(Workers),
	{{continue, Name}, [delete, Table, Part+1, Seed]}.

make_workers(Init, Work, Procs) ->
	WCount = if
		Procs < 0 -> -Procs;
		Procs > 0 -> Procs * erlang:system_info(schedulers);
		true -> erlang:error(invalid_process_count)
	end,
	Randoms = lists:map(fun make_seed/1, lists:seq(1, WCount)),
	make_workers(Init, Work, Randoms, 0, WCount, []).

make_workers(_I, _W, [], M, M, Agg) ->
	ready(Agg),
	Agg;
make_workers(Init, Work, [Seed|R], Cnt, Max, Agg) ->
	Coordinator = self(),
	Worker = spawn(fun() -> worker(Coordinator, Init, Work, Cnt, Max, Seed) end),
	make_workers(Init, Work, R, Cnt+1, Max, [Worker | Agg]).

worker(Coordinator, Init, Work, Cnt, Max, Seed) ->
	random:seed(Seed),
	InitState = Init(Cnt, Max),
	Coordinator ! {self(), ready},
	receive
		{NewCoordinator, start} -> ok % wait for signal from coordinator
	end,
	Work(InitState),
	NewCoordinator ! {self(), done},
	ok.


start([]) -> ok;
start([Worker|Workers]) ->
	Worker ! {self(), start},
	start(Workers).
wait_for(Ws) -> wait_for_signal(done, Ws).
ready(Ws) -> wait_for_signal(ready, Ws).

wait_for_signal(_, []) -> ok;
wait_for_signal(Signal, [Worker|Workers]) ->
	receive
		{Worker, Signal} -> wait_for_signal(Signal, Workers)
	end.

make_seed(_) -> make_seed().
make_seed() ->
	{
		random:uniform(?RAND_MAX),
		random:uniform(?RAND_MAX),
		random:uniform(?RAND_MAX)
	}.

make_randoms(Amnt, Range) -> make_randoms(Amnt, Range, []).
make_randoms(0, _, Acc) -> Acc;
make_randoms(Amnt, Range, Acc) ->
	make_randoms(Amnt-1, Range, [random:uniform(Range) | Acc]).
