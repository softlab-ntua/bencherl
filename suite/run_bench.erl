%% The benchmark executor.
%% The executor is responsible for starting the necessary slave nodes, for 
%% running a benchmark and for stopping the slaves, after the execution of the
%% benchmark is over.

-module(run_bench).

-export([main/0]).

main() ->

	try

		% Load run configuration settings.
		{ok, Conf} = file:consult("scratch/run_bench.conf"),
		{_,M} = lists:keyfind(bench, 1, Conf),
		{_,InputSize} = lists:keyfind(input_size, 1, Conf),
		{_,OTP} = lists:keyfind(otp, 1, Conf),
		ErlProgram = case OTP of
			[] 	-> "erl";
			_	-> OTP ++ "/bin/erl"
		end,
		{_,ErlArgs} = lists:keyfind(erl_args, 1, Conf),
		{_,Snames} = lists:keyfind(slaves, 1, Conf),
		{_,N} = lists:keyfind(number_of_slaves, 1, Conf),
		{_,S} = lists:keyfind(number_of_schedulers, 1, Conf),
		{_,Iterations} = lists:keyfind(iterations, 1, Conf),
		{_,OutFile} = lists:keyfind(outfile, 1, Conf),
		{_,StatFile} = lists:keyfind(statfile, 1, Conf),
		{_,DataDir} = lists:keyfind(datadir, 1, Conf),
		{_,What} = lists:keyfind(what, 1, Conf),
		NS = case What of
				node -> N;
				sched -> S
			 end,

		% Start the slaves.
		Slaves = lists:map(fun(Sn)->
			[Name,Host]=string:tokens(atom_to_list(Sn), "@"),
			{ok, Slave} = slave:start(list_to_atom(Host), list_to_atom(Name), 
			ErlArgs, self(), ErlProgram),
			Slave
		end, lists:sublist(Snames, N)),

		% Open the statistics file.				
		{ok, SF} = file:open(StatFile, [append]),
		io:format(SF, "~w ", [NS]),

		{ok, OF} = file:open(OutFile, [write]),

		% Run the benchmark for all argument sets.
		Fun = fun(Bargs) ->

			Times=lists:map(fun(_) ->
				Coordinator = self(),
				% In a new process, please.
				spawn(node(), fun() -> 
					group_leader(OF, self()),
					T0 = now(),
					apply(M, run, [Bargs, Slaves, [{datadir, DataDir}]]), 
					T1 = now(),	
					Coordinator ! {done, timer:now_diff(T1, T0)/1000}
					end
				),
				receive {done,T} -> T end
				end, lists:seq(1,Iterations)),
			io:format(SF, "(~w) ~w ", [Bargs, median(Times)])	
		end,
		lists:foreach(Fun, M:bench_args(InputSize)),
		file:close(OF),

		% Close the statistics file.
		io:nl(SF),
		file:close(SF),

		% Stop the slaves.
		lists:foreach(fun(Slave)-> slave:stop(Slave) end, Slaves)

	catch
		E:D ->
	    io:format("Exception ~p while running benchmark:\n~p\n~p\n", 
		[E, D, erlang:get_stacktrace()])
	end.

median(L) ->
	SL = lists:sort(L),
	Length = length(SL),
	case Length band 1 of
		1 -> lists:nth(round((Length + 1) / 2), SL);
		0 -> 
			[A,B] = lists:sublist(SL, round(Length / 2), 2),
			(A + B) / 2
	end.
