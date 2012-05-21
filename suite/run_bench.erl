%% The coordinator of the benchmark execution.
%% The coordinator is responsible for starting the necessary Erlang nodes,
%% for running the benchmark and for stopping the nodes, after the execution
%% of the benchmark is over.

-module(run_bench).

-export([main/0]).

main() ->

	try

		% Load configuration settings.
		{ok, Settings} = file:consult("scratch/run_bench.config"),
		{_,M} = lists:keyfind(bench, 1, Settings),
		{_,OTP} = lists:keyfind(otp, 1, Settings),
		Program = case OTP of
			[] 	-> "erl";
			_	-> OTP ++ "/bin/erl"
		end,
		{_,Args} = lists:keyfind(args, 1, Settings),
		{_,Nnames} = lists:keyfind(nodes, 1, Settings),
		{_,N} = lists:keyfind(nnodes, 1, Settings),
		{_,S} = lists:keyfind(nschedulers, 1, Settings),
		{_,Iterations} = lists:keyfind(iterations, 1, Settings),
		{_,OutFile} = lists:keyfind(outfile, 1, Settings),
		{_,StatFile} = lists:keyfind(statfile, 1, Settings),
		{_,DataDir} = lists:keyfind(datadir, 1, Settings),
		{_,What} = lists:keyfind(what, 1, Settings),
		NS = case What of
				node -> N;
				sched -> S
			 end,

		% Start the nodes.
		Nodes = lists:map(fun(Nn)->
			[Name,Host]=string:tokens(atom_to_list(Nn), "@"),
			{ok, Node} = slave:start(list_to_atom(Host), list_to_atom(Name), 
			Args, self(), Program),
			Node
		end, lists:sublist(Nnames, N)),

		% Open the statistics file.				
		{ok, SF} = file:open(StatFile, [append]),
		io:format(SF, "~w ", [NS]),

		% Run the benchmark for all argument sets.
		Fun = fun(Bargs) ->

			Times=lists:map(fun(_) ->
				T0 = now(),
				Coordinator = self(),
				% In a new process, please.
				spawn(node(), fun() -> 
					{ok, OF} = file:open(OutFile, [append]),
					group_leader(OF, self()),
					apply(M, run, [Bargs, Nodes, [{datadir, DataDir}]]), 
					Coordinator ! done,
					file:close(OF)
					end
				),
				receive done -> ok end,
				T1 = now(),
				timer:now_diff(T1, T0)/1000
				end, lists:seq(1,Iterations)),
			io:format(SF, "(~w) ~w ", [Bargs, median(Times)])	
		end,
		lists:foreach(Fun, M:bench_args()),

		% Close the statistics file.
		io:nl(SF),
		file:close(SF),

		% Stop the nodes.
		lists:foreach(fun(Node)-> slave:stop(Node) end, Nodes)

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
