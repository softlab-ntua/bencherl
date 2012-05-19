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
		{_,OutFile} = lists:keyfind(outfile, 1, Settings),
		{_,StatFile} = lists:keyfind(statfile, 1, Settings),
		{_,DataDir} = lists:keyfind(datadir, 1, Settings),
	
		% Start the nodes.
		Nodes = lists:map(fun(Nn)->
			[Name,Host]=string:tokens(atom_to_list(Nn), "@"),
			{ok, Node} = slave:start(list_to_atom(Host), list_to_atom(Name), 
			Args, self(), Program),
			Node
		end, lists:sublist(Nnames, N)),

		% Open the statistics file.				
		{ok, SF} = file:open(StatFile, [append]),
		io:format(SF, "~w ~w ", [S, N]),

		% Run the benchmark for all argument sets.
		Fun = fun(Bargs) ->
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
			io:format(SF, "(~w) ~w ", [Bargs, timer:now_diff(T1, T0)/1000]) 		
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
