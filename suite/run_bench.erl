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
		M = lists:keyfind(bench, 1, Settings),
		OTP = lists:keyfind(otp, 1, Settings),
		Program = case OTP of
			[] 	-> "erl";
			_	-> OTP ++ "/bin/erl"
		end,
		Args = lists:keyfind(args, 1, Settings),
		Nnames = lists:keyfind(nodes, 1, Settings),
		OutFile = lists:keyfind(outfile1, Settings),
		DataDir = lists:keyfind(datadir, 1, Settings),
	
		% Start the nodes.
		Nodes = lists:map(fun(Nn)->
			[Name,Host]=string:tokens(atom_to_list(Nn), "@"),
			{ok, Node} = slave:start(list_to_atom(Host), list_to_atom(Name), 
			Args, self(), Program),
			Node
		end, Nnames),
				
		% Run the benchmark for all argument sets.
		Fun = fun(Bargs) ->
			T0 = now(),
			Coordinator = self(),
			% In a new process, please.
			spawn(node(), fun() -> 
				{ok, F} = file:open(OutFile, [append]),
				group_leader(F, self()),
				apply(M, run, [Bargs, Nodes, [{datadir, DataDir}]]), 
				Coordinator ! done,
				file:close(F)
				end
			),
			receive done -> ok end,
			T1 = now(),
			io:format("(~w) ~w ", [Bargs, timer:now_diff(T1, T0)/1000]) 		
		end,
		lists:foreach(Fun, M:bench_args()),

		% Stop the nodes.
		lists:foreach(fun(N)-> slave:stop(N) end, Nodes)

	catch
		E:D ->
	    io:format("Exception ~p while running benchmark:\n~p\n~p\n", 
		[E, D, erlang:get_stacktrace()])
	end.
