%% The coordinator of the benchmark execution.
%% The coordinator is responsible for starting the necessary Erlang nodes,
%% for running the benchmark and for stopping the nodes, after the execution
%% of the benchmark is over.

-module(run_bench).

-export([main/0]).

main() ->

	try

		% Load configuration settings.
		{ok, T} = file:consult("scratch/run_bench.config"),
		load(T),
		M = get(bench),
		OTP = get(otp),
		Args = get(args),
		Nnames = get(nodes),
		Cookie = get(cookie),
		OutFile = get(outfile),
		DataDir = get(datadir),
	
		% Start the nodes.
		Nodes = lists:map(fun(Nn)->
			[Name,Host]=string:tokens(atom_to_list(Nn), "@"),
			{ok, Node} = slave:start(list_to_atom(Host), list_to_atom(Name), 
			Args, self(), OTP),
			erlang:setcookie(Node, Cookie),
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

load([]) ->
	ok;
load([{K,V} | R]) ->
	put(K, V),
	load(R).
