%% The benchmark executor.
%% The executor is responsible for starting the necessary slave nodes, for
%% running a benchmark and for stopping the slaves, after the execution of the
%% benchmark is over.

-module(run_bench).

-export([main/0]).

-include_lib("kernel/include/inet.hrl").

main() ->
    try
        %% Load run configuration settings.
	 BencherlRoot = os:getenv("BENCHERL_ROOT"),
        {ok,Conf} = file:consult(BencherlRoot ++ "/scratch/run_bench.conf"),
        {_,M} = lists:keyfind(bench, 1, Conf),
        {_,Version} = lists:keyfind(version, 1, Conf),
        {_,OTP} = lists:keyfind(otp, 1, Conf),
        {_,OutputFormat} = lists:keyfind(output_format, 1, Conf),
        ErlProgram = case OTP of
                         [] -> "erl";
                         _  -> OTP ++ "/bin/erl"
                     end,
        {_,ErlArgs} = lists:keyfind(erl_args, 1, Conf),
        {_,Master} = lists:keyfind(master, 1, Conf),
        {_,Snames} = lists:keyfind(slaves, 1, Conf),
        {_,N} = lists:keyfind(number_of_slaves, 1, Conf),
        {_,S} = lists:keyfind(number_of_schedulers, 1, Conf),
        {_,Iterations} = lists:keyfind(iterations, 1, Conf),
        {_,OutFile} = lists:keyfind(outfile, 1, Conf),
        {_,MeasFile} = lists:keyfind(measfile, 1, Conf),
        {_,DataDir} = lists:keyfind(datadir, 1, Conf),
        {_,What} = lists:keyfind(what, 1, Conf),
        {_,UseLongNames} = lists:keyfind(use_long_names, 1, Conf),
        {_,Cores} = lists:keyfind(number_of_cores, 1, Conf),
        NS = case What of
                 node  -> N;
                 sched -> S
             end,

	%% Start the slaves.
        Slaves =
            lists:map(
              fun(Sn) -> [Name|Rest] = string:tokens(atom_to_list(Sn), "@"),
                         {ok, Host} =
                             case Rest of
                                 [] ->
                                     {ok, Hname} = inet:gethostname(),
                                     case UseLongNames of
                                         true  -> {ok, #hostent{h_name=H}} =
                                                      inet:gethostbyname(Hname),
                                                  {ok, H};
                                         false -> {ok, Hname}
                                     end;
                                 _ -> {ok, hd(Rest)}
                             end,
                         {ok, Slave} =
                             slave:start(list_to_atom(Host), list_to_atom(Name),
                                         ErlArgs, self(), ErlProgram),
                         Slave
              end, lists:sublist(Snames, N)),

	%% Open the measurements file.
        {ok, MF} = file:open(MeasFile, [append]),
        {ok, OF} = file:open(OutFile, [write]),

	%% Run the benchmark for all argument sets.
        RunFun = fun(Coordinator, CurArgs) -> group_leader(OF, self()),
                    T0 = now(),
                    Return = apply(M, run, [CurArgs, Slaves, [{datadir, DataDir}, {master, Master}, {schedulers, S}]]),
                    Dur = timer:now_diff(now(), T0)/1000,
                    Coordinator ! {done, {Return, Dur}}
        end,
        RecordFun = fun(Text, Times) ->
                LabelFormat = 
                    begin
                        case Text of
                            {_, _} -> "~p";
                            _ ->
                                IsIntList =
                                    lists:all(fun(Elem)->is_integer(Elem) end, Text),
                                case IsIntList of
                                    true ->
                                        "~w";
                                    false ->
                                        "~p"
                                end
                        end
                    end,
		PTimes = case OutputFormat of
			min -> [lists:min(Times)];
			max -> [lists:max(Times)];
			avg -> [lists:sum(Times) / length(Times)];
			avg_min_max ->
				[ lists:sum(Times) / length(Times),
				  lists:min(Times),
				  lists:max(Times)
			        ];
			plain -> Times
		end,
                case Text of
                    {Str, L} -> 
                        FStr = remove_whitespace_and_new_lines(lists:flatten(io_lib:format(LabelFormat,[Str]))),
                        io:format(MF, "~s\t~w\t~p\t", [FStr, L, NS]);
                    _ ->
                        FText = remove_whitespace_and_new_lines(lists:flatten(io_lib:format(LabelFormat,[Text]))),
                        io:format(MF, "\t~s\t~p\t", [FText, NS])
                end,
		io:format(MF, string:join([io_lib:format("~w", [A]) || A <- PTimes], " "), []),
		io:nl(MF)
        end,
        Fun =
            fun(Bargs) ->
                    RunAndAggregate = fun(RecursiveRAA, CurrentState, AggregateResults) ->
                            Coordinator = self(),
                            %% In a new process, please.
                            spawn(node(), fun() -> RunFun(Coordinator, case CurrentState of initial -> Bargs; {state, C} -> [C | Bargs] end) end),
                            receive
                                {done, {R, T}} ->
                                    case R of
                                        {{continue, ignore}, State} -> RecursiveRAA(RecursiveRAA, {state, State}, AggregateResults);
                                        {{continue, Name}, State} -> RecursiveRAA(RecursiveRAA, {state, State}, orddict:append(Name, T, AggregateResults));
                                        {{done, ignore}, _} -> AggregateResults;
                                        {{done, Name}, _} -> orddict:append(Name, T, AggregateResults);
                                        _ -> orddict:append(standard, T, AggregateResults)
                                    end
                            end
                    end,
                    Aggregated = orddict:new(),
                    Times = lists:foldl(fun(_, A) -> RunAndAggregate(RunAndAggregate, initial, A) end, Aggregated, lists:seq(1,Iterations)),
                    case orddict:to_list(Times) of
                        [{standard, T}] -> RecordFun(Bargs, T);
                        T -> lists:foreach(fun({Name, SubTimes}) -> RecordFun({Name, Bargs}, SubTimes) end, T)
                    end
            end,
        lists:foreach(Fun, M:bench_args(Version, [{number_of_cores, Cores}, {slaves, Slaves}])),
        file:close(OF),

	%% Close the measurements file.
        file:close(MF),

	%% Stop the slaves.
        lists:foreach(fun(Slave)-> slave:stop(Slave) end, Slaves)

    catch
        E:D ->
            io:format("Exception ~p while running benchmark:\n~p\n~p\n",
                      [E, D, erlang:get_stacktrace()])
    end.


remove_whitespace_and_new_lines(Str) ->
    lists:reverse(remove_whitespace_and_new_lines([], Str)).

remove_whitespace_and_new_lines(Compl, []) ->
    Compl;
remove_whitespace_and_new_lines(Compl, [C | Str]) ->
    case C of
        $\s -> remove_whitespace_and_new_lines([$=|Compl], Str);
        $\n -> remove_whitespace_and_new_lines(Compl, Str);
        $\t -> remove_whitespace_and_new_lines(Compl, Str);
        _   -> remove_whitespace_and_new_lines([C|Compl], Str)
    end.
