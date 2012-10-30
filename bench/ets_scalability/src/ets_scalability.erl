-module(ets_scalability).

-export([bench_args/2, run/3]).

bench_args(Version, _) ->
    [[all, Version] | list_of_scenarios(Version)].

list_of_scenarios(Version) ->
    NrOfOperations = case Version of
                         short        -> 20000;
                         intermediate -> 200000;
                         long         -> 2000000
                     end,
    Scenarios = [{100,0,0},{50,50,0}, {20,10,70}, {9,1,90}, {1,0,99}],
    KeyRangeSizes = [NrOfOperations div round(math:pow(10, X)) || X <-lists:seq(0, 0)],%4)],
    TableTypes = [set],%, ordered_set],
    [[TableType, NrOfOperations, KeyRangeSize, Scenario] || 
        Scenario <- Scenarios, 
        KeyRangeSize <- KeyRangeSizes, 
        TableType <- TableTypes].

run([all, Version|_], _, _) ->
    lists:foreach(
      fun (Scenario) ->
              run(Scenario, nothing, nothing)
      end
      ,list_of_scenarios(Version));
run([TableType, NrOfOperations, KeyRangeSize, Scenario|_], _, _) ->
    Table = ets:new(test_table, 
                    [TableType,
                     public, {write_concurrency,true}, 
                     {read_concurrency,true}]),
    NrOfSchedulers = erlang:system_info(schedulers),
    OperationsPerProcess = NrOfOperations div NrOfSchedulers,
    OperationsPerProcessReminder = NrOfOperations rem NrOfSchedulers,
    lists:foreach(
      fun (ProcessNr) -> 
              start_do_operations_process(
                Table, 
                OperationsPerProcess + 
                    case ProcessNr =< OperationsPerProcessReminder of
                        true ->
                            1;
                        false ->
                            0
                    end, 
                KeyRangeSize, 
                Scenario,
                now())
      end,  
      lists:seq(1, NrOfSchedulers)),
    receive_ready_msg_from_workers(NrOfSchedulers),
    ets:delete(Table),
    ok.

receive_ready_msg_from_workers(0) ->
    ok;
receive_ready_msg_from_workers(NrOfWorkers) ->
    receive
        ready ->
            receive_ready_msg_from_workers(NrOfWorkers -1)
    end.

start_do_operations_process(Table, 
                            NrOfOperations, 
                            KeyRangeSize, 
                            Scenario,
                            RandomGenState) ->
    CreatorPid = self(),
    spawn(fun () ->
                  do_operations(Table, 
                                NrOfOperations, 
                                KeyRangeSize, 
                                Scenario,
                                RandomGenState),
                  CreatorPid ! ready 
          end).

do_operations(_, 0, _, _, _) ->
    ok;
do_operations(Table,
              NrOfOperations, 
              KeyRangeSize, 
              Scenarios = {PercentageInserts, PercentageDeletes, _PercentageLookups}, 
              RandomGenState) ->
    {OperationSelecRandomNum, NewRandomGenState1} = 
        random:uniform_s(100, RandomGenState),
    {Key, NewRandomGenState2} = 
        random:uniform_s(KeyRangeSize, NewRandomGenState1),
    case OperationSelecRandomNum of
        N when N =< PercentageInserts ->
            ets:insert(Table, {Key});
        N when N =< (PercentageInserts + PercentageDeletes) ->
            ets:delete(Table, {Key});
        _ ->
            ets:lookup(Table, {Key})    
    end,
    do_operations(Table, NrOfOperations - 1, KeyRangeSize, Scenarios, NewRandomGenState2).
