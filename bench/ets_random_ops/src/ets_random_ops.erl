-module(ets_random_ops).

-export([bench_args/2, run/3]).

-define(READ_CONCURRENCY_VERSION, "R14B").
-define(WRITE_CONCURRENCY_VERSION, "R13B02-1").

bench_args(Version, _) ->
    NrOfOperations = 
        case Version of
            short        -> 150000;
            intermediate -> 1500000;
            long         -> 15000000
        end,
    % % Insert, % Delete, % Lookup, % NoOp
    Scenarios = [{100,0,0,0},
                 {50,50,0,0},
                 {20,10,70,0},
                 {1,0,99,0}],
    KeyRangeSizes = [1000000],
    TableTypes = [set, ordered_set],
    WorkerHeapSizes = [233],
    ReleaseVersion = erlang:system_info(otp_release),
    ConcurrencyOptionsList = if
	ReleaseVersion >= ?READ_CONCURRENCY_VERSION -> [[{read_concurrency, true}, {write_concurrency, true}]];
	ReleaseVersion >= ?WRITE_CONCURRENCY_VERSION -> [[{write_concurrency, true}]];
	true -> [[]]
    end,
    [[TableType, 
      NrOfOperations, 
      KeyRangeSize, 
      Scenario, 
      ConcurrencyOptions, 
      WorkerHeapSize] || 
        Scenario <- Scenarios, 
        KeyRangeSize <- KeyRangeSizes, 
        TableType <- TableTypes,
        ConcurrencyOptions <- ConcurrencyOptionsList,
        WorkerHeapSize <- WorkerHeapSizes].


run([TableType, 
     NrOfOperations, 
     KeyRangeSize, 
     Scenario, 
     ConcurrencyOptions, 
     WorkerHeapSize|_], _, _) ->
    Table = ets:new(test_table, 
                    [TableType, public | ConcurrencyOptions]),
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
                WorkerHeapSize,
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
            receive_ready_msg_from_workers(NrOfWorkers - 1)
    end.

start_do_operations_process(Table, 
                            NrOfOperations, 
                            KeyRangeSize, 
                            Scenario,
                            WorkerHeapSize,
                            RandomGenState) ->
    CreatorPid = self(),
    spawn_opt(fun () ->
                      do_operations(Table, 
                                    NrOfOperations, 
                                    KeyRangeSize, 
                                    Scenario,
                                    RandomGenState),
                      CreatorPid ! ready 
              end,
              [{min_heap_size, WorkerHeapSize}]).

do_operations(_, 0, _, _, _) ->
    ok;
do_operations(Table,
              NrOfOperations, 
              KeyRangeSize, 
              Scenarios = {PercentageInserts, 
                           PercentageDeletes, 
                           PercentageLookups, 
                           _ProcentageNothing}, 
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
        N when N =< (PercentageInserts + PercentageDeletes + PercentageLookups) ->
            ets:lookup(Table, {Key});
        _ ->
            nothing
    end,
    do_operations(Table, 
                  NrOfOperations - 1, 
                  KeyRangeSize, 
                  Scenarios, 
                  NewRandomGenState2).
