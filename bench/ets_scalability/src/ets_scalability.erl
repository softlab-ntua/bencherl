-module(ets_scalability).

-export([bench_args/2, run/3]).

bench_args(Version, _) ->
    small_nr_fixed_keys:list_of_scenarios(Version).%% ++ ets_usage_scenarios:list_of_scenarios(Version).


run([ets_usage_scenarios, TableType, NrOfOperations, KeyRangeSize, Scenario, ConcurrencyOptions, WorkerHeapSize|_], _, _) ->
    ets_usage_scenarios:run_bench(TableType, NrOfOperations, KeyRangeSize, Scenario, ConcurrencyOptions, WorkerHeapSize);
run([small_nr_fixed_keys, NrOfOperations, TableImpel|_], _, _) ->
    small_nr_fixed_keys:run_bench(NrOfOperations, TableImpel).

