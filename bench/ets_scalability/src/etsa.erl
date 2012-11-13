-module(etsa).

-compile(export_all).

new(_,_) ->
    new().

new() ->
    NrOfSchedulers = erlang:system_info(schedulers),
    CreateETSProcFun = 
        fun() ->
                EtsTable = 
                    ets:new(test_table, 
                            [set, 
                             public, 
                             {write_concurrency,true}, 
                             {read_concurrency,true}]),
                spawn(fun() -> ets_process_fun(EtsTable) end)
        end,
    lists:foldl(fun(Index, Array) ->
                        array:set(Index, CreateETSProcFun(), Array)
                end, 
                array:new(NrOfSchedulers), 
                lists:seq(0, NrOfSchedulers -1)).

ets_process_fun(SubTable) ->
    receive
        {insert, Tuple} ->
            ets:insert(SubTable, Tuple),
            ets_process_fun(SubTable);
        {delete, Key} ->
            ets:delete(SubTable, Key),
            ets_process_fun(SubTable);
        {lookup, Requester, Key} ->
            Value = ets:lookup(SubTable, Key),

            Requester ! {ets_value_found, Value},
            ets_process_fun(SubTable);
        {delete_table, Requester} ->
            ets:delete(SubTable),
            Requester ! table_deleted
    end.


delete(EtsaID) ->
    lists:foreach(fun(EtsProcID) ->
                          EtsProcID ! {delete_table, self()},
                          receive
                              table_deleted ->
                                  ok
                          end
                  end, 
                  array:to_list(EtsaID)).

insert(EtsaID, Tuple) ->
    HashValue = erlang:phash2(element(1,Tuple), array:size(EtsaID)),
    EtsProcID = array:get(HashValue, EtsaID),
    EtsProcID ! {insert, Tuple}.

delete(EtsaID, Key) ->
    HashValue = erlang:phash2(Key, array:size(EtsaID)),
    EtsProcID = array:get(HashValue, EtsaID),
    EtsProcID ! {delete, Key}.


lookup(EtsaID, Key) ->
    HashValue = erlang:phash2(Key, array:size(EtsaID)),
    EtsProcID = array:get(HashValue, EtsaID),
    EtsProcID ! {lookup, self(), Key},
    receive
        {ets_value_found, Value} ->
            Value
    end.

test() ->
    EtsmID = new(),
    insert(EtsmID, {1}),
    insert(EtsmID, {2}),
    insert(EtsmID, {3}),
    [{1}] = lookup(EtsmID, 1),
    [{2}] = lookup(EtsmID, 2),
    [{3}] = lookup(EtsmID, 3),
    delete(EtsmID, 1),
    delete(EtsmID, 2),
    delete(EtsmID, 3),
    [] = lookup(EtsmID, 1),
    [] = lookup(EtsmID, 2),
    [] = lookup(EtsmID, 3),
    delete(EtsmID),
    ok.
