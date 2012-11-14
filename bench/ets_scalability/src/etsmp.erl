-module(etsmp).

-compile(export_all).

new(_,_) ->
    new().

new() ->
    NrOfSchedulers = erlang:system_info(schedulers),
    NrOfSubTables = prime_greater_or_equal_than(NrOfSchedulers),
    AllocateETSFun = 
	fun() ->
		ets:new(test_table, 
			[set, 
			 public, 
			 {write_concurrency,true}, 
			 {read_concurrency,true}])
	end,
    lists:foldl(fun(Index, Array) ->
			array:set(Index, AllocateETSFun(), Array)
		end, 
		array:new(NrOfSubTables), 
		lists:seq(0, NrOfSubTables -1)).

delete(EtsmID) ->
    lists:foreach(fun(EtsID) ->
			  ets:delete(EtsID)
		  end, 
		  array:to_list(EtsmID)).

insert(EtsmID, Tuple) ->
    HashValue = erlang:phash2(element(1,Tuple), array:size(EtsmID)),
    SubTable = array:get(HashValue, EtsmID),
    ets:insert(SubTable, Tuple).

delete(EtsmID, Key) ->
    HashValue = erlang:phash2(Key, array:size(EtsmID)),
    SubTable = array:get(HashValue, EtsmID),
    ets:delete(SubTable, Key).

lookup(EtsmID, Key) ->
    HashValue = erlang:phash2(Key, array:size(EtsmID)),
    SubTable = array:get(HashValue, EtsmID),
    ets:lookup(SubTable, Key).


prime_greater_or_equal_than(Number) ->
    case Number =< 2 of
        true ->
            2;
        false ->
            prime_greater_or_equal_than(Number, 3, [2])     
    end.

prime_greater_or_equal_than(Number, NextNumber, PrimesSoFar) ->
    IsPrime = 
        lists:all(
          fun(Prime) -> 
                  (NextNumber rem Prime) =/= 0
          end,
          PrimesSoFar),
    case IsPrime of
        true ->
            case Number =< NextNumber of
                true ->
                    NextNumber;
                false ->
                    prime_greater_or_equal_than(
                      Number, 
                      NextNumber + 1, 
                      PrimesSoFar ++ [NextNumber])     
            end;
        false ->
            prime_greater_or_equal_than(
              Number, 
              NextNumber + 1, 
              PrimesSoFar)     
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
