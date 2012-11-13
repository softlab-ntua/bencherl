-module(etsm).

-compile(export_all).

new(_,_) ->
    new().

new() ->
    NrOfSchedulers = erlang:system_info(schedulers),
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
		array:new(NrOfSchedulers), 
		lists:seq(0, NrOfSchedulers -1)).

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
