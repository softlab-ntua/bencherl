% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Tests for comparing and illustrating the differences of four types of
% hashtables: hashtable, tracked hashtable, lazy hashtable and map hashtable.
%
% See also hashtable.erl, tracked_hashtable.erl, lazy_hashtable.erl,
% map_hashtable.erl and their respective test modules.
%
% Directly depends on the following modules: hashtable, tracked_hashtable,
% lazy_hashtable, map_hashtable.
%
-module(hashtables_comparison_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').
-define(MyFourthKey, 'MyFourthKey').



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( " ~n Comparison of tables creation: " ),
	MyH1 = hashtable:new( 10 ),
	true = hashtable:isEmpty( MyH1 ),
	hashtable:display( "Vanilla hashtable", MyH1 ),
	MyH1Optimised = hashtable:optimise( MyH1 ),
	hashtable:display( "Optimized hash table", MyH1Optimised ),

	test_facilities:display( "" ),
	MyTH1 = tracked_hashtable:new( 10 ),
	true = tracked_hashtable:isEmpty( MyTH1 ),
	tracked_hashtable:display( "Vanilla tracked table ", MyTH1 ),

	test_facilities:display( "" ),
	MyLH1 = lazy_hashtable:new( 10 ),
	true = lazy_hashtable:isEmpty( MyLH1 ),
	lazy_hashtable:display( "Vanilla lazy hashtable", MyLH1 ),

	test_facilities:display( "" ),
	MyM1 = map_hashtable:new( 10 ),
	true = map_hashtable:isEmpty( MyM1 ),
	map_hashtable:display( "Vanilla map hashtable", MyM1 ),


	test_facilities:display( "End of the comparison of tables creation." ),



	test_facilities:display(
						 "Comparison of tables' state after adding entries:" ),

	% Adding entries in hashtable:
	test_facilities:display( "Adding entries in hash table:" ),
	MyH2 = hashtable:new( 4 ),
	MyH3 = hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyH2 ),
	false = hashtable:isEmpty( MyH3 ),

	MyH4 = hashtable:addEntry( ?MySecondKey, "MySecondValue", MyH3 ),
	false = hashtable:isEmpty( MyH4 ),

	MyH5 = hashtable:addEntry( ?MyThirdKey, [1,2,3], MyH4 ),
	false = hashtable:isEmpty( MyH5 ),
	hashtable:display( MyH5 ),

	hashtable:display( MyH5 ),
	MyH5Optimised = hashtable:optimise( MyH5 ),
	hashtable:display( "Optimised hashtable", MyH5Optimised ),

	test_facilities:display( "Adding entries in tracked hashtable:"),

	MyTH2 = tracked_hashtable:new(4 ),
	MyTH3 = tracked_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyTH2 ),
	false = tracked_hashtable:isEmpty( MyTH3 ),

	MyTH4 = tracked_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyTH3 ),
	false = tracked_hashtable:isEmpty( MyTH4 ),

	MyTH5 = tracked_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyTH4 ),
	false = tracked_hashtable:isEmpty( MyTH5 ),
	tracked_hashtable:display( "Tracked hashtable: ", MyTH5 ),

	test_facilities:display( "Adding entries in lazy hashtable:" ),

	MyLH2 = lazy_hashtable:new( 4 ),
	MyLH3 = lazy_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyLH2 ),
	false = lazy_hashtable:isEmpty( MyLH3 ),

	MyLH4 = lazy_hashtable:addEntry( ?MySecondKey, "MySecondValue", MyLH3 ),
	false = lazy_hashtable:isEmpty( MyLH4 ),

	MyLH5 = lazy_hashtable:addEntry( ?MyThirdKey, [1,2,3], MyLH4 ),
	false = lazy_hashtable:isEmpty( MyLH5 ),
	lazy_hashtable:display( "Lazy hashtable: ", MyLH5 ),

	test_facilities:display( "" ),


	test_facilities:display( "Looking up for ~s in hashtable: ~p",
		   [ ?MyFirstKey, hashtable:lookupEntry( ?MyFirstKey, MyH5 ) ] ),

	{ value, "MyFirstValue" } = hashtable:lookupEntry( ?MyFirstKey, MyH5 ),

	test_facilities:display( "Removing that entry." ),
	MyH6 = hashtable:removeEntry( ?MyFirstKey, MyH5 ),
	false = hashtable:isEmpty( MyH6 )
		,
	test_facilities:display( "Looking up for ~s hashtable: ~p", [ ?MyFirstKey,
		hashtable:lookupEntry( ?MyFirstKey, MyH6 ) ] ),

	hashtable_key_not_found = hashtable:lookupEntry( ?MyFirstKey, MyH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	hashtable:display( "Hashtable", MyH6 ),

	test_facilities:display( "" ),

	test_facilities:display( "Looking up for ~s in tracked hashtable: ~p",
		[ ?MyFirstKey,
		 tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH5 ) ] ),

	{ value, "MyFirstValue" } =
		tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH5 ),

	test_facilities:display( "Removing that entry." ),
	MyTH6 = tracked_hashtable:removeEntry( ?MyFirstKey, MyTH5 ),
	false = tracked_hashtable:isEmpty( MyTH6 ),

	test_facilities:display( "Looking up for ~s in tracked hashtable: ~p",
		[ ?MyFirstKey,
		 tracked_hashtable:lookupEntry( ?MyFirstKey, MyTH6 ) ] ),

	hashtable_key_not_found = tracked_hashtable:lookupEntry( ?MyFirstKey,
															MyTH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	tracked_hashtable:display( "Tracked hashtable", MyTH6 ),

	test_facilities:display( "" ),
	test_facilities:display( "Looking up for ~s in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH5 ) ] ),

	{ value, "MyFirstValue" } = lazy_hashtable:lookupEntry( ?MyFirstKey,
														   MyLH5 ),

	test_facilities:display( "Removing that entry." ),
	MyLH6 = lazy_hashtable:removeEntry( ?MyFirstKey, MyLH5 ),
	false = lazy_hashtable:isEmpty( MyLH6 )
		,
	test_facilities:display( "Looking up for ~s in lazy hashtable: ~p",
		[ ?MyFirstKey, lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH6 ) ] ),

	hashtable_key_not_found = lazy_hashtable:lookupEntry( ?MyFirstKey, MyLH6 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	lazy_hashtable:display( "Lazy hashtable", MyLH6 ),

	test_facilities:display( "" ),
	test_facilities:display( "Testing double key registering." ),

	test_facilities:display( "" ),
	MyH7 = hashtable:addEntry( ?MyThirdKey, anything, MyH6 ),
	hashtable:display( MyH7 ),

	test_facilities:display( "Enumerating the hash table: ~p",
		[ hashtable:enumerate( MyH6 ) ] ),

	test_facilities:display( "Listing the hash table keys: ~p",
		[ hashtable:keys( MyH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 hashtable:keys( MyH6 ) ),

	MyH8 = hashtable:addEntries( [ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyH7 ),

	MyH9 = hashtable:merge( MyH4, MyH8 ),
	test_facilities:display( "Merged table: ~s, size of buckets is ~B.",
		[ hashtable:toString( MyH9 ), hashtable:get_bucket_count( MyH9 ) ] ),

	MyH10 = hashtable:optimise( MyH9 ),
	test_facilities:display( "The optimised table: ~s size of buckets is ~B.",
		[ hashtable:toString( MyH10 ), hashtable:get_bucket_count( MyH10 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in tracked table ~p:"
					"~n ~p", [ Keys, hashtable:selectEntries( Keys, MyH10 ) ] ),

	test_facilities:display(""),

	MyTH7 = tracked_hashtable:addEntry( ?MyThirdKey, anything, MyTH6 ),
	tracked_hashtable:display( MyTH7 ),

	test_facilities:display( "Enumerating the tracked hash table: ~p.",
		[ tracked_hashtable:enumerate( MyTH6 ) ] ),

	test_facilities:display( "Listing the tracked table keys: ~p.",
		[ tracked_hashtable:keys( MyTH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey,?MyThirdKey ],
										 tracked_hashtable:keys( MyTH6 ) ),

	MyTH8 = tracked_hashtable:addEntries(
							  [ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyTH7 ),

	MyTH9 = tracked_hashtable:merge( MyTH4, MyTH8 ),

	test_facilities:display( "Merged tracked table: ~s.",
				[ tracked_hashtable:toString( MyTH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p in tracked table:"
		" ~n ~p", [ Keys, tracked_hashtable:selectEntries( Keys, MyTH9 ) ] ),


	test_facilities:display( "" ),
	MyLH7 = lazy_hashtable:addEntry( ?MyThirdKey, anything, MyLH6 ),
	lazy_hashtable:display( MyLH7 ),

	test_facilities:display( "Enumerating the lazy table: ~p.",
		[ lazy_hashtable:enumerate( MyLH6 ) ] ),

	test_facilities:display( "Listing the lazy table keys: ~p.",
		[ lazy_hashtable:keys( MyLH6 ) ] ),

	true = list_utils:unordered_compare( [ ?MySecondKey, ?MyThirdKey ],
										 lazy_hashtable:keys( MyLH6 ) ),

	MyLH8 = lazy_hashtable:addEntries(
			[ {?MyThirdKey,3}, {?MyFourthKey,4} ], MyLH7 ),

	MyLH9 = lazy_hashtable:merge( MyLH4, MyLH8 ),


	test_facilities:display( "Merged lazy table: ~s.",
			[ lazy_hashtable:toString( MyLH9 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys in lazy table ~p:"
		"~n ~p", [ Keys, lazy_hashtable:selectEntries( Keys, MyLH9 ) ] ),





	test_facilities:stop().
