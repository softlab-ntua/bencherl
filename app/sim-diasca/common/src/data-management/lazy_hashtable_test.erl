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

% Creation date: January 4, 2012.
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Unit tests for the lazy hashtable implementation.
%
% See the lazy_hashtable.erl tested module.
%
-module(lazy_hashtable_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-define(MyFirstKey,  'MyFirstKey').
-define(MySecondKey, 'MySecondKey').
-define(MyThirdKey,  'MyThirdKey').



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyH1 = lazy_hashtable:new( 10 ),

	true = lazy_hashtable:isEmpty( MyH1 ),

	lazy_hashtable:display( "Vanilla lazy hashtable", MyH1 ),

	%lazy_hashtable:display( MyH1 ),
	test_facilities:display( "Adding entries in lazy hashtable." ),
	MyH2 = lazy_hashtable:new( 4 ),
	MyH3 = lazy_hashtable:addEntry( ?MyFirstKey, "MyFirstValue", MyH2 ),
	false = lazy_hashtable:isEmpty( MyH3 ),

	MyH4 = lazy_hashtable:addEntry( ?MySecondKey, [ 1, 2, 3 ], MyH3 ),
	false = lazy_hashtable:isEmpty( MyH4 ),

	lazy_hashtable:display( "The lazy hashtable", MyH4 ),

	MyH4Size = lazy_hashtable:size( MyH4 ),
	test_facilities:display( "Size of table '~s': ~B entries",
							 [ lazy_hashtable:toString( MyH4 ), MyH4Size ] ),

	test_facilities:display( "Looking up for ~s: ~p", [ ?MyFirstKey,
			lazy_hashtable:lookupEntry( ?MyFirstKey, MyH4 ) ] ),

	{ value, "MyFirstValue" } = lazy_hashtable:lookupEntry( ?MyFirstKey,
															MyH4 ),

	test_facilities:display( "Removing that entry." ),
	MyH5 = lazy_hashtable:removeEntry( ?MyFirstKey, MyH4 ),
	false = lazy_hashtable:isEmpty( MyH5 ),

	test_facilities:display( "Extracting the same entry from "
							 "the same initial table." ),
	{ "MyFirstValue", MyH5 } = lazy_hashtable:extractEntry( ?MyFirstKey, MyH4 ),

	test_facilities:display( "Looking up for ~s: ~p", [ ?MyFirstKey,
		lazy_hashtable:lookupEntry( ?MyFirstKey, MyH5 ) ] ),

	hashtable_key_not_found = lazy_hashtable:lookupEntry( ?MyFirstKey, MyH5 ),

	% removeEntry can also be used if the specified key is not here, will return
	% an identical table.
	lazy_hashtable:display( MyH5 ),
	test_facilities:display( "Testing double key registering." ),

	MyH6 = lazy_hashtable:addEntry( ?MySecondKey, anything, MyH5 ),
	lazy_hashtable:display( MyH6 ),

	test_facilities:display( "Enumerating the hashtable: ~p.",
		[ lazy_hashtable:enumerate( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable keys: ~p.",
		[ lazy_hashtable:keys( MyH4 ) ] ),

	test_facilities:display( "Listing the hashtable values: ~p",
		[ lazy_hashtable:values( MyH4 ) ] ),

	test_facilities:display( "Applying a fun to all values of "
							 "previous hashtable" ),


	FunValue = fun( V ) ->
				io:format( " - hello value '~p'!~n", [ V ] ),
				% Unchanged here:
				V
	end,

	lazy_hashtable:mapOnValues( FunValue, MyH4 ),


	test_facilities:display( "Applying a fun to all entries of "
							 "previous hashtable:" ),

	FunEntry = fun( E={ K, V } ) ->
				io:format( " - hello, key '~p' associated to value '~p'!~n",
						   [ K, V ] ),
				% Unchanged here:
				E
	end,

	lazy_hashtable:mapOnEntries( FunEntry, MyH4 ),


	test_facilities:display( "Folding on the same initial hashtable to "
							 "count the number of entries." ),

	FunCount = fun( _Entry, AccCount ) ->
					   AccCount + 1
			   end,

	2 = lazy_hashtable:foldOnEntries( FunCount, _InitialCount=0, MyH4 ),

	0 = lazy_hashtable:foldOnEntries( FunCount, _InitialCount=0, MyH1 ),


	true = list_utils:unordered_compare( [ ?MyFirstKey, ?MySecondKey ],
										 lazy_hashtable:keys( MyH4 ) ),

	MyH7 = lazy_hashtable:addEntry( ?MyThirdKey, 3, MyH6 ),

	% MyH8 should have { AnotherKey, [1,2,3] } and { ?MyThirdKey, 3 }:
	MyH8 = lazy_hashtable:merge( MyH4, MyH7 ),

	% Any optimisation would be automatic:
	test_facilities:display( "Merged table: ~s.",
							 [ lazy_hashtable:toString( MyH8 ) ] ),

	Keys = [ ?MyFirstKey, ?MyThirdKey ],

	test_facilities:display( "Listing the entries for keys ~p:~n ~p",
					[ Keys, lazy_hashtable:selectEntries( Keys, MyH8 ) ] ),

	test_facilities:stop().
