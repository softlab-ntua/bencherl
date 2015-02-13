% Copyright (C) 2011-2014 Olivier Boudeville
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

% Creation date: November 10, 2011.
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Implementation of an hashtable which optimizes itself lazily: the decision to
% perform a check for optimization is made based on the number of operations
% triggered since last check. It allows to avoid triggering these checkings too
% frequently or to spend too much resources determining whether a check should
% be done.
%
% See lazy_table_test.erl for the corresponding test.
% See hashtable.erl


% A lazy_hashtable is a { Hashtable, NumberOfOperations } pair where:
%
% - Hashtable is an hashtable, refer to hashtable.erl for more detail
%
% - NumberOfOperations represents the number of operations on the corresponding
% hashtable performed since the last optimization check, so that the next check
% can be triggered appropriately
%
%
% Note: we provide different three types of hashtables:
%
% - 'hashtable', the most basic, safest, reference implementation
%
% - 'tracked_hashtable', an attempt of optimisation of it (not necessarily the
% best)
%
% - 'lazy_hashtable' (this module), which is probably the most efficient
% implementation, supposedly as reliable as hashtable
%
% They are to provide the same API (signatures and contracts).
%
-module(lazy_hashtable).

% Directly depends on the hashtable module.


% Exact same API as the one of hashtable:
%
-export([ new/0, new/1, addEntry/3, addEntries/2,
		  removeEntry/2, lookupEntry/2, hasEntry/2,
		  getEntry/2, extractEntry/2,
		  addToEntry/3, subtractFromEntry/3, toggleEntry/2,
		  appendToEntry/3, deleteFromEntry/3, popFromEntry/2,
		  enumerate/1, selectEntries/2, keys/1, values/1,
		  isEmpty/1, size/1, getEntryCount/1,
		  mapOnEntries/2, mapOnValues/2,
		  foldOnEntries/3,
		  merge/2, optimise/1, toString/1, toString/2, display/1, display/2 ]).


% These functions are exported only to ease the tracked_hashtable
% implementation:
%
% (so that we can switch implementations)
%
-export([ new_with_buckets/1 ]).


% Records the number of changes operated on this hashtable since its last
% optimisation:
%
-type operation_count() :: non_neg_integer().


-opaque lazy_hashtable() :: { hashtable:hashtable(), operation_count() }.
%-type lazy_hashtable() :: { hashtable:hashtable(), operation_count() }.

-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().


-export_type([ lazy_hashtable/0, key/0, value/0, entry/0 ]).


% We want to be able to use our size/1 from here as well:
-compile( { no_auto_import, [ size/1 ] } ).


% Implementation notes:
%
% Only when the number of operation on internal hashtable content exceeds a
% specified threshold (by default, is 50), the necessary of effectuating a
% optimisation on the internal hashtable is verified and it is optimisated in
% case of necessary.


% For operation count trigger setting:
%
-include("lazy_hashtable.hrl").


% Returns a new empty lazy table.
%
-spec new() -> lazy_hashtable().
new() ->

	% Starts at an average size:
	NumberOfBuckets = ?operation_count_trigger div 2,

	NewHashtable = hashtable:new_with_buckets( NumberOfBuckets ),

	{ NewHashtable, _InitialOpCount=0 }.



% As lazy hashtables manage by themselves their size, no need to specify any
% target size. This function is only defined so that we can transparently switch
% APIs with the hashtable module.
%
-spec new( hashtable:entry_count() | hashtable:entries() ) -> lazy_hashtable().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	new();

new( InitialEntries ) when is_list( InitialEntries ) ->

	BlankTable = new(),

	addEntries( InitialEntries, BlankTable ).



% Defined also to allow seamless change of hashtable modules:
%
% (helper)
%
-spec new_with_buckets( hashtable:bucket_count() ) -> lazy_hashtable().
new_with_buckets( _NumberOfBuckets ) ->
	new().



% Adds specified key/value pair into the specified lazy hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
% If the count of performed operations since last optimization check exceeds the
% specified threshold, we will verify whether an hashtable optimization is
% necessary and, if yes, it will be performed.
%
-spec addEntry( key(), value(), lazy_hashtable() ) -> lazy_hashtable().
addEntry( Key, Value, _LazyHashtable={ Hashtable, OpCount } ) ->

	AugmentedTable = hashtable:addEntry( Key, Value, Hashtable ),

	% Optimization to be performed if enough operations were done:
	optimise_table_if_necessary( { AugmentedTable, OpCount + 1 } ).



% Adds specified list of key/value pairs into the specified lazy table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntries( hashtable:entries(), lazy_hashtable() ) -> lazy_hashtable().
addEntries( EntryList, _LazyHashtable={ Hashtable, OpCount } ) ->

	AugmentedTable = hashtable:addEntries( EntryList, Hashtable ),

	% This may lead to a count vastly greater than the threshold, but it is not
	% a problem:
	%
	UpdatedOpCount = OpCount + length( EntryList ),

	optimise_table_if_necessary( { AugmentedTable, UpdatedOpCount } ).



% Removes specified key/value pair, as designated by the key, from the specified
% lazy hashtable.
%
% Does nothing if the key is not found.
%
% Returns an updated lazy table.
%
-spec removeEntry( key(), lazy_hashtable() ) -> lazy_hashtable().
removeEntry( Key, _LazyHashtable={ Hashtable, OpCount } ) ->

	UpdatedTable = hashtable:removeEntry( Key, Hashtable ),

	optimise_table_if_necessary( { UpdatedTable, OpCount + 1 } ).



% Looks-up specified entry (designated by its key) in specified lazy table.
%
% Returns either 'hashtable_key_not_found' if no such key is registered in the
% table, or { value, Value }, with Value being the value associated to the
% specified key.
%
-spec lookupEntry( key(), lazy_hashtable() ) ->
						 'hashtable_key_not_found' | { 'value', value() }.
lookupEntry( Key, _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:lookupEntry( Key, Hashtable ).



% Tells whether the specified key exists in the table: returns true or false.
%
-spec hasEntry( key(), lazy_hashtable() ) -> boolean().
hasEntry( Key, _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:hasEntry( Key, Hashtable ).



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise a bad match is
% triggered.
%
-spec getEntry( key(), lazy_hashtable() ) -> value().
getEntry( Key, _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:getEntry( Key, Hashtable ).



% Extracts specified entry from specified hashtable, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec extractEntry( key(), lazy_hashtable() ) -> { value(), lazy_hashtable() }.
extractEntry( Key, _LazyHashtable={ Hashtable, OpCount } ) ->

	{ Value, NewHashtable } = hashtable:extractEntry( Key, Hashtable ),

	NewLazyTable = { NewHashtable, OpCount + 1 },

	{ Value, NewLazyTable }.



% Applies (maps) the specified anonymous function to each of the key-value
% entries contained in this hashtable.
%
% Allows to apply "in-place" an operation on all entries without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: as the fun may return modified keys, the whole structure of the
% hashtable may change (ex: different buckets used for replaced entries,
% colliding keys resulting in having less entries afterwards, etc.).
%
% One may request the returned hashtable to be optimised after this call.
%
-spec mapOnEntries( fun( ( entry() ) -> entry() ), lazy_hashtable() ) ->
						  lazy_hashtable().
mapOnEntries( Fun, _LazyHashtable={ Hashtable, OpCount }  ) ->

	NewHashtable = hashtable:mapOnEntries( Fun, Hashtable ),

	{ NewHashtable, OpCount }.



% Applies (maps) the specified anonymous function to each of the values
% contained in this hashtable.
%
% Allows to apply "in-place" an operation on all values without having to
% enumerate the content of the hashtable and iterate on it (hence without having
% to duplicate the whole content in memory).
%
% Note: the keys are left as are, hence the structure of the hashtable does not
% change.
%
-spec mapOnValues( fun( ( value() ) -> value() ), lazy_hashtable() ) ->
						 lazy_hashtable().
mapOnValues( Fun, _LazyHashtable={ Hashtable, OpCount } ) ->

	NewHashtable = hashtable:mapOnValues( Fun, Hashtable ),

	{ NewHashtable, OpCount }.



% Folds specified anonymous function on all entries of the specified lazy
% hashtable.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec foldOnEntries( fun( ( entry(), basic_utils:accumulator() )
						  -> basic_utils:accumulator() ),
					 basic_utils:accumulator(),
					 lazy_hashtable() ) ->
						   basic_utils:accumulator().
foldOnEntries( Fun, InitialAcc, _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:foldOnEntries( Fun, InitialAcc, Hashtable ).



% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no addition can be performed on the associated value.
%
-spec addToEntry( key(), number(), lazy_hashtable() ) -> lazy_hashtable().
addToEntry( Key, Value, LazyHashtable ) ->

	case lookupEntry( Key, LazyHashtable ) of

		{ value, Number } ->
			addEntry( Key, Number + Value, LazyHashtable );

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no subtraction can be performed on the associated value.
%
-spec subtractFromEntry( key(), number(), lazy_hashtable() )
					   -> lazy_hashtable().
subtractFromEntry( Key, Value, LazyHashtable ) ->

	case lookupEntry( Key, LazyHashtable ) of

		{ value, Number } ->
			addEntry( Key, Number - Value, LazyHashtable );

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% An exception is thrown if the key does not exist or if its associated value is
% not a boolean.
%
-spec toggleEntry( key(), lazy_hashtable() ) -> lazy_hashtable().
toggleEntry( Key, _LazyHashtable={ Hashtable, OpCount } ) ->
	{ hashtable:toggleEntry( Key, Hashtable ), OpCount + 1 }.




% Returns a new lazy hashtable, which started from LazyHashtableBase and was
% enriched with the LazyHashtableAdd entries whose keys where not already in
% LazyHashtableBase (if a key is in both tables, the one from LazyHashtableBase
% will be kept).
%
% May trigger an automatic optimization.
%
-spec merge( lazy_hashtable(), lazy_hashtable() ) -> lazy_hashtable().
merge( _LazyHashtableBase={ HashtableBase, BaseOptCount },
	   _LazyHashtableAdd={ HashtableAdd, _HashOptCount } ) ->

	UpdatedHashtable = hashtable:merge( HashtableBase, HashtableAdd ),

	% The entry count in the added hashtable corresponds to the number of
	% operations performed during the merge:
	%
	EntryCountInAddedTable = hashtable:getEntryCount( HashtableAdd ),

	UpdatedOpCount = BaseOptCount + EntryCountInAddedTable,

	optimise_table_if_necessary( { UpdatedHashtable, UpdatedOpCount } ).



% Optimises this hashtable.
%
% A no-operation for lazy hashtables.
%
-spec optimise( lazy_hashtable() ) -> lazy_hashtable().
optimise( Hashtable ) ->
	Hashtable.



% Checks whether an optimisation of the internal hashtable is deemed potentially
% useful, i.e. if the operation count is past a threshold.
%
% Returns { H, NewOpCount } (i.e. a lazy table) where H is either an optimised
% hashtable or the specified one, and NewOpCount is either zero or the specified
% CurrentOpCount, depending on whether an optimisation was triggered or not.
%
-spec optimise_table_if_necessary( lazy_hashtable() ) -> lazy_hashtable().
optimise_table_if_necessary( LazyTable={ Hashtable, CurrentOpCount } ) ->

	case ( CurrentOpCount >= ?operation_count_trigger ) of

		true ->

			% Like getEntryCount but allows to re-use Entries:
			Entries = hashtable:enumerate( Hashtable ),
			EntryCount = length( Entries ),

			% Number of elements of the underlying tuple:
			BucketCount = hashtable:get_bucket_count( Hashtable ),

			case hashtable:must_optimise( EntryCount, BucketCount ) of

				true ->
					%io:format( "Lazy table is optimised.~n" ),
					NewHashtable = hashtable:optimise_unconditionally(
								EntryCount, BucketCount, Entries, Hashtable ),
					{ NewHashtable, _NewOpCount=0 };

				false ->
					LazyTable

			end;

		false ->

			% The operation count since last optimization does not exceed yet
			% the given threshold, there is no need to optimize:
			%
			LazyTable

	end.



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToEntry( key(), term(), lazy_hashtable() )
	-> lazy_hashtable().
appendToEntry( Key, Element, LazyHashtable ) ->

	case lookupEntry( Key, LazyHashtable ) of

		{ value, List } ->
			addEntry( Key, [ Element | List ], LazyHashtable );

		hashtable_key_not_found ->
			throw( { hashtable_key_not_found, Key } )

	end.



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
-spec deleteFromEntry( key(), term(), lazy_hashtable() ) -> lazy_hashtable().
deleteFromEntry( Key, Element, LazyHashtable ) ->

	case lookupEntry( Key, LazyHashtable ) of

		{ value, List } ->
			addEntry( Key, lists:delete( Element, List ), LazyHashtable );

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable.
%
-spec popFromEntry( key(), lazy_hashtable() ) ->
						  { term(), lazy_hashtable() }.
popFromEntry( Key, LazyHashtable ) ->

	case lookupEntry( Key, LazyHashtable ) of

		{ value, [ H | T ] } ->
			{ H, addEntry( Key, T, LazyHashtable ) };

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Returns a flat list whose elements are all the key/value pairs of the
% hashtable, in no particular order.
%
% Ex: [ {K1,V1}, {K2,V2}, ... ].
%
-spec enumerate( lazy_hashtable() ) -> hashtable:entries().
enumerate( _LazyHashtable={ Hashtable, _OpCount } ) ->
	lists:flatten( tuple_to_list( Hashtable ) ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec selectEntries( [ key() ], lazy_hashtable() ) -> hashtable:entries().
selectEntries( Keys, _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:selectEntries( Keys, Hashtable ).


% Returns a list containing all the keys of this hashtable.
%
-spec keys( lazy_hashtable() ) -> [ key() ].
keys( _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:keys( Hashtable ).


% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( lazy_hashtable() ) -> [ value() ].
values( _LazyHashtable={ Hashtable, _OpCount }  ) ->
	hashtable:values( Hashtable ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
%
-spec isEmpty( lazy_hashtable() ) -> boolean().
isEmpty( _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:isEmpty( Hashtable ).



% Returns the size (number of entries) of this hashtable.
%
-spec size( lazy_hashtable() ) -> hashtable:entry_count().
size( _LazyTable={ Hashtable, _CurrentOpCount } ) ->
	hashtable:size( Hashtable ).



% Returns the number of entries (key/value pairs) stored in the specified lazy
% hashtable.
%
-spec getEntryCount( lazy_hashtable() ) -> hashtable:entry_count().
getEntryCount( LazyHashtable ) ->
	size( LazyHashtable ).



% Returns a textual description of the specified hashtable.
%
-spec toString( lazy_hashtable() ) -> string().
toString( _LazyHashtable={ Hashtable, _OpCount } ) ->
	hashtable:toString( Hashtable ).



% Returned string is either quite raw (if using 'internal') or a bit more
% elaborate (if using 'user_friendly').
%
-spec toString( lazy_hashtable(), 'internal' | 'user_friendly' ) -> string().
toString( _LazyHashtable={ Hashtable, _OpCount }, DescriptionType ) ->
	hashtable:toString( Hashtable, DescriptionType ).



% Displays the specified hashtable on the standard output.
%
-spec display( lazy_hashtable() ) -> basic_utils:void().
display( _LazyHashtable={ Hashtable, OpCount } ) ->

	hashtable:display( Hashtable ),

	display_operation_count( OpCount ) .



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( string(), lazy_hashtable() ) -> basic_utils:void().
display( Title, _LazyHashtable={ Hashtable, OpCount } ) ->

	hashtable:display( Title, Hashtable ),

	display_operation_count( OpCount ) .




% Section for helper functions.


display_operation_count( OpCount ) ->

	case OpCount of

		?operation_count_trigger ->
			io:format( "No operation performed on this lazy hashtable since "
					   "last optimisation.~n" );

		_Other ->
			io:format( "~B operation(s) have been performed on this lazy "
					   "hashtable since last optimisation.~n", [ OpCount ] )

	end.
