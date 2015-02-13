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
%
% Creation date: November 10, 2011.
% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Tracked hashtable implementation.
%
% See tracked_hashtable_test.erl for the corresponding test.
% See hashtable.erl
%
%
% Note: we provide different three types of hashtables:
%
% - 'hashtable', the most basic, safest, reference implementation
%
% - 'tracked_hashtable' (this module), an attempt of optimisation of it (not
% necessarily the best)
%
% - 'lazy_hashtable', which is probably the most efficient implementation
%
% They are to provide the same API (signatures and contracts).
%
% However this tracked version is deemed less effective than the lazy version,
% and thus is not updated/tested as much as the others (for example: error cases
% have not been uniformised, insofar that they can still issue badmatches while
% other implementations raise more proper exceptions).



% A tracked_hashtable is a { Hashtable, NumberOfEntries, NumberOfBuckets }
% triplet where:
%
% - Hashtable is a hashtable(), refer to the hashtable module for more detail
%
% - NumberOfEntries represents the number of entries in the hashtable; it is
% zero when a new hashtable is created
%
% - NumberOfBuckets is the number of buckets of the internal hashtable; a
% default number of buckets is chosen at the creation of a new hashtable
%
% Directly depends on the hashtable module.
%
-module(tracked_hashtable).


% Same as hashtable:
-export([ new/0, new/1, new_with_buckets/1, addEntry/3, addEntries/2,
		  removeEntry/2, lookupEntry/2, hasEntry/2,
		  getEntry/2, extractEntry/2,
		  addToEntry/3, subtractFromEntry/3, toggleEntry/2,
		  appendToEntry/3, deleteFromEntry/3, popFromEntry/2,
		  enumerate/1, selectEntries/2, keys/1, values/1,
		  isEmpty/1, size/1, getEntryCount/1,
		  mapOnEntries/2, mapOnValues/2,
		  foldOnEntries/3,
		  merge/2, optimise/1, toString/1, toString/2, display/1, display/2 ]).


%-type tracked_hashtable() :: { hashtable:hashtable(), hashtable:entry_count(),
%	hashtable:bucket_count() }.

-opaque tracked_hashtable() :: { hashtable:hashtable(), hashtable:entry_count(),
	hashtable:bucket_count() }.


-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().


-export_type([ tracked_hashtable/0, key/0, value/0, entry/0 ]).


% We want to be able to use our size/1 from here as well:
-compile( { no_auto_import, [ size/1 ] } ).



% Implementation notes:
%
% - each time the content of the internal hashtable is modified, the meta-data
% must be updated; if the number of entries stored in the table changed, the
% load factor of this hashtable is updated; if it is not anymore between the
% accepted bounds, the hashtable is then be optimised




% Creates a new empty tracker table.
%
% A new empty tracked hashtable is returned.
%
-spec new() -> tracked_hashtable().
new() ->
	% Starts at minimal size, otherwise will soon be shrunk anyway:
	NewHashtable = hashtable:new_with_buckets( 1 ),
	{ NewHashtable, _EntryCount=0, _BucketCount=1 }.



% As tracked hashtables manage by themselves their size, no need to specify any
% target size. This function is only defined so that we can transparently switch
% APIs with the hashtable module.
%
new( _ExpectedNumberOfEntries ) ->
	new().



% Defined also to allow seamless change of hashtable modules:
%
new_with_buckets( _NumberOfBuckets ) ->
	new().


% Adds specified key/value pair into the specified tracked hash table.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
% As the load factor of the tracked hashtable is verified at each additional
% entry, the tracked hashtable is optimised as soon as possible.
%
-spec addEntry( key(), value(), tracked_hashtable() )
		-> tracked_hashtable().
addEntry( Key, Value,
		 _TrackedHashtable={ Hashtable, EntryCount, NumberOfBuckets } ) ->

	% We pay inter-module calls to preserve hashtable opaqueness.

	% A problem is that we must distinguish between a new key being added
	% (impacting the load factor) or being updated (load factor unchanged).

	{ Newhashtable, Diagnosis } = hashtable:addDiagnosedEntry( Key, Value,
			Hashtable ),

	% Should a new element be added, we verify and optimise the hashtable if
	% necessary:
	%
	case Diagnosis of

		updated ->
			% Bucket size did not change, so we just updated an existing entry,
			% and there is no need to optimise:
			{ Newhashtable, EntryCount, NumberOfBuckets };

		added ->

			% Here a new key has been added, we might have to optimise:
			case hashtable:must_optimise( EntryCount, NumberOfBuckets ) of

				true ->

					Entries = hashtable:enumerate( Newhashtable ),

					OptimisedTable = hashtable:optimise_unconditionally(
						 EntryCount+1, NumberOfBuckets, Entries,
						 Newhashtable ),

					{ OptimisedTable, EntryCount+1,
					  hashtable:get_bucket_count( OptimisedTable ) };

				false ->
					{ Newhashtable, EntryCount+1, NumberOfBuckets }

			end

	end.



% Adds specified list of key/value pairs into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntries( hashtable:entries(), tracked_hashtable() )
	-> tracked_hashtable().
addEntries( EntryList,
		_TrackedHashtable={ Hashtable, _EntryCount, NumberOfBuckets } ) ->

	% We want to optimise only at end (to avoid useless reshuffles with longer
	% entry lists) yet counting the total number of entries correctly
	% (w.r.t. duplicated keys):
	%
	AugmentedTable = hashtable:addEntries( EntryList, Hashtable ),

	Entries = hashtable:enumerate( AugmentedTable ),

	% Depends on key collisions (cannot be predicted):
	NewEntryCount = length( Entries ),

	case hashtable:must_optimise( NewEntryCount, NumberOfBuckets ) of

		true ->

			OptimisedTable = hashtable:optimise_unconditionally( NewEntryCount,
						NumberOfBuckets, Entries, AugmentedTable ),
			{ OptimisedTable, NewEntryCount,
			  hashtable:get_bucket_count( OptimisedTable ) };

		false ->

			{ AugmentedTable, NewEntryCount, NumberOfBuckets }

	end.




% Removes specified key/value pair from the specified hash table.
%
% Does nothing if the key is not found.
%
-spec removeEntry( key(), tracked_hashtable() ) ->
						 tracked_hashtable().
removeEntry( Key, TrackedHashtable={ Hashtable, EntryCount, BucketCount } ) ->

	case hashtable:removeDiagnosedEntry( Key, Hashtable ) of

		{ deleted, NewTable } ->

			NewEntryCount = EntryCount - 1,

			% Here an entry has been removed, we might have to optimise:
			case hashtable:must_optimise( NewEntryCount, BucketCount ) of

				true ->

					Entries = hashtable:enumerate( NewTable ),

					OptimisedTable = hashtable:optimise_unconditionally(
						 NewEntryCount, BucketCount, Entries, NewTable ),

					{ OptimisedTable, NewEntryCount,
					  hashtable:get_bucket_count( OptimisedTable ) };

				false ->
					{ NewTable, NewEntryCount, BucketCount }

			end;

		unchanged ->
			TrackedHashtable

	end.



% Looks-up specified entry (designated by its key) in specified tracked
% hashtable.
%
% Returns either 'hashtable_key_not_found' if no such key is registered in the
% table, or {value,Value}, with Value being the value associated to the
% specified key.
%
-spec lookupEntry( key(), tracked_hashtable() ) ->
	'hashtable_key_not_found' | { 'value', value() }.
lookupEntry( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:lookupEntry( Key, Hashtable ).



% Looks-up specified entry (designated by its key) in specified tracked
% hashtable: returns eigher true or false.
-spec hasEntry( key(), tracked_hashtable() ) -> boolean().
hasEntry( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:hasEntry( Key, Hashtable ).



% Retrieves the value corresponding to specified key and returns it directly.
%
% The key/value pair is expected to exist already, otherwise a bad match is
% triggered.
%
-spec getEntry( key(), tracked_hashtable() ) -> value().
getEntry( Key, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:getEntry( Key, Hashtable ).



% Extracts specified entry from specified hashtable, i.e. returns the associated
% value and removes that entry from the table.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec extractEntry( key(), tracked_hashtable() ) ->
						  { value(), tracked_hashtable() }.
extractEntry( Key, _TrackedHashtable={ Hashtable, NEnt, NBuck } ) ->

	{ Value, NewHashtable } = hashtable:extractEntry( Key, Hashtable ),

	NewTrackedTable = { NewHashtable, NEnt - 1, NBuck },

	{ Value, NewTrackedTable }.



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
-spec mapOnEntries( fun( ( entry() ) -> entry() ), tracked_hashtable() ) ->
						  tracked_hashtable().
mapOnEntries( Fun, _TrackedHashtable={ Hashtable, _NEnt, _NBuck }  ) ->

	NewHashtable = hashtable:mapOnEntries( Fun, Hashtable ),

	% Might have changed as well:
	NEnt = hashtable:size( NewHashtable ),
	NBuck = hashtable:get_bucket_count( NewHashtable ),

	{ NewHashtable, NEnt, NBuck }.



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
-spec mapOnValues( fun( ( value() ) -> value() ), tracked_hashtable() ) ->
						  tracked_hashtable().
mapOnValues( Fun, _TrackedHashtable={ Hashtable, NEnt, NBuck }  ) ->

	NewHashtable = hashtable:mapOnValues( Fun, Hashtable ),

	{ NewHashtable, NEnt, NBuck }.



% Folds specified anonymous function on all entries of the specified tracked
% hashtable.
%
% The order of transformation for entries is not specified.
%
% Returns the final accumulator.
%
-spec foldOnEntries( fun( ( entry(), basic_utils:accumulator() )
						  -> basic_utils:accumulator() ),
					 basic_utils:accumulator(),
					 tracked_hashtable() ) ->
						   basic_utils:accumulator().
foldOnEntries( Fun, InitialAcc, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:foldOnEntries( Fun, InitialAcc, Hashtable ).



% Returns a new tracked hashtable, which started from TrackedHashtableBase and
% was enriched with the TrackedHashtableAdd entries whose keys where not
% already in TrackedHashtableBase (if a key is in both tables, the one from
% TrackedHashtableBase will be kept).
%
-spec merge( tracked_hashtable(), tracked_hashtable() ) -> tracked_hashtable().
merge( _TrackedHashtableBase={ HashtableBase, _NEntB, _NBuckB },
	 _TrackedHashtableAdd={ HashtableAdd, _NEntA, _NBuckA } ) ->

	UpdatedHashtable = hashtable:merge( HashtableBase, HashtableAdd ),
	Entries = hashtable:enumerate( UpdatedHashtable ),

	% Depends on key collisions (cannot be predicted):
	NewEntryCount = length( Entries ),

	UpdatedTableSize = hashtable:get_bucket_count( UpdatedHashtable ),

	case hashtable:must_optimise( NewEntryCount, UpdatedTableSize ) of

		true ->
			OptimisedTable = hashtable:optimise_unconditionally( NewEntryCount,
					UpdatedTableSize, Entries, UpdatedHashtable ),

			{ OptimisedTable, NewEntryCount,
			  hashtable:get_bucket_count( OptimisedTable ) };

		false ->
			{ UpdatedHashtable, NewEntryCount, UpdatedTableSize }

	end.




% Optimises this hashtable.
%
% A no-operation for tracked hashtables.
%
-spec optimise( tracked_hashtable() ) -> tracked_hashtable().
optimise( Hashtable ) ->
	Hashtable.



% Adds a specified value to the value of specified key, supposed the existing
% one to be numerical,
%
% A case clause is triggered if the key did not exist; a bad arithm is triggered
% if no addition can be performed on the associated value.
%
-spec addToEntry( key(), number(), tracked_hashtable() )
	-> tracked_hashtable().
addToEntry( Key, Value, TrackedHashtable ) ->
	{ value, Number } = lookupEntry( Key, TrackedHashtable ),
	addEntry( Key, Number+Value, TrackedHashtable ).



% Subtracts specified value to the value, supposed to be numerical, associated
% to specified key.
%
% A case clause is triggered if the key did not exist, a bad arithm is triggered
% if no subtraction can be performed on the associated value.
%
-spec subtractFromEntry( key(), number(), tracked_hashtable() )
	-> tracked_hashtable().
subtractFromEntry( Key, Value, TrackedHashtable ) ->
	{ value, Number } = lookupEntry( Key, TrackedHashtable ),
	addEntry( Key, Number-Value, TrackedHashtable ).



% Toggles the boolean value associated with specified key: if true will be
% false, if false will be true.
%
% A case clause is triggered if the entry does not exist or it is not a boolean
% value.
%
-spec toggleEntry( key(), tracked_hashtable() )
	-> tracked_hashtable().
toggleEntry( Key, _TrackedHashtable={ Hashtable, EntryCount, NumberOfBuckets } )
		->

	{ hashtable:toggleEntry( Key, Hashtable ), EntryCount, NumberOfBuckets }.



% Appends specified element to the value of specified key, supposing the value
% to be a list.
%
% A case clause is triggered if the entry does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToEntry( key(), term(), tracked_hashtable() )
				   -> tracked_hashtable().
appendToEntry( Key, Element, TrackedHashtable ) ->
	{ value, List } = lookupEntry( Key, TrackedHashtable ),
	addEntry( Key, [ Element | List ], TrackedHashtable ).



% Deletes the first match of specified element from the value associated to
% specified key, that value being supposed to be a list.
%
% A case clause is triggered if the entry did not exist.
% If the element is not in the specified list, the list will not be modified.
%
-spec deleteFromEntry( key(), term(), tracked_hashtable() )
	-> tracked_hashtable().
deleteFromEntry( Key, Element, TrackedHashtable ) ->
	{ value, List } = lookupEntry( Key, TrackedHashtable ),
	addEntry( Key, lists:delete( Element, List ), TrackedHashtable ).



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and the new hashtable.
%
-spec popFromEntry( key(), tracked_hashtable() ) ->
						  { term(), tracked_hashtable() }.
popFromEntry( Key, TrackedHashtable ) ->
	{ value, [ H | T ] } = lookupEntry( Key, TrackedHashtable ),
	{ H, addEntry( Key, T, TrackedHashtable ) }.



% Returns a flat list whose elements are all the key/value pairs of the
% hashtable.
%
% Ex: [ {K1,V1}, {K2,V2}, ... ].
-spec enumerate( tracked_hashtable() ) -> hashtable:entries().
enumerate( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	lists:flatten( tuple_to_list( Hashtable ) ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec selectEntries( [ key() ], tracked_hashtable() ) -> hashtable:entries().
selectEntries( Keys, _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->

	hashtable:selectEntries( Keys, Hashtable ).


% Returns a list containing all the keys of this hashtable.
%
-spec keys( tracked_hashtable() ) -> [ key() ].
keys( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:keys( Hashtable ).


% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( tracked_hashtable() ) -> [ value() ].
values( _TrackedHashtable={ Hashtable, _NEnt, _NBuck }  ) ->
	hashtable:values( Hashtable ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
-spec isEmpty( tracked_hashtable() ) -> boolean().
isEmpty( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:isEmpty( Hashtable ).



% Returns the size (number of entries) of this hashtable.
%
-spec size( tracked_hashtable() ) -> hashtable:entry_count().
size( _TrackedHashTable={ _Hashtable, NEntries, _NBuckets } ) ->
	NEntries.



% Returns the number of entries (key/value pairs) stored in the specified
% tracked hashtable.
-spec getEntryCount( tracked_hashtable() ) -> hashtable:entry_count().
getEntryCount( TrackedHashtable  ) ->
	size( TrackedHashtable ).



% Returns a textual description of the specified hashtable.
-spec toString( tracked_hashtable() ) -> string().
toString( _TrackedHashtable={ Hashtable, _NEnt, _NBuck } ) ->
	hashtable:toString( Hashtable ).



% Returns a textual description of the specified hashtable, with specified
% display setting.
%
-spec toString( tracked_hashtable(), 'internal' | 'user_friendly' ) -> string().
toString( _TrackedHashtable={ Hashtable, _NEnt, _NBuck }, DescriptionType ) ->
	hashtable:toString( Hashtable, DescriptionType ).


% Displays the specified hashtable on the standard output.
%
-spec display( tracked_hashtable() ) -> basic_utils:void().
display( _TrackedHashtable={ Hashtable, _ElementCount, NumberOfBuckets } ) ->

	hashtable:display( Hashtable ),
	io:format( " and its bucket size is ~B.~n", [ NumberOfBuckets ] ).



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( string(), tracked_hashtable() ) -> basic_utils:void().
display( Title,
		 _TrackedHashtable={ Hashtable, _ElementCount, NumberOfBuckets} ) ->

	hashtable:display( Title, Hashtable ),
	io:format( " and its bucket size is ~B.~n", [ NumberOfBuckets ] ).
