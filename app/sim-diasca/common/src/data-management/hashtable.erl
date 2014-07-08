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
% Creation date: July 2, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Generic hash table implementation.
% See hashtable_test.erl for the corresponding test.


% An hashtable is basically a tuple whose size (number of elements) is the
% number of buckets in the hashtable. Each element of the tuple is a list
% containing key/value pairs.
%
%
% Note: we provide different three types of hashtables:
%
% - 'hashtable' (this module), the most basic, safest, reference implementation
%
% - 'tracked_hashtable', an attempt of optimisation of it (not necessarily the
% best)
%
% - 'lazy_hashtable', which is probably the most efficient implementation
%
% They are to provide the same API (signatures and contracts).
%
% A fourth implementation could be map-based (R17 and above) and aggressively
% inlined.
%
-module(hashtable).


% To avoid code duplication yet having fastest speed:
-compile( { inline, [ get_bucket_index/2 ] } ).



% Directly depends on the text_utils module.

% Heavily inspired of the tupleStore example from 'Concurrent Programming in
% Erlang' (Joe Armstrong), section 9.8.



% The hashtable is implemented thanks to a tuple whose size is the number of
% buckets specified at the hashtable creation.
%
% Each tuple element (hence each bucket) is a list of key/value pairs.

% Maybe the ETS module, proplists, dict, etc. could/should be used instead.

% When the table holds less than 50 elements, probably that using functions like
% lists:keystore/4 and lists:keymember/3 would be faster.


% There is no function that is specific to this implementation, to enforce
% substitutability.


% The standard hashtable API:
%
-export([ new/0, new/1,
		  addEntry/3, addDiagnosedEntry/3,
		  addEntries/2, addDiagnosedEntries/2,
		  removeEntry/2, removeDiagnosedEntry/2,
		  lookupEntry/2, hasEntry/2,
		  getEntry/2, addToEntry/3, subtractFromEntry/3, toggleEntry/2,
		  appendToEntry/3, deleteFromEntry/3, popFromEntry/2,
		  enumerate/1, selectEntries/2, keys/1, values/1,
		  isEmpty/1, getEntryCount/1,
		  merge/2, optimise/1, toString/1, toString/2, display/1, display/2 ]).



% These functions are exported only to ease the tracked_hashtable
% implementation, so that we can switch implementations.
%
% Not intended to be used in user code.
%
-export([ new_with_buckets/1, delete_bucket/3, delete_bucket_verbose/3,
		  replace_bucket/4,
		  get_bucket_index_for/2, get_bucket_count/1, get_ideal_bucket_count/1,
		  must_optimise/2, optimise_unconditionally/4 ]).


% The default expected number of entries:
-define(DefaultNumberOfEntries,32).


% Not necessarily an atom, but surely not a string (as lists are interpreted as
% lists of keys):
%
-type key() :: atom() | binary() | pid() | tuple().

-type value() :: term().

-type entry() :: { key(), value() }.
-type entries() :: [ entry() ].

-type entry_count() :: basic_utils:count().
-type bucket_count() :: pos_integer().


% A problem is that the number of buckets (hence the size of the tuple) is
% determined at runtime:
%
%-type hashtable() :: tuple().
-opaque hashtable() :: tuple().
%-opaque hashtable( K, V ) :: tuple( [ { K, V } ] ).

-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   bucket_count/0, hashtable/0 ]).





% Returns a new empty hashtable dimensioned for the default number of entries.
%
-spec new() -> hashtable().
new() ->
	new( ?DefaultNumberOfEntries ).



% Returns a new empty hashtable dimensioned for the specified expected number of
% entries.
%
-spec new( entry_count() | entries() ) -> hashtable().
new( ExpectedNumberOfEntries ) when is_integer( ExpectedNumberOfEntries ) ->
	NumberOfBuckets = get_ideal_bucket_count( ExpectedNumberOfEntries ),
	create_tuple( NumberOfBuckets, _DefaultValue=[] );

new( InitialEntries ) when is_list( InitialEntries ) ->

	BlankTable = new(),

	addEntries( InitialEntries, BlankTable ).



% Returns a new empty hashtable dimensioned with the specified number of
% buckets.
%
% (helper)
%
-spec new_with_buckets( bucket_count() ) -> hashtable().
new_with_buckets( NumberOfBuckets ) ->
	create_tuple( NumberOfBuckets, _DefaultValue=[] ).




% Adds specified key/value pair into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntry( key(), value(), hashtable() ) -> hashtable().
addEntry( Key, Value, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	% Retrieve appropriate bucket:
	PreviousBucket = element( KeyIndex, Hashtable ),

	NewBucket = replace_bucket( Key, Value, PreviousBucket, [] ),

	setelement( KeyIndex, Hashtable, NewBucket ).



% Adds specified key/value pair into the specified hashtable, and returns an
% update diagnosis.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addDiagnosedEntry( key(), value(), hashtable() ) ->
							   { hashtable(), 'added' | 'updated' }.
addDiagnosedEntry( Key, Value, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	% Retrieve appropriate bucket:
	PreviousBucket = element( KeyIndex, Hashtable ),

	{ Diagnosis, NewBucket } = replace_bucket_diagnose( Key, Value,
														PreviousBucket, [] ),

	NewTable = setelement( KeyIndex, Hashtable, NewBucket ),

	{ NewTable, Diagnosis }.



% Adds specified list of key/value pairs into the specified hashtable.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addEntries( entries(), hashtable() ) -> hashtable().
addEntries( _EntryList=[], Hashtable ) ->
	Hashtable;

addEntries( [ { EntryName, EntryValue } | Rest ], Hashtable ) ->
	addEntries( Rest, addEntry( EntryName, EntryValue, Hashtable ) ).



% Adds specified list of key/value pairs into the specified hashtable, and
% returns an update diagnosis.
%
% If there is already a pair with this key, then its previous value will be
% replaced by the specified one.
%
-spec addDiagnosedEntries( entries(), hashtable() ) ->
								 { hashtable(), 'added' | 'updated' }.
addDiagnosedEntries( Entries, Hashtable ) ->
	lists:foldl( fun( _Entry={K,V}, _Acc={ Table, _Diag='added' } ) ->
						 NewTable = addEntry( K, V, Table ),
						 { NewTable, added };

					% Implicitly, Diag is 'updated' here:
					( _Entry={K,V}, _Acc={ Table, _Diag } ) ->
						 % Returns directly { NewTable, NewDiagnosis }:
						 addDiagnosedEntry( K, V, Table )

					end,
					_InitialAcc={ Hashtable, _InitialDiag=updated },
					_List=Entries ).



% Removes specified key/value pair, as designated by the key, from the specified
% hashtable.
%
% Does nothing if the key is not found.
%
% Returns an updated table.
%
-spec removeEntry( key(), hashtable() ) -> hashtable().
removeEntry( Key, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	PreviousBucket = element( KeyIndex, Hashtable ),

	NewBucket = delete_bucket( Key, PreviousBucket, _Acc=[] ),

	setelement( KeyIndex, Hashtable, NewBucket ).



% Removes specified key/value pair, as designated by the key, from the specified
% hashtable.
%
% Does nothing if the key is not found.
%
% Returns a diagnosis and an updated table.
%
-spec removeDiagnosedEntry( key(), hashtable() ) ->
					  { 'deleted', hashtable() } | 'unchanged'.
removeDiagnosedEntry( Key, Hashtable ) ->

	KeyIndex = get_bucket_index( Key, Hashtable ),

	PreviousBucket = element( KeyIndex, Hashtable ),

	case delete_bucket_verbose( Key, PreviousBucket, _Acc=[] ) of

		% Diagnosis is either 'deleted' or 'unchanged'
		{ deleted, NewBucket } ->
			NewTable = setelement( KeyIndex, Hashtable, NewBucket ),
			{ deleted, NewTable };

		unchanged ->
			unchanged

	end.



% Looks-up specified entry (designated by its key) in specified hashtable.
%
% Returns either 'hashtable_key_not_found' if no such key is registered in the
% table, or { value, Value }, with Value being the value associated to the
% specified key.
%
-spec lookupEntry( key(), hashtable() ) ->
				 'hashtable_key_not_found' | { 'value', value() }.
lookupEntry( Key, Hashtable ) ->
	lookupInList( Key, element( get_bucket_index( Key, Hashtable ),
		Hashtable ) ).



% Tells whether the specified key exists in the table: returns true or false.
%
-spec hasEntry( key(), hashtable() ) -> boolean().
hasEntry( Key, Hashtable ) ->

	case lookupInList( Key,
			element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, _Value } ->
			true;

		% hashtable_key_not_found ->
		_ ->
			false

	end.



% Retrieves the value corresponding to specified (existing) key and returns it
% directly.
%
% The key/value pair is expected to exist already, otherwise an exception is
% raised.
%
-spec getEntry( key(), hashtable() ) -> value().
getEntry( Key, Hashtable ) ->

	case lookupInList( Key, element( get_bucket_index( Key, Hashtable ),
									Hashtable ) ) of

		% Most likely case first:
		{ value, Value } ->
			Value;

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Returns the number of entries (key/value pairs) stored in the specified
% hashtable.
%
% Note: might be a bit expensive.
%
-spec getEntryCount( hashtable() ) -> entry_count().
getEntryCount( Hashtable ) ->
	erlang:length( enumerate( Hashtable ) ).



% Adds specified value to the value, supposed to be numerical, associated to
% specified key.
%
% An exception is thrown if the key does not exist, a bad arithm is triggered if
% no addition can be performed on the associated value.
%
-spec addToEntry( key(), number(), hashtable() ) -> hashtable().
addToEntry( Key, Value, Hashtable ) ->

	case lookupInList( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, Number } ->
			addEntry( Key, Number + Value, Hashtable );

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
-spec subtractFromEntry( key(), number(), hashtable() ) -> hashtable().
subtractFromEntry( Key, Value, Hashtable ) ->

	case lookupInList( Key,
		element(get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, Number } ->
			addEntry( Key, Number - Value, Hashtable );

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
-spec toggleEntry( key(), hashtable() ) -> hashtable().
toggleEntry( Key, Hashtable ) ->

	case lookupInList( Key,
			element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, true } ->
			addEntry( Key, false, Hashtable );

		{ value, false } ->
			addEntry( Key, true, Hashtable );

		{ value, Other } ->
			throw( { non_boolean_value, Other } );

		%hashtable_key_not_found ->
		_ ->
			throw( { hashtable_key_not_found, Key } )

	end.



% Returns a new hashtable, which started from HashtableBase and was enriched
% with the HashtableAdd entries whose keys where not already in HashtableBase
% (if a key is in both tables, the one from HashtableBase will be kept).
%
-spec merge( hashtable(), hashtable() ) -> hashtable().
merge( HashtableBase, HashtableAdd ) ->

	% Uses the fact that when two entries with the same key are added, the final
	% associated value is the one of the latest to be added.

	lists:foldl(
		fun( {Key,Value}, Acc ) -> addEntry( Key, Value, Acc ) end,
		_InitialAcc=HashtableAdd,
		_List=enumerate(HashtableBase) ).



% Appends specified element to the value, supposed to be a list, associated to
% specified key.
%
% An exception is thrown if the key does not exist.
%
% Note: no check is performed to ensure the value is a list indeed, and the
% '[|]' operation will not complain if not.
%
-spec appendToEntry( key(), term(), hashtable() ) -> hashtable().
appendToEntry( Key, Element, Hashtable ) ->

	case lookupInList( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, List } ->
			addEntry( Key, [ Element | List ], Hashtable );

		%hashtable_key_not_found ->
		_ ->
			throw( { hashtable_key_not_found, Key } )

	end.



% Deletes the first match of the specified element in the value associated to
% specified key, this value being assumed to be a list.
%
% An exception is thrown if the key does not exist.
%
% If the element is not in the specified list, the list will not be modified.
%
-spec deleteFromEntry( key(), term(), hashtable() ) -> hashtable().
deleteFromEntry( Key, Element, Hashtable ) ->

	case lookupInList( Key,
		element( get_bucket_index( Key, Hashtable ), Hashtable ) ) of

		{ value, List } ->
			addEntry( Key, lists:delete( Element, List ), Hashtable );

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, Key } )

	end.



% Pops the head of the value (supposed to be a list) associated to specified
% key, and returns a pair made of the popped head and of the new hashtable.
%
-spec popFromEntry( key(), hashtable() ) -> { term(), hashtable() }.
popFromEntry( Key, Hashtable ) ->

	case lookupEntry( Key, Hashtable ) of

		{ value, [ H | T ] } ->
			{ H, addEntry( Key, T, Hashtable ) };

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
-spec enumerate( hashtable() ) -> entries().
enumerate( Hashtable ) ->
	lists:flatten( tuple_to_list(Hashtable) ).



% Returns a list of key/value pairs corresponding to the list of specified keys,
% or throws a badmatch is at least one key is not found.
%
-spec selectEntries( [ key() ], hashtable() ) -> entries().
selectEntries( Keys, Hashtable ) ->
	selectEntries( Keys, Hashtable, _Acc=[] ).

selectEntries( _Keys=[], _Hashtable, Acc ) ->
	Acc;

selectEntries( _Keys=[ K | T ], Hashtable, Acc ) ->

	case lookupEntry( K, Hashtable ) of

		{ value, V } ->
			selectEntries( T, Hashtable, [ { K, V } | Acc ] );

		%hashtable_key_not_found ->
		_ ->
			% Badmatches are not informative enough:
			throw( { hashtable_key_not_found, K } )

	end.



% Returns a list containing all the keys of this hashtable.
%
-spec keys( hashtable() ) -> [ key() ].
keys( Hashtable ) ->
	get_keys_from_buckets( tuple_to_list( Hashtable ), _Acc=[] ).



% Returns a list containing all the values of this hashtable.
%
% Ex: useful if the key was used as an index to generate this table first.
%
-spec values( hashtable() ) -> [ value() ].
values( Hashtable ) ->
	get_values_from_buckets( tuple_to_list( Hashtable ), _Acc=[] ).



% Returns whether the specified hashtable is empty (not storing any key/value
% pair).
%
-spec isEmpty( hashtable() ) -> boolean().
isEmpty( Hashtable ) ->
	BucketList = tuple_to_list( Hashtable ),
	is_empty( BucketList ).


% Tells whether the specified list of lists is empty.
%
is_empty( [] ) ->
	true;

is_empty( [ _L=[] | T ] ) ->
	is_empty( T );

is_empty( _Any ) ->
	% Here we have an overall list which is not empty, whose first element is
	% itself not an empty list, thus there is at least one entry and we can stop
	% here:
	false.



% Optimises this hashtable with regard to its load factor (see
% http://en.wikipedia.org/wiki/Hash_table#Load_factor).
%
% To be called whenever the size of a given hashtable is not expected to change
% substantially. The principle is to determine the optimal number of buckets for
% the current number of stored entries, allowing to perform fast look-ups and to
% use the right amount of memory for that (i.e. to rely on the best CPU vs RAM
% trade-off).
%
-spec optimise( hashtable() ) -> hashtable().
optimise( Hashtable ) ->

	% Like getEntryCount, but allows to re-use Entries:
	Entries = enumerate( Hashtable ),
	EntryCount = length( Entries ),

	% Number of elements of the underlying tuple:
	BucketCount = tuple_size( Hashtable ),

	case must_optimise( EntryCount, BucketCount ) of

		% Outside bounds, re-hash:
		true ->
			optimise_unconditionally( EntryCount, BucketCount, Entries,
									 Hashtable );

		false ->
			Hashtable

	end.



% Returns whether an optimisation ought to be triggered.
%
% Too high a load factor (more than 0.75) induces slow look-ups, too small (less
% than 0.5) wastes memory:
%
-spec must_optimise( entry_count(), bucket_count() ) -> boolean().
must_optimise( EntryCount, BucketCount ) ->

	% BucketCount is expected to be never null:
	LoadFactor = ( EntryCount + 1 ) / BucketCount,

	( LoadFactor < 0.5 ) orelse ( LoadFactor > 0.75 ).



% Performs an optimisation of the specified hashtable.
%
-spec optimise_unconditionally( entry_count(), bucket_count(), entries(),
								hashtable() ) -> hashtable().
optimise_unconditionally( EntryCount, CurrentBucketCount, Entries,
						 Hashtable ) ->

	IdealBucketCount = get_ideal_bucket_count( EntryCount ),

	% Avoids useless reshuffles (ex: due to rounding errors):
	case IdealBucketCount of

		CurrentBucketCount ->
			Hashtable;

		_ ->
			NewTable = new_with_buckets( IdealBucketCount ),
			addEntries( Entries, NewTable )

	end.


% Returns a textual description of the specified hashtable.
%
-spec toString( hashtable() ) -> string().
toString( Hashtable ) ->
	toString( Hashtable, user_friendly ).


% Returned string is either quite raw (if using 'internal') or a bit more
% elaborate (if using 'user_friendly').
%
-spec toString( hashtable(), 'internal' | 'user_friendly' ) -> string().
toString( Hashtable, user_friendly ) ->

	case enumerate( Hashtable ) of

		[] ->
			"Empty hashtable";

		L ->

			% Enforces a consistent order:
			Strings = [ io_lib:format( "~p: ~p", [ K, V ] )
					   || { K, V } <- lists:sort( L ) ],

			% Flatten is needed, in order to use the result with ~s:
			lists:flatten( io_lib:format( "Hashtable with ~B entry(ies):~s~n",
				[ length( L ),
				  text_utils:string_list_to_string( Strings ) ] ) )

	end;

toString( Hashtable, internal ) when tuple_size( Hashtable ) > 0 ->

	lists:foldl(

		fun( Bucket, Acc ) ->
			Acc ++ io_lib:format( "  + ~s~n", [ bucket_toString( Bucket ) ] )
		end,

		io_lib:format( "Hashtable with ~B bucket(s) and ~B entry(ies): ~n",
			[ tuple_size( Hashtable ), hashtable:getEntryCount( Hashtable ) ] ),

		tuple_to_list( Hashtable ) );

toString( _Hashtable, internal ) ->
	io_lib:format( "Empty hashtable~n", [] ).




% Displays the specified hashtable on the standard output.
%
-spec display( hashtable() ) -> basic_utils:void().
display( Hashtable ) ->
	io:format( "~s~n", [ toString( Hashtable ) ] ).



% Displays the specified hashtable on the standard output, with the specified
% title on top.
%
-spec display( string(), hashtable() ) -> basic_utils:void().
display( Title, Hashtable ) ->
	io:format( "~s:~n~s~n", [ Title, toString( Hashtable ) ] ).




% Section for helper functions.



% Returns the ideal number of buckets needed for specified number of entries.
%
-spec get_ideal_bucket_count( entry_count() ) -> basic_utils:count().
get_ideal_bucket_count( EntryCount ) ->

	IdealLoadFactor = 0.65,

	% To avoid requesting zero bucket:
	erlang:max( round( EntryCount / IdealLoadFactor ), 1 ).


% Returns a new tuple, whose size is the specified length and whose elements are
% all set to specified default value.
%
create_tuple( _Length=0, _DefaultValue ) ->
	throw( at_least_one_bucket_per_hashtable );

create_tuple( Length, DefaultValue ) ->
	create_tuple( Length, DefaultValue, _Acc=[] ).


% Final step:
create_tuple( _N=0, _DefaultValue, Acc ) ->
	list_to_tuple( Acc );


% Building from n-1 to n elements:
create_tuple( N, DefaultValue, Acc ) ->
	create_tuple( N-1, DefaultValue, [ DefaultValue | Acc ] ).



% Removes the (first) entry pair whose key matches the specified one, if any.
%
% (returns an identical list if the key is not found)
%
delete_bucket( Key, [ { Key, _Value } | T ], Acc ) ->
	% Forget the pair if the key if matching, and just stop:
	lists:append( T, Acc );

delete_bucket( Key, [ H | T ], Acc ) ->
	% Keeps everything else (non-matching entries):
	delete_bucket( Key, T, [ H | Acc ] );

delete_bucket( _Key, [], Acc ) ->
	% Nothing at all was deleted in this call:
	Acc.



% Returns, if an entry with the specified key was found, { 'deleted', NewBucket
% }, i.e. a pair made of an atom telling whether a deletion was done, and a list
% whose first entry having a matching key is removed, otherwise 'unchanged'.
%
% (like delete_bucket/3, but gives more information, used for example by
% tracked_hashtable)
%
-spec delete_bucket_verbose( key(), entries(), entries() ) ->
				{ 'deleted', entries() } | 'unchanged'.
delete_bucket_verbose( Key, [ { Key, _Value } | T ], Acc ) ->
	% Forget the pair if the key if matching, and stops:
	{ deleted, lists:append( T, Acc ) };

delete_bucket_verbose( Key, [ H | T ], Acc ) ->
	% Keeps everything else (non-matching entries):
	delete_bucket_verbose( Key, T, [ H | Acc ] );

delete_bucket_verbose( _Key, [], _Acc ) ->
	% Nothing was deleted in this call:
	unchanged.



% Replaces, in specified list, a key/value pair by another.
%
% Updates the pair if this key was already declared, otherwise add the new
% entry.
%
% Note: order does not matter.
%
-spec replace_bucket( key(), value(), entries(), entries() ) -> entries().
replace_bucket( Key, Value, [], Acc ) ->
	% Key was not there previously, just adding it:
	[ { Key, Value } | Acc ];

replace_bucket( Key, Value, [ { Key, _ } | T ], Acc ) ->
	% Add the key, join the two parts of the list and return it:
	[ { Key, Value } | lists:append( T, Acc ) ];

replace_bucket( Key, Value, [ H | T ], Acc ) ->
	% Another key, continue iterating:
	replace_bucket( Key, Value, T, [ H | Acc ] ).



% Replaces in specified list a key/value pair by another, and tells whether it
% is an addition or an update.
%
% Updates the pair if this key was already declared, otherwise add the new
% entry.
%
% (like replace_bucket/4, but gives more information, used for example by
% tracked_hashtable)
%
% Note: order does not matter.
%
% Returns { Diagnosis, NewBucket }.
%
replace_bucket_diagnose( Key, Value, _RestOfBucket=[], Acc ) ->
	% Key was not there previously, just adding it:
	{ added, [ { Key, Value } | Acc ] };

replace_bucket_diagnose( Key, Value, [ { Key, _ } | T ], Acc ) ->
	% Add the key, join the two parts of the list and return it:
	{ updated,[ { Key, Value } | lists:append( T, Acc ) ] };

replace_bucket_diagnose( Key, Value, [ H | T ], Acc ) ->
	% Another key, continue iterating:
	replace_bucket_diagnose( Key, Value, T, [ H | Acc ] ).



% Returns the number of buckets in this hashtable.
%
% Not intended to be used by user code.
%
% (helper)
%
-spec get_bucket_count( hashtable() ) -> bucket_count().
get_bucket_count( Hashtable ) ->
	tuple_size( Hashtable ).



% Returns a string describing a hashtable bucket (list of key/value pairs):
%
bucket_toString( Bucket ) when length(Bucket) > 0 ->
	lists:foldl(

		fun( { Key, Value }, Acc ) ->
			Acc ++ io_lib:format( "     * ~w -> ~s~n",
				[ text_utils:term_to_string( Key ),
				  text_utils:term_to_string( Value ) ] )
		end,

		io_lib:format( "Bucket with ~B element(s):~n",
			[ length(Bucket) ] ),

		Bucket );

bucket_toString( _EmptyBucket ) ->
	"Empty bucket".



% Returns the value corresponding to the key in the specified list.
%
lookupInList( _Key, _TargetList=[] ) ->

	% We hesitated and considered returning the key since, if this function is
	% used like '{value,V} = hashtable:lookupInList( K,L)', if the key is not
	% found, the raised 'badmatch' will directly specify the offending key
	% instead of a mere {badmatch,hashtable_key_not_found}.
	%
	% However now getEntry/2 throws an exception and should be used instead.

	%{ hashtable_key_not_found, Key };
	hashtable_key_not_found;

lookupInList( Key, _TargetList=[ { Key, Value } | _T ] ) ->
	{ value, Value };

lookupInList( Key, _TargetList=[ _H | T ] ) ->
	lookupInList( Key, T ).



% Iterates over buckets and fetches the keys.
%
get_keys_from_buckets( _Buckets=[], Acc ) ->
	Acc;

get_keys_from_buckets(  _Buckets=[ H | T ], Acc ) ->
	get_keys_from_buckets( T, [ Key || { Key, _Value } <- H ] ++ Acc ).



% Iterates over buckets and fetches the values.
%
get_values_from_buckets( _Buckets=[], Acc ) ->
	Acc;

get_values_from_buckets(  _Buckets=[ H | T ], Acc ) ->
	get_values_from_buckets( T, [ Value || { _Key, Value } <- H ] ++ Acc ).



% Returns the number of the bucket associated to specified key in specified
% hashtable.
%
% If having N buckets, returns a value in [1,N].
%
% Typically defined to avoid code duplication, but meant to be inlined.
%
-spec get_bucket_index( key(), hashtable() ) -> bucket_count().
get_bucket_index( Key, Hashtable ) ->
	erlang:phash2( Key, tuple_size( Hashtable ) ) + 1.



% Returns the number of the bucket associated to specified key in specified
% hashtable.
%
% If having N buckets, returns a value in [1,N].
%
% Defined (exactly as get_bucket_index/2, which is defined for inlining) and
% exported only for opaqueness purposes.
%
-spec get_bucket_index_for( key(), hashtable() ) -> bucket_count().
get_bucket_index_for( Key, Hashtable ) ->
	erlang:phash2( Key, tuple_size( Hashtable ) ) + 1.
