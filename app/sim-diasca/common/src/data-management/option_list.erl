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
% Creation date: Saturday, February 20, 2010.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)




% Generic option list implementation, loosely based on proplist.
% See option_list_test.erl for the corresponding test.
%
% An option list is basically a list containing key/value pairs, keys being
% generally (hence: not necessarily) atoms, values being any Erlang term.
%
% In an option list, usually no duplicate keys are expected to exist.
% Operations on option list tend to preserve the order of their entries.
%
% See also the proplists standard module.
%
-module(option_list).

-export([ new/0, new/1, set/2, get/2, lookup/2, update_with/2, extract/2,
		  enumerate/1, to_string/1 ]).


-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().
-type entries() :: hashtable:entries().

-type option_list() :: entries().


% For better type information (but no type overriding with different arity
% permitted):
%
%-type option_list( _K, _V ) :: option_list().


-export_type([ key/0, value/0, entry/0, entries/0, option_list/0 ]).


% Note: our option lists only have {Key,Value} pairs (ex: no entry is made of a
% single atom).



% Creates a new, empty option list.
%
-spec new() -> option_list().
new() ->
	[].



% Creates a new option list from specified list containing {Key,Value} pairs
%
-spec new( entries() ) -> option_list().
new( Entries ) ->
	% The internal representation happens to be the same:
	Entries.



% Sets, in specified option list, the specified entry.
%
% Returns an updated option list.
%
% The first previously existing entry found with Key (if any) is replaced
% 'in place' by this entry.
%
% If none is found, the specified entry is put as first element.
%
-spec set( entry(), option_list() ) -> option_list().
set( Entry, OptionList ) ->
	set( Entry, OptionList, _Acc=[] ).


% Helper function:
set( Entry, _OptionList=[], Acc ) ->
	% Here no key matched:
	[ Entry | lists:reverse( Acc ) ];

set( Entry={ Key, _Value }, [ { Key, _AnyValue } | T ], Acc ) ->
	% Same key found, recursion is over:
	lists:reverse( Acc ) ++ [ Entry | T ];

set( Entry, [ NonMatchingEntry | T ], Acc ) ->
	% Different key found:
	set( Entry, T, [ NonMatchingEntry | Acc ] ).



% Returns the value associated to the specified key in specified option list.
%
% Throws an exception if an entry with that key could not be found.
%
-spec get( key(), option_list() ) -> value().
get( Key, OptionList ) ->

	case proplists:get_value( Key, OptionList ) of

		undefined ->
			throw( { key_not_found, Key, OptionList } );

		Value ->
			Value

	end.



% Returns the value associated to the specified key in specified option list, if
% found, otherwise (key not found), returns 'undefined'.
%
-spec lookup( key(), option_list() ) -> 'undefined' | value().
lookup( Key, OptionList ) ->
	proplists:get_value( Key, OptionList ).



% Updates BaseOptionList with the entries of UpdatingOptionList.
%
% Merges the two specified option lists into the returned one, knowing that all
% entries found with the same key in both option lists will end up with the
% value defined in the second, UpdatingOptionList.
%
-spec update_with( option_list(), option_list() ) -> option_list().
update_with( BaseOptionList, _UpdatingOptionList=[] ) ->
	BaseOptionList;

update_with( BaseOptionList, [ H | T ] ) ->
	update_with( set( H, BaseOptionList ), T ).



% Extracts the entry that is specified by its key from the specified option
% list, and returns either 'undefined' if not entry with that key could be
% found, or { Value, RemainingOptionList } where Value is the value associated
% to this key and RemainingOptionList is the original option list from which
% this entry (the first found with the specified key) has been removed (order
% preserved).
%
-spec extract( key(), option_list() ) -> { value(), option_list() }.
extract( Key, OptionList ) ->
	extract( Key, OptionList, _Acc=[] ).


extract( Key, _OptionList=[], Acc ) ->
	throw( { extract_key_not_found, Key, lists:reverse( Acc ) } );

extract( Key, _OptionList=[ { Key, Value } | T ], Acc ) ->
	{ Value, lists:reverse( Acc ) ++ T };

extract( Key, _OptionList=[ E | T ], Acc ) ->
	extract( Key, T, [ E | Acc ] ).




% Enumerates specified option list: returns an (ordered) list of { Key, Value }
% pairs, possibly with duplicates.
%
-spec enumerate( option_list() ) -> entries().
enumerate( OptionList ) ->
	OptionList.



% Returns a string describing the specified option list.
%
-spec to_string( option_list() ) -> string().
to_string( OptionList ) ->
	Strings = [ io_lib:format( "~p: ~p", [ K, V ] ) || { K, V } <- OptionList ],
	text_utils:strings_to_string( Strings ).
