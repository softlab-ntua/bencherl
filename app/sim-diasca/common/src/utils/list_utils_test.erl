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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the list management utils.
%
% See the list_utils.erl tested module.
%
-module(list_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Testing list management:
	L = [ 12, 4, 13, 2, 56, 0 ],

	GetIndex = 3,

	GetValue = list_utils:get_element_at( L, GetIndex ),
	test_facilities:display( "Getting item #~B of list ~w: ~B.",
							[ GetIndex, L, GetValue ] ),

	13 = GetValue,


	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%test_facilities:display( "   Getting item #~B of list ~w: ~B.",
	% [ OutOfBoundsIndex, L,
	%	list_utils:get_element_at( L, OutOfBoundsIndex ) ] ),

	RemoveIndex = 3,

	ShortenList = list_utils:remove_element_at( L, RemoveIndex ),

	test_facilities:display(
		"List obtained after having removed item #~B of list ~w: ~w.",
		[ RemoveIndex, L, ShortenList ] ),

	% Hardcoded for checking:
	CorrectShortenList = [ 12, 4, 2, 56, 0 ],

	ShortenList = CorrectShortenList,

	[ 12, 4, 2, 56 ] = list_utils:remove_last_element( CorrectShortenList ),

	1 = list_utils:get_index_of( 12, CorrectShortenList ),
	2 = list_utils:get_index_of(  4, CorrectShortenList ),
	5 = list_utils:get_index_of(  0, CorrectShortenList  ),
	try

		list_utils:get_index_of( 42, CorrectShortenList  ),

		throw( { test_failed, get_index_of, 42 } )

	catch { non_existing_element, 42 } ->

			ok

	end,

	%OutOfBoundsIndex = 0,
	%OutOfBoundsIndex = 100,
	%test_facilities:display( "   List obtained after having removed item #~B
	% of list ~w: "
	%	" ~w.", [ OutOfBoundsIndex, L,
	%	list_utils:remove_element_at( L, OutOfBoundsIndex ) ] ),

	L1 = [ 1, 2, 3, 4, 2 ],

	L2 = [ 2, 3 ],

	Subtracted = list_utils:subtract_all_duplicates( L1, L2 ),

	test_facilities:display( "Displaying the subtraction with "
		"duplicates removal of ~w by ~w: ~w.", [ L1, L2, Subtracted ] ),

	[ 1, 4 ] = Subtracted,

	%list_utils:delete_existing( non_existing, L1 ),

	% Checks order, removal of first matching only:
	[ 1, 3, 4, 2 ] = list_utils:delete_existing( 2, L1 ),

	% Checks order, removal of all matching elements:
	[ 1, 3, 4 ] = list_utils:delete_all_in( 2, L1 ),


	true  = list_utils:is_list_of_integers( [ 1, 2, 3 ] ),
	true  = list_utils:is_list_of_integers( [] ),
	true  = list_utils:is_list_of_integers( "This is a trap!" ),

	false  = list_utils:is_list_of_integers( [ 1, 2, 3.0 ] ),
	false  = list_utils:is_list_of_integers( a ),


	true = list_utils:unordered_compare( [], [] ),
	true = list_utils:unordered_compare( [a,b], [a,b] ),
	true = list_utils:unordered_compare( [a,b], [b,a] ),

	false = list_utils:unordered_compare( [a,b], [a,c] ),
	false = list_utils:unordered_compare( [a,b], [a] ),

	{ [], [  a, b, c, d, e ] } = list_utils:split_at( [ a, b, c, d, e ], 0 ),

	{ [ c, b, a ], [ d, e] } = list_utils:split_at( [ a, b, c, d, e ], 3 ),

	{ [ e, d, c, b, a ], [] } = list_utils:split_at( [ a, b, c, d, e ], 100 ),

	Uniquified = list_utils:uniquify( L1 ),

	test_facilities:display( "Displaying a uniquified version of ~w: ~w.",
			   [ L1, Uniquified ] ),

	% Supposedly the order will be consistent, although this is not requested:
	[ 3, 2, 1, 4 ] = Uniquified,

	false = list_utils:has_duplicates( [] ),
	false = list_utils:has_duplicates( [ 1, 2 ] ),
	true  = list_utils:has_duplicates( [ 1, 2, 1 ] ),

	DupList1 = [],
	Dup1 = list_utils:get_duplicates( DupList1 ),
	io:format( "Duplicates in ~w are ~w.~n", [ DupList1, Dup1 ] ),
	[] = Dup1,
	false = list_utils:has_duplicates( DupList1 ),

	DupList2 = [ a, b, c ],
	Dup2 = list_utils:get_duplicates( DupList2 ),
	io:format( "Duplicates in ~w are ~w.~n", [ DupList2, Dup2 ] ),
	[] = Dup2,
	false = list_utils:has_duplicates( DupList2 ),

	DupList3 = [ a, a, b, b, b, c, c, c ],
	Dup3 = list_utils:get_duplicates( DupList3 ),
	io:format( "Duplicates in ~w are ~w.~n", [ DupList3, Dup3 ] ),
	[ {b,3}, {c,3}, {a,2} ] = Dup3,
	true = list_utils:has_duplicates( DupList3 ),

	DupList4 = [ a, b, a, a, c ],
	Dup4 = list_utils:get_duplicates( DupList4 ),
	io:format( "Duplicates in ~w are ~w.~n", [ DupList4, Dup4 ] ),
	[ {a,3} ] = Dup4,
	true = list_utils:has_duplicates( DupList4 ),

	DupList5 = [ a, b, c, d, b, d, a, b, e, f, f ],
	Dup5 = list_utils:get_duplicates( DupList5 ),
	io:format( "Duplicates in ~w are ~w.~n", [ DupList5, Dup5 ] ),
	[ {d,2}, {b,3}, {f,2}, {a,2} ] = Dup5,
	true = list_utils:has_duplicates( DupList5 ),


	List1 = [ a, b, 1, c, 14 ],
	List2 = [ 14, d, b ],

	ExpectedList = lists:sort( [ 14, b ] ),
	ResultList = list_utils:intersect( List1, List2 ),

	test_facilities:display( "Displaying the intersection of ~p and ~p: ~p.",
			   [ List1, List2, ResultList ] ),

	ExpectedList = lists:sort( ResultList ),

	Ring = list_utils:list_to_ring( [ a, b, c, d, e, f, g ] ),

	{ a, FirstRing } = list_utils:head( Ring ),

	{ b, SecondRing } = list_utils:head( FirstRing ),

	{ [ c, d, e, f, g, a, b, c ], _ThirdRing } = list_utils:get_next(
													_RingCount=8, SecondRing ),

	DrawCount = 3,

	test_facilities:display( "Drawing ~B elements from ~w: ~w.",
		  [ DrawCount, L, list_utils:draw_elements_from( List1, DrawCount ) ] ),


	ASeed = { 113, 798, 8914 },
	random_utils:set_random_state( ASeed ),

	Lpermuted = list_utils:random_permute( L ),

	test_facilities:display(
		"List obtained after having uniformly permuted list ~w:  ~w.",
		[ L, Lpermuted ] ),

	test_facilities:display(
		"List obtained after having uniformly permuted list ~w (again): ~w.",
		[ L, list_utils:random_permute( Lpermuted ) ] ),

	random_utils:set_random_state( ASeed ),

	Lreciprocal = list_utils:random_permute_reciprocal( Lpermuted ),

	test_facilities:display( "List obtained after having restored the random "
							 "state and performed a reciprocal permutation on "
							 "~w: ~w.",
							 [ Lpermuted, Lreciprocal ] ),

	% Must match:
	L = Lreciprocal,

	DrawList = [ {first,1}, {second,2}, {third,1} ],

	test_facilities:display( "Drawing an element from ~w, got: '~w'.",
		[ DrawList, list_utils:draw_element_weighted( DrawList ) ] ),

	test_facilities:display( "Drawing an element from ~w, got: '~w'.",
		[ DrawList, list_utils:draw_element_weighted( DrawList ) ] ),

	test_facilities:display( "Drawing an element from ~w, got: '~w'.",
		[ DrawList, list_utils:draw_element_weighted( DrawList ) ] ),


	%ProbabilityCount = 2000,
	ProbabilityCount = 50,

	FirstProbabilityList = [ {first,1}, {second,0}, {third,1} ],

	NoSecondList = [ list_utils:draw_element_weighted( FirstProbabilityList )
					 || _X <- lists:seq( 1, ProbabilityCount ) ],

	test_facilities:display(
		"List obtained after ~B drawings in following probability table: ~w "
		"is ~w; frequency dispatching: ~w.",
		[ ProbabilityCount, FirstProbabilityList, NoSecondList,
		  list_utils:get_duplicates( NoSecondList ) ] ),


	SecondProbabilityList = [ {first,1}, {second,2}, {third,1} ],

	SecondTwiceList = [ list_utils:draw_element_weighted(
		  SecondProbabilityList ) || _X <- lists:seq( 1, ProbabilityCount ) ],

	test_facilities:display(
		"List obtained after ~B drawings in following probability table: ~w "
		"is ~w; frequency dispatching: ~w.",
		[ ProbabilityCount, SecondProbabilityList, SecondTwiceList,
		  list_utils:get_duplicates( SecondTwiceList ) ] ),

	test_facilities:stop().
