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
% Creation date: July 1, 2007.


% Gathering of various facilities about lists.
%
% See list_utils_test.erl for the corresponding test.
%
-module(list_utils).



% For list_impl:
-include("data_types.hrl").



% Basic list operations:
%
-export([ get_element_at/2, insert_element_at/3,
		  remove_element_at/2, remove_last_element/1,
		  get_last_element/1, get_index_of/2, split_at/2, uniquify/1,
		  has_duplicates/1, get_duplicates/1, intersect/2,
		  subtract_all_duplicates/2, delete_existing/2, delete_all_in/2,
		  append_at_end/2, is_list_of_integers/1,
		  unordered_compare/2, add_if_not_existing/2 ]).



% listimpl-relation operations:
%
-export([ safe_listimpl_delete/2, listimpl_add/2 ]).


% Ring-related operations:
%
-export([ list_to_ring/1, head/1, get_next/2, get_ring_size/1 ]).



% A ring behaves as an (infinite) list whose next element after its last is its
% first again.
%
% Internally, the first list is the working one (from which elements may be
% extracted), while the second is a copy of the full reference one.
%
-opaque ring() :: { list(), list() }.



% Random operations on lists:
%
-export([ random_permute/1, random_permute_reciprocal/1,
		  draw_element/1, draw_element/2, draw_element_weighted/1,
		  draw_elements_from/2 ]).


-export_type([ ring/0 ]).



% Section for basic list operations.


% Index start at position #1, not #0.

% Returns the element in the list at the specified index, in [1..length(List)].
%
% If the index is out of bounds, a function_clause is raised.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Not tail recursive version:
%
%% get_element_at( List, 1 ) ->
%%	hd(List);
%
%% get_element_at( [ _H | T ], Index ) ->
%%	get_element_at( T, Index-1 ).
%
-spec get_element_at( list(), basic_utils:positive_index() ) -> any().
get_element_at( List, Index ) ->

	%io:format( " - getting element #~B of ~w~n", [ Index, List ] ),

	lists:nth( Index, List ).


% Inserts specified element at specified position in specified list.
%
% For example, insert_element_at( foo, [ a, b, c, d ], 3 ) will return
% [ a, b, foo, c, d ].
%
-spec insert_element_at( any(), list(), basic_utils:positive_index() ) ->
							   list().
insert_element_at( Element, List, Index ) ->

	%io:format( " - inserting element ~p at #~B in ~w~n",
	%		   [ Element, Index, List ] ),

	insert_element_at( Element, List, Index, _Acc=[] ).


insert_element_at( Element, _List=[], _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] );

insert_element_at( _Element, _List=[], Index, Acc ) ->
	% Rebuilds input parameters:
	throw( { invalid_index, Index + length( Acc ), lists:reverse( Acc ) } );

insert_element_at( Element, List, _Index=1, Acc ) ->
	lists:reverse( [ Element | Acc ] ) ++ List;

insert_element_at( Element, _List=[ H | T ], Index, Acc ) ->
	insert_element_at( Element, T, Index - 1, [ H | Acc ] ).




% Returns a list corresponding to the specified one with the element at
% specified index removed.
%
% If the index is out of bounds, a function_clause like
% '[{list_utils,remove_element_at,...}]' is triggered.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Signature: remove_element_at( List, Index ).
%
% Curiously lists:nth exists, but no function to remove an element specified by
% its index seems to be available in the lists module.
%
% Not tail recursive version:
%remove_element_at( [ _H | T ], _LastIndex=1 ) ->
%   T;
%
%remove_element_at( [ H | T ], _Index=N ) ->
%   [ H | remove_element_at( T, _NextIndex=N-1 ) ].
%
% Tail recursive version:
%
-spec remove_element_at( list(), basic_utils:positive_index()) -> list().
remove_element_at( List, Index ) ->
	remove_element_at( List, Index, _Result=[] ).

remove_element_at( [ _H | RemainingList ], 1, Result ) ->
	lists:reverse( Result ) ++ RemainingList;

remove_element_at( [ H | RemainingList ], Index, Result ) ->
	remove_element_at( RemainingList, Index-1, [ H | Result ] ).



% Removes the last element of the specified list.
%
% Crashes (with 'no function clause') if the input list is empty.
%
% Note: not computationnally efficient, usually removing the last element
% suggests a bad code design.
%
-spec remove_last_element( list() ) -> list().
remove_last_element( List ) ->
	remove_last_element( List, _Acc=[] ).


remove_last_element( _List=[ _Last ], Acc ) ->
	lists:reverse( Acc );

remove_last_element( _List=[ H | T ], Acc ) ->
	remove_last_element( T, [ H | Acc ] ).



% Returns the last element of the specified list.
%
% Note: not computationnally efficient, usually having to retrieve the last
% element suggests a bad code design.
%
% Crashes (with 'no function clause') if the input list is empty.
%
-spec get_last_element( list() ) -> term().
get_last_element( _List=[ SingleElement ] ) ->
	SingleElement;

get_last_element( _List=[ _H | T ] ) ->
	get_last_element( T ).



% Returns the index, in [1..length(List)], of the (first occurrence of the
% )specified element in the specified list. Throws an exception if the element
% is not found.
%
% Ex: 3 = get_index_of( bar, [ foo, ugh, bar, baz ] )
%
-spec get_index_of( term(), list() ) -> basic_utils:count().
get_index_of( Element, List ) ->
	get_index_of( Element, List, _Count=1 ).


get_index_of( Element, _List=[], _Count ) ->
	throw( { non_existing_element, Element } );

get_index_of( Element, _List=[ Element | _T ], Count  ) ->
	Count;

get_index_of( Element, _List=[ _H | T ], Count ) ->
	get_index_of( Element, T, Count + 1 ).



% Splits the specified (plain) list in two parts (two plain lists, that are
% returned): the first contains the first elements, up to MaxLen (in reverse
% order), and the second the others (if any).
%
% Ex: split_at( [ a, b, c, d, e ], 3 ) = { [ c, b, a ], [ d, e] }
%
-spec split_at( list(), basic_utils:count() ) -> { list(), list() }.
split_at( List, MaxLen ) ->
	split_at( List, _Count=0, MaxLen, _Acc=[] ).


% (helper)
split_at( InputList, _Count=MaxLen, MaxLen, AccList ) ->
	% Max len reached, stopping here:
	{ AccList, InputList };

split_at( _InputList=[], _Count, _MaxLen, AccList ) ->
	% Input list exhausted:
	{ AccList, _InputList=[] };


split_at( _List=[ H | T ], Count, MaxLen, Acc ) ->
	split_at( T, Count + 1, MaxLen, [ H | Acc ] ).




% Returns a list whose elements are the ones of the specified list, except that
% they are unique (all their duplicates have been removed).
%
% No specific order is respected in the returned list.
%
% Ex: if L = [1,2,3,2,2,4,5,5,4,6,6,5], then basic_utils:uniquify(L) is:
% [3,6,2,5,1,4].
%
-spec uniquify( list() ) -> list().
uniquify( List ) ->
	% There is probably a more efficient way of doing the same:
	sets:to_list( sets:from_list( List ) ).



% Tells whether there are in the specified list elements that are present more
% than once.
%
has_duplicates( List ) ->
	length( uniquify( List ) ) =/= length( List ).



% Returns the duplicates in the specified list: returns a list of {
% DuplicatedTerm, DuplicationCount } pairs, where each duplicated term is
% specified, alongside the total number of occurrences of that terms in the
% specified list.
%
% Duplicates are returned in the reverse order of their appearance in the
% specified list; for example, for input list [a,a,b,b,b,c,c,c], returned
% duplicates are [{b,3},{c,3},{a,2}].
%
-spec get_duplicates( list() ) -> list( { term(), basic_utils:count() } ).
get_duplicates( List ) ->
	get_duplicates( List, _Acc=[] ).

get_duplicates( _List=[], Acc ) ->
	Acc;

get_duplicates( _List=[ Term | T ], Acc ) ->

	% io:format( "Inquiring about term '~p' into ~p.~n", [ Term, T ] ),

	case count_and_filter_term( Term, _InitialList=T, _FilteredList=[],
								_InitialCount=0 ) of

		not_found ->
			% No a duplicated element, just iterating on the next term:
			get_duplicates( T, Acc );

		{ TermCount, FilteredList } ->
			% We already extracted the first Term:
			get_duplicates( FilteredList, [ { Term, TermCount + 1 } | Acc ] )

   end.



count_and_filter_term( _Term, _List=[], _FilteredList, _CurrentCount=0 ) ->
	not_found;

count_and_filter_term( _Term, _List=[], FilteredList, CurrentCount ) ->
	{ CurrentCount, FilteredList };

% Term found:
count_and_filter_term( Term, _List=[ Term | H ], FilteredList, CurrentCount ) ->
	count_and_filter_term( Term, H, FilteredList, CurrentCount + 1 );

% Other term:
count_and_filter_term( Term, _List=[ OtherTerm | H ], FilteredList,
					   CurrentCount ) ->
	count_and_filter_term( Term, H, [ OtherTerm | FilteredList ],
						   CurrentCount ).



% Returns the intersection of the two specified lists, i.e. the list of all
% elements that are in both lists.
%
% See also: subtract_all_duplicates/2.
%
-spec intersect( list(), list() ) -> list().
intersect( L1, L2 ) ->
	lists:filter( fun( E ) -> lists:member( E, L2 ) end, L1 ).



% Returns a list equal to L1 except that all elements found in L2 have been
% removed, even if in L1 they were duplicated.
%
% Note: like lists:subtract, except that *all* occurences from L2 in L1 (not
% only the first one) are removed.
%
% Example: [1,4] = basic_utils:subtract_all_duplicates( [1,2,3,4,2], [2,3] )
%
% Taken from
% http://www.trapexit.org/Finding_Elements_in_One_Array_but_Not_Another
%
-spec subtract_all_duplicates( list(), list() ) -> list().
subtract_all_duplicates( L1, L2 ) ->
	lists:filter( fun( E ) -> not lists:member( E, L2 ) end, L1 ).



% Returns a copy of the specified list where the first element matching Elem is
% deleted, ensuring at least one of these elements exists (as opposed to
% lists:delete/2). The order of the specified list is preserved.
%
-spec delete_existing( term(), list() ) -> list().
delete_existing( Elem, List ) ->
	delete_existing( Elem, List, _Acc=[] ).


delete_existing( Elem, _List=[], _Acc ) ->
	throw( { element_to_delete_not_found, Elem } );

delete_existing( Elem, _List=[ Elem | T ], Acc ) ->
	% The first element found stops the iteration:
	lists:reverse( Acc ) ++ T;

delete_existing( Elem, _List=[ H | T ], Acc ) ->
	delete_existing( Elem, T, [ H | Acc ] ).



% Returns a copy of the specified list where all elements matching Elem are
% deleted, whether or not there is any.
%
% The element order of the specified list is preserved.
%
-spec delete_all_in( term(), list() ) -> list().
delete_all_in( Elem, List ) ->
	delete_all_in( Elem, List, _Acc=[] ).


delete_all_in( _Elem, _List=[], Acc ) ->
	lists:reverse( Acc );

delete_all_in( Elem, _List=[ Elem | T ], Acc ) ->
	% An element not to retain:
	delete_all_in( Elem, T, Acc );

delete_all_in( Elem, _List=[ H | T ], Acc ) ->
	% Non-matching, keep it:
	delete_all_in( Elem, T, [ H | Acc ] ).




% Appends specified element at the end of specified list, without changing the
% order of the list.
%
% Ex: append_at_end( d, [a,b,c] ) returns [a,b,c,d].
%
% Note: usually such an addition should be avoided, as it is costly.
%
-spec append_at_end( any(), list() ) -> nonempty_list().
append_at_end( Elem, L ) when is_list(L) ->
	% Should be more efficient than lists:reverse( [Elem|lists:reverse(L)] ):
	L ++ [ Elem ].



% Returns whether the specified list contains only integers.
-spec is_list_of_integers( any() ) -> boolean().
is_list_of_integers( [] ) ->
	true;

is_list_of_integers( [ H | T ] ) when is_integer(H) ->
	is_list_of_integers( T );

is_list_of_integers( _ ) ->
	false.



% Compares the two specified lists with no regard to the order of their
% elements: returns true iff they have the exact same elements (differentiating
% between 1 and 1.0 for example), possibly in a different order.
%
-spec unordered_compare( list(), list() ) -> boolean().
unordered_compare( L1, L2 ) ->
	lists:sort( L1 ) =:= lists:sort( L2 ).



% Adds each element of PlainList (a plain list) in ListImpl (a ?list_impl list),
% if this element is not already there, and returns the corresponding
% ?list_impl. So at the end all elements of PlainList will be exactly once in
% the returned list, with all past elements still there.
%
% Supposedly PlainList is rather short and ListImpl can be rather long.
%
-spec add_if_not_existing( list(), ?list_impl_type ) -> ?list_impl_type.
add_if_not_existing( _PlainList=[], ListImpl ) ->
		ListImpl ;

add_if_not_existing( _PlainList=[ H | T ], ListImpl ) ->

  case ?list_impl:is_member( H, ListImpl ) of

	 true ->
		 % Already there, not to added:
		 add_if_not_existing( T, ListImpl );

	 false ->
		 % Not present, so let's add it:
		 add_if_not_existing( T, ?list_impl:add_element( H, ListImpl ) )

  end.





% Section for listimpl-relation operations.


% Ensures that the specified element was indeed in the specified list before
% removing it and returning the resulting list.
%
-spec safe_listimpl_delete( term(), ?list_impl_type ) -> ?list_impl_type.
safe_listimpl_delete( Element, List ) ->

	case ?list_impl:is_element( Element, List ) of

		true ->
			?list_impl:del_element( Element, List );

		false ->
			throw( { non_existing_element_to_delete, Element,
					?list_impl:to_list( List ) } )

	end.

	% Quicker, less safe (no checking) version:
	%?list_impl:del_element( Element, List );



% Returns a list_impl list made of the first list_impl list to which the
% elements of the specified plain list have been added (it is basically a
% ?list_impl ++ list() -> ?list_impl operations).
%
-spec listimpl_add( ?list_impl_type, list() ) -> ?list_impl_type.
listimpl_add( ListImplList, _PlainList=[] ) ->
	ListImplList;

listimpl_add( ListImplList, _PlainList=[ H | T ] ) ->
	listimpl_add( ?list_impl:add_element( H, ListImplList ), T ).




% Ring-related section.



% Returns a ring corresponding to the specified list.
%
-spec list_to_ring( list() ) -> ring().
list_to_ring( InputList ) ->
	{ InputList, InputList }.



% Pops the head of specified ring: return { Head, UpdatedRing }.
%
-spec head( ring() ) -> { term(), ring() }.
head( { _WorkingList=[], ReferenceList } ) ->
	% Replenish:
	%
	% Dialyzer does not want an opaque argument to be used:
	%head( { ReferenceList, ReferenceList } );
	head( list_to_ring( ReferenceList ) );

head( { _WorkingList=[ H | T ], ReferenceList } ) ->
	{ H, { T, ReferenceList } }.



% Returns a list of the Count popped elements (in their order in the ring), and
% the updated ring.
%
% Ex: for a new ring based on [ a, b, c, d ], if Count=6 then [ a, b, c, d, a,
% b] will be returned.
%
-spec get_next( basic_utils:count(), ring() ) -> { [ term() ], ring() }.
get_next( Count, Ring ) ->
	% Quite similar to a map:foldl/3:
	get_next_helper( Count, Ring, _Acc=[] ).


get_next_helper( _Count=0, Ring, Acc ) ->
	{ lists:reverse( Acc ), Ring };

get_next_helper( Count, Ring, Acc ) ->
	{ H, NewRing } = head( Ring ),
	get_next_helper( Count-1, NewRing, [ H | Acc ] ).



% Returns the number of elements in the specified ring.
%
-spec get_ring_size( ring() ) -> basic_utils:count().
get_ring_size( _Ring={  _WorkingList, ReferenceList } ) ->
	length( ReferenceList ).



% Section to perform random operations on lists.



% Returns a random uniform permutation of the specified list.
%
% Inspired from http://paste.lisp.org/display/74804.
%
% All these algorithms would need random access to a list, which is not readily
% possible here, hence must be emulated.
%
% See also the 'Speedy unsort:shuffle/1,2' thread in the erlang-questions
% mailing list for counterparts.
%
-spec random_permute( list() ) -> list().
random_permute( List ) ->
	random_permute( List, length( List ) ).


random_permute( _List, _RemainingLen=0 ) ->
	[];

random_permute( List, RemainingLen ) ->

	% Checking is commented-out:
	%RemainingLen = length( List ),

	% (using remove_element_at/2 should be quicker than using
	% proplists:delete/2, as we stop at the first matching element found)
	%
	Index = random_utils:get_random_value( RemainingLen ),

	%io:format( "Index=~p, ", [ Index ] ),

	% We put the drawn element at head, and recurse in the remaining list:
	[ get_element_at( List, Index )
		| random_permute( remove_element_at( List, Index ),
						  RemainingLen - 1 ) ].



% Returns a reciprocal random uniform permutation of the specified list,
% compared to random_permute/1.
%
% Consists on the reciprocal operation, so that, if starting from a random state
% S (see set_random_state/1) and if L2 = random_permute( L1 ), then, if starting
% again from S, L1 = random_permute_reciprocal( L2 ).
%
-spec random_permute_reciprocal( list() ) -> list().
random_permute_reciprocal( List ) ->

	% This is a little trickier than random_permute/1; we have to reverse
	% operations for latest to first, hence we must start with the latest drawn
	% value. So we draw them all first, and start by the end of that list,
	% taking into account that the range is decremented at each draw:
	%
	ReciprocalIndex = lists:reverse( [ random_utils:get_random_value( L )
			|| L <- lists:reverse( lists:seq( 1, length( List ) ) ) ] ),

	%io:format( "Reciprocal index = ~p~n", [ ReciprocalIndex ] ),

	random_permute_reciprocal( lists:reverse( List ), ReciprocalIndex,
							   _Acc=[] ).


random_permute_reciprocal( _List=[], _ReciprocalIndex=[], Acc ) ->
	Acc;

random_permute_reciprocal( _List=[ H | T ], _ReciprocalIndex=[ I | Is ],
						 Acc ) ->

	NewAcc = insert_element_at( _Elem=H, Acc, _Index=I ),

	random_permute_reciprocal( T, Is, NewAcc ).



% Draws one element at random of the specified list, knowing they all have the
% same probability of being drawn (uniform probability).
%
-spec draw_element( [ any() ] ) -> any().
draw_element( _ElementList=[] ) ->
	throw( cannot_draw_from_empty_list );

draw_element( ElementList ) ->
	Len = length( ElementList ),
	draw_element( ElementList, Len ).



% Draws one element at random of the specified list, whose length must be the
% specified one (allows to precompute it once for multiple drawings), knowing
% all elements have the same probability of being drawn (uniform probability).
%
-spec draw_element( [ any() ], basic_utils:count() ) -> any().
draw_element( ElementList, Length ) ->

	DrawnIndex = random_utils:get_random_value( Length ),

	get_element_at( ElementList, DrawnIndex ).

	% Alternate, less efficient, implementation:

	% Same probability:
	%UniformList = [ { Elem, 1 } || Elem <- ElementList ],
	%draw_element_weighted( UniformList ).



% Draws one element at random of the specified list, which is a list of {
% Element, Probability } pairs: returns the drawn element, knowing that it will
% be chosen according to the probability associated to each element.
%
% Probabilities are managed as integer values defined relatively to each other
% (and they do not have to sum up to 1.0); they must be positive or null
% integers, and their sum must not be null.
%
% Ex: ElementList = [ {first,1}, {second,2}, {third,1} ] is excepted to return
% on average 'second' twice as frequently as 'first' or 'third'.
%
% Using [ {first,1}, {second,0}, {third,1} ] instead would mean that 'second'
% would never be drawn.
%
-spec draw_element_weighted( [ { any(), integer() } ] ) -> any().
draw_element_weighted( ElementList ) ->
	draw_element_weighted( ElementList, sum_probabilities( ElementList ) ).


-spec sum_probabilities( [ { _Element, number() } ] ) -> number().
sum_probabilities( ElementList ) ->
	sum_probabilities( ElementList, _Acc=0 ).


sum_probabilities( _ElementList=[], Acc ) ->
	Acc;

sum_probabilities( _ElementList=[ { _Element, Probability } | T ], Acc ) ->
	sum_probabilities( T, Acc + Probability ).



% Sum must be equal to the sum of all probabilities in ElementList.
draw_element_weighted( _ElementList, 0 ) ->
	throw( null_total_probability );

draw_element_weighted( ElementList, Sum ) ->
	DrawnValue = random_utils:get_random_value( Sum ),
	%io:format( "draw_element: drawn ~B.~n", [ DrawnValue ] ),
	select_element( ElementList, DrawnValue, _CurrentSum=0 ).



select_element( [ { Element, Probability } | _T ], DrawnValue, CurrentSum )
		when Probability + CurrentSum >= DrawnValue ->
	% Just gone past the probability range:
	Element;

select_element( [ { _Element, Probability } | T ], DrawnValue, CurrentSum ) ->
	% Drawn value still not reached, continuing:
	select_element( T, DrawnValue, CurrentSum + Probability ).



% Draws the specified number of elements at random of the specified list,
% knowing they all have the same probability of being drawn initially, but when
% an element is drawn, it is removed from the candidate list so that the next
% drawing operates on the resulting shorten list.
%
-spec draw_elements_from( [ any() ], basic_utils:count() ) -> [ any() ].
draw_elements_from( ElementList, Count ) ->
	draw_elements_from( ElementList, Count, _Acc=[] ).


draw_elements_from( _ElementList, _Count=0, Acc ) ->
	Acc;

draw_elements_from( ElementList, Count, Acc ) ->

	Drawn = draw_element( ElementList ),

	ShortenList = lists:delete( Drawn, ElementList ),

	draw_elements_from( ShortenList, Count - 1, [ Drawn | Acc ] ).
