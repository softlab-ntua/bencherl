% Copyright (C) 2012-2014 Olivier Boudeville
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
% Creation date: Tuesday, March 5, 2013



% Gathering of various convenient facilities regarding graphs.
%
% See graph_utils_test.erl for the corresponding test.
%
-module(graph_utils).



% Pathfinding-related operations.
%
-export([ find_breadth_first/3, find_breadth_first/4 ]).



% The type of data-structure accounting for storing the already-explored
% vertices (semantically used as a list):
%
-define( list_impl, gb_sets ).


% Type declarations:


% The node of a graph of interest (any type):
-type vertex() :: any().



% Tells whether the specified node can be considered as a solution:
-type predicate() :: fun( ( vertex(), basic_utils:user_data() ) -> boolean() ).


% Lists the children nodes of the specified one:
-type feeder() :: fun( ( vertex(), basic_utils:user_data() ) -> [ vertex() ] ).


-export_type([ vertex/0, predicate/0, feeder/0 ]).




% Pathfinding-related operations, operating on directed, potentially cyclic,
% graphs.


% Breadth-first look-up in an implicit graph.
%
% Based on a queue.

% See: http://en.wikipedia.org/wiki/Breadth-first_search



% Tries to find, breadth-first, the first vertex to satisfy specified predicate,
% from specified initial vertex, relying on the specified feeder function to
% obtain outbound edges of a vertex of interest.
%
% Returns the path found, i.e. the ordered list of vertices, from the initial
% vertex to the found one.
%
-spec find_breadth_first( vertex(), predicate(), feeder() ) ->
								'not_found' | [ vertex() ].
find_breadth_first( InitialVertex, Predicate, Feeder ) ->
	find_breadth_first( InitialVertex, Predicate, Feeder, _UserData=undefined ).



% Tries to find, breadth-first, the first vertex to satisfy specified predicate,
% from specified initial vertex, relying on the specified feeder function to
% obtain outbound edges of a vertex of interest.
%
% Returns the path found, i.e. the ordered list of vertices, from the initial
% vertex to the found one.
%
% The user data might be anything, from 'undefined' to a description of the
% graph structure (see graph_utils_test.erl).
%
-spec find_breadth_first( vertex(), predicate(), feeder(),
				basic_utils:user_data() ) -> 'not_found' | [ vertex() ].
find_breadth_first( InitialVertex, Predicate, Feeder, UserData ) ->

	% We rely on a queue, to examine vertices at increasing ranges
	% (breadth-first). We insert elements at its back, but pop them from its
	% front (like a list). Elements are paths (reversed lists of vertices).
	%
	InitialQueue = queue:in( [ InitialVertex ], queue:new() ),

	% A sorted list allows to explore each vertex only once (otherwise cycles
	% would prevent termination):
	Explored = ?list_impl:new(),

	find_bfs_helper( Predicate, Feeder, InitialQueue, Explored,
					 UserData ).



% Helper.
%
% One difficulty is that we want not only to find a satisfying vertex, but also
% to return the *path* to it. So, instead of simply recording a vertex in the
% queue, we record the list of vertices leading to that vertex (which is its
% head).
%
find_bfs_helper( Predicate, Feeder, CandidateQueue, ExploredList, UserData ) ->

	case queue:out( CandidateQueue ) of

		{ empty, _Q } ->
			no_path_found;

		{ { value, Path=[ Vertex | _LeadingPath ] }, PoppedQueue } ->

			%io:format( "Examining vertex ~p.~n", [ Vertex ] ),

			case Predicate( Vertex, UserData ) of

				true ->
					% Found!
					%io:format( "Vertex ~p validated!~n", [ Vertex ] ),
					lists:reverse( Path );

				false ->
					% Search must continue; once this generation will have been
					% inspected, will recurse in the next one:
					Children = Feeder( Vertex, UserData ),

					SelectedChildren = [ C || C <- Children,
								  not ?list_impl:is_member( C, ExploredList ) ],

					%io:format( "Vertex ~p not suitable, enqueuing ~w.~n",
					%		    [ Vertex, SelectedChildren ] ),

					% Uncomment if wanting to check the cycle management:

					% case SelectedChildren of

					%	Children ->
					%		ok;

					%	_ ->
					%		io:format( "Removed: ~w.~n", [ lists:subtract(
					%						 Children, SelectedChildren ) ] )

					% end,

					NewQueue = lists:foldl( fun( C, Q ) ->
													queue:in( [ C | Path ], Q )
											end,
								 _Acc0=PoppedQueue,
								 _List=SelectedChildren ),

					NewExplored = ?list_impl:add( Vertex, ExploredList ),

					find_bfs_helper( Predicate, Feeder, NewQueue,
									 NewExplored, UserData )

			end

	end.
