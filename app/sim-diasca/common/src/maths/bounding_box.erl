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
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for bounding box management.
%
% Currently the types of supported bounding boxes are:
%
%  - circle, either obtained from the 'lazy' algorithm or the 'mec' algorithm
%  - cuboid, see http://en.wikipedia.org/wiki/Cuboid
%
% With the lazy algorithm, circle parameters are simply deduced from the
% smallest enclosing rectangle; it is fast and easy, yet less precis than MEC.
%
% MEC leads to determine the Minimal Enclosing Circle. The operation involves
% computing the convex hull of the points. It is expensive, but not a problem
% when precomputing.
%
% See bounding_box_test.erl for the corresponding test.
%
-module(bounding_box).


-export([ get_lazy_circle_box/1, get_minimal_enclosing_circle_box/1,
		  to_string/1 ]).


% For record declarations of bounding boxes:
-include("bounding_box.hrl").


-type circle() :: #circle{}.


-type right_cuboid() :: #right_cuboid{}.


% All known types of bounding boxes:
-type bounding_box() :: circle() | right_cuboid().


-export_type([ circle/0, right_cuboid/0, bounding_box/0 ]).



% Returns a disc which is a bounding-box for the specified list of points, which
% must not be empty.
%
% Note: this bounding box is not the smallest one, but is very lightweight to
% compute.
%
% Returns the disc information: { Center, SquareRadius }.
%
-spec get_lazy_circle_box( [ linear_2D:point() ] ) ->
		{ linear_2D:integer_point(), linear:square_distance() }.
get_lazy_circle_box( PointList ) ->

	{ TopLeft, BottomRight } = linear_2D:compute_smallest_enclosing_rectangle(
							  PointList ),

	Center = linear_2D:get_integer_center( TopLeft, BottomRight ),

	% We divide by 4, as we are dealing with squared quantities:
	SquareRadius = linear_2D:square_distance( TopLeft, BottomRight ) / 4,

	{ Center, SquareRadius }.



% Returns { Center, SquareRadius } which defines a bounding-box consisting on
% the minimal enclosing circle (MEC) for the specified list of points.
%
% Note: this bounding box is the smallest possible circle, but requires quite a
% lot of computations.
%
% Apparently there is now way of adding a point to an existing MEC without
% recomputing everything from scratch. So we do not rely on an 'updateMEC'
% function.

-spec get_minimal_enclosing_circle_box( [ linear_2D:point() ] ) ->
		{ linear_2D:point(), linear:square_distance() }.
get_minimal_enclosing_circle_box( _PointList=[] ) ->
	throw( no_point_to_enclose );

get_minimal_enclosing_circle_box( _PointList=[ P ] ) ->
	% Only one point, epsilon-based comparison allows for a null radius:
	{ _Center=P, _SquareRadius=0 };

get_minimal_enclosing_circle_box( _PointList=[ P1, P2 ] ) ->

	% Here we have two points, which defines the circle:
	Center = linear_2D:get_center( P1, P2 ),

	% Division by 4, not 2, as we deal with square quantities:
	SquareRadius = linear_2D:square_distance( P1, P2 ) / 4,

	{ Center, SquareRadius };


get_minimal_enclosing_circle_box( _PointList=[ P1, P2, P3 ] ) ->

	%io:format( "get_minimal_enclosing_circle_box for 3 points: "
	%		   "~w, ~w and ~w.~n", [ P1, P2, P3 ] ),

	% Here we have three points, a triangle, which defines the circumscribed
	% circle, whose center is the intersection of the three perpendicular
	% bisectors.
	%
	% Let La be the perpendicular bisector of [P1,P2] and Lb the one of [P1,P3]:
	Pa = linear_2D:get_center( P1, P2 ),
	La = linear_2D:get_line( Pa, linear_2D:vectorize( P1, P2 ) ),
	Pb = linear_2D:get_center( P1, P3 ),
	Lb = linear_2D:get_line( Pb, linear_2D:vectorize( P1, P3 ) ),

	case linear_2D:intersect( La, Lb ) of

		no_point ->
			% Here the three vertices must be aligned, we could find the two
			% most distant points and use them as a MEC diameter.
			throw( flat_triangle );

		Center ->
			{ Center, linear_2D:square_distance( Center, P1 ) }

	end;


get_minimal_enclosing_circle_box( PointList ) ->

	% Here we have at least three points, let's work an the hull instead:
	% See http://www.cs.mcgill.ca/~cs507/projects/1998/jacob/solutions.html
	% for the solution.
	%io:format( "MEC for ~w.~n", [ PointList ] ),

	case linear_2D:compute_convex_hull( PointList ) of

		[ P1, P2 | H ] ->
			% We start with a side S defined by P1 and P2 here:
			try_side( P1, P2, H );

		Other ->
			throw( { unexpected_hull, Other } )

	end.




% Returns { MinAngle, MinVertex }, the minimum angle subtended by the segment
% [ P1, P2 ] among points in the Points list.
%
-spec find_minimal_angle( linear_2D:point(), linear_2D:point(),
						 [ linear_2D:point() ] ) ->
		{ number(), 'undefined' } | { integer(), linear_2D:point() }.
find_minimal_angle( _P1, _P2, _Points=[] ) ->
	throw( { find_minimal_angle, not_enough_points } );

find_minimal_angle( P1, P2, _Points=[ Pfirst | OtherPoints ] ) ->

	%io:format( "Trying to find minimal angle in ~w for ~w and ~w.~n",
	%		   [ Points, P1, P2 ] ),

	BootstrapAngle = linear_2D:abs_angle_deg( Pfirst, P1, P2 ),

	find_minimal_angle( P1, P2, OtherPoints, _MinAngle=BootstrapAngle,
					_MinVertex=Pfirst ).


% Points was not empty, thus not 'undefined' in the returned pair:
find_minimal_angle( _P1, _P2, _Points=[], MinAngle, MinVertex ) ->
	{ MinAngle, MinVertex };

find_minimal_angle( P1, P2, [ P | OtherPoints ], MinAngle, MinVertex ) ->

	case linear_2D:abs_angle_deg( P, P1, P2 ) of

		 Angle when Angle =< MinAngle ->
			 % We have a new winner here:
			 find_minimal_angle( P1, P2, OtherPoints, Angle, P );

		 _NonMinimalAngle ->
			 find_minimal_angle( P1, P2, OtherPoints, MinAngle, MinVertex )

	 end.



% Helper:
% Tries the side of the convex hull H defined by vertices P1 and P2.
try_side( P1, P2, H ) ->

	{ MinAngle, MinVertex } = find_minimal_angle( P1, P2, H ),

	%io:format( "Trying side [ ~w, ~w ], min vertex: ~w.~n", [ P1, P2, MinVertex
	%] ),

	case MinAngle of

		FirstAngle when FirstAngle > 90 ->
			% Finished, P1 and P2 determine the diametric circle, reusing the
			% code for that:
			get_minimal_enclosing_circle_box( [ P1, P2 ] );

		_FirstAngleTooSmall ->

			SecondAngle = linear_2D:abs_angle_deg( P1, MinVertex, P2 ),

			case linear_2D:is_obtuse( SecondAngle ) of

				false ->
					ThirdAngle = linear_2D:abs_angle_deg( P2, MinVertex, P1 ),

					case linear_2D:is_obtuse( ThirdAngle ) of

						false ->
							% MEC determined by P1, P2 and MinVertex:
							get_minimal_enclosing_circle_box(
							  [ P1, P2, MinVertex ] );

						true ->
							% Here we try the new S, defined by the opposite
							% points of P2, i.e. MinVertex and P1.

							% We must however reconstruct beforehand the list of
							% remaining points. H contains MinVertex but not P2:
							NewH = [ P2 | lists:delete( MinVertex, H ) ],
							try_side( MinVertex, P1, NewH )

					end;

				true ->
					% Here we try the new S, defined by the opposite points of
					% P1, i.e. MinVertex and P2.

					% We must however reconstruct beforehand the list of
					% remaining points. H contains MinVertex but not P1:
					NewH = [ P1 | lists:delete( MinVertex, H ) ],
					try_side( MinVertex, P2, NewH )

			end

	end.



% Returns a textual description of the specified bounding box.
%
-spec to_string( bounding_box() ) -> string().
to_string( #circle{ center=Center, square_radius=SquareRadius } ) ->

	io_lib:format( "circle whose center is ~w and square radius is ~w",
				[ Center, SquareRadius ] );


to_string( #right_cuboid{ base_vertex=BaseVertex, abscissa_length=XLen,
						 ordinate_length=YLen, elevation_length=ZLen } ) ->

	io_lib:format( "right cuboid whose base vertex is ~s, and lengths along "
				   "the X, Y and Z axes are respectively ~w, ~w and ~w",
				   [ linear_3D:to_string( BaseVertex ), XLen, YLen, ZLen ] ).
