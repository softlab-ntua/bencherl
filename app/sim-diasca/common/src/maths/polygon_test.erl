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


% Unit tests for polygon management.
%
% Depends on the gui module.
%
% See the polygon tested module.
%
-module(polygon_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	MyTriangle = polygon:update_bounding_box( lazy_circle,
	  polygon:set_edge_color( yellow,
			  polygon:get_triangle( {110,110}, {250,155}, {120,335} ) ) ),

	test_facilities:display( "Triangle description:~n~s",
			   [ polygon:to_string( MyTriangle ) ] ),


	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
	  polygon:set_fill_color( red,
			  polygon:get_upright_square( _Center={250,250},
										  _EdgeLength=50 ) ) ),

	test_facilities:display( "Upright square description:~n~s",
			   [ polygon:to_string( MyUprightSquare ) ] ),


	% Simplest concave polygone has 4 vertices (clockwise defined):
	MyConcavePolygon = polygon:get_polygon( [
			  {0,0}, {20,0}, {10,20}, {14,14} ] ),

	test_facilities:display( "Concave polygon description:~n~s",
			   [ polygon:to_string( MyConcavePolygon ) ] ),


	test_facilities:display( "Diameter information ({P1,P2,SquareDistance}):"
			   "  - for the triangle, we have: ~w"
			   "  - for the upright square, we have: ~w"
			   "  - for the concave polygon, we have: ~w",
			   [ polygon:get_diameter( MyTriangle ),
				 polygon:get_diameter( MyUprightSquare ),
				 polygon:get_diameter( MyConcavePolygon )
			   ] ),

	test_facilities:display( "Smallest enclosing rectangle "
			   "({TopLeftCorner,BottomRightCorner}):"
			   "  - for the triangle, we have: ~w"
			   "  - for the upright square, we have: ~w"
			   "  - for the concave polygon, we have: ~w",
			   [ polygon:get_smallest_enclosing_rectangle( MyTriangle ),
				 polygon:get_smallest_enclosing_rectangle( MyUprightSquare ),
				 polygon:get_smallest_enclosing_rectangle( MyConcavePolygon )
			   ] ),


	test_facilities:display( "Areas:"
			   "  - for the triangle, we have: ~w"
			   "  - for the upright square, we have: ~w"
			   "  - for the concave polygon, we have: ~w",
			   [ polygon:get_area( MyTriangle ),
				 polygon:get_area( MyUprightSquare ),
				 polygon:get_area( MyConcavePolygon )
			   ] ),


	% Clockwise order:
	MySimpleTriangle = polygon:get_triangle( {0,0}, {0,5}, {10,0} ),

	test_facilities:display( "Clockwise test:"
			   "  - is the triangle defined in clockwise order: ~w"
			   "  - is the upright square defined in clockwise order: ~w"
			   "  - is the concave polygon defined in clockwise order: ~w",
			   [ polygon:is_in_clockwise_order( MyTriangle ),
				 polygon:is_in_clockwise_order( MyUprightSquare ),
				 polygon:is_in_clockwise_order( MyConcavePolygon )
			   ] ),


	% Area would be negative if its absolute value was not selected:
	AreaSimple = 25.0 = polygon:get_area( MySimpleTriangle ),

	test_facilities:display( "Area of simple triangle:~n~s is: ~f.",
			   [ polygon:to_string( MySimpleTriangle ), AreaSimple ] ),


	test_facilities:display( "Convexity test:"
			   "  - is the triangle convex: ~w"
			   "  - is the upright square convex: ~w"
			   "  - is the concave polygon convex: ~w",
			   [ polygon:is_convex( MyTriangle ),
				 polygon:is_convex( MyUprightSquare ),
				 polygon:is_convex( MyConcavePolygon )
			   ] ),

	% Rendering tests: see gui_test.erl.

	test_facilities:stop().
