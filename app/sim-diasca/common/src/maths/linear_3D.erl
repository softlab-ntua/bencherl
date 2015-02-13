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


% Gathering of various three dimensional linear facilities.
%
% See linear_3D_test.erl for the corresponding test.
%
-module(linear_3D).



% Operations on points:
%
-export([ get_origin/0, are_close/2, is_within/3, is_within_square/3,
		  square_distance/2, distance/2, cross_product/2, roundify/1,
		  get_integer_center/2, get_center/2, translate/2 ]).


% Operations on vectors:
%
-export([ vectorize/2, square_magnitude/1, magnitude/1, scale/2, make_unit/1,
		  dot_product/2 ]).



% Operations common to points and vectors:
%
-export([ add/1, add/2 ]).



% Textual conversions:
%
-export([ to_string/1 ]).



% For epsilon:
-include("math_utils.hrl").



-type point() :: { linear:coordinate(), linear:coordinate(),
				  linear:coordinate() }.

-type integer_point() :: { linear:integer_coordinate(),
				   linear:integer_coordinate(), linear:integer_coordinate() }.


% Vectors could/should be aliased to points:
-type vector() :: { linear:coordinate(), linear:coordinate(),
				   linear:coordinate() }.

-type integer_vector() :: { linear:integer_coordinate(),
				   linear:integer_coordinate(), linear:integer_coordinate() }.



% A line, whose equation A.x+B.y+C.z+D=0, can be defined by its four
% coefficients {A,B,C,D}.
%
-type line() :: { number(), number(), number(), number() }.

-type shape() :: 'sphere' | 'right_cuboid'.


-export_type([ point/0, integer_point/0, vector/0, integer_vector/0,
			   line/0, shape/0 ]).




% Point section.


% Returns the origin of this referential.
%
-spec get_origin() -> point().
get_origin() ->
	{ 0, 0, 0 }.



% Returns whether the two specified points are close, i.e. if they could be
% considered as representing the same point (equality operator on points).
%
-spec are_close( point(), point() ) -> boolean().
are_close( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 )
		andalso math_utils:are_close( Z1, Z2 ).



% Tells whether point P is within a distance D from point C, using some margin
% to overcome numerical errors.
%
-spec is_within( point(), point(), number() ) -> boolean().
is_within( P, C, D ) ->
	% "Taylor series", square(epsilon) is negligible here:
	square_distance( P, C ) < D * ( D + ?epsilon ).



% Tells whether point P is within a square distance SquareD from point C.
%
-spec is_within_square( point(), point(), number() ) -> boolean().
is_within_square( P, C, SquareD ) ->
	square_distance( P, C ) < SquareD.



% Returns the square of the distance between the two specified points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and square_magnitude as well.
%
-spec square_distance( point(), point() ) -> linear:square_distance().
square_distance( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->

	XDiff = X2 - X1,
	YDiff = Y2 - Y1,
	ZDiff = Z2 - Z1,

	XDiff*XDiff + YDiff*YDiff + ZDiff*ZDiff.



% Returns the distance between the two specified points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and magnitude as well.
%
-spec distance( point(), point() ) -> linear:distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% Returns the cross-product of the two specified points, i.e. the magnitude
% of the vector that results from a regular 3D cross product of the input
% vectors.
-spec cross_product( vector(), vector() ) -> vector().
cross_product( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
	{ Y1*Z2 - Z1*Y2, Z1*X2 - X1*Z2, X1*Y2 - Y1*X2 }.




% Returns a point (or vector) whose coordinates have been rounded to nearest
% integer.
%
-spec roundify( point() ) -> integer_point().
roundify( {X,Y,Z} ) ->
	{ erlang:round(X), erlang:round(Y), erlang:round(Z) }.



% Returns a vertex corresponding the middle of the two specified vertices,
% returned with integer coordinates.
%
-spec get_integer_center( point(), point() ) -> integer_point().
get_integer_center( P1, P2 ) ->
	roundify( get_center( P1, P2 ) ).



% Returns a vertex corresponding the middle of the two specified vertices,
% returned with possibly floating-point coordinates.
%
-spec get_center( point(), point() ) -> point().
get_center( {X1,Y1,Z1}, {X2,Y2,Z2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2, (Z1+Z2)/2 }.



% Returns a point corresponding to the specified point P translated by the
% specified vector V.
%
-spec translate( point(), vector() ) -> point().
translate( _P={X,Y,Z}, _V={Vx,Vy,Vz} ) ->
	{ X+Vx, Y+Vy, Z+Vz }.





% Section for sets of points.



% Vector section.


% Returns a vector V made from the specified two points: V=P2-P1.
%
-spec vectorize( point(), point() ) -> vector().
vectorize( _P1={X1,Y1,Z1}, _P2={X2,Y2,Z2} ) ->
	{ X2-X1, Y2-Y1, Z2-Z1 }.



% Returns the square of the magnitude of the specified vector.
%
-spec square_magnitude( vector() ) -> linear:square_distance().
square_magnitude( _V={X,Y,Z} ) ->
	X*X + Y*Y + Z*Z.



% Returns the magnitude of the specified vector.
%
-spec magnitude( vector() ) -> linear:distance().
magnitude( V ) ->
	math:sqrt( square_magnitude(V) ).



% Scales specified vector of specified factor.
%
-spec scale( vector(), number() ) -> vector().
scale( _V={X,Y,Z}, Factor ) ->
	{ Factor*X, Factor*Y, Factor*Z }.






% Returns the specified vector with an unit length (magnitude of 1):
%
% (epsilon-based test for null vectors with floating-point coordinates could
% be done here).
%
-spec make_unit( vector() ) -> vector().
make_unit( {0,0,0} ) ->
	throw( cannot_make_null_vector_unit );

make_unit( V ) ->
	scale( V, 1 / magnitude(V) ).



% Returns the dot product of the two specified vectors.
%
-spec dot_product( vector(), vector() ) -> number().
dot_product( _V1={X1,Y1,Z1}, _V2={X2,Y2,Z2} ) ->
	X1*X2 + Y1*Y2 + Z1*Z2.



% Returns the sum of the two specified vectors.
%
-spec add( vector(), vector() ) -> vector().
add( { X1, Y1, Z1 }, { X2, Y2, Z2 } ) ->
	{ X1 + X2, Y1 + Y2, Z1 + Z2 }.



% Returns the sum of all vectors in specified lists.
%
-spec add( [ vector() ] ) -> vector().
add( Vectors ) ->

	lists:foldl( fun( { X, Y, Z }, _AccVec={ Xa, Ya, Za } ) ->
						 { X + Xa, Y + Ya, Z + Za }
				 end,
				 _InitialAcc={ 0, 0, 0 },
				 _List=Vectors ).





% Textual conversion section.


% Returns a stringified representation of specified parameter.
%
-spec to_string( point() ) -> string().
to_string( { X, Y, Z } ) ->
	io_lib:format( "{ ~w, ~w, ~w }", [ X, Y, Z ] ).
