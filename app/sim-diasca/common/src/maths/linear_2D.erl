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


% Gathering of various two dimensional linear facilities.
%
% See linear_2D_test.erl for the corresponding test.
%
-module(linear_2D).



% Operations on points:
%
-export([ get_origin/0, are_close/2, is_within/3, is_within_square/3,
		  square_distance/2, distance/2, cross_product/2, roundify/1,
		  get_integer_center/2, get_center/2, translate/2 ]).


% Operations on vectors:
%
-export([ vectorize/2, square_magnitude/1, magnitude/1, scale/2, make_unit/1,
		  normal_left/1, normal_right/1, dot_product/2 ]).


% Operations on lines:
%
-export([ get_line/2, intersect/2, get_abscissa_for_ordinate/2 ]).


% Operations related to angles:
%
-export([ is_strictly_on_the_right/3, is_obtuse/1, abs_angle_rad/3, angle_rad/3,
		  abs_angle_deg/3, angle_deg/3 ]).


% Operations on set of points:
%
-export([ compute_smallest_enclosing_rectangle/1,
		  compute_max_overall_distance/1, compute_convex_hull/1 ]).


% Textual conversions:
%
-export([ to_string/1, to_string/2 ]).


% Only useful for tests:
%
-export([ find_pivot/1, sort_by_angle/2 ]).



% For epsilon:
-include("math_utils.hrl").



-type point() :: { linear:coordinate(), linear:coordinate() }.
-type integer_point() :: { linear:integer_coordinate(),
						  linear:integer_coordinate() }.

% { Width, Height }:
-type dimensions() :: { linear:integer_coordinate(),
						  linear:integer_coordinate() }.

% Vectors could/should be aliased to points:
-type vector() :: { linear:coordinate(), linear:coordinate() }.
-type integer_vector() :: { linear:integer_coordinate(),
						   linear:integer_coordinate() }.



% A line, whose equation A.x+B.y+C=0, can be defined by its three coefficients
% {A,B,C}.
%
-type line() :: { number(), number(), number() }.

-type shape() :: 'circle' | 'rectangle' | 'square' | 'triangle' | 'polygon'.


-export_type([ point/0, integer_point/0, dimensions/0,
			   vector/0, integer_vector/0, line/0,
			   shape/0 ]).




% Point section.


% Returns the origin of this referential.
%
-spec get_origin() -> point().
get_origin() ->
	{ 0, 0 }.


% Returns whether the two specified points are close, i.e. if they could be
% considered as representing the same point (equality operator on points).
%
-spec are_close( point(), point() ) -> boolean().
are_close( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	math_utils:are_close( X1, X2 ) andalso math_utils:are_close( Y1, Y2 ).



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
square_distance( {X1,Y1}, {X2,Y2} ) ->
	XDiff = X2-X1,
	YDiff = Y2-Y1,
	XDiff*XDiff + YDiff*YDiff.



% Returns the distance between the two specified points.
%
% For comparison purposes, computing the square root is useless.
%
% Could rely on vectorize and magnitude as well.
%
-spec distance( point(), point() ) -> linear:distance().
distance( P1, P2 ) ->
	math:sqrt( square_distance( P1, P2 ) ).



% Returns the cross-product of the two specified 2D points, i.e. the magnitude
% of the vector that would result from a regular 3D cross product of the input
% vectors, taking their Z values implicitly as 0.
-spec cross_product( point(), point() ) -> number().
cross_product( {X1,Y1}, {X2,Y2} ) ->
	X1*Y2 - Y1*X2.



% Returns a point (or vector) whose coordinates have been rounded to nearest
% integer.
%
-spec roundify( point() ) -> integer_point().
roundify( {X,Y} ) ->
	{ erlang:round(X), erlang:round(Y) }.



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
get_center( {X1,Y1}, {X2,Y2} ) ->
	{ (X1+X2)/2, (Y1+Y2)/2 }.



% Returns a point corresponding to the specified point P translated by the
% specified vector V.
%
-spec translate( point(), vector() ) -> point().
translate( _P={X,Y}, _V={Vx,Vy} ) ->
	{ X+Vx, Y+Vy }.




% Section for sets of points.


% Computes the smallest rectangle that encloses the specified list of points.
%
% Returns { TopLeft, BottomRight }.
%
-spec compute_smallest_enclosing_rectangle( [ point() ] ) ->
												  { point(), point() }.
compute_smallest_enclosing_rectangle( Points ) ->
	compute_smallest_enclosing_rectangle( Points, undefined, undefined ).




% Helper:
compute_smallest_enclosing_rectangle( _Points=[], TopLeft, BottomRight ) ->
	{ TopLeft, BottomRight };

compute_smallest_enclosing_rectangle( [ _Points=P | Others ], undefined,
									 undefined ) ->
	% First found initializes best, knowing at least two points are expected:
	compute_smallest_enclosing_rectangle( Others, _TopLeft=P, _BottomRight=P );

compute_smallest_enclosing_rectangle( [ _Points={ X, Y } | Others ], { Xt, Yt },
									 { Xb, Yb } ) ->
	Xmin = erlang:min( X, Xt ),
	Ymin = erlang:min( Y, Yt ),
	Xmax = erlang:max( X, Xb ),
	Ymax = erlang:max( Y, Yb ),
	compute_smallest_enclosing_rectangle( Others, { Xmin, Ymin },
										 { Xmax, Ymax } ).



% Computes the maximum distance between two points in the specified list of
% points.
%
% Returns {P1,P2,square_distance(P1,P2)} so that (square) distance is maximal.
% We ensure that each internal edge is examined only once: when the distance
% between a given vertex V and all other vertices have been computed, V is
% removed from the list and a new mAximum is searched within this subset.
%
% Here there is only one vertex left:
%
-spec compute_max_overall_distance( [ point() ] )
		-> { point(), point(), linear:square_distance() }.
compute_max_overall_distance( Points ) when length( Points ) < 2 ->
	throw( { no_computable_overall_distance, Points } );

compute_max_overall_distance( Points ) ->
	compute_max_overall_distance( Points, undefined ).


% Helper.
compute_max_overall_distance( _Points=[ _H ], Longest ) ->
	Longest;

% Here we have not compute a distance yet:
compute_max_overall_distance( _Points=[ H | Others ], undefined ) ->
	FirstEntry = compute_max_distance_between( H, Others ),
	compute_max_overall_distance( Others, _FirstBest=FirstEntry );

% At least one other vertex remains, and at least one distance was computed:
compute_max_overall_distance( _Points=[ H | Others ],
					  Best={ _V1, _V2, LongestSquareDistance } ) ->

	case compute_max_distance_between( H, Others ) of

		NewBest={ _H, _PmaxForH, LongestSquareDistanceFromH }
		  when LongestSquareDistanceFromH > LongestSquareDistance ->
			% We have a new winner:
			compute_max_overall_distance( Others, NewBest );

		_Other ->
			% Here LongestSquareDistance is not beaten:
			compute_max_overall_distance( Others, Best )

	end.



% Computes the maximum distance between a point (P) and a list of other points.
%
% Returns {P,Pmax,LongestSquareDistance} with LongestSquareDistance being the
% distance between P and Pmax, Pmax being chosen so that LongestSquareDistance
% is maximal.
%
% As there must have been at least one point in the list, Pmax exists here
% (never undefined):
-spec compute_max_distance_between( point(), [ point() ] )
	-> { point(), point(), linear:square_distance() }.
compute_max_distance_between( _P, [] ) ->
	throw( no_computable_max_distance );

compute_max_distance_between( P, Points ) ->
	compute_max_distance_between( P, Points, undefined ).


compute_max_distance_between( P, _Points=[], {Pmax,LongestSquareDistance} ) ->
	{P,Pmax,LongestSquareDistance};

compute_max_distance_between( P, _Points=[ Pnew | OtherPoints ], undefined ) ->
	% First point examined is at first by construction the first best:
	compute_max_distance_between( P, OtherPoints,
						  { Pnew, linear_2D:square_distance( P, Pnew ) } );

compute_max_distance_between( P, _Points=[ Pnew | OtherPoints ],
							  Best={ _Pmax, LongestSquareDistance } ) ->

	case linear_2D:square_distance( P, Pnew ) of

		SquareDistance when SquareDistance > LongestSquareDistance ->
			% We have a new winner:
			compute_max_distance_between( P, OtherPoints,
										  { Pnew, SquareDistance } );

		_LesserSquareDistance ->
			% Previous best not beaten, let's keep it:
			compute_max_distance_between( P, OtherPoints, Best )

	end.




% Sorting by angle section.


% Finds the pivot, i.e. the leftmost point with the highest ordinate.
%
% The point list is supposed not having duplicates.
%
% Returns {Pivot,PivotLessList} where PivotLessList is the (unordered) input
% list, without the Pivot.
%
-spec find_pivot( [ point() ] ) -> { point(), [ point() ] }.
find_pivot( _PointList = [ FirstPivot | Others ] ) ->
	% First found is the first pivot:
	find_pivot( Others, FirstPivot, _NewList=[] ).


% Helper:
find_pivot( [], Pivot, NewList ) ->
	{ Pivot, NewList };

% Higher than the pivot, thus not wanted as pivot:
find_pivot( [ Point={_X,Y} | Others ], Pivot={ _Xp, Yp }, NewList ) when Y<Yp ->
	find_pivot( Others, Pivot, [ Point | NewList ] );

% Lower than the pivot, thus wanted:
find_pivot( [ Point={_X,Y} | Others ], PreviousPivot={ _Xp, Yp }, NewList )
  when Y>Yp ->
	find_pivot( Others, Point, [ PreviousPivot | NewList ] );


% Same level as the pivot, but at its right, thus not wanted:
find_pivot( [ Point={X,_Y} | Others ], Pivot={ Xp, _Y }, NewList ) when X>Xp ->
	find_pivot( Others, Pivot, [ Point | NewList ] );

% Same level as the pivot, but at its left, thus wanted:
find_pivot( [ Point={X,_Yp} | Others ], PreviousPivot={ Xp, _Yp }, NewList )
  when X<Xp ->
	find_pivot( Others, Point, [ PreviousPivot | NewList ] );

% Duplicated pivot, abnormal:
find_pivot( [ Pivot | _Others ], Pivot, _NewList ) ->
	throw( { duplicate_pivot, Pivot } ).




% Returns a list containing the points sorted according to an increasing angle
% between the abscissa axis and the vector from the pivot that each point.
%
% Note: all points having the same abscissa as the pivot, except the highest
% one, will be removed from the returned list.
%
-spec sort_by_angle( point(), [ point() ] ) -> [ point() ].
sort_by_angle( Pivot, Points ) ->
	sort_by_angle( Pivot, Points, _LeftPoints=[], _MiddlePoint=undefined,
				   _RightPoints=[] ).



% Helper.
%
% LeftPoints and RightPoints are lists of {Angle,Point} pairs.
%
-spec sort_by_angle( point(), [ point() ], [ { number(), point() } ],
					  'undefined', [ { number(), point() } ] ) -> [ point() ];
				   ( point(), [ point() ], [ { number(), point() } ], point(),
					  [ { number(), point() } ] ) ->  [ point() ].
sort_by_angle( _Pivot, _Points=[], LeftPoints, undefined, RightPoints ) ->

	%io:format( "sort_by_angle: no middle point found.~n" ),
	% Not having a middle point to integrate here:

	L = lists:keysort( _Index=1, LeftPoints )
		++ lists:keysort( _Index=1, RightPoints ),
	%io:format( "Full list: ~w.~n", [ L ] ),
	reverse_and_drop_angle( L, [] );

sort_by_angle( _Pivot, _Points=[], LeftPoints, MiddlePoint, RightPoints ) ->
	%io:format( "sort_by_angle: at least one middle point found.~n" ),
	L = lists:keysort( _Index=1, LeftPoints )
		++ [ {dummy,MiddlePoint} | lists:keysort( _Index=1, RightPoints ) ],
	reverse_and_drop_angle( L, [] );

% Note that Y<=Yp by definition of the pivot, hence Y-Yp<=0:
sort_by_angle( Pivot={Xp,Yp}, [ Point={X,Y} | T ], LeftPoints, MiddlePoint,
			   RightPoints ) ->
	case X-Xp of

		0 ->
			% Here we are just above the pivot, tan(Pi/2) is infinite.
			case MiddlePoint of

				undefined ->
					% First found is first best:
					sort_by_angle( Pivot, T, LeftPoints, Point, RightPoints );

				{ _Xm, Ym } ->

					case Y < Ym of

						true ->
							% This point is above the previous highest middle
							% point, previous middle point can be dropped on the
							% floor:
							sort_by_angle( Pivot, T, LeftPoints, Point,
										   RightPoints );

						false ->
							% The current point can be dropped on the floor, as
							% it is below the highest middle point:
							sort_by_angle( Pivot, T, LeftPoints, MiddlePoint,
										   RightPoints )

					end
			end;

		DeltaX when DeltaX > 0 ->
			% This is a point on the right of the pivot, stores the tangent of
			% the angle the vector defined by the pivot and that point makes
			% with the abscissa axis:
			sort_by_angle( Pivot, T, LeftPoints, MiddlePoint,
						   [ { (Y-Yp) / DeltaX, Point } | RightPoints ] );

		NegativeDeltaX ->
			% This is a point on the left of the pivot:
			sort_by_angle( Pivot, T,
						[ { (Y-Yp) / NegativeDeltaX, Point } | LeftPoints ],
						MiddlePoint, RightPoints )

	end.



% Helper:
reverse_and_drop_angle( [], Acc ) ->
	Acc;

reverse_and_drop_angle( [ {_Tangent,Point} | T ], Acc ) ->
	reverse_and_drop_angle( T, [ Point | Acc ] ).








% Vector section.



% Returns a vector V made from the specified two points: V=P2-P1.
%
-spec vectorize( point(), point() ) -> vector().
vectorize( _P1={X1,Y1}, _P2={X2,Y2} ) ->
	{ X2-X1, Y2-Y1 }.


% Returns the square of the magnitude of the specified vector.
%
-spec square_magnitude( vector() ) -> linear:square_distance().
square_magnitude( _V={X,Y} ) ->
	X*X + Y*Y.


% Returns the magnitude of the specified vector.
%
-spec magnitude( vector() ) -> linear:distance().
magnitude( V ) ->
	math:sqrt( square_magnitude(V) ).



% Scales specified vector of specified factor.
-spec scale( vector(), number() ) -> vector().
scale( _V={X,Y}, Factor ) ->
	{ Factor*X, Factor*Y }.




% Returns the specified vector with an unit length (magnitude of 1):
%
% (epsilon-based test for null vectors with floating-point coordinates could
% be done here).
%
-spec make_unit( vector() ) -> vector().
make_unit( {0,0} ) ->
	throw( cannot_make_null_vector_unit );

make_unit( V ) ->
	scale( V, 1 / magnitude(V) ).



% Returns a (non-unit) vector which is normal to specified vector V, and is on
% the left of V in the standard basis.
%
-spec normal_left( vector() ) -> vector().
normal_left( _V={X,Y} ) ->
	{ -Y, X }.



% Returns a (non-unit) vector which is normal to specified vector V, and is on
% the right of V in the standard basis.
%
-spec normal_right( vector() ) -> vector().
normal_right( _V={X,Y} ) ->
	{ Y, -X }.


% Returns the dot product of the two specified vectors.
-spec dot_product( vector(), vector() ) -> number().
dot_product( _V1={X1,Y1}, _V2={X2,Y2} ) ->
	X1*X2 + Y1*Y2.




% Line section.


% Returns the three coefficients {A,B,C} for the line passing by point P and
% being perpendicular to vector V, whose equation A.x+B.y+C=0.
%
-spec get_line( point(), point() ) -> line().
get_line( _P={Xp,Yp}, _V={Vx,Vy} ) ->
	% Here we know that:
	% P is on the line: A.Xp+B.Yp+C=0  (I)
	% Let M be (X,Y). If M is on the line, the PM.V=0 (null dot product), thus:
	% (X-Xp)*Vx+(Y-Yp)*Vy=0 (II)
	% We want to determine A, B and C:
	% (II) is: Vx.X + Vy*Y - (Xp.Vx+Yp.Vy) = 0 thus:
	A=Vx,
	B=Vy,
	C = - ( Xp*Vx + Yp*Vy ),
	{ A, B, C }.



% Returns the intersection of the two specified lines, if it is a point,
% otherwise the atom no_point (the intersection can be void, if the lines are
% parallel but different, or a full line, if they are the same line).
%
% First line has for equation a.x+b.y+c=0, second has for equation u.x+v.y+w=0.
%
-spec intersect( line(), line() ) -> 'no_point' | point().
intersect( _D1={A,B,C}, _D2={U,V,W} ) ->

	% We will try to substitute y, as determined from first equation, into the
	% second one:
	case B of

		0 ->
			% Thus A.X = -C
			case A of

				0 ->
					% Either empty or the same:
					no_point;

				_ ->
					X = -C/A,
					case V of

						0 ->
							no_point;

						_ ->
							Y= -(W+U*X) / V,
							{X,Y}

					end

			end;

		_ ->
			% General case: Y = - (C+A.X)/B (I), will be replaced in second
			% equation:
			case U of

				0 ->
					% Thus Y:
					Y= -W/V,
					% Now X from first:
					case A of

						0 ->
							no_point;

						_ ->
							X= - (B*Y+C)/A,
							{X,Y}

					end;

				_ ->
					% General case, substituing (I) in second equation we have:
					% (B.U-V.A).X = V.C-B.D
					case B*U-V*A of

						0 ->
							no_point;

						Denom ->
							X = (V*C-B*W) / Denom,
							Y = - (C+A*X) / B,
							{X,Y}

					end

			end

	end.



% Returns the abscissa of a point on line L having Y for ordinate.
%
% Line L must not have for equation Y=constant (i.e. its A parameter must not be
% null).
%
-spec get_abscissa_for_ordinate( line(), linear:coordinate() ) ->
									   linear:coordinate().
get_abscissa_for_ordinate( _L={A,B,C}, Y ) ->
	% For y=K, x=-(C+BK)/A
	-(C+B*Y) / A.




% Angle section.


% Returns true iff P is strictly on the right of the oriented segment going from
% P1 to P2.
%
-spec is_strictly_on_the_right( point(), point(), point() ) -> boolean().
is_strictly_on_the_right( P, P1, P2 ) ->

	Vec_P1P2 = vectorize( P1, P2 ),

	RightNormal = normal_right( Vec_P1P2 ),

	Vec_P1P  = vectorize( P1, P ),

	DotProduct = dot_product( Vec_P1P, RightNormal ),

	DotProduct > 0.



% Returns whether specified angle (in degrees, canonical form) is obtuse.
%
-spec is_obtuse( unit_utils:int_degrees() ) -> boolean().
is_obtuse( AngleInDegrees ) ->
	AngleInDegrees > 90 andalso AngleInDegrees < 180.



% Returns the angle, in radians, between the vector AB and AC.
%
% Note: with this function we cannot tell whether one vector is ahead of the
% other, i.e. if we should use the returned angle or its opposite to go from AB
% to AC.
%
-spec abs_angle_rad( point(), point(), point() ) -> unit_utils:radians().
abs_angle_rad( A, B, C ) ->

	AB = vectorize( A, B ),

	M1 = magnitude(AB),

	case math_utils:is_null(M1) of

		true ->
			throw( { degenerate_angle, { A, B } } );

		_ ->
			ok

	end,

	AC = vectorize( A, C ),

	M2 = magnitude( AC ),

	case math_utils:is_null( M2 ) of

		true ->
			throw( { degenerate_angle,{ A, C } } );
		_ ->
			ok

	end,

	%io:format( "AB=~w, AC=~w, M1=~f, M2=~f.~n", [AB,AC,M1,M2] ),
	math:acos( dot_product( AB, AC ) / ( magnitude( AB ) * magnitude( AC ) ) ).



% Returns the signed (oriented) angle, in radians, between the vector AB and AC.
%
% Note: with this function we can tell that we must rotate counter-clockwise of
% the returned angle to go from AB to AC.
%
-spec angle_rad( point(), point(), point() ) -> unit_utils:radians().
angle_rad( A, B, C ) ->

	{ X1, Y1 } = vectorize( A, B ),

	{ X2, Y2 } = vectorize( A, C ),

	math:atan2( Y2, X2 ) - math:atan2( Y1, X1 ).



% Returns the angle, in canonical degrees, between the vector AB and AC.
%
% Note: with this function we cannot tell whether one vector is ahead of the
% other, i.e. if we should use the returned angle or its opposite to go from AB
% to AC.
%
-spec abs_angle_deg( point(), point(), point() ) -> unit_utils:int_degrees().
abs_angle_deg( A, B, C ) ->
	 math_utils:canonify( math_utils:radian_to_degree(
							abs_angle_rad( A, B, C ) ) ).



% Returns the signed (oriented) angle, in canonical degrees, between the vector
% AB and AC.
%
% Note: with this function we can tell that we must rotate counter-clockwise of
% the returned angle to go from AB to AC.
%
-spec angle_deg( point(), point(), point() ) -> unit_utils:int_degrees().
angle_deg( A, B, C ) ->
	 math_utils:canonify( math_utils:radian_to_degree(
							angle_rad( A, B, C ) ) ).





% Convex hull section.



% Computes the convex hull corresponding to the specified list of points.
%
% Returns the list of points that defines the hull.
%
-spec compute_convex_hull( [point()] ) -> [point()].
compute_convex_hull( Points ) ->

	{ Pivot, RemainingPoints } = find_pivot( Points ),

	case length(RemainingPoints) of

		Len when Len < 2 ->
			throw( not_enough_points_for_convex_hull );

		_Other ->
			% We have at least 2 points in addition to the pivot.
			%io:format( "Pivot is ~w, remaining points: ~w.~n",
			%		   [Pivot,RemainingPoints] ),

			[ P1, P2 | T ] = sort_by_angle( Pivot, RemainingPoints ),

			% Initially only the pivot is known to belong to the convex hull.
			% We had P1, next to be validated against P2.
			%
			% We also add the pivot to the end of the NextPoints list, so that
			% the hull can be closed.
			compute_graham_scan_hull( _ToValidate=[ P1, Pivot ],
							  _NewPoint=P2, _NextPoints=( T++ [Pivot] ) )

	end.




% Computes the Graham scan for the specified list of points, expected to be
% already sorted by increasing angle between the abscissa axis and the vector
% from the pivot to each of these points (i.e. in increasing order of the angle
% they and the point P make with the x-axis, in counter-clockwise order).
%
% See: http://en.wikipedia.org/wiki/Graham_scan
%
% Returns the corresponding convex hull, in clock-wise order.
%
-spec compute_graham_scan_hull( [ point() ], point(), [ point() ] ) ->
									  [ point() ].
compute_graham_scan_hull( ToValidate, _Pivot, _NextPoints=[] ) ->
	% Last new point is by construction always to pivot.

	%io:format( "compute_graham_scan_hull: "
	%		   "exhausted input points, returning: ~w.~n", [ ToValidate ] ),

	ToValidate;


compute_graham_scan_hull( ToValidate=[ P2, P1 | T ], NewPoint,
						  NextPoints=[ Next | OtherNext ] ) ->

	% Should P2 be on the line defined by P1 and NewPoint, then P2 will be
	% considered as not being on the left: in the convex hull, only necessary
	% points will be kept, i.e. no point on the boundary of the hull will be
	% kept.
	%
	% Note: The test seems to be wrongly negated; however it is correct as
	% actually we describe the algorithm as seen if represented in a basis whose
	% ordinates are increasing when going from the top to the bottom (i.e. like
	% when rendered on a classical GUI, the Y axis going then downward).
	%
	case is_strictly_on_the_right( P2, P1, NewPoint ) of

		false ->

			%io:format( "compute_graham_scan_hull: point ~w is on the right of "
			%			"segment from ~w to ~w, keeping ~w.~n",
			%			[ P2, P1, NewPoint, P2 ] ),

			% Here, the point P2 is on the right of the segment going from P1 to
			% the Next point, thus P2 can be kept and we can continue with the
			% next points:
			compute_graham_scan_hull( [ NewPoint | ToValidate ], Next,
									 OtherNext );

		true ->

			%io:format( "compute_graham_scan_hull: point ~w is on the left of "
			%			"segment from ~w to ~w, eliminating ~w.~n",
			%			[ P2, P1, NewPoint, P2 ] ),

			% Here, the point P2 is on the left of (or in) the segment going
			% from P1 to the Next point, thus P2 is to be discarded, and will
			% have to check predecessor(s) of P2 against the Next point.
			%

			compute_graham_scan_hull( [ P1 | T ], NewPoint, NextPoints )


	end;

% Note however that the first point examined after the pivot (FP1) may have to
% be discarded because of the second. If we just removed FP1, then the
% ToValidate list would just contain the pivot, thus triggering a function
% clause. In that case we just have to replace FP1 by the next(FP1)=P2 here, and
% thus ToValidate will always contain at least two elements:

% This clause matches whenever we just removed the first point in the list
% examined after the pivot. So the first parameter (ToValidate) is just a list
% with one element, the pivot, that was added to close the hull. As we have no
% intermediate point, we accept directly the next point (Next), knowing it will
% be checked at the next recursion:
%compute_graham_scan_hull( [ Pivot ], NewPoint, [ Next | OtherNext ] ) ->
%	compute_graham_scan_hull( [ NewPoint, Pivot ], Next, OtherNext ).
% A bit faster as we know L is actually [ Pivot ]:
%
compute_graham_scan_hull( L, NewPoint, [ Next | OtherNext ] ) ->
	compute_graham_scan_hull( [ NewPoint | L ], Next, OtherNext ).




% Textual conversion section.


% Returns a precise textual representation of specified point.
%
-spec to_string( point() ) -> string().
to_string( { X, Y } ) ->
	io_lib:format( "{ ~w, ~w }", [ X, Y ] ).



% Returns a human-friendly, approximated textual representation of specified
% point, based on specified print-out precision (number of digits after the
% comma).
%
-spec to_string( point(), basic_utils:count() ) -> string().
to_string( { X, Y }, DigitCountAfterComma ) ->

	% We want to avoid displaying larger texts for coordinates, like
	% 0.10000000000000009:

	%XRounded = math_utils:round_after( X, DigitCountAfterComma ),

	%YRounded = math_utils:round_after( Y, DigitCountAfterComma ),

	%text_utils:format( "{ ~.*w, ~.*w }", [ Precision, X, Precision, Y ] ).

	XString = case is_float( X ) of

				  true ->
					  text_utils:format( "~.*f", [ DigitCountAfterComma, X ] );

				  false ->
					  % Integer:
					  text_utils:format( "~B", [ X ] )

	end,

	YString = case is_float( Y ) of

				  true ->
					  text_utils:format( "~.*f", [ DigitCountAfterComma, Y ] );

				  false ->
					  % Integer:
					  text_utils:format( "~B", [ Y ] )

	end,

	text_utils:format( "{ ~s, ~s }", [ XString, YString ] ).
