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


% Gathering of various general purpose basic math facilities.
%
% See math_utils_test.erl for the corresponding test.
%
-module(math_utils).


% General operations:
-export([ floor/1, ceiling/1, round_after/2, modulo/2, clamp/3, squarify/1 ]).


% Operations on floating-point values:
-export([ are_close/2, are_close/3,
		  are_relatively_close/2, are_relatively_close/3,
		  get_relative_difference/2, is_null/1 ]).


% Operations with angles:
-export([ radian_to_degree/1, canonify/1 ]).


% For epsilon define:
-include("math_utils.hrl").


% Type declarations:

-type non_zero_integer() :: pos_integer() | neg_integer().


% Variance, as used to describe a Gaussian curve:
-type variance() :: float().


% For percentages:
-type percent() :: float().


% For probabilities:
-type probability() :: float().


-export_type([ non_zero_integer/0, variance/0, percent/0, probability/0 ]).





% General section.


% Floors returns the biggest integer smaller than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
%
-spec floor( number() ) -> integer().
floor( X ) ->

	T = erlang:trunc( X ),

	case X - T of

		Neg when Neg < 0 ->
			T - 1;

		%Pos when Pos > 0 ->
		%	T;

		_PositiveOrNull ->
			T

	end.



% Ceiling returns the smallest integer bigger than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
%
-spec ceiling( number() ) -> integer().
ceiling( X ) ->

	T = erlang:trunc( X ),

	case X - T of

		Pos when Pos > 0 ->
			T + 1;

		%Neg when Neg < 0 ->
		%	T;

		_NegativeOrNull ->
			T

	end.



% Rounds the specified floating-point number at specified offset after the
% decimal point.
%
% Ex: round_after( 12.3456, 3 ) = 12.346.
%
-spec round_after( float(), basic_utils:count() ) -> float().
round_after( F, DigitCount ) ->

	Multiplier = math:pow( 10, DigitCount ),

	% Certainly clumsy, but works:
	erlang:round( Multiplier * F ) / Multiplier.



% Returns the positive remainder of the division of X by Y, in [0;Y[.
%
% In Erlang, -5 rem 3 is -2, whereas this function will return 1,
% since -5 = -2 * 3 + 1.
%
-spec modulo( integer(), non_zero_integer() ) -> non_neg_integer().
modulo( X, Y ) when X > 0 ->
	X rem Y;

modulo( X, Y ) when X < 0 ->

	K = (-X div Y) + 1,

	PositiveX = X + K*Y,

	%io:format( "K=~B, PositiveX=~B~n.", [ K, PositiveX ] ),

	PositiveX rem Y;

modulo( 0, _Y ) ->
	0.



% Clamps specified value between specified bounds: the returned value V is in
% [Min,Max].
%
% We expect that Min <= Max.
%
-spec clamp( number(), number(), number() ) -> number().
clamp( Min, _Max, Value ) when Value < Min ->
	Min;

clamp( _Min, Max, Value ) when Value > Max ->
	Max;

clamp( _Min, _Max, Value ) ->
	Value.



% Returns the square, augmented of a little margin, of the specified element.
%
squarify( L ) ->
	% "Taylor series", square( epsilon ) is negligible here:
	L * ( L + ?epsilon ).




% Floating-point section.


% Returns true iff the two specified floating-point numbers are deemed close
% enough to be equal, based on default epsilon threshold.
%
-spec are_close( number(), number() ) -> boolean().
are_close( X, Y ) ->
	erlang:abs( X - Y ) < ?epsilon.


% Returns true iff the two specified floating-point numbers are deemed close
% enough to be equal, based on specified epsilon threshold.
%
-spec are_close( number(), number(), number() ) -> boolean().
are_close( X, Y, Epsilon ) ->
	erlang:abs( X - Y ) < Epsilon.



% Returns true iff the two specified floating-point numbers are deemed close
% enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the default epsilon threshold, i.e. the
% maximum tolerance.
%
-spec are_relatively_close( number(), number() ) -> boolean().
are_relatively_close( X, Y ) ->
	are_relatively_close(  X, Y, ?epsilon ).



% Returns true iff the two specified (usually floating-point) numbers are deemed
% close enough, relatively, to be equal.
%
% The difference between these numbers, divided by their average (a.k.a. the
% relative error), must be smaller than the specified epsilon threshold,
% i.e. the maximum tolerance.
%
% Ex: to know whether X and Y are equal with a 5% tolerance, use:
% math_utils:are_relatively_close( X, Y, _Tolerance=0.05 ).
%
-spec are_relatively_close( number(), number(), number() ) -> boolean().
are_relatively_close( X, Y, Epsilon ) ->

	% We will divide by X+Y ... provided this is not null:
	case -X of

		Y ->
			% X+Y=0, okay; they will be relatively close iff absolutely close
			% (between them, and to zero) here (think for example to X=3 and
			% Y=-3):
			are_close( X, Y, Epsilon );

		_ ->
			2 * erlang:abs( X - Y ) / ( X + Y ) < Epsilon

	end.



% Returns the relative difference between the two specified numbers.
%
-spec get_relative_difference( number(), number() ) ->
								 'difference_not_computable' | float().
get_relative_difference( X, Y ) ->

	case -X of

		Y ->
			difference_not_computable;

		_ ->
			2 * erlang:abs( X - Y ) / ( X + Y )

	end.



% Returns true iff the specified floating-point number is deemed close enough to
% zero to be null.
%
-spec is_null( number() ) -> boolean().
is_null( X ) ->
	erlang:abs( X ) < ?epsilon.




% Angle section.

% As we try to remain as much as possible with integer computations, for angle
% we tend to prefer expressing them in degrees rather than in radians.

% Angles in degrees are preferably kept in the [0;360[ interval, i.e. as
% positive integers.


% Converts specified angle in radian into the same angle expressed in degrees.
%
-spec radian_to_degree( unit_utils:radians() ) -> unit_utils:degrees().
radian_to_degree( AngleInRadians ) ->
	AngleInRadians * 180 / math:pi().


% Canonifies specified angle in degrees, i.e. ensures the returned value that
% corresponds to the specified angle is in the [0;360[ interval.
%
-spec canonify( number() ) -> unit_utils:int_degrees().
canonify( AngleInDegrees ) when is_integer( AngleInDegrees ) ->
	modulo( AngleInDegrees, 360 );

% Here we assume it is a floating-point value, positive or not.
canonify( AngleInDegrees ) ->
	AngleInDegrees - 360 * floor( AngleInDegrees / 360 ).
