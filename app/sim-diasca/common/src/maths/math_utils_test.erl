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


% Unit tests for the math basic toolbox facilities.
% See the math_utils tested module.
%
-module(math_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Roundings = [ -1.1, -1.0, -0.9, 0.0, 0.9, 1.0, 1.1 ],

	[ test_facilities:display( "Floor for ~p is ~p.", [ V,
			math_utils:floor(V) ] ) || V <- Roundings ],

	[ test_facilities:display( "Ceiling for ~p is ~p.", [ V,
			math_utils:ceiling(V) ] ) || V <- Roundings ],

	TruncateTargets = [ - 12345.6789, -1.23456789, 0.0, 12.3456789, 123.4568 ],

	[
	 [ test_facilities:display( "Rounding ~p after ~B digit(s) is ~p.", [ V, D,
			math_utils:round_after( V, D ) ] ) || V <- TruncateTargets ]
	 || D <- [ 0, 1, 2, 3 ] ],


	Modulo = 3,
	[ test_facilities:display( "~p modulo ~p is ~p.", [ X, Modulo,
			math_utils:modulo( X, Modulo ) ] ) || X <- lists:seq(-7,7) ],

	2 = math_utils:clamp( 1, 3, 2 ),
	1 = math_utils:clamp( 1, 3, 0 ),
	3 = math_utils:clamp( 1, 3, 6 ),

	2.0 = math_utils:clamp( 1, 3, 2.0 ),
	1   = math_utils:clamp( 1, 3, 0.0 ),
	3   = math_utils:clamp( 1, 3, 6.0 ),

	2   = math_utils:clamp( 1.0, 3.0, 2 ),
	1.0 = math_utils:clamp( 1.0, 3.0, 0 ),
	3.0 = math_utils:clamp( 1.0, 3.0, 6 ),


	[ test_facilities:display( "Canonical form for ~p degrees is ~p degrees.",
							  [ A, math_utils:canonify(A) ] ) ||
		A <- [ -721, -721.0, -720, -720.0, -719, -719.0, -100, -100.0,
			   0, 0.0, 100, 100.0, 359, 359.0, 360, 360.0, 361, 361.0,
			   400, 400.0 ] ],


	X1 = 300000.0,
	X2 = 300000.1,
	X3 = 300000.0000000000001,
	Y  = 300000.0,


	% Only the result of the fourth test should make a difference between
	% absolute/relative comparisons:

	true  = math_utils:are_close( X1, Y ),
	false = math_utils:are_close( X1, 0 ),
	true  = math_utils:are_close( X3, Y ),
	false = math_utils:are_close( X2, Y ),

	true  = math_utils:are_relatively_close( X1, Y ),
	false = math_utils:are_relatively_close( X1, 0 ),
	true  = math_utils:are_relatively_close( X3, Y ),
	true  = math_utils:are_relatively_close( X2, Y ),


	% '°' does not output well on the console (ex: "90.000000 Â°."):
	[ test_facilities:display( "Angle ~p rad is ~f degrees.", [ Angle,
			math_utils:radian_to_degree( Angle ) ] ) || Angle <-
				   [ 0, math:pi()/2, 1.0, math:pi(), 2*math:pi() ] ],

	test_facilities:stop().
