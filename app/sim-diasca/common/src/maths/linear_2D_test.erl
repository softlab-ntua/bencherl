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


% Unit tests for the linear 2D facilities.
%
% See the linear_2D tested module.
%
-module(linear_2D_test).



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	V={9,1},

	NL = linear_2D:normal_left( V ),
	NR = linear_2D:normal_right( V ),

	0 = linear_2D:dot_product( V, NL ),
	0 = linear_2D:dot_product( V, NR ),

	test_facilities:display( "~p is a (non-unit) left normal for vector ~p, "
			   "and ~p is a right normal.", [NL,V,NR] ),

	true  = linear_2D:is_strictly_on_the_right( NR, {0,0}, V ),
	false = linear_2D:is_strictly_on_the_right( NL, {0,0}, V ),
	false = linear_2D:is_strictly_on_the_right( V,  {0,0}, V ),

	true  = linear_2D:is_strictly_on_the_right( V, {0,0}, NL ),
	false = linear_2D:is_strictly_on_the_right( V, {0,0}, NR ),

	NonV = linear_2D:scale( V, -1 ),

	true  = linear_2D:is_strictly_on_the_right( NL, {0,0}, NonV ),
	false = linear_2D:is_strictly_on_the_right( NR, {0,0}, NonV ),

	Pa    = {469,243},
	Pivot = {348,268},
	Pb    = {421,193},

	false = linear_2D:is_strictly_on_the_right( Pa, Pivot, Pb ),

	A={0,0},

	B1={1,0},
	B2={3,3},
	B3={-5,3},

	C1={0,1},
	C2={-2,-1},
	C3={1,-4},

	[ test_facilities:display( "Unoriented angle between the vertex ~w and ~w, "
				 "~w is ~f degrees, oriented angle is ~f degrees.",
				 [ P1, P2, P3, linear_2D:abs_angle_deg( P1, P2, P3 ),
					 linear_2D:angle_deg( P1, P2, P3 ) ] ) || P1 <- [A],
														P2 <- [B1,B2,B3],
														P3 <- [C1,C2,C3] ],

	true  = linear_2D:are_close( B1, linear_2D:translate(B1,{0.000001,0} )),
	false = linear_2D:are_close( B1, A ),

	true  = linear_2D:is_within( A, C1, 1 ),
	true  = linear_2D:is_within( A, B1, 1-0.0000001 ),
	false = linear_2D:is_within( A, B2, 2 ),

	test_facilities:stop().
