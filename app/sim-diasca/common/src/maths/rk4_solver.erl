% Copyright (C) 2014 Olivier Boudeville
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
% Creation date: Monday, November 17, 2014



% Basic numerical solver based on the classic Runge–Kutta fourth-order method,
% operating on three dimensions.
%
% See: http://en.wikipedia.org/wiki/List_of_Runge–Kutta_methods
%
% We want to evaluate a given function f, whose spec could be:
%     f( time(), vector() ) -> vector()
%
% complying to equation dy/dt = f( t, y ).
%
% For that we compute yn+1 = yn + h.sum(bi.ki) with ki = f( ti, yi ), with ti
% and yi depending on the order, and h being the chosen timestep. ki and bi are
% determined by the corresponding Butcher tableau.
%
% The implementation of f corresponds here to the anonymous function F.
%
-module(rk4_solver).



-export([ compute_next_estimate/4 ]).


% Simulation time:
%
-type time() :: float().


% Definition depends on the function of interest, more precisely on the
% dimension of the space the function to evaluate is an endomorphism of:
%
% (vectors and points are not distinguished here)
%
-type vector() :: linear_3D:vector().


% The function f in the equation that we want to solve numerically:
%
-type f() :: fun( ( time(), vector() ) -> vector() ).


-export_type([ time/0, vector/0, f/0 ]).




% We use here the "original" Runge–Kutta method, i.e. the classic fourth-order
% method.



% Computes the next point (yn+1), based on the current one (yn), the function
% (F) and the timestep (h).
%
-spec compute_next_estimate( f(), vector(), time(), time() ) -> vector().
compute_next_estimate( F, Point, Time, Step ) ->

	%io:format( "~w computing at ~p from point ~p.~n",
	%		   [ self(), Time, Point ] ),

	% Ad-hoc implementation of a Butcher tableau, for RK4 (s=4):

	% yn+1 = yn + h.sum(i=1 to s, bi.ki)
	%
	% with ki = f( tn + ci.h, yn + h.sum(j=1 to s, aij.kj) )

	% Here:
	%
	% yn+1 = yn + h.( 1/6.k1 + 1/3.k2 + 1/3.k3 + 1/6.k4 )

	% With:
	%
	% k1 = f( tn,       yn          )
	% k2 = f( tn + h/2, yn + h/2.k1 )
	% k3 = f( tn + h/2, yn + h/2.k2 )
	% k4 = f( tn + h,   yn + h.  k3 )

	K1 = F( Time, Point ),

	HalfStep = Step / 2.0,

	% tn + h/2:
	OneHalfStepAfter = Time + HalfStep,

	% yn + h/2.k1:
	SecondPoint = linear_3D:add( Point, linear_3D:scale( K1, HalfStep ) ),

	K2 = F( OneHalfStepAfter, SecondPoint ),

	% yn + h/2.k2:
	ThirdPoint = linear_3D:add( Point, linear_3D:scale( K2, HalfStep ) ),

	K3 = F( OneHalfStepAfter, ThirdPoint ),

	% yn + h.k3:
	FourthPoint = linear_3D:add( Point, linear_3D:scale( K3, Step ) ),

	OneFullStepAfter = Time + Step,
	K4 = F( OneFullStepAfter, FourthPoint ),

	SK1 = linear_3D:scale( K1, Step / 6.0 ),
	SK2 = linear_3D:scale( K2, Step / 3.0 ),
	SK3 = linear_3D:scale( K3, Step / 3.0 ),
	SK4 = linear_3D:scale( K4, Step / 6.0 ),

	% yn+1 = yn + h.( 1/6.k1 + 1/3.k2 + 1/3.k3 + 1/6.k4 )
	linear_3D:add( [ Point, SK1, SK2, SK3, SK4 ] ).
