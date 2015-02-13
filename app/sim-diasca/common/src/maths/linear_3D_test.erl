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


% Unit tests for the linear 3D facilities.
%
% See the linear_3D tested module.
%
-module(linear_3D_test).



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	V1 = { 9,   1,     0 },
	V2 = { 10,  10,    5 },
	V3 = { 0.0, 0.0, 0.0 },
	V4 = { 1,   2,     3 },

	Vectors = [ V1, V2, V3, V4 ],

	{ 20.0, 13.0, 8.0 } = Sum = linear_3D:add( Vectors ),

	test_facilities:display( "Sum of vectors ~p is ~p.", [ Vectors, Sum ] ),

	test_facilities:stop().
