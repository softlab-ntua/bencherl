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


% Gathering of various facilities for linear operations.
%
-module(linear).


% These type names are too general to be defined in the hrl file (i.e. in the
% root namespace).


% Cartesian coordinates in a referential:
%
-type coordinate() :: number().


% Cartesian integer coordinates in a referential:
%
-type integer_coordinate() :: integer().




% Distance between two points:
%
-type distance() :: number().


% Mostly for clarity:
%
-type radius() :: distance().



% Integer distance between two points (ex: to express lengths):
%
-type integer_distance() :: integer().


% Square of a distance between two points (cheaper to compute, when applying the
% square root operator is not needed, like when comparing distances):
%
-type square_distance() :: number().



% Area of a surface:
%
-type area() :: float().


-export_type([ coordinate/0, integer_coordinate/0,
			   distance/0, radius/0, integer_distance/0, square_distance/0,
			   area/0 ]).
