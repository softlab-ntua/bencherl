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


% Describes a polygon, convex or not, simple or not.
%
-record( polygon, {

		   % List of points:
		   vertices = [] :: [ linear_2D:point() ],

		   % Rendering information, if any, as an option list.
		   %
		   % Supported options:
		   %   - edge_color :: gui_color:color_by_decimal()
		   %   - fill_color :: gui_color:color_by_decimal()
		   %
		   rendering = [] :: option_list:option_list(),

		   % Bounding-box information:
		   % (can be for example a circle or a right_cuboid)
		   bounding_box = undefined :: 'undefined' |
									  bounding_box:bounding_box()

		  }

	   ).
