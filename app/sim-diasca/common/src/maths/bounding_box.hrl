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


% Common definitions about bounding boxes.



% Record declarations:


% Circle-based 2D bounding box.
%
-record( circle, {



			% The center of the circle:
			center :: linear_2D:point(),

			% The square of the radius (R^2) of this circle:
			square_radius :: linear:square_distance()

							}).



% Right cuboid-based 3D bounding box (i.e. rectangular parallelepiped).
%
% Each of its faces is either parallel or orthogonal to each of the canonical
% axes.
%
% If base_vertex={X,Y,Z}, abscissa_length is A, ordinate_length is B,
% elevation_length is C, then cuboid is made of all points {Xp,Yp,Zp} where:
%
%  - X <= Xp < X + A
%
%  - Y <= Yp < X + B
%
%  - Z <= Zp < Z + C
%
%
% See http://en.wikipedia.org/wiki/Cuboid
%
-record( right_cuboid, {

			% A vertex of the cuboid:
			base_vertex :: linear_3D:point(),

			% The length along the abscissa axis (X):
			abscissa_length :: linear:distance(),

			% The length along the ordinate axis (Y):
			ordinate_length :: linear:distance(),

			% The length along the elevation axis (Z):
			elevation_length :: linear:distance()

							}).
