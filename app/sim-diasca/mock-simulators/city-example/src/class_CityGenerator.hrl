% Copyright (C) 2012-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)




% Description of a city that is to be procedurally created.
%
% The city is defined into a 3D cartesian system (orthonormal basis).
%
% The earth is assumed locally plane, a {X,Y,Z} coordinate uses Z as its
% elevation relative to the left-most (smallest X), nearest (smallest Y), lower
% (smallest Z) point of the corresponding right cuboid.
%
%
%     Z
%     ^   Y
%     |  /
%     | /    x city center
%     |/
%     +---------> X
%
-record( city_description, {


		% Name of the city:
		name :: string(),


		% Vector from the origin of the coordinate system to the center of the
		% right cuboid corresponding to the city:
		%
		% (useful for example to take altitudes into account)
		%
		center :: linear_3D:vector(),


		% Dimensions of the city { XLen, YLen, ZLen } in its local referential
		% (which is the absolute referential translated by the center
		% vector): extent of the city (which is a contained into a right_cuboid
		% bounding box), from the origin and alongside the three canonical axes,
		% specified thanks to the point opposite to the origin:
		%
		dimensions :: linear_3D:point(),


		% Number of incinerators in the city;
		incinerator_count :: basic_utils:count(),


		% Number of landfills in the city;
		landfill_count :: basic_utils:count(),


		% Number of residential waste source in the city:
		residential_waste_source_count :: basic_utils:count(),


		% Number of industrial waste source in the city:
		industrial_waste_source_count :: basic_utils:count(),


		% The number of road junctions in the city:
		road_junction_count :: basic_utils:count(),


		% The number of waste trucks operating in the city:
		waste_truck_count :: basic_utils:count()

						 } ).
