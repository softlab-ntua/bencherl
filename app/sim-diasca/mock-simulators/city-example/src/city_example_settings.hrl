% Copyright (C) 2014 EDF R&D

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


% Gathering of common settings, applicable to all models of the City-example
% case.


% For benchmarking reasons, we need here to relax the checkings done by the
% engine as this particular road network may generate shorter roads leading to
% smaller durations, themselves leading to too high rounding errors when
% performing time conversions:
%


% We choose on purpose an abnormally high threshold before an error is
% triggered, as we want to avoid that, due to the procedural generation, the
% simulation fails at any given scale:
%
-define( city_max_relative_error, 0.5 ).



% Length of the edge of a weather cell, in meters:
-define( weather_cell_length, 2400 ).
