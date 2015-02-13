% Copyright (C) 2008-2014 EDF R&D

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


% Shared constructs for most tests.
% Avoids code duplication.



% Defines trace title, if not already specified:
-ifndef(TraceTitle).

  -define(TraceTitle,get_title()).

-endif.




% For trace facilities:
-include("traces_for_tests.hrl").


% Included so that tests just have to include this header:

% For simulation_settings:
-include("class_TimeManager.hrl").


% For deployment_settings:
-include("class_DeploymentManager.hrl").


% For load_balancing_settings:
-include("class_LoadBalancer.hrl").


% For performance_tracker_settings:
-include("class_PerformanceTracker.hrl").
