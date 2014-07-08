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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


%-export([ is_performanceTracker_activated/0, performanceTracker_disable/0 ]).

%-compile( {inline,[ is_performanceTracker_activated/0,
% performanceTracker_disable/0] } ).

% The name under which the performance tracker process is to be registered:
% (use ?performance_tracker_name to retrieve it).
-define( performance_tracker_name, sim_diasca_performance_tracker ).


% Where the performance tracker is registered.
% its registedType can be local_and_global or global_only
-define( performance_tracker_registration_type, local_and_global ).
