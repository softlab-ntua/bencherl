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



% Unit tests for the Network class implementation.
%
% See the class_Network.erl module.
%
-module(class_Network_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


% For network constants and macros:
-include("class_Network.hrl").



% Runs the tests.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Network Test"

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %	  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
	  %		{ blacklisted_patterns, [ "^Second" ] } ]

	},


	% Specifies the list of computing hosts that can be used:
	%
	% (see the sim-diasca-host-candidates-sample.txt example in the
	% sim-diasca/conf directory)
	DeploymentSettings = #deployment_settings{

		enable_performance_tracker = true

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Creating a new Network." ),

	DeviceExpectedCount = 20,

	class_Actor:create_initial_actor( class_Network,
	   [ "Test network", DeviceExpectedCount ] ),

	StopTick = 500,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, "
		"for a stop at tick offset ~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },


	?test_info( "Waiting for the simulation to end, "
		"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->

			?test_info( "Simulation stopped spontaneously." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
