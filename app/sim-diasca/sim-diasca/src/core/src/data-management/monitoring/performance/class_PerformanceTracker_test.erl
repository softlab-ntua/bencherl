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



% Unit tests for the PerformanceTracker class implementation.
% See the class_PerformanceTracker.erl module.
%
-module(class_PerformanceTracker_test).


% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Use default simulation settings (50Hz, batch reproducible) except for the
	% simulation name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Sim-Diasca Performance Tracker test"

	},

	% Default deployment setting

	DeploymentSettings = #deployment_settings{

		enable_performance_tracker = true

											  },

	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	?test_info( "Deployment manager created, retrieving the load balancer." ),
	DeploymentManagerPid ! { getLoadBalancer, [], self() },
	LoadBalancerPid = test_receive(),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Creating the initial test actors." ),

	class_Actor:create_initial_actor(
	  class_PerformanceTracker_TestActor,
	  [ "First Initial Performance Tracker Test Actor",
	   _TerminationTickOffset1=80 ],
	  LoadBalancerPid ),

	class_Actor:create_initial_actor(
	  class_PerformanceTracker_TestActor,
	  [ "Second Initial Performance Tracker Test Actor",
	   _TerminationTickOffset2=100 ],
	  LoadBalancerPid ),


	PerformanceTrackerPid = class_PerformanceTracker:get_tracker(),

	% For this very specific test, we link to the performance tracker, as we
	% want to crash this test if this tracker crashes (by default on the
	% contrary we do not want to be affected by its possible failure):
	%
	erlang:link( PerformanceTrackerPid ),

	PerformanceTrackerPid ! { setTickerPeriod, _Milliseconds=50 },

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=36, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	?test_info( "Browsing the report results, if not in batch mode." ),

	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
