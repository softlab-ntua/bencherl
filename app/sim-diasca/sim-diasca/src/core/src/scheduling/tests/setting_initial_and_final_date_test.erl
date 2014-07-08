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



% Overall unit test of the Sim-Diasca management of initial and final date.



% The test will start a simulation at a user-specified start and stop date.
%
-module(setting_initial_and_final_date_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the local test simulation.
%
-spec run() -> no_return().
run() ->

	?test_start,


	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Setting initial and final date test"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing
	% hosts are specified (to be updated depending on your environment):
	% (note that localhost is implied)
	DeploymentSettings = #deployment_settings{

		computing_hosts =
			{ use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" },

		perform_initial_node_cleanup = true

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),

	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Deployment manager created, retrieving the load balancer." ),

	DeploymentManagerPid ! { getLoadBalancer, [], self() },
	LoadBalancerPid = test_receive(),

	?test_info( "Requesting to the load balancer the creation of "
		"a first initial test actor." ),

	ActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ "First periodic test actor",
			  _SchedulingSettings={ periodic, _Period=3 },
			  _CreationSettings=no_creation, _TerminationTickOffset=80 ],
			  LoadBalancerPid ),

	ActorPid ! { getAAI, [], self() },
	Id = test_receive(),

	?test_info_fmt( "The actor identifier for that actor is ~w.", [ Id ] ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	% This is the PID of the root time manager:
	RootTimeManagerPid = test_receive(),

	InitialDate = { 2014, 12, 31 },
	InitialTime = { 23, 59, 59 },

	RootTimeManagerPid ! { setInitialSimulationDate,
						  [ InitialDate, InitialTime ] },


	FinalDate = { 2015, 1, 1 },
	FinalTime = { 0, 0 ,1 },

	RootTimeManagerPid ! { setFinalSimulationDate, [ FinalDate, FinalTime ] },

	RootTimeManagerPid ! { addSimulationListener, self() },

	RootTimeManagerPid ! { getInitialTick, [], self() },
	ReadInitial = test_receive(),

	RootTimeManagerPid ! { getFinalTick, [], self() },
	ReadFinal = test_receive(),

	% Checkings, 2 virtual seconds (100 ticks) must elapse between start and
	% stop:
	3179364479950 = ReadInitial,
	100 = ReadFinal - ReadInitial,

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! start,


	?test_info( "Requesting textual timings (first)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	FirstTimingString = test_receive(),

	?test_info_fmt( "Received first time: ~s.", [ FirstTimingString ] ),

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Requesting textual timings (second)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	SecondTimingString = test_receive(),

	?test_info_fmt( "Received second time: ~s.", [ SecondTimingString ] ),

	RootTimeManagerPid ! { removeSimulationListener, self() },

	sim_diasca:shutdown(),

	?test_stop.
