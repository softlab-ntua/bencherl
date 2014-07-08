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



% Overall unit test of the Sim-Diasca deployment and scheduling framework.
%
-module(scheduling_one_creating_actor_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs a distributed simulation (of course if relevant computing hosts are
% specified).
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used,
	% except for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Scheduling one creating actor test"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
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

	% Describes how actors will be created, later:
	ActorCreationPolicy = { _ActorInterCount=10,
		_CreatedActorPolicy = { _ActorSchedulingPolicy={periodic,4},
			_ActorCreationPolicy=no_creation } },

	% The created actor *will* create actors in the course of the simulation,
	% but these will not in turn create actors:

	FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ "First periodic test actor", { periodic, _Period=3 },
			ActorCreationPolicy, _TerminationTickOffset=80 ], LoadBalancerPid ),


	FirstActorPid ! { getAAI, [], self() },
	2 = test_receive(),

	?test_info_fmt( "First actor has for PID ~w and for AAI 2.",
				   [ FirstActorPid ] ),


	?test_info( "First actor has a correct AAI." ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),



	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },


	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Requesting textual timings (second)." ),

	sim_diasca:shutdown(),

	?test_stop.
