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



% Overall unit test of the Sim-Diasca actor placement.



% The test will try to create all initial actors on the same node, bypassing the
% default placement policy.
%
-module(scheduling_initial_placement_hint_test).



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

		simulation_name = "Scheduling initial placement hint test"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
	% (note that localhost is implied)
	DeploymentSettings = #deployment_settings{

	  computing_hosts =
			{ use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" },

	  perform_initial_node_cleanup=true

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	sim_diasca:init( SimulationSettings, DeploymentSettings,
					LoadBalancingSettings ),


	?test_info( "Deployment manager created." ),



	?test_info( "Requesting the creation of the placed initial test actors." ),

	SchedulingSettings = none_applicable,
	CreationSettings = no_creation,
	TerminationTickOffset = 80,

	FirstActorPid = class_Actor:create_initial_placed_actor( class_TestActor,
			[ "First test actor", SchedulingSettings, CreationSettings,
			 TerminationTickOffset ], this_is_my_placement_hint ),

	SecondActorPid = class_Actor:create_initial_placed_actor( class_TestActor,
			[ "Second test actor", SchedulingSettings, CreationSettings,
			 TerminationTickOffset ], this_is_my_placement_hint ),

	ThirdActorPid = class_Actor:create_initial_placed_actor( class_TestActor,
			[ "Third test actor", SchedulingSettings, CreationSettings,
			 TerminationTickOffset ], this_is_my_placement_hint ),


	FirstActorPid ! { getHostingNode, [], self() },
	% Assignment:
	ActorNode = test_receive(),

	SecondActorPid ! { getHostingNode, [], self() },
	ThirdActorPid  ! { getHostingNode, [], self() },


	% Pattern matching:
	ActorNode = test_receive(),
	ActorNode = test_receive(),

	?test_info_fmt( "Common node for initial actors is ~w.", [ ActorNode ] ),

	?test_info( "No need to start the simulation." ),

	sim_diasca:shutdown(),

	?test_stop.
