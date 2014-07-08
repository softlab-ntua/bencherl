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



% Integration tests for the interaction between the time manager, an equipment
% and failure and repair models.
%
% See class_TimeManager.erl, class_Equipment.erl, class_FailureModel.erl,
% class_RepairModel.erl, class_ExponentialFailureModel.erl,
% class_GaussianRepairModel.erl, class_ReliabilityProbe.erl, etc.
%
-module(equipment_integration_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


% Creates a series of Count equipments, named accordingly, stopping at 100.
%
% Returns a list of the corresponding PIDs.
%
createEquipment( _Count=0, _FailureModelPid, _RepairModelPid ) ->
	[];

createEquipment( Count, FailureModelPid, RepairModelPid ) ->

	ActorName = io_lib:format( "Test-Equipment-~B", [ Count ] ),

	NewActorPid = class_Actor:create_initial_actor( class_TestEquipment,
		[ ActorName, _StopTick=100+Count, FailureModelPid, RepairModelPid ] ),

	[ NewActorPid |
	 createEquipment( Count-1, FailureModelPid, RepairModelPid ) ].



% Runs the test.
%
% Note: some probes are created specifically from this test, because we do not
% want all (potentially numerous) equipments to carry each a probe.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Equipment Integration Test"

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %	  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
	  %		{ blacklisted_patterns, ["^Second" ] } ]

	},

	% Specifies the list of computing hosts that can be used:
	%
	% (see the sim-diasca-host-candidates-sample.txt example in the
	% sim-diasca/conf directory)
	DeploymentSettings = #deployment_settings{},

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	DeploymentManagerPid = sim_diasca:init( SimulationSettings, 
								  DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Creating an exponential failure model." ),

	% Parameters are MTTFday,MTTFhour,MTTFminute,MTTFsecond:
	MyExponentialFailureModel = class_Actor:create_initial_actor(
		class_ExponentialFailureModel, [ {0,0,0,10} ] ),

	?test_info( "Creating a gaussian failure model." ),

	% Parameters are {MTTFday,MTTFhour,MTTFminute,MTTFsecond},MTTFvariance:
	MyGaussianFailureModel = class_Actor:create_initial_actor(
		class_GaussianFailureModel, [ {0,0,0,3}, 3 ] ),


	?test_info( "Creating a uniform repair model." ),

	% Parameters are MaxTTRday,MaxTTRhour,MaxTTRminute,MaxTTRsecond:
	MyUniformRepairModel = class_Actor:create_initial_actor(
		class_UniformRepairModel, [ {0,0,0,5} ] ),

	?test_info( "Creating a gaussian repair model." ),

	% Parameters are {MTTRday,MTTRhour,MTTRminute,MTTRsecond},MTTRvariance:
	MyGaussianRepairModel = class_Actor:create_initial_actor(
		class_GaussianRepairModel, [ {0,0,0,10}, 2 ] ),



	?test_info( "Creating reliability probes." ),

	% They could have been declared as results as well:
	MyFirstReliabilityProbe = class_ReliabilityProbe:synchronous_new(
		"Reliability probe 1", "Monitoring of the state of equipment #1." ),

	MySecondReliabilityProbe = class_ReliabilityProbe:synchronous_new(
		"Reliability probe 2", "Monitoring of the state of equipment #2." ),

	MyThirdReliabilityProbe  = class_ReliabilityProbe:synchronous_new(
		"Reliability probe 3", "Monitoring of the state of equipment #3." ),


	StopTick = 2000,



	% Creates actors that will automatically subscribe themselves to the manager
	% and that will terminate on specified tick.
	%
	% No reference kept, as the actor life cycle is managed by the time manager.

	?test_info( "Creating three test equipments." ),


	% Terrance and Phillip will use different models, whereas Phillip and Roger
	% will be stricty identically defined (except the probe of course), to
	% ensure that they nevertheless have a different history, as they are
	% expected to share their failure and reparation models instead of having
	% each their own.
	Terrance = class_Actor:create_initial_actor( class_TestEquipment,
		[ "Terrance", _Termination=StopTick div 2, MyGaussianFailureModel,
		 MyUniformRepairModel ] ),

	Terrance ! { setReliabilityProbe, MyFirstReliabilityProbe, self() },

	% Ensures synchronicity:
	probe_set = test_receive(),


	Phillip = class_Actor:create_initial_actor( class_TestEquipment,
		[ "Phillip", StopTick, MyExponentialFailureModel,
		 MyGaussianRepairModel ] ),

	Phillip ! { setReliabilityProbe, MySecondReliabilityProbe, self() },

	% Ensures synchronicity:
	probe_set = test_receive(),


	Roger = class_Actor:create_initial_actor( class_TestEquipment,
		[ "Roger", StopTick, MyExponentialFailureModel,
		 MyGaussianRepairModel ] ),

	Roger ! { setReliabilityProbe, MyThirdReliabilityProbe, self() },

	% Ensures synchronicity:
	probe_set = test_receive(),


	%EquimentCount = 0,
	EquimentCount = 10,
	%EquimentCount = 5000,

	Equipments = createEquipment( EquimentCount, MyGaussianFailureModel,
	   MyUniformRepairModel ),

	?test_info_fmt( "Created equipments: ~p.", [ Equipments ] ),


	?test_info_fmt( "Starting time manager, "
		"for a stop at tick offset #~B.", [ StopTick ] ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

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
