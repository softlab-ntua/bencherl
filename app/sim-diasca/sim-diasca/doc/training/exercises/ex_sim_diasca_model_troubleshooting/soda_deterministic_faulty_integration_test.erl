% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% Faulty integration test for the soda deterministic example case.
%
% See also:
%
% - class_FaultySodaVendingMachine.erl
% - class_FaultyDeterministicThirstyCustomer.erl
%
-module(soda_deterministic_faulty_integration_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
%
-spec run() -> no_return().
run() ->

	?test_start,


	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Faulty Soda Deterministic Integration Test"

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
	  %    { blacklisted_patterns, ["^Second" ] } ]

	  %result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

	},


	DeploymentSettings = #deployment_settings{

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		%
		additional_elements_to_deploy = [ {"mock-simulators/soda-test",code} ],

		% Note that the configuration file below has not to be declared above as
		% well:
		enable_data_exchanger = { true,
					  [ "mock-simulators/soda-test/src/soda_parameters.cfg" ] }

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								  DeploymentSettings, LoadBalancingSettings ),


	% First machine starts with 10 cans, 2 euros each:
	SVM1 = class_Actor:create_initial_actor( class_SodaVendingMachine,
			[ _FirstMachineName="First soda machine", _FirstInitialCanCount=10,
			  _FirstCanCost=2.0 ] ),

	% Second machine starts with 10 cans also, 1 euro each:
	SVM2 = class_FaultySodaVendingMachine:new_link(
		"Second soda machine", 10, 1 ),

	% First customer uses SVM1, is thirsty 1 minute after having drunk, and has
	% 6 euros in his pockets:
	_TC1 = class_FaultyDeterministicThirstyCustomer:new_link(
		"John", SVM1, 1, 6 ),

	% Second customer uses SVM1 too, is thirsty 3 minutes after having drunk,
	% and has 8 euros in his pockets:
	_TC2 = class_FaultyDeterministicThirstyCustomer:new_link(
		"Terry", SVM1, 3, 8 ),

	% Third customer uses SVM2, is thirsty 2 minutes after having drunk, and has
	% 15 euros in his pockets:
	_TC3 = class_FaultyDeterministicThirstyCustomer:new_link(
		"Michael", SVM2, 2, 15 ),

	% We want this test to end once a specified number of ticks are elapsed:
	StopTick = 30.0,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, "
		"for a stop at tick offset #~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?test_info( "Waiting for the simulation to end, "
		"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	?test_info( "Removing deployment manager, which will in turn take care "
		"of the deletion of all actors and agents on all nodes." ),

	sim_diasca:shutdown(),

	?test_stop.
