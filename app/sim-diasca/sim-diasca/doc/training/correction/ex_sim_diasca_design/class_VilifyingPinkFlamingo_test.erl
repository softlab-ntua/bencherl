% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Unit tests for the interaction between the time manager and training actors.
%
% See the class_TimeManager.erl and class_VilifyingPinkFlamingo.erl tested
% modules.
%
-module(class_VilifyingPinkFlamingo_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
-spec run() -> no_return().
run() ->

	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Vilifying Pink Flamingo Example"

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %  [ { targeted_patterns,   [ {".*",[data_and_plot]} ] },
	  %    { blacklisted_patterns, ["^Second" ] } ]

	  %result_specification = [ {targeted_patterns, [ {".*",data_only} ] } ]

	},


	% Note: we expect this test case to be compiled in a "training" directory:
	DeploymentSettings = #deployment_settings{

		% We want to embed additionally this test and its specific
		% prerequisites:
		additional_elements_to_deploy = [ {"training",code} ]

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							  DeploymentSettings, LoadBalancingSettings ),


	% Create a few flamingos, with name and initial height:
	Syd = class_Actor:create_initial_actor( class_VilifyingPinkFlamingo,
										   [ "Syd", 120.0 ] ),

	David = class_Actor:create_initial_actor( class_VilifyingPinkFlamingo,
										   [ "David", 117.0 ] ),

	Nick = class_Actor:create_initial_actor( class_VilifyingPinkFlamingo,
											[ "Nick", 125.5 ] ),

	Roger = class_Actor:create_initial_actor( class_VilifyingPinkFlamingo,
											 [ "Roger", 91.0 ] ),

	class_Actor:create_initial_actor( class_VilifyingPinkFlamingo,
									 [ "Richard", 119.0 ] ),


	% Cross-reference some flamingos:

	% Initially Syd will see David as a rival, whereas David will see Nick as a
	% rival; Roger will do like Syd, seeing David as a rival.
	%
	% Richard lives in its own world: it sees nobody as a rival, and is seen by
	% nobody as such:
	Syd   ! { beNotifiedOfRival, David },
	David ! { beNotifiedOfRival, Nick },
	Roger ! { beNotifiedOfRival, David },

	% We want this test to end once a specified number of ticks are elapsed:
	StopTick = 80,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, "
		"for a stop at tick offset ~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?test_info( "Waiting for the simulation to end, "
		"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
					   "specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
