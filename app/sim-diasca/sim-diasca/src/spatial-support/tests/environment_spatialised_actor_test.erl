% Copyright (C) 2014 EDF R&D

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


% Integration test for the spatial support.
%
% See also:
%
% - class_TwoDimensionalEnvironment.erl
% - class_SpatialisedActor.erl
%
-module(environment_spatialised_actor_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
%
-spec run() -> no_return().
run() ->

	?test_start,


	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca Spatial Integration Test",

	  % Using 100Hz here:
	  tick_duration = 0.01

	  % We leave it to the default specification (all_outputs):
	  % result_specification =
	  %  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
	  %    { blacklisted_patterns, ["^Second" ] } ]

	  %result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
							"sim-diasca-host-candidates.txt" }

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),


	% Let's create first a proper environment:
	EnvPid = class_Actor:create_initial_actor( class_TwoDimensionalEnvironment,
			[ _Width=400, _Height=300, _BorderSettings=torus ] ),

	% This one will go from left to right:
	%
	% (one may specify a huger perception period - thus depriving the
	% environment from the corresponding updates - and/or an undefined maximum
	% speed to test updates triggered by the environment)

	_FirstActorPid = class_Actor:create_initial_actor(
			class_TestSpatialisedActor,
			[ _FName="First Actor", _FInitialPosition={ -100.0, 0.0 },
			  _FPerceptionRadius=40.0, _FPerceptionPeriod=20,
			  _FMaxSpeed=5.0, _FTerminationOffset=500, EnvPid ] ),


	% This one just sits idle at the origin:
	%
	_SecondActorPid = class_Actor:create_initial_actor(
			class_TestSpatialisedActor,
			[ _SName="Second Actor", _SInitialPosition={ 0.0, 999990.0 },
			  _SPerceptionRadius=80.0, _SPerceptionPeriod=1500000,
			  _SMaxSpeed=undefined, _STerminationOffset=none, EnvPid ] ),


	% During the eastward movement of the first actor:
	%
	% - initially (at abscissa -100.0) and until having reached -80.0: none sees
	% the other
	%
	% - from -80.0 to -40.0: first is seen by second (and first does not see
	% anything)
	%
	% - from -40.0 to 40.0: both see the other
	%
	% - from 40.0 to 80.0: first is seen by second (and first does not see
	% anything)


	% We want this test to end once a specified virtual duration elapsed, in
	% seconds:
	SimulationDuration = 150,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	_RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
		"in virtual time of ~Bms.", [ SimulationDuration ] ),

	% Currently disabled as may fail in
	% class_TimeManager:beginTimeManagerTick/2, in:
	% true = ?list_impl:is_empty( ?getAttr(actors_to_trigger_in_one_diasca) ),

	%RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	%receive

	%	simulation_stopped ->
	%		?test_info( "Simulation stopped spontaneously, "
	%				   "specified stop tick must have been reached." )

	%end,

	?test_info( "Browsing the report results, if in batch mode." ),
	%class_ResultManager:browse_reports(),

	%sim_diasca:shutdown(),

	?test_stop.
