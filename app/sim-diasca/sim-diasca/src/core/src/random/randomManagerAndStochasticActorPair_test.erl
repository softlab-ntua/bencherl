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



% Unit tests for the interaction between the random manager and stochastic
% actors.
%
% See the class_RandomManager.erl and class_TestStochasticActor.erl tested
% modules.
%
-module(randomManagerAndStochasticActorPair_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Prefer reusing most default settings:
	SimulationSettings = #simulation_settings{

	  simulation_name = "Stochastic Actor Test"

	},


	DeploymentSettings = #deployment_settings{},

	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings, 
								 DeploymentSettings, LoadBalancingSettings ),

	% The random laws the test actor will rely on:
	% ({ RandomLawName, RandomSettings })
	RandomLaws = [
		{ my_first_uniform,  {uniform,     5} },
		{ my_second_uniform, {uniform,   100} },
		{ my_gaussian,       {gaussian, 50,2} },
		{ my_exponential,    {exponential,80} }
	],


	% Creates an actor that will automatically subscribe itself to the manager
	% and that will terminate on specified tick:
	class_Actor:create_initial_actor( class_TestStochasticActor,
			  [ "Cartman", RandomLaws, _CartmanTerminationProbability=20 ] ),

	% Other actors:
	class_Actor:create_initial_actor( class_TestStochasticActor,
			  [ "Kenny", RandomLaws, _KennyTerminationProbability=99 ] ),

	class_Actor:create_initial_actor( class_TestStochasticActor,
			  [ "Kyle", RandomLaws, _KyleTerminationProbability=10 ] ),

	class_Actor:create_initial_actor( class_TestStochasticActor,
			  [ "Stan", RandomLaws, _StanTerminationProbability=0 ] ),


	% A TestStochasticActor requesting - and consuming - no law was successfully
	% tested as well.


	% We want this test to end once a specified number of ticks are elapsed:
	StopTick = 30,

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
