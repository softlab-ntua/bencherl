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



% The test could run until tick offset #120, however the next tick after offset
% #80 at which this only actor will be scheduled will trigger its
% termination. Therefore the simulation should end automatically at this tick.
%
% The test actor actually intended to stop at offset #50, but it planned
% actually its spontaneous ticks at #48 then #51, thus delaying its termination.
%
% Test can be run on for example two computers, to ensure an empty node (as
% there is only one actor) is correctly managed.
%
-module(scheduling_one_erratic_actor_test).



% To check that the unique erratic actor is correctly scheduled, here is the
% overall behaviour it is expected to respect:



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs a distributed simulation (of course if relevant computing hosts are
% specified).
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Scheduling one erratic actor test"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested, use an host file otherwise
	% fall back to local).
	DeploymentSettings = #deployment_settings{},


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

	FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ "First erratic test actor",
			_SchedulingSettings={ erratic, _MinRange=6 },
			_CreationSettings=no_creation, _TerminationTickOffset=80 ],
			LoadBalancerPid ),

	FirstActorPid ! { getAAI, [], self() },
	2 = test_receive(),

	?test_info_fmt( "First actor has for PID ~w and for AAI 2.",
		[ FirstActorPid ] ),


	?test_info( "First actor has a correct AAI." ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },


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

	sim_diasca:shutdown(),

	?test_stop.
