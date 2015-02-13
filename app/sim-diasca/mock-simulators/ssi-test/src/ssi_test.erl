% Copyright (C) 2010-2014 EDF R&D

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

% SSI-test stands for 'Sim-Diasca Scalability Integration Test', a business-free
% test dedicated to scalability assessment.

% This is one of the test cases of the Mock Simulators, used to emulate a
% third-party set of simulation cases and models that are to form with the
% Sim-Diasca engine a full-blown simulator: we test the ability of the engine to
% integrate approriately any user code.

% Aiming at showing and testing all main sim-diasca features, a simplified
% forest ecosystem context is designed. This ecosystem is composed of the
% following components:

% - class_Forest.erl
% - class_ForestDweller.erl
% - class_Oak.erl
% - class_Squirrel.erl
% - class_FemaleRedSquirrel.erl
% - class_MaleRedSquirrel.erl



% This test shows and tests:
%
% - the different ways for actor creation:
%
%  - initial actor creation: an actor created before simulation starting (see
%  initial squirrel instance creation in class_Forest.erl)
%
%  - initial placed actor creation: an actor created with a place hint before
%     simulation starting (see initial oak instance creation in class_Forest)
%
%  - placed actor creation in run time, i.e during the simulation (see female
%  and male squirrel creation in class_FemaleRedSquirrel.erl)
%
% - the different scheduling modes for actor spontaneous activities:
%
%   - in periodic way: see class_forest instance
%
%   - in passive way: see class_oak instance
%
%   - in mix way: see class_FemaleRedSquirrel and class_MaleRedSquirrel.erl
%   instance
%
% - the different ways for probe creation:
%
%   - create probe by class_Probe.erl and save the data in plain text
%
%   - create probe (or rather virtual probe) via class_DataLogger.erl and save
%   data in mnesia with setData by sending wooper message and setData directly
%   in probe table by calling dataLogger static method
%
% - the actor communication
%
% - the scalable capacity by parametering the forest dweller number, longevity
% of the dweller and the simulation duration time
%
-module(ssi_test).


-define( Tested_modules, [ class_Forest, class_ForestDweller, class_Oak,
			  class_Squirrel, class_FemaleRedSquirrel, class_MaleRedSquirrel ]
		).


% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the test.
-spec run() -> no_return().
run() ->

	?test_start,

	% Use default simulation settings (50Hz, batch reproducible) except for the
	% simulation name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Sim-Diasca Integration Test in an ecosystem context"

		%result_specification = all_outputs

	},

	% Specifies the list of computing hosts that can be used: (see the
	% sim-diasca-host-candidates-sample.txt example in the sim-diasca/conf
	% directory)
	DeploymentSettings = #deployment_settings{

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		additional_elements_to_deploy = [

			 { "mock-simulators/ssi-test", code },
			 { "mock-simulators/ssi-test/src/ssi_test.dat", data },
			 { "mock-simulators/ssi-test/src/ssi_test.cfg", data }

										 ],

		enable_performance_tracker = false

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

	PerformanceTrackerPid = class_PerformanceTracker:get_tracker(),

	case PerformanceTrackerPid of

		not_registered ->
			?test_info( "Performance tracker is not activated." );

		TrackerPid ->
			% Only an option:
			TrackerPid ! { setTickerPeriod, _Milliseconds=10 }

	end,


	% Following is the actor creation section.

	% Firstly, a forest actor is created with given LoadBalancerPid and the
	% forest longevity is defined as 2000ms, i.e. 100 ticks for 50Hz.

	?test_info( "Requesting to the load balancer the creation of "
		"a initial forest." ),

	ForestPid = class_Actor:create_initial_actor( class_Forest,
			[ _ForestName="Forest", _ForestLongevity=2000 ], LoadBalancerPid ),

	% Then, all initial forest dwellers (placed or non placed) are created by
	% sending initialForestCreation to forest with the number of initial actor
	% to be created:
	class_Forest:create_initial_foresters( _NbOaks=10, _NbSquirrels=50,
						 _ForestPid=ForestPid ),

	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=100, self() ] },

	% Following is the probe relative session, in particular for illustrating
	% the personnalisation of probe view

	% Getting one virtual probe created in class_Forest construct

	ForestPid ! { getVirtualProbe, [], self() },
	ForestVirtualProbe = test_receive(),

	ForestPid ! { getDataLoggerPid, [], self() },
	DataLoggerPid = test_receive(),

	?test_info( "Changing the canvas size." ),
	DataLoggerPid ! { setCanvasSize, [ ForestVirtualProbe, 800, 500 ] },

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.
