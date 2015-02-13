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



% Overall unit test of the Sim-Diasca result management facilities.
%
-module(result_management_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Generates some data to test the data-logging.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Result management test"

		% Allows to test various combinations of result specifications:

		% Default result specification can be left. Otherwise add a comma above
		% and uncomment one of the specifications below:

		% Correct, accepted specifications:

		%result_specification = no_output
		%result_specification = all_outputs
		%result_specification = all_basic_probes_only
		%result_specification = all_virtual_probes_only

		% Will select nothing:
		%result_specification = []

		% Will select all (basic) probes:
		%result_specification = [ { targeted_patterns, [ ".*" ] } ]

		% Will select nothing here, as blacklisted:
		%result_specification = [
		%	  { targeted_patterns, [ {".*",[plot_only]} ] },
		%	  { blacklisted_patterns, ["My virtual probe" ] } ]


		% Incorrect, rejected specifications:

		%result_specification = unexpected_option
		%result_specification = [ { targeted_patterns, unexpected_pattern } ]
		%result_specification = [ { targeted_patterns, [ ".*" ] },
		%					unexpected_option ]

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
	% (note that localhost is implied)
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

	IsBatch = executable_utils:is_batch(),

	?test_info( "Creating an actor that will make use of two virtual probes." ),
	LoadBalancerPid ! { createInitialActor,
		   [ class_DataLoggingActor, [ "First data-logging test actor",
					_TerminationTickOffset=200, _Listener=self() ] ],
		   self() },

	_ActorPid = test_receive(),


	?test_info( "Creating also a basic probe directly from the test." ),

	class_Probe_test:manage_facility_probe(
	  "My basic probe created from test" ),

	?test_info( "Creating also a virtual probe directly from the test." ),
	datalogging_test:manage_facility_probe(
	  "My virtual probe created from test" ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	case IsBatch of

		true ->
			% Nothing more, in batch mode.
			ok;

		false ->
			% Display more information in interactive mode:
			mnesia:start(),
			mnesia:info(),
			observer:start(),
			io:format( "~n(hit CTRL-M on the TV window to view "
					  "the virtual probe tables, by double-clicking "
					  "on their name)~n~n" )

	end,

	?test_stop.
