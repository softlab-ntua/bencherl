% Copyright (C) 2011-2014 EDF R&D

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



% Overall unit test of the Sim-Diasca data exchange facilities.
%
-module(data_exchange_test).



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

		simulation_name = "Data Exchange test"

	},


	% Examples of configuration files:
	LocalPath = "sim-diasca/src/core/src/data-management/data-exchange",

	ValidConfigurationFilename = file_utils:join( LocalPath,
		"valid_example_configuration_file.cfg" ),

	%InvalidConfigurationFilename = file_utils:join( LocalPath,
	%	"invalid_example_configuration_file.cfg" ),


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing hosts are
	% specified (to be updated depending on your environment):
	% (note that localhost is implied)
	DeploymentSettings = #deployment_settings{

		% Useful to exclude the local host, in order to test the case-specific
		% data-exchanger that will be created:
		%computing_hosts = { use_host_file_otherwise_local,
		%			   "sim-diasca-host-candidates.txt", exclude_localhost },

		computing_hosts = { use_host_file_otherwise_local,
					   "sim-diasca-host-candidates.txt" },

		% Will work flawlessly:
		%enable_data_exchanger = true
		%enable_data_exchanger = false
		%enable_data_exchanger = { true, [] }
		enable_data_exchanger = { true, [ ValidConfigurationFilename ] }


		% Will be rejected:
		%enable_data_exchanger = unexpected
		%enable_data_exchanger = { true, unexpected }
		%enable_data_exchanger = { false, [] }
		%enable_data_exchanger = { true, [ InvalidConfigurationFilename ] }

		%enable_data_exchanger = { true, [ InvalidConfigurationFilename,
		%							   ValidConfigurationFilename ] }

		% Would lead to data being defined twice:
		%enable_data_exchanger = { true, [ ValidConfigurationFilename,
		%							   ValidConfigurationFilename ] }


	},

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	?test_info( "Deployment manager created, "
				"retrieving the root data exchanger." ),

	% Of course will fail if enable_data_exchanger has been set to false
	% previously:
	%
	ExchangeSettings = class_DataExchanger:get_case_exchange_settings(),

	RootDataExchangerPid = class_DataExchanger:get_root_exchanger(),

	?test_info( "Now testing the defining of initial data." ),

	%RootDataExchangerPid ! { traceDistributedData, [], self() },
	%distributed_data_traced = test_receive(),

	% We may not specify exchanger PIDs; then a look-up will be performed:

	class_DataExchanger:define_initial_data( example_key_1, example_value_1 ),

	% We can use a data list as well:
	class_DataExchanger:define_initial_data( [

			 { example_key_2, example_value_2 },
			 { example_key_3, example_value_3 },
			 { example_key_5, example_value_5, mutable },
			 { example_key_for_actors, 1, mutable }

											  ] ),

	% Trying to define an already defined data:
	%class_DataExchanger:define_initial_data( example_key_1, whatever ),

	% Trying to set a non-already defined data:
	%class_DataExchanger:modify_initial_data( example_key_unknown, whatever ),

	%RootDataExchangerPid ! { traceDistributedData, [], self() },
	%distributed_data_traced = test_receive(),

	%Qualifier = const,
	Qualifier = mutable,
	%Qualifier = unexpected_qualifier,

	% Using the PID cached in this test, and a qualifier here:
	class_DataExchanger:define_initial_data( example_key_4, example_value_4,
		Qualifier, ExchangeSettings ),

	RootDataExchangerPid ! { traceDistributedData, [], self() },
	distributed_data_traced = test_receive(),

	% Trying a disallowed overwriting (target data is already defined):
	%class_DataExchanger:define_initial_data( example_key_2, example_value_2,
	%	mutable, ExchangeSettings ),

	% Trying a disallowed overwriting (target data is const):
	%class_DataExchanger:modify_initial_data( example_key_2, example_value_2,
	%	mutable, ExchangeSettings ),

	% Trying an allowed overwriting (target data is mutable):
	class_DataExchanger:modify_initial_data( example_key_4,
		other_example_value_4, const, ExchangeSettings ),

	% Attempt to perform a const violation:
	%class_DataExchanger:modify_initial_data( example_key_4,
	%	other_example_value_4, const, ExchangeSettings ),

	RootDataExchangerPid ! { traceDistributedData, [], self() },
	distributed_data_traced = test_receive(),

	?test_info( "Now testing the reading of initial data." ),

	% Successful reading of an updated data:
	other_example_value_4 = class_DataExchanger:read_initial_data(
													example_key_4 ),

	% Reading a non-existing data will fail:
	%non_existing_value = class_DataExchanger:read_initial_data(
	%													a_non_existing_key ),

	{ 1, mutable } = class_DataExchanger:read_qualified_initial_data(
			example_key_for_actors, ExchangeSettings ),

	?test_info( "Now setting initial data." ),

	class_DataExchanger:modify_initial_data( example_key_5, 2, mutable,
											ExchangeSettings ),

	class_DataExchanger:modify_initial_data( example_key_5, 3, const,
											ExchangeSettings ),

	% Expected by test actor:
	class_DataExchanger:modify_initial_data( example_key_for_actors, 3,
											ExchangeSettings ),

	% This one should fail, as now is const due to the last call:
	%class_DataExchanger:modify_initial_data( example_key_for_actors, 4,
	%										ExchangeSettings ),

	% Setting a non-already defined key will not be allowed:
	%class_DataExchanger:modify_initial_data( other_example_key_for_actors, 1,
	%					mutable, ExchangeSettings ),

	class_DataExchanger:define_initial_data( other_example_key_for_actors, 1,
						mutable, ExchangeSettings ),

	?test_info( "Retrieving the load balancer." ),

	DeploymentManagerPid ! { getLoadBalancer, [], self() },
	LoadBalancerPid = test_receive(),


	?test_info( "Requesting to the load balancer the creation of "
		"a first initial test actor for data exchange." ),

	FirstKey = key_1,


	% If the second key is the same as the first, the two actors will step over
	% each other and some commits are expected to fail:
	%SecondKey = key_1,
	SecondKey = key_2,

	class_DataExchanger:define_initial_data( [ { FirstKey, 0, mutable },
			{ SecondKey, 0, mutable } ], ExchangeSettings ),

	?test_info( "Creating two initial data-exchanging test actors." ),

	% This one will terminate before the end of the simulation:
	%
	LoadBalancerPid ! { createInitialActor,
		   [ class_DataExchangeTestActor, [ "First data-exchange test actor",
					FirstKey, _FirstTerminationTickOffset=100 ] ],
		   self() },

	_FirstActorPid = test_receive(),


	% This one will terminate after the end of the simulation:
	%
	LoadBalancerPid ! { createInitialActor,
		   [ class_DataExchangeTestActor, [ "Second data-exchange test actor",
					SecondKey, _SecondTerminationTickOffset=200 ] ],
		   self() },

	_SecondActorPid = test_receive(),


	RootDataExchangerPid ! { traceDistributedData, [], self() },
	distributed_data_traced = test_receive(),


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


	?test_info( "Removing deployment manager, which will in turn take care "
		"of the deletion of all actors and agents on all nodes." ),

	sim_diasca:shutdown(),

	?test_stop.
