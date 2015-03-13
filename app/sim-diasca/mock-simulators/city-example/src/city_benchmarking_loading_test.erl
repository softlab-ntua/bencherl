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


% The purpose of this module is to load and start a simulation from the full
% description of the initial state of a city, as stored in file by the
% counterpart generation module.
%
% See the city_benchmarking_generation_test.erl to generate such a
% initialisation file.



% This is useful for larger cases like this one, as the procedural generation
% might be very long: better generate the initial state once for all, and re-use
% it at will in various simulation instances.



% Example of intended use:
%
% make city_benchmarking_loading_run CMD_LINE_OPT="--batch --duration long
% --scale huge"
%
%
-module(city_benchmarking_loading_test).


% Launchers specific to this case, for a run made from the shell:
%
-export([ run/2 ]).


% For ?gis_name:
-include("class_GIS.hrl").


% For ?city_example_version:
-include("city_example_version.hrl").


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For the city_description record:
-include("class_CityGenerator.hrl").



% Implementation notes:
%
% See get_benchmark_settings/0 to dimension your test.
%
% Typically to be run non-interactively (batch mode, with no trace supervisor),
% potentially with all traces disabled and all settings chosen for production
% rather than development.
%
% This should translate into having the user:
%
%  - rebuild everything accordingly, from the Sim-Diasca root directory:
% 'make clean all EXECUTION_TARGET=production'
%
%  - run this benchmarking case generator, from its directory:
% 'make city_benchmarking_loading_run CMD_LINE_OPT="--batch --duration long
%    --scale huge" EXECUTION_TARGET=production';




% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	{ ScaleSetting, DurationSetting } = city_benchmarking:get_case_settings(),

	run_common( ScaleSetting, DurationSetting, _StopShell=true ).



% Runs the test with specified settings.
%
-spec run( city_benchmarking:benchmarking_scale(),
		   city_benchmarking:benchmarking_duration() ) -> no_return().
run( ScaleSetting, DurationSetting ) ->

	city_benchmarking:check_scale_setting( ScaleSetting ),

	run_common( ScaleSetting, DurationSetting, _StopShell=false ).



% Helper, common to all specifications.
%
run_common( ScaleSetting, DurationSetting, StopShell ) ->

	?test_start,

	VersionString = text_utils:version_to_string( ?city_example_version ),

	Filename = text_utils:format(
				 "city-example-instances-version-~s-scale-~s.init",
				 [ VersionString, ScaleSetting ] ),

	io:format( "Running a City-example simulation from an initial state "
			   "corresponding to version ~s, with scale '~s', to be read "
			   "from pre-generated file '~s' and run with a '~s' simulation "
			   "duration.~n",
			   [ VersionString, ScaleSetting, Filename, DurationSetting ] ),


	case file_utils:is_existing_file_or_link( Filename ) of

		true ->
			ok;

		false ->

			?notify_error_fmt( "Initialisation file '~s' not found, "
							   "one may run: '"
							   "make city_benchmarking_loading_run "
							   "CMD_LINE_OPT=\"--batch --scale ~s\"' to "
							   "generate it first.~n",
							   [ Filename, ScaleSetting ] ),

			throw( { initialisation_file_not_found, Filename } )

	end,


	{ _CityDescription, EndTimestamp={ EndDate, EndTime }, TimestepDuration } =
	  city_benchmarking:get_benchmark_settings( ScaleSetting, DurationSetting ),


	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca City-example Benchmarking Loading Case",

	  tick_duration = TimestepDuration,

	  initialisation_files = [ Filename ],

	  % We restrict the wanted results, as otherwise larger cases could exhaust
	  % the number of used file descriptors; so we keep only the probes
	  % associated to some incinerators:

	  result_specification = no_output

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
					   "sim-diasca-host-candidates-for-scale-benchmarks.txt" },

		%node_availability_tolerance = fail_on_unavailable_node,

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		%
		additional_elements_to_deploy = [ { ".", code } ],

		plugin_directories = [
					 "../../../sim-diasca/src/core/src/plugins/tests/" ],

		enable_data_logger = false,

		% Would alter wrongly the benchmark:
		enable_performance_tracker = false,
 
		perform_initial_node_cleanup = false
	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	GISPid = basic_utils:get_registered_pid_for( ?gis_name, _Scope=global ),

	case executable_utils:is_batch() of

		true ->
			ok;

		false ->
			GISPid ! { render, [], self() }

	end,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	RootTimeManagerPid ! { setFinalSimulationDate, [ EndDate, EndTime ] },

	?test_info_fmt( "Starting simulation, for a stop at ending timestamp ~s.",
					[ basic_utils:get_textual_timestamp( EndTimestamp ) ] ),


	GISPid ! traceContent,

	% Wait for render completion, otherwise instances might be already removed:
	case executable_utils:is_batch() of

		true ->
			ok;

		false ->
			receive

				{ wooper_result, gis_rendering_done } ->
					ok

			end

	end,

	RootTimeManagerPid ! { start, self() },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	GISPid ! delete,

	sim_diasca:shutdown(),

	case StopShell of

		true ->
			% Stopping the VM:
			?test_stop;

		false ->
			% Stays on shell:
			?test_stop_on_shell

	end.
