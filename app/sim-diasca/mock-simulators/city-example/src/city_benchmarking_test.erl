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



% Benchmarking case, based on City-example.
%
% The purpose of this case is to recreate a sharable yet representative
% simulation case that is scalable enough in order to benchmark Sim-Diasca on
% various manycore platforms.
%
% The scale and duration of the test can be chosen from the command-line (both
% are medium by default).
%
% Example of intended use:
%
% make city_benchmarking_run CMD_LINE_OPT="--batch --duration long --scale huge"
%
% See also benchmarking_scale() and benchmarking_duration() below, for their
% respective allowed values.
%
-module(city_benchmarking_test).


% Launchers specific to this case, for a run made from the shell:
%
-export([ run_constant_scale/1, run_constant_duration/1, run/2 ]).


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
% rather than development, on a specified list of computing hosts.
%
% This should translate into having the user:
%
%  - create a new 'sim-diasca-host-candidates-for-scale-benchmarks.txt' file,
%  listing all the hosts that shall be used in the distributed simulation (see
%  sim-diasca-host-candidates.txt for an example)
%
%  - rebuild everything accordingly, from the Sim-Diasca root directory:
% 'make clean all EXECUTION_TARGET=production'
%
%  - run this benchmarking case, from its directory:
% 'make city_benchmarking_run CMD_LINE_OPT="--batch --duration long
%    --scale huge" EXECUTION_TARGET=production'




% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	{ ScaleSetting, DurationSetting } = city_benchmarking:get_case_settings(),

	run_common( ScaleSetting, DurationSetting, _StopShell=true ).



% Runs a series of tests of constant scale, iterating in terms of durations.
%
-spec run_constant_scale( city_benchmarking:benchmarking_scale() ) ->
								no_return().
run_constant_scale( Scale ) ->

	Durations = city_benchmarking:get_duration_options(),

	io:format( "~nRunning ~B ~p simulations, of increasing duration:~n",
			   [ length( Durations ), Scale ] ),

	[
	  begin

		  io:format( "~n~n  Running a ~p simulation for a ~p duration...~n~n",
					 [ Scale, D ] ),

		  run( Scale, D )

	  end || D <- Durations ],

	io:format( "~n~n All ~p simulations successfully run.~n", [ Scale ] ).



% Runs a series of tests of constant duration, iterating in terms of scales.
%
-spec run_constant_duration( city_benchmarking:benchmarking_duration() ) ->
								   no_return().
run_constant_duration( Duration ) ->

	Scales = city_benchmarking:get_scale_options(),

	io:format( "~nRunning ~B ~p simulations, of increasing scale:~n",
			   [ length( Scales ), Duration ] ),

	[
	  begin

		  io:format( "~n~n  Running a ~p simulation for a ~p scale...~n~n",
					 [ Duration, S ] ),

		  run( S, Duration )

	  end || S <- Scales ],

	io:format( "~n~n All ~p simulations successfully run.~n",
			 [ Duration ] ).




% Runs the test with specified settings.
%
-spec run( city_benchmarking:benchmarking_scale(),
		   city_benchmarking:benchmarking_duration() ) -> no_return().
run( ScaleSetting, DurationSetting ) ->

	city_benchmarking:check_scale_setting( ScaleSetting ),
	city_benchmarking:check_duration_setting( DurationSetting ),

	run_common( ScaleSetting, DurationSetting, _StopShell=false ).



% Helper, common to all specifications.
%
run_common( ScaleSetting, DurationSetting, StopShell ) ->

	?test_start,

	io:format( "Running the City-example benchmarking case v.~s, "
			   "with scale '~s' and duration '~s'.~n",
			   [ text_utils:version_to_string( ?city_example_version),
				 ScaleSetting, DurationSetting ] ),

	{ CityDescription, EndTimestamp={ EndDate, EndTime }, TimestepDuration } =
	  city_benchmarking:get_benchmark_settings( ScaleSetting, DurationSetting ),


	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca City-example Benchmarking Case",

	  tick_duration = TimestepDuration,

	  % We restrict the wanted results, as otherwise larger cases could exhaust
	  % the number of used file descriptors; so we keep only some probes
	  % associated to incinerators:

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
		enable_performance_tracker = false

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	GISPid = class_Actor:create_initial_actor( class_GIS,
											   [ _DataSource=none ] ),

	CityGeneratorPid = class_CityGenerator:synchronous_new_link(
												CityDescription, GISPid ),


	CityGeneratorPid ! { generateCity, [], self() },
	receive

		{ wooper_result, city_generated } ->
			ok

	end,

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

	% Generator not needed anymore here:
	CityGeneratorPid ! delete,

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

	GISPid ! delete,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	case StopShell of

		true ->
			% Stopping the VM:
			?test_stop;

		false ->
			% Stays on shell:
			?test_stop_on_shell

	end.
