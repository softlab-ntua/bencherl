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



% Allows to select the intended simulation scale:
-type benchmarking_scale() :: 'tiny' | 'small' | 'medium' | 'large' | 'huge'.


% Allows to select the intended simulation duration:
-type benchmarking_duration() :: 'brief' | 'short' | 'medium' | 'long'.




% Returns the duration (in simulation time) of a fundamental time-step.
%
get_time_step_duration() ->

	% 5 seconds per time step, expressed in virtual seconds:
	%
	5.0.



% Returns the main settings to determine the size of this test, in terms of
% scale (of the virtual city) and duration (which depends on the simulation
% frequency and on the virtual time and date until the simulation should run,
% starting from 1/1/2000 at 00:00).
%
-spec get_benchmark_settings( benchmarking_scale(), benchmarking_duration() ) ->
	{ class_CityGenerator:city_description(), basic_utils:timestamp(),
	 virtual_seconds() }.
% Tiny scale:
get_benchmark_settings( _BenchmarkingScale=tiny, BenchmarkingDuration ) ->

	% km²:
	Area = 64.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	CityDescription = #city_description{

			name="Yzeure",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=2,
			landfill_count=1,
			residential_waste_source_count=10,
			industrial_waste_source_count=2,
			road_junction_count=12,
			waste_truck_count=18

										},

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() };


% Small scale:
get_benchmark_settings( _BenchmarkingScale=small, BenchmarkingDuration ) ->

	% Km²:
	Area = 643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	CityDescription = #city_description{

			name="Orleans",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=8,
			landfill_count=4,
			residential_waste_source_count=150,
			industrial_waste_source_count=30,
			road_junction_count=200,
			waste_truck_count=80

										},

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() };



% Medium scale:
get_benchmark_settings( _BenchmarkingScale=medium, BenchmarkingDuration ) ->

	% Km²:
	Area = 2643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	%
	CityDescription = #city_description{

			name="Rennes",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=24,
			landfill_count=9,
			residential_waste_source_count=700,
			industrial_waste_source_count=130,
			road_junction_count=950,
			waste_truck_count=707

										},

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() };



% Large scale:
get_benchmark_settings( _BenchmarkingScale=large, BenchmarkingDuration ) ->

	% Km²:
	Area = 172643.24,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 217,
	MaxAltitude = 278,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	CityDescription = #city_description{

			name="Paris",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=48,
			landfill_count=18,
			residential_waste_source_count=6500,
			industrial_waste_source_count=480,
			road_junction_count=8500,
			waste_truck_count=1912

										},

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() };



% Huge scale:
get_benchmark_settings( _BenchmarkingScale=huge, BenchmarkingDuration ) ->

	% Km²:
	Area = 8921485.0,

	% Meters:
	SideLen = erlang:round( class_CityGenerator:area_to_side_length( Area ) ),

	MinAltitude = 1937,
	MaxAltitude = 2602,

	% This city happens to be just translated in terms of altitude from the
	% origin (for X and Y, it is centered):
	CityDescription = #city_description{

			name="Beijing",

			dimensions={ SideLen, SideLen, MaxAltitude - MinAltitude },

			center={ - SideLen / 2, - SideLen / 2,
					( MinAltitude + MaxAltitude ) / 2 },

			incinerator_count=572,
			landfill_count=95,
			residential_waste_source_count=200000,
			industrial_waste_source_count=20000,
			road_junction_count=195000,
			waste_truck_count=95100

										},

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() }.




% Returns the date and time of the end (in virtual time) of the simulation.
%
-spec get_benchmark_ending_deadline( benchmarking_duration() ) ->
	basic_utils:timestamp().
get_benchmark_ending_deadline( brief ) ->
	% Just a simulation for 8 hours:
	{ { 2000, 1, 1 }, { 8, 0, 0 } };

get_benchmark_ending_deadline( short ) ->
	% Just a simulation for 3 days:
	{ { 2000, 1, 4 }, { 0, 0, 0 } };

get_benchmark_ending_deadline( medium ) ->
	% One month:
	{ { 2000, 2, 1 }, { 0, 0, 0 } };

get_benchmark_ending_deadline( long ) ->
	% One year:
	{ { 2001, 1, 1 }, { 0, 0, 0 } }.



% Returns the list of the allowed settings in order to specify the scale of this
% use case.
%
-spec get_scale_options() -> [ benchmarking_scale() ].
get_scale_options() ->
	[ tiny , small , medium , large, huge ].



% Returns the list of the allowed settings in order to specify the duration of
% this use case.
%
-spec get_duration_options() -> [ benchmarking_duration() ].
get_duration_options() ->
	[ brief, short, medium, long ].



% Returns { ScaleSetting, DurationSetting }, i.e. the scale and duration
% settings for this simulation case.
%
get_case_settings() ->

	ScaleSetting = case init:get_argument( '-scale' ) of

		{ ok, [ [ ScaleString ] ] } when is_list( ScaleString ) ->
			ScSetting = text_utils:string_to_atom( ScaleString ),
			check_scale_setting( ScSetting );

		{ ok, OtherScale } ->
			throw( { invalid_scale_specification, OtherScale } );

		_ ->
			% Default:
			small

	end,

	DurationSetting = case init:get_argument( '-duration' ) of

		{ ok, [ [ DurationString ] ] } when is_list( DurationString ) ->
			DurSetting = text_utils:string_to_atom( DurationString ),
			check_duration_setting( DurSetting );


		{ ok, OtherDuration } ->
			throw( { invalid_duration_specification, OtherDuration } );


		_ ->
			% Default:
			brief

	end,

	{ ScaleSetting, DurationSetting }.




% Ensures the specified scale setting is valid.
%
check_scale_setting( ScaleSetting ) ->

	case lists:member( ScaleSetting, get_scale_options() ) of

		true ->
			ScaleSetting;

		false ->
			throw( { invalid_scale_specification, ScaleSetting } )

	end.



% Ensures the specified duration setting is valid.
%
check_duration_setting( DurationSetting ) ->

	case lists:member( DurationSetting, get_duration_options() ) of

		true ->
			DurationSetting;

		false ->
			throw( { invalid_duration_specification, DurationSetting } )

	end.




% Runs the test, determining the settings from the command-line, otherwise using
% defaults.
%
-spec run() -> no_return().
run() ->

	{ ScaleSetting, DurationSetting } = get_case_settings(),

	run_common( ScaleSetting, DurationSetting, _StopShell=true ).



% Runs a series of tests of constant scale, iterating in terms of durations.
%
-spec run_constant_scale( benchmarking_scale() ) -> no_return().
run_constant_scale( Scale ) ->

	Durations = get_duration_options(),

	io:format( "~nRunning ~B ~p simulations, of increasing duration:~n",
			   [ length( Durations ), Scale ] ),

	[
	  begin

		  io:format( "~n~n  Running a ~p simulation for a ~p duration...~n~n",
					 [ Scale, D ] ),

		  run( Scale, D )

	  end || D <- Durations ],

	io:format( "~n~n All ~p simulations successfully run.~n",
			 [ Scale ] ).



% Runs a series of tests of constant duration, iterating in terms of scales.
%
-spec run_constant_duration( benchmarking_duration() ) -> no_return().
run_constant_duration( Duration ) ->

	Scales = get_scale_options(),

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
-spec run( benchmarking_scale(), benchmarking_duration() ) -> no_return().
run( ScaleSetting, DurationSetting ) ->

	check_scale_setting( ScaleSetting ),
	check_duration_setting( DurationSetting ),

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
		get_benchmark_settings( ScaleSetting, DurationSetting ),


	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca City-example Benchmarking Case",

	  tick_duration = TimestepDuration,

	  % We restrict the wanted results, as otherwise larger cases could exhaust
	  % the number of used file descriptors; so we keep only the probes
	  % associated to incinerators and waste trucks:

	   result_specification = [ { targeted_patterns, [

					  { "Incinerator.*", [ data_and_plot ] }

								] } ]

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
					   "sim-diasca-host-candidates-for-scale-benchmarks.txt" },

		%node_availability_tolerance = fail_on_unavailable_node,

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		additional_elements_to_deploy = [
									 { "mock-simulators/city-example", code } ],

		plugin_directories = [
					 "../../../sim-diasca/src/core/src/plugins/tests/" ],

		% Would alter wrongly the benchmark:
		enable_performance_tracker = false

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),

	GISPid = class_GIS:new_link( _DataSource=none ),

	CityGeneratorPid = class_CityGenerator:synchronous_new_link(
												CityDescription, GISPid ),

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
