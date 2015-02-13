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


% The purpose of this module is to generate the full description of the initial
% state of a city and store it in file, in the prospect of running a simulation
% of it afterwards (see city_benchmarking_loading_test.erl).

% This is useful for larger cases like this one, as the procedural generation
% might be very long: better generate the initial state once for all, and re-use
% at will in various simulation instances.



% Example of intended use:
%
% make city_benchmarking_generation_run CMD_LINE_OPT="--batch --duration long
% --scale huge"
%
%
-module(city_benchmarking_generation_test).


% Launchers specific to this case, for a run made from the shell:
%
-export([ run/2 ]).


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
% 'make city_benchmarking_generation_run CMD_LINE_OPT="--batch --duration long
%    --scale huge" EXECUTION_TARGET=production'




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

	VersionString = text_utils:version_to_string( ?city_example_version),

	io:format( "Generating an initial state for the City-example benchmarking "
			   "case v.~s, with scale '~s'.~n",
			   [ VersionString, ScaleSetting ] ),

	Filename = text_utils:format(
				 "city-example-instances-version-~s-scale-~s.init",
				 [ VersionString, ScaleSetting ] ),

	case file_utils:is_existing_file( Filename ) of

		true ->

			BackupFilename = Filename ++ "-"
				++ basic_utils:get_textual_timestamp_for_path(),

			?notify_warning_fmt( "Initialisation file '~s' was already "
								 "existing, it has been moved to backup "
								 "file '~s'.",
								 [ Filename, BackupFilename ] ),

			file_utils:move_file( Filename, BackupFilename );

		false ->
			ok

	end,


	% Cannot use raw, as the city generator will write it as well:
	InitFile = file_utils:open( Filename, _Opts=[ write, exclusive,
												  delayed_write ] ),

	file_utils:write( InitFile, "% This is a Sim-Diasca initialisation file "
								"for the City-example case.~n~n", [] ),

	file_utils:write( InitFile, "% Version: ~s.~n", [ VersionString ] ),
	file_utils:write( InitFile, "% Scale: ~s.~n~n", [ ScaleSetting ] ),

	file_utils:write( InitFile, "% Created on ~s by ~s, on host ~s.~n~n",
					  [ basic_utils:get_textual_timestamp(),
						system_utils:get_user_name(),
						net_utils:localhost()
					  ] ),

	{ CityDescription, _EndTimestamp, TimestepDuration } =
	  city_benchmarking:get_benchmark_settings( ScaleSetting, DurationSetting ),

	CityName = city_benchmarking:get_city_name_from_scale( ScaleSetting ),

	CityDescription = city_descriptions:get_description_for( CityName ),

	file_utils:write( InitFile, "% City description: ~s~n",
					  [ city_descriptions:to_string( CityDescription ) ] ),


	% Rather than creating a very rich mock-up environment, it is simpler to
	% initialise the engine ( with minimal settings) and to never start it:
	%
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca City-example Benchmarking Generation Case",

	  tick_duration = TimestepDuration,

	  result_specification = no_output

	},


	DeploymentSettings = #deployment_settings{

	   computing_hosts = { use_host_file_otherwise_local,
						   "sim-diasca-host-candidates.txt" },

	   % All code from mock-simulators/city-example/src:
	   additional_elements_to_deploy = [ { ".", code } ],

	   enable_data_logger = false

	},


	LoadBalancingSettings = #load_balancing_settings{},


	% A deployment manager is created directly on the user node:
	_DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),


	GISPid = class_Actor:create_initial_actor( class_GIS,
											   [ _DataSource=none ] ),

	CityGeneratorPid = class_CityGenerator:synchronous_new_link(
												CityDescription, GISPid ),

	CityGeneratorPid ! { writeInitialisation, [ InitFile ], self() },

	receive

		{ wooper_result, initialisation_written } ->
			ok

	end,


	% Generator not needed anymore here:
	CityGeneratorPid ! delete,

	file_utils:write( InitFile, "~n% End of initialisation data.~n", [] ),

	file_utils:close( InitFile ),

	Message = io_lib:format(
				"~nInitialisation file '~s' successfully generated.~n~n",
				[ Filename ] ),

	?notify_info( Message ),

	io:format( Message ),

	case StopShell of

		true ->
			% Stopping the VM:
			?test_stop;

		false ->
			% Stays on shell:
			?test_stop_on_shell

	end.
