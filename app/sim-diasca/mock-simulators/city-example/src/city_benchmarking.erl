% Copyright (C) 2012-2014 EDF R&D

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


-module(city_benchmarking).


-export([ get_benchmark_settings/2, get_scale_options/0, get_duration_options/0,
		  get_benchmark_ending_deadline/1, get_case_settings/0,
		  check_scale_setting/1, check_duration_setting/1,
		  get_time_step_duration/0, get_city_name_from_scale/1 ]).


% For virtual_seconds/0;
-include("class_TimeManager.hrl").



% Allows to select the intended simulation scale:
-type benchmarking_scale() :: 'tiny' | 'small' | 'medium' | 'large' | 'huge'.


% Allows to select the intended simulation duration:
-type benchmarking_duration() :: 'brief' | 'short' | 'medium' | 'long'.


-export_type([ benchmarking_scale/0, benchmarking_duration/0 ]).



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
get_benchmark_settings( BenchmarkingScale, BenchmarkingDuration ) ->

	CityName = case BenchmarkingScale of

				  tiny ->
					  "Yzeure";

				  small ->
					  "Orleans" ;

				  medium ->
					  "Rennes" ;

				  large ->
					  "Paris" ;

				  huge ->
					  "Beijing"

	end,

	CityDescription = city_descriptions:get_description_for( CityName ),

	EndDate = get_benchmark_ending_deadline( BenchmarkingDuration ),

	{ CityDescription, EndDate, get_time_step_duration() }.




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


% Returns the name of the coty that corresponds to specified benchmarking scale.
%
-spec get_city_name_from_scale( benchmarking_scale() ) -> string().
get_city_name_from_scale( _BenchmarkingScale=tiny ) ->
	"Yzeure";

get_city_name_from_scale( _BenchmarkingScale=small ) ->
	"Orleans" ;

get_city_name_from_scale( _BenchmarkingScale=medium ) ->
	"Rennes" ;

get_city_name_from_scale( _BenchmarkingScale=large ) ->
	"Paris" ;

get_city_name_from_scale( _BenchmarkingScale=huge ) ->
	"Beijing".
