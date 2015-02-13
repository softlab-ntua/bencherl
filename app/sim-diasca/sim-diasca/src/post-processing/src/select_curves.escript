#!/usr/bin/env escript
%%! -pa ../../../../common/src/utils -pa ../../../../common/src/data-management -pa ../../../../wooper/src -pa ../../../../traces/src -pa ../../../../common/src

% Note: this script is a failed attempt, as the include paths for traces could
% never be set up appropriately (there is a very weird behaviour regarding -pa
% paths, most probably the escript support of R14B03 is buggy in this regard).


% Copyright (C) 2011 EDF R&D

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


% Adapted from time_series_analyzer_test.erl.


% The relative paths to find BEAM *and* header files are a bit of a mess: the
% script will fail if not run directly from the directory it is in.

% For app_start (relies on the -pa setting for traces):
-include_lib("src/traces_for_apps.hrl").


% Allows to select a subset of the curves in the specifed time-series.
main( [ SourceDataFilename | T ] ) ->

	case file_utils:is_existing_file( SourceDataFilename ) of

		true ->
			ok;

		false ->
			throw( { non_existing_datafile, SourceDataFilename } )

	end,

	case basic_utils:is_list_of_integers( T ) of

		true ->
			ok;

		false ->
			io:format( "Error, ~p is not a list of integers.~n", [T] ),
			display_syntax(),
			halt( 5 )

	end,
	run( SourceDataFilename, T );

main( Other ) ->

	io:format( "Error, invalid parameters were provided ('~s').~n", [ Other ] ),

	display_syntax(),

	halt( 10 ).




display_syntax() ->

	ScriptName = escript:script_name(),

	io:format( "Usage: " ++ ScriptName
			  ++ " DATA_FILENAME [list of the indexes of selected curves]~n"
			  ++ "Example: " ++ ScriptName ++ " my-data.dat 1 3 4 6~n" ).



% Run the selection operation.
run( SourceDataFilename, CurveIndexList ) ->

	?app_start,

	AnalyzerPid = class_TimeSeriesAnalyzer:synchronous_new_link(
		SourceDataFilename,
		_SeriesFilters=[ { curve_selector_series_filter, CurveIndexList } ],
		_CommonCurveFilters=[],
		_CurveSpecificFilters=[] ),

	AnalyzerPid ! delete,

	?app_stop.
