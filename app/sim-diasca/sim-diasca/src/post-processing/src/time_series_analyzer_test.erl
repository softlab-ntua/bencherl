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



% Overall unit test of the Sim-Diasca time-series post-processing facilities.
%
-module(time_series_analyzer_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Generates some data to test the data-logging.
%
-spec run() -> no_return().
run() ->

	?test_start,

	TestDataFilename = "time_series_test.dat",
	%TestDataFilename = "time_series_test-warningless.dat",
	%TestDataFilename = "non_existing_file",

	% Parameters for the "selector" series filter: we select only the first two
	% curves, and the fourth (out of 6, here), in that order.
	CurveSelection = [ 1, 2, 4 ],

	AnalyzerPid = class_TimeSeriesAnalyzer:synchronous_new_link(
		TestDataFilename,
		_SeriesFilters=[ { curve_selector_series_filter, CurveSelection } ],
		_CommonCurveFilters=[ { extrema_curve_filter, [] } ],
		_CurveSpecificFilters=[] ),

	AnalyzerPid ! delete,

	?test_stop.
