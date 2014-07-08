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


% Unit test for the RandomManager class implementation, regarding the gaussian
% distribution.
%
% See the class_RandomManager.erl module.
%
-module(class_RandomManager_Gaussian_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).



show_gaussian( RandomManagerPid ) ->

	Mu = 40,
	Sigma = 1.2,

	?test_debug_fmt( "Requesting a gaussian random value "
		"with settings mean = ~p, deviation = ~p.", [ Mu, Sigma ] ),

	RandomManagerPid ! { getGaussianValue, [ Mu, Sigma ], self() },

	receive

		{ wooper_result, { gaussian_value, Value } } ->
			?test_debug_fmt(
				"Received gaussian random value: ~p.", [ Value ] )

	end.



show_gaussian( RandomManagerPid, Mu, Sigma ) ->

	?test_debug_fmt( "Requesting a gaussian random value "
		"with mean = ~p, deviation = ~p.", [ Mu, Sigma ] ),

	RandomManagerPid ! { getGaussianValue, [ Mu, Sigma ], self() },

	receive

		{ wooper_result, { gaussian_value, Value } } ->
			?test_debug_fmt(
				"Received gaussian random value: ~p.", [ Value ] )

	end.



draw_gaussian_values( _Count=0, Table, _Mu, _Sigma, _RandomManagerPid ) ->
	Table;

draw_gaussian_values( Count, Table, Mu, Sigma, RandomManagerPid ) ->
	RandomManagerPid ! { getPositiveIntegerGaussianValue, [ Mu, Sigma ],
						self() },

	% Wanting a random value in ] 1, ?table_span ]:
	receive

		{ wooper_result, { positive_integer_gaussian_value, Value } }
				when Value > ?table_span ; Value == 0 ->
			draw_gaussian_values( Count, Table, Mu, Sigma, RandomManagerPid );

		{ wooper_result, { positive_integer_gaussian_value, Value } } ->
			NewCount = element( Value, Table ) + 1,
			draw_gaussian_values( Count-1, setelement( Value, Table, NewCount ),
				Mu, Sigma, RandomManagerPid )

	end.




% At index V there is the number of times V has been drawn.
make_table( Size ) ->
	erlang:make_tuple( Size, 0 ).


send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, ProbePid ) ->
	send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, ProbePid,
				_Count=1 ).


send_tables( _FirstTable, _SecondTable, _ThirdTable, _FourthTable, _ProbePid,
			_Count=?table_span+1 ) ->
	ok;

send_tables( FirstTable, SecondTable, ThirdTable, FourthTable,
		ProbePid, Count ) ->

	ProbePid ! { setData, [ Count, { element(Count,FirstTable),
		element(Count,SecondTable),  element(Count,ThirdTable),
		element(Count,FourthTable) } ] },

	send_tables( FirstTable, SecondTable, ThirdTable, FourthTable, ProbePid,
				Count+1 ).



compute_mean( Table ) ->
	List = tuple_to_list( Table ),

	% Multiply the number of draws by the drawn value:
	% (hope the sum is not zero! Starting at index 1)
	compute_mean( List, 1, 0 ) / compute_sum( List, 0 ).


% Counts the number of draws.
compute_sum( [], Count ) ->
	Count;

compute_sum( [ H | T ], Count ) ->
	compute_sum( T, Count+H ).


% Counts the sum of draws.
compute_mean( [], _Index, Acc ) ->
	Acc;

compute_mean( [ H | T ], Index, Acc ) ->
	compute_mean( T, Index + 1, Acc + H * Index ).



test_gaussian_random( RandomManagerPid, Mu, Sigma ) ->

	?test_info( "Requesting gaussian random values (first)." ),

	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),

	?test_info( "Requesting gaussian random values (second)." ),

	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),

	?test_info( "Computing and displaying the full actual "
		"gaussian distribution." ),


	Values = make_table( ?table_span ),

	FirstGaussianTable = draw_gaussian_values( 500, Values, Mu, Sigma,
											  RandomManagerPid ),

	%io:format( "Gaussian table = ~p~n", [FirstGaussianTable] ),

	SecondGaussianTable = draw_gaussian_values( 5000-500, FirstGaussianTable,
			Mu, Sigma, RandomManagerPid ),

	ThirdGaussianTable = draw_gaussian_values( 50000-5000, SecondGaussianTable,
			Mu, Sigma, RandomManagerPid ),

	FourthGaussianTable = draw_gaussian_values( 500000-50000,
			ThirdGaussianTable, Mu, Sigma, RandomManagerPid ),

	Mean = compute_mean( FourthGaussianTable ),

	?test_info_fmt( "Mean of this full actual "
		"gaussian distribution is ~p.", [ Mean ] ),

	MyGaussianProbe = class_Probe:create_facility_probe(

		_Title="Gaussian probe",

		_Curves=[ "After 500 draws","After 5000 draws","After 50000 draws",
				 "After 500000 draws" ],

		_Zones=[],

		io_lib:format( "Test of a gaussian "
			"distribution with mean mu = ~p and variance sigma = ~p.",
			[ Mu, Sigma ]),

		io_lib:format( "Drawn values (mean value is ~w)", [ Mean ] ),

		"Number of times a value has been drawn" ),

	send_tables( FirstGaussianTable, SecondGaussianTable,
		ThirdGaussianTable, FourthGaussianTable, MyGaussianProbe ),

	?test_info( "Requesting the generation of gaussian probe report." ),

	class_Probe:generate_report_for( MyGaussianProbe ),

	MyGaussianProbe ! delete.




% Runs the tests, no prior RandomManager expected to be alive.
%
-spec run() -> no_return().
run() ->

	?test_start,

	class_ResultManager:create_mockup_environment(),

	?test_info( "Creating a new RandomManager." ),
	class_RandomManager:create(),

	RandomManagerPid = basic_utils:wait_for_global_registration_of(
												   ?random_manager_name ),

	Mu = 20,
	Sigma = 10,
	test_gaussian_random( RandomManagerPid, Mu, Sigma ),

	?test_info( "Removing random manager." ),
	RandomManagerPid ! delete,

	?test_stop.
