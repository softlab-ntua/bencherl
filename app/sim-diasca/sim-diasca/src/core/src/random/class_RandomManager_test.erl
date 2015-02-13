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



% Overall unit tests for the RandomManager class implementation.
%
% See the class_RandomManager.erl module.
%
% See also the various tests for each supported distribution
% (class_RandomManager_*_test.erl).
%
-module(class_RandomManager_test).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For random_manager_name:
-include("class_RandomManager.hrl").


-define(table_span,70).



% No need to use test_receive/0 below, as returned values are tagged.


show_uniform( RandomManagerPid, UpperBound ) ->

	?test_debug_fmt(
		"Requesting a uniform random value in [1,~p].", [ UpperBound ] ),

	RandomManagerPid ! { getUniformValue, UpperBound, self() },

	receive

		{ wooper_result, { uniform_value, Value } } ->
			?test_debug_fmt( "Received uniform random value: ~p.", [ Value ] )

	end.



draw_uniform_values( 0, Table, _MaxValue, _RandomManagerPid ) ->
	Table;

draw_uniform_values( Count, Table, MaxValue, RandomManagerPid ) ->
	RandomManagerPid ! { getUniformValue, MaxValue, self() },

	% Wanting a random value in ]1,?table_span]:
	receive

		{ wooper_result, { uniform_value, Value } } ->
			NewCount = element( Value, Table ) + 1,
			draw_uniform_values( Count-1,
				setelement( Value, Table, NewCount ),
				MaxValue, RandomManagerPid )

	end.




show_exponential( RandomManagerPid, Lambda ) ->

	?test_debug_fmt( "Requesting an exponential random value with lambda = ~p.",
					 [ Lambda ] ),

	RandomManagerPid ! { getExponentialValue, Lambda, self() },

	receive

		{ wooper_result, { exponential_value, Value } } ->
			?test_debug_fmt( "Received exponential random value: ~p.",
							 [ Value ] )

	end.



draw_exponential_values( 0, Table, _Lambda, _RandomManagerPid ) ->
	Table;

draw_exponential_values( Count, Table, Lambda, RandomManagerPid ) ->

	RandomManagerPid ! { getPositiveIntegerExponentialValue, Lambda, self() },

	% Wanting a random value in ]1,?table_span]:
	receive

		{ wooper_result, { positive_integer_exponential_value, Value } }
				when Value > ?table_span ; Value =< 0 ->
			draw_exponential_values( Count, Table, Lambda, RandomManagerPid );

		{ wooper_result, { positive_integer_exponential_value, Value } } ->
			NewCount = element( Value, Table ) + 1,
			draw_exponential_values( Count-1,
				setelement( Value, Table, NewCount ), Lambda, RandomManagerPid )

	end.




show_gaussian( RandomManagerPid ) ->

	Mu = 5,
	Sigma = 2 ,

	?test_debug_fmt( "Requesting a Gaussian random value "
		"with following settings: mean = ~p, deviation = ~p.",
		[ Mu, Sigma ] ),

	RandomManagerPid ! { getGaussianValue, [ Mu, Sigma ], self() },

	receive

		{ wooper_result, { gaussian_value, Value } } ->
			?test_debug_fmt( "Received a Gaussian random value: ~p.",
							 [ Value ] )

	end.



show_gaussian( RandomManagerPid, Mu, Sigma ) ->

	?test_debug_fmt( "Requesting a Gaussian random value with "
					"mean = ~p, deviation = ~p.", [ Mu, Sigma ] ),

	RandomManagerPid ! { getGaussianValue, [ Mu, Sigma ], self() },

	receive

		{ wooper_result, { gaussian_value, Value } } ->
			?test_debug_fmt( "Received a Gaussian random value: ~p.",
							 [ Value ] )

	end.



draw_gaussian_values( 0, Table, _Mu, _Sigma, _RandomManagerPid ) ->
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
%
make_table( Size ) ->
	erlang:make_tuple( Size, 0 ).


compute_mean( Table ) ->
	List = tuple_to_list( Table ),
	% Multiply the number of draws by the drawn value:
	% (hope the sum is not zero! Starting at index 1)
	compute_mean( List, 1, 0 ) / compute_sum( List, 0 ).


% Counts the number of draws.
compute_sum( [], Count ) ->
	Count;

compute_sum( [ H | T ], Count ) ->
	compute_sum( T, Count + H ).


% Counts the sum of draws.
compute_mean( [], _Index, Acc ) ->
	Acc;

compute_mean( [ H | T ], Index, Acc ) ->
	compute_mean( T, Index + 1, Acc + H * Index ).



test_uniform_random( RandomManagerPid, MaxValue ) ->

	?test_info( "Requesting uniform random values." ),

	show_uniform( RandomManagerPid, MaxValue ),
	show_uniform( RandomManagerPid, MaxValue ),
	show_uniform( RandomManagerPid, MaxValue ),
	show_uniform( RandomManagerPid, MaxValue ),
	show_uniform( RandomManagerPid, MaxValue ),

	?test_info( "Computing and displaying the full actual uniform distribution."
			   ),

	Values = make_table( ?table_span ),

	FirstUniformTable = draw_uniform_values( 50, Values, MaxValue,
		RandomManagerPid ),

	SecondUniformTable = draw_uniform_values( 500-50, FirstUniformTable,
			MaxValue, RandomManagerPid ),

	ThirdUniformTable = draw_uniform_values( 5000-500, SecondUniformTable,
											MaxValue, RandomManagerPid ),

	FourthUniformTable = draw_uniform_values( 50000-5000, ThirdUniformTable,
											 MaxValue, RandomManagerPid ),

	Mean = compute_mean( FourthUniformTable ),

	?test_info_fmt( "Mean of this full actual uniform distribution is ~f.",
				   [ Mean ] ).


test_exponential_random( RandomManagerPid, Lambda ) ->

	?test_info( "Requesting exponential random values." ),

	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),
	show_exponential( RandomManagerPid, Lambda ),

	?test_info( "Computing and displaying the full actual "
				"exponential distribution." ),

	Values = make_table( ?table_span ),

	FirstExponentialTable = draw_exponential_values( 50, Values, Lambda,
		RandomManagerPid ),

	SecondExponentialTable = draw_exponential_values( 500-50,
			FirstExponentialTable, Lambda, RandomManagerPid ),

	ThirdExponentialTable = draw_exponential_values( 5000-500,
			SecondExponentialTable, Lambda, RandomManagerPid ),

	FourthExponentialTable = draw_exponential_values( 50000-5000,
					   ThirdExponentialTable, Lambda, RandomManagerPid ),

	Mean = compute_mean( FourthExponentialTable ),

	?test_info_fmt( "Mean of this full actual exponential distribution is ~f.",
				   [ Mean ] ).


test_gaussian_random( RandomManagerPid, Mu, Sigma ) ->

	?test_info( "Requesting Gaussian random values (first)." ),

	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),
	show_gaussian( RandomManagerPid ),

	?test_info( "Requesting Gaussian random values (second)." ),

	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),
	show_gaussian( RandomManagerPid, Mu, Sigma ),

	?test_info( "Computing and displaying the full actual "
			   "Gaussian distribution." ),

	Values = make_table( ?table_span ),

	FirstGaussianTable = draw_gaussian_values( 50, Values, Mu, Sigma,
											  RandomManagerPid ),

	%io:format( "Gaussian table = ~w~n", [ FirstGaussianTable ] ),

	SecondGaussianTable = draw_gaussian_values( 500-50, FirstGaussianTable,
			Mu, Sigma, RandomManagerPid ),

	ThirdGaussianTable = draw_gaussian_values( 5000-500,SecondGaussianTable,
			Mu,Sigma,RandomManagerPid ),

	FourthGaussianTable = draw_gaussian_values( 50000-5000, ThirdGaussianTable,
			Mu, Sigma, RandomManagerPid ),

	Mean = compute_mean( FourthGaussianTable ),

	?test_info_fmt( "Mean of this full actual Gaussian distribution is ~f.",
					[ Mean ] ).



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

	Max = ?table_span,
	test_uniform_random( RandomManagerPid, Max ),

	Lambda = 0.1,
	test_exponential_random( RandomManagerPid, Lambda ),

	Mu = 20,
	Sigma = 10,
	test_gaussian_random( RandomManagerPid, Mu, Sigma ),


	?test_info( "Removing random manager." ),
	wooper:delete_synchronously_instance( RandomManagerPid ),

	?test_stop.
