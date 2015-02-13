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




% Management of random number generation.
%
% Now all actors, notably stochastic ones, have their own (private) random
% generator, while still able to preserve reproducibility.
%
% As a consequence, the random manager module is now mostly useful for the
% (static) functions for stochastic laws it exports.

% Inspired from http://www.trapexit.org/Random_Numbers_Biased
%
% Note: it is possible to request only a seed from a RandomManager, and to
% generate afterwards one's own random series. That is what the class_Actor
% instances do, indirectly.
%
-module(class_RandomManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, SeedInformations, IsPrivate ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/2, new_link/2, synchronous_new/2,
		 synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
		 remote_synchronisable_new_link/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).



% Method declarations.
%
% For each get* method, there is get*/n, the direct request, and get*/n+1, the
% counterpart actor oneway, and get*/n+2, the same oneway apart that it returns
% a series of values (a list) instead of a unique one.
%
-define( wooper_method_export,
		 getUniformValue/2, getUniformValue/3,
		 getExponentialValue/2, getPositiveIntegerExponentialValue/2,
		 getGaussianValue/3, getPositiveIntegerGaussianValue/3 ).



% Static method declarations (to be directly called from module):
-define( wooper_static_method_export, create/0, getManager/0, get_new_seed/0,
		 remove/0 ).



% Static method declarations, for standalone random generation:
-export([
		 get_uniform_value/1, get_uniform_value/2,
		 get_uniform_values/2, get_uniform_values/3,
		 get_uniform_floating_point_value/1,
		 get_exponential_value/1, get_exponential_values/2,
		 get_positive_integer_exponential_value/1,
		 get_positive_integer_exponential_values/2,
		 get_gaussian_value/2, get_gaussian_values/3,
		 get_positive_integer_gaussian_value/2,
		 get_positive_integer_gaussian_values/3
]).



% Section for the description of random laws.



-type uniform_law() :: { 'uniform', pos_integer() }.



% Parameter is Lambda:
-type exponential_law() :: { 'exponential', number() }.

-type positive_integer_exponential_law() ::
						{ 'positive_integer_exponential', number() }.


% Parameters are respectively Mu and Sigma:
-type gaussian_law() :: { 'gaussian', number(), float() }.

-type positive_integer_gaussian_law() :: { 'positive_integer_gaussian',
										   float(), float() }.


-type random_law() ::  uniform_law()
					 | exponential_law()
					 | positive_integer_exponential_law()
					 | gaussian_law()
					 | positive_integer_gaussian_law().


-export_type([ random_law/0 ]).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"RandomManagement").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For random constants:
-include("class_RandomManager.hrl").



% Implementation notes:
%
% There are three basic classes of built-in random distributions:
%
% - uniform (a.k.a. white noise)
%
% - exponential
%
% - Gaussian (a.k.a. normal)
%
%
% For each distribution law, following variations are available:
%
% - a simple non-synchronized method (ex: getUniformValue/2), returning one
% value to the caller PID (not necessarily an actor)
%
% - a simple synchronized method (ex: getUniformValue/3), returning one value to
% the caller actor
%
% - a more complex synchronized method (ex: getUniformValues/3) [note the final
% 's'), returning a series of values and an identifier to the caller actor so
% that the caller is able to track multiple distributions simulatenously
%
% The last two cases use actor oneways, they send their answer through a oneway
% actor call to the caller actor, calling corresponding set*Value/ set*Values
% actor method (ex: setUniformValue).
%
% The uniform law can be based either on the random module (random:uniform/1) or
% on the crypto module (crypto:rand_uniform/2). The two forms yield different
% but quite similar results. See use_crypto_module below.
%
% We rely on random_utils:get_random_value which allows to swap implementations.
%
% Exponential and Gaussian laws generate by default floating-point numbers.
%
% For convenience, counterparts returning positive integer values have been
% defined (ex: getGaussianValue/getPositiveIntegerGaussianValue).



% A Gaussian (a.k.a. normal, bell curve) law is fully determined when two
% parameters are given:
%
% - its mean (Mu), the average value of the samples
%
% - its variance (Sigma), whose square root is the standard deviation S
%
% About 68% of the samples are in [Mu-S;Mu+S].
% About 95.4% of the samples (i.e. almost all) are in [Mu-2S;Mu+2S].
%
% See also: http://en.wikipedia.org/wiki/Standard_deviation



% Where a random manager should be registered.
% Could be local_only or global_only as well:
-define( registration_type, local_and_global ).


-type seed_info() :: random_utils:random_state() | 'default_seed'
					| 'time_based_seed'.



% Constructs a new random manager.
%
% Signature: construct( State, SeedInformations, IsPrivate ), with:
%
% - SeedInformations allows to choose the random seed to be used, it can be:
%
%  - a triplet {A,B,C}, to set explicitly the seed, to be tailored for
%  reproducibility or for ergodic mode
%
%  - default_seed, to use default (fixed) values in the process dictionary
%
%  - time_based_seed, as they are based on current time, each run will result in
%  different random series
%
% - IsPrivate tells whether this random manager will be privately held (hence
% should not be registered in naming service) or if it is a (registered)
% singleton (can be the atom true of false)
%
-spec construct( wooper:state(), seed_info(), boolean() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, "RandomManager" ),

	class_InstanceTracker:register_agent( State ),

	% Then the class-specific actions:
	StartingState = setAttributes( TraceState, [

		{ is_private, IsPrivate },
		{ trace_categorization, ?TraceEmitterCategorization }

												] ),

	case IsPrivate of

		true ->
			?send_trace( StartingState,
				"Creating a private random manager." );

		false ->
			?send_trace( StartingState,
				"Creating a public random manager." ),

			try

				basic_utils:register_as( ?random_manager_name,
					?registration_type )

			catch

				Exception ->
					?send_error( StartingState,
						"Random manager could not be registered." ),
					throw( { random_manager_could_not_register, Exception } )

			end,

			?send_debug_fmt( StartingState, "Random manager registered as ~w.",
							 [ ?registration_type ] )

	end,

	?send_trace_fmt( StartingState,
		"Random manager will use following seed informations: ~w.",
		[ SeedInformations ] ),

	random_utils:start_random_source( SeedInformations ),

	?send_debug( StartingState, "Random manager created." ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting random manager." ),

	% Private random managers not owned.
	random_utils:stop_random_source(),
	case ?getAttr(is_private) of

		true ->
			ok;

		false ->
			basic_utils:unregister( ?random_manager_name, ?registration_type )

	end,

	class_InstanceTracker:unregister_agent(),

	?debug( "Random manager deleted." ),

	% Then allow chaining:
	State.




% Methods section.



% For each random distribution, there is at least:
%
% - a request-based method
%
% - a static method, which thus relies on the state of random generation of the
% caller process.



% All these functions are doubled, to support the request of one random value or
% a given number of values.
%
% Finally, distributions that may return floating-point values (ex: exponential,
% Gaussian) have also versions that return positive integer values.





% Uniform section.




% Returns an integer random value generated from an uniform distribution, in
% specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
% (request)
%
-spec getUniformValue( wooper:state(), integer(), integer() ) ->
					   request_return( { 'uniform_value', integer() } ).
getUniformValue( State, Nmin, Nmax ) ->

	Value = random_utils:get_random_value( Nmin, Nmax ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	?wooper_return_state_result( State, { uniform_value, Value } ).



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% (request)
%
-spec getUniformValue( wooper:state(), pos_integer() ) ->
					   request_return( { 'uniform_value', pos_integer() } ).
getUniformValue( State, N ) ->

	Value = random_utils:get_random_value( N ),

	%?debug_fmt( "Returning uniform value ~w.", [ Value ] ),

	?wooper_return_state_result( State, { uniform_value, Value } ).



% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
%
% (static method)
%
-spec get_uniform_value( pos_integer() ) -> pos_integer().
get_uniform_value( N ) ->
	random_utils:get_random_value( N ).



% Returns an integer random value generated from an uniform distribution, in
% specified range.
%
% Given two integers Nmin and Nmax, returns a random integer uniformly
% distributed between these two bounds (both included), updating the random
% state in the process dictionary.
%
% (static method)
%
-spec get_uniform_value( integer(), integer() ) -> integer().
get_uniform_value( Nmin, Nmax ) ->
	random_utils:get_random_value( Nmin, Nmax ).





% Returns a list of Count integer uniform values in [ 1, N ] (both included).
%
% Given an integer N >= 1, returns random integers uniformly distributed between
% 1 and N, updating the random state in the process dictionary.
%
% (static method)
%
-spec get_uniform_values( pos_integer(), basic_utils:count() ) ->
								[ pos_integer() ].
get_uniform_values( N, Count ) ->
	random_utils:get_random_values( N, Count ).



% Returns a list of Count integer uniform values in [Nmin,Nmax] (both included),
% updating the random state in the process dictionary.
%
% (static method)
%
-spec get_uniform_values( integer(), integer(), basic_utils:count() ) ->
								[ integer() ].
get_uniform_values( Nmin, Nmax, Count ) ->
	random_utils:get_random_values( Nmin, Nmax, Count ).



% Returns a floating-point integer random value generated from an uniform
% distribution.
%
% Given an integer N >= 1, returns a random floating-point value uniformly
% distributed between 1 and N (both included), updating the random state in the
% process dictionary.
%
% (static method)
%
-spec get_uniform_floating_point_value( pos_integer() ) -> float().
get_uniform_floating_point_value( N ) ->

	% Currently not knowing how to generate it uniformely, always corresponds to
	% an integer here:
	float( get_uniform_value( N ) ).




% Exponential section.
%
% Note: each of the three forms comes in two versions, with floating-point or
% (positive) integer values being returned.




% Returns an exponential floating-point random value with Lambda being the rate
% parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (request)
%
-spec getExponentialValue( wooper:state(), number() ) ->
			  request_return( { 'exponential_value', float() } ).
getExponentialValue( State, Lambda ) ->

	Value = get_exponential_value( Lambda ),

	%?debug_fmt( "Returning exponential value ~w.", [ Value ] ),
	?wooper_return_state_result( State, { exponential_value, Value } ).



% Returns an exponential floating-point random value with Lambda being the rate
% parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (static)
%
-spec get_exponential_value( number() ) -> float().
get_exponential_value( Lambda ) ->

	% Note: with Erlang, math:log(x) is ln(x):
	- math:log( random_utils:get_random_value() ) / Lambda.



% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (request)
%
-spec getPositiveIntegerExponentialValue( wooper:state(), number() ) ->
		  request_return( { 'positive_integer_exponential_value', integer() } ).
getPositiveIntegerExponentialValue( State, Lambda ) ->

	Value = round( get_exponential_value( Lambda ) ),

	%?debug_fmt( "Returning positive integer exponential value ~w.",
	%	[ Value ] ),

	?wooper_return_state_result( State,
		{ positive_integer_exponential_value, Value } ).



% Returns an exponential (positive) integer random value with Lambda being the
% rate parameter.
%
% The probability density function is p(x) = Lambda.exp(-Lambda.x), whose
% integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (static)
%
-spec get_positive_integer_exponential_value( number() ) -> integer().
get_positive_integer_exponential_value( Lambda ) ->
	round( get_exponential_value( Lambda ) ).



% Returns a list of Count exponential values according to the specified Lambda
% setting.
%
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (static)
%
-spec get_exponential_values( number(), basic_utils:count() ) -> [ float() ].
get_exponential_values( Lambda, Count ) ->
	generate_exponential_list( Lambda, Count ).



% Returns a list of Count (positive) integer exponential values according to the
% specified Lambda setting.
%
% Lambda is the rate parameter: the probability density function is
% p(x) = Lambda.exp(-Lambda.x), whose integral is 1.
%
% Mean value of drawn samples is 1/Lambda.
%
% See http://en.wikipedia.org/wiki/Exponential_distribution
%
% Using inverse transform sampling.
%
% (static)
%
-spec get_positive_integer_exponential_values( number(),
								 basic_utils:count() ) -> [ pos_integer() ].
get_positive_integer_exponential_values( Lambda, Count ) ->
	generate_positive_integer_exponential_list( Lambda, Count ).







% Gaussian section.
%
% Note: each of the three forms comes in two version, with floating-point or
% positive integer values being returned.
%
% Note also that the function order matters, and some share some arities.




% Reordering was needed, as there are two getGaussianValue/3 (one request/one
% actor oneway).



% Returns a random value generated from the normal (Gaussian) distribution with
% specified settings.
%
% Given a mean Mu and a variance Sigma, returns a random floating-point value
% drawn according to the corresponding Gaussian law, updating the state in the
% process dictionary.
%
% (request)
%
-spec getGaussianValue( wooper:state(), number(), number() ) ->
							  request_return( { gaussian_value, float() } ).
getGaussianValue( State, Mu, Sigma ) ->

	Value = sigma_loop( Mu, Sigma ),

	%?debug_fmt( "Returning Gaussian value ~w.", [ Value ] ),

	?wooper_return_state_result( State, { gaussian_value, Value } ).



% Returns a random value generated from the normal (Gaussian) distribution with
% specified settings.
%
% Given a mean Mu and a variance Sigma, returns a random floating-point value
% drawn according to the corresponding Gaussian law, updating the state in the
% process dictionary.
%
% (static)
%
-spec get_gaussian_value( number(), number() ) -> float().
get_gaussian_value( Mu, Sigma ) ->
	sigma_loop( Mu, Sigma ).





% Returns a positive integer random value generated from the normal (Gaussian)
% distribution with specified settings.
%
% Given a mean Mu and a variance Sigma, returns random integers drawn according
% the corresponding Gaussian law, updating the state in the process dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
% (request)
%
-spec getPositiveIntegerGaussianValue( wooper:state(), number(), number() ) ->
		request_return( { positive_integer_gaussian_value, pos_integer() } ).
getPositiveIntegerGaussianValue( State, Mu, Sigma ) ->

	Value = sigma_loop_positive_integer( Mu, Sigma ),

	%?debug_fmt( "Returning positive integer Gaussian value ~w.", [ Value ] ),

	?wooper_return_state_result( State,
		{ positive_integer_gaussian_value, Value } ).



% Returns a positive integer random value generated from the normal (Gaussian)
% distribution with specified settings.
%
% Given a mean Mu and a variance Sigma, returns random integers drawn according
% the corresponding Gaussian law, updating the state in the process dictionary.
%
% The result is a non-negative integer (not a float). Values will be drawn until
% they are non-negative.
%
% (static)
%
-spec get_positive_integer_gaussian_value( number(), number() ) ->
												 pos_integer().
get_positive_integer_gaussian_value( Mu, Sigma ) ->
	sigma_loop_positive_integer( Mu, Sigma ).





% Returns a list of Count Gaussian values.
%
% Given a mean Mu and a variance Sigma, returns random floating-point values
% drawn according the corresponding Gaussian law, updating the state in the
% process dictionary.
%
% (static)
%
-spec get_gaussian_values( number(), number(), basic_utils:count() ) ->
								 [ float() ].
get_gaussian_values( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count ).





% Returns a list of Count positive integer Gaussian values.
%
% Given a mean Mu and a variance Sigma, returns random integers drawn according
% the corresponding Gaussian law, updating the state in the process dictionary.
%
% (static)
%
-spec get_positive_integer_gaussian_values( number(), number(),
				basic_utils:count() ) -> [ pos_integer() ].
get_positive_integer_gaussian_values( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count ).



% 'Static' methods (module functions):


% Creates the random manager asynchronously, with default settings (mean of
% zero, sigma of 1).
%
% (static)
%
-spec create() -> pid().
create() ->

	% Not created here as an actor:
	new_link( _SeedInformations=default_seed, _IsPrivate=false ).



% Returns the Pid of the current random manager if it exists, otherwise
% random_manager_not_found.
%
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
%
% (static)
%
-spec getManager() -> pid().
getManager() ->

	% Waits gracefully for the random manager to exist:
	basic_utils:wait_for_global_registration_of( ?random_manager_name ).



% Deletes (asynchronously) any global random manager.
%
% (static)
%
-spec remove() -> 'ok' | 'random_manager_not_found'.
remove() ->

	case global:whereis_name( ?random_manager_name ) of

		undefined ->
			random_manager_not_found;

		RandomManagerPid ->
			RandomManagerPid ! delete,
			% It will unregister itself.
			ok

	end.




% Section for helper functions (not methods).


% generate_*_list could use higher-order functions.



% Generates a list of Count exponential random values.
%
-spec generate_exponential_list( number(), basic_utils:count() ) -> [ float() ].
generate_exponential_list( Lambda, Count ) ->
	generate_exponential_list( Lambda, Count, [] ).


generate_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_exponential_list( Lambda, Count, Acc ) ->
	generate_exponential_list( Lambda, Count-1,
		[ get_exponential_value( Lambda )  | Acc ] ).



% Generates a list of Count positive integer exponential random values.
%
-spec generate_positive_integer_exponential_list( number(),
						   basic_utils:count() ) -> [ pos_integer() ].
generate_positive_integer_exponential_list( Lambda, Count ) ->
	generate_positive_integer_exponential_list( Lambda, Count, [] ).


generate_positive_integer_exponential_list( _Lambda, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_exponential_list( Lambda, Count, Acc ) ->
	generate_positive_integer_exponential_list( Lambda, Count-1,
		[ erlang:round( get_exponential_value( Lambda ) ) | Acc ] ).





% Generates a list of Count Gaussian random values.
%
-spec generate_gaussian_list( number(), number(), basic_utils:count() ) ->
									[ float() ].
generate_gaussian_list( Mu, Sigma, Count ) ->
	generate_gaussian_list( Mu, Sigma, Count, [] ).


generate_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_gaussian_list( Mu, Sigma, Count-1,
		[ sigma_loop( Mu, Sigma )  | Acc ] ).



% Generates a list of Count positive integer Gaussian random values.
%
-spec generate_positive_integer_gaussian_list( number(), number(),
						  basic_utils:count() ) -> [ pos_integer() ].
generate_positive_integer_gaussian_list( Mu, Sigma, Count ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count, [] ).


generate_positive_integer_gaussian_list( _Mu, _Sigma, _Count=0, Acc ) ->
	Acc;

generate_positive_integer_gaussian_list( Mu, Sigma, Count, Acc ) ->
	generate_positive_integer_gaussian_list( Mu, Sigma, Count-1,
		[ erlang:round( sigma_loop( Mu, Sigma ) ) | Acc ] ).





% Generates a new normal value and updates the state.
%
% Returns the computed value.
%
-spec sigma_loop( number(), number() ) -> float().
sigma_loop( Mu, Sigma ) ->

	V1 = 2.0 * random_utils:get_random_value() - 1.0,
	V2 = 2.0 * random_utils:get_random_value() - 1.0,
	S  = (V1 * V1) + (V2 * V2),

	% Loop until S < 1.0:
	if

		S >= 1.0 ->
			sigma_loop( Mu, Sigma );

		true ->
			% Here S < 1.0:
			Scale = math:sqrt( ( -2.0 * math:log( S ) ) / S ),
			%io:format( "sigma_loop returning a value.~n" ),
			Mu + ( Sigma * Scale * V1)

	end.



% Generates a new integer non-negative normal value and updates the state.
%
% Returns the computed value.
%
-spec sigma_loop_positive_integer( number(), number() ) -> pos_integer().
sigma_loop_positive_integer( Mu, Sigma ) ->

	% Loops until a positive integer is found:
	case round( sigma_loop( Mu, Sigma ) ) of

		TriedValue when TriedValue < 0 ->
			sigma_loop_positive_integer( Mu, Sigma );

		NonNegativeValue ->
			NonNegativeValue

	end.



% Returns a new seed triplet.
%
-spec get_new_seed() -> random_utils:seed().
get_new_seed() ->
	random_utils:get_random_seed().
