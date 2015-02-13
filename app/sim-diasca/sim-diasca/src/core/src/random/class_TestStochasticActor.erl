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


% Test of Stochastic Actor class, regarding random management.
%
% Note: to be used with randomManagerAndStochasticActorPair_test.erl.
%
-module(class_TestStochasticActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_StochasticActor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName,
		 ListOfRandomLaws, TerminationProbability ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"StochasticActor.Test").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new test actor.
%
% TerminationProbability is a probability (here in [0,0.6], which gives the
% probability that this actor terminates, when deciding what to do next.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), [ class_RandomManager:random_law() ],
				math_utils:probability() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	StochasticState = class_StochasticActor:construct( State, ActorSettings,
											   ActorName, ListOfRandomLaws ),

	?send_info( StochasticState, "Creating a new test stochastic actor." ),

	% Probabilities of all possible behaviour:
	Behaviours = [ { terminate, round( 100 * TerminationProbability ) },
				   { be_passive, 10 }, { talk, 30 } ],

	% Prepare different lists of random values:
	setAttributes( StochasticState, [

		{ behaviour_table, Behaviours },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

	] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting test stochastic actor." ),

	% erlang:unlink() not used, as done manager-side.
	?debug( "Test stochastic actor deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% The core of the test stochastic actor behaviour.
%
% (oneway)
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?info( "Test stochastic actor acting." ),

	% Let's draw for fun some random values:
	FirstRandomValue = class_StochasticActor:get_random_value_from(
		my_first_uniform, State ),

	SecondRandomValue = class_StochasticActor:get_random_value_from(
		my_first_uniform, State ),

	ThirdRandomValue = class_StochasticActor:get_random_value_from(
		my_second_uniform, State ),

	FourthRandomValue = class_StochasticActor:get_random_value_from(
		my_second_uniform, State ),

	FifthRandomValue = class_StochasticActor:get_random_value_from(
		my_gaussian, State ),

	SixthRandomValue = class_StochasticActor:get_random_value_from(
		my_exponential, State ),

	?info_fmt( "Test Actor drew ~p from first uniform list, ~p from second, "
			   "~p from gaussian one and ~p from exponential one.",
			   [ { FirstRandomValue,SecondRandomValue },
				 { ThirdRandomValue,FourthRandomValue },
				 FifthRandomValue, SixthRandomValue ] ),

	% Now let's decide what we do next:
	NextState = case list_utils:draw_element_weighted(
					   ?getAttr(behaviour_table) ) of


		terminate ->
			?info( "Test Stochastic Actor preparing termination." ),
			executeOneway( State, declareTermination );


		be_passive ->
			?info( "Test Stochastic Actor will be passive next." ),
			State;


		talk ->
			NextTickOffset = ?getAttr(current_tick_offset)
				+ class_StochasticActor:get_random_value_from( my_first_uniform,
															  State ) + 1,

			?info_fmt( "Test Stochastic Actor talked and "
					   "now requests to be spontaneously scheduled "
					   "at tick offset #~B.", [ NextTickOffset ] ),

			executeOneway( State, addSpontaneousTick, NextTickOffset )

	end,

	?wooper_return_state_only( NextState ).



-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Just schedule the first next tick:
	NewState = executeOneway( State, addSpontaneousTick,
							 ?getAttr(current_tick_offset) + 1 ),

	?wooper_return_state_only( NewState ).
