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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)

% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.

% The objective of this module is to show the significant features of a
% Sim-Diasca actor in multiple scheduling modes.
%
% This means that it has a periodic schedule and that it can be also triggered
% by messages.



% Class modelling a female red squirrel actor: it defines the specific female
% squirrel actor attributes and its spontaneous behaviours.
%
-module(class_FemaleRedSquirrel).


% Determines what are the mother classes
-define( wooper_superclasses, [ class_Squirrel ] ).


-define( wooper_construct_parameters, ActorSettings, SquirrelName,
		GivenAge, ForestPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
	remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
	construct/5 ).


% Declarations of class-specific methods (besides inherited ones).
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, beAlert/3,
		theWinner/3 ).


% Static method declarations (to be directly called from module):
%-define( wooper_static_method_export, ).


-include("ssi_test_types.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization, "SSI-Test.class_FemaleRedSquirrel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new female red squirrel actor:
%
% - number_of_offsprings: the total number of offsprings of this female
%
% - breeding_age: a female is capable to breed at this age
%
% - current_agenda: is a tick list at which the female will breed
%
% - offsprings_per_litter: the number of offsprings per litter
%
% - state can be available, weak (when a newborn is less than 10 weeks),
% gestation, nursing
%
% Reference to class_ForestDweller.erl for other attributes
%
construct( State, ?wooper_construct_parameters ) ->

	% Firstly, the mother class:
	SquirrelState = class_Squirrel:construct( State, ActorSettings,
						SquirrelName, GivenAge, ForestPid ),

	StartingState = setAttributes( SquirrelState, [

		{gender,female},
		{number_of_offsprings,0},
		% A female can breed at 60 weeks of age and once per 60-week cycle:
		{breeding_age,6},
		{breeding_agenda,[]},
		{offsprings_per_litter,3},
		{gestation_period,5},
		{nursing_period,10},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

	] ),

	?send_info( StartingState, "Creating a new female squirrel." ),

	StartingState.



% Methods implementation session


% The spontaneous behaviour of a squirrel instance.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	% A squirrel is terminating when it comes to the natural lifespan
	TerminationOffset = ?getAttr(lifespan),
	WaitTicks = ?getAttr(termination_waiting_ticks),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->

			case ?getAttr(termination_initiated) of

				false ->

					?info( "I am preparing a deferred termination." ),
					executeOneway( State, notifyTermination );

				true when WaitTicks > 0 ->

					NewWaitTick = ?getAttr(termination_waiting_ticks) - 1,

					TermState = setAttribute( State, termination_waiting_ticks,
								  NewWaitTick ),

					executeOneway( TermState, scheduleNextSpontaneousTick );


				_Other ->

					% Following two calls could also have been grouped into an
					% overloading of the default declareTermination/1
					% implementation:
					?info_fmt( "I am terminating at #~B.", [ PastOffset ] ),

					executeOneway( State, declareTermination )

			end;

		_CurrentOffset ->
			trigger_spontaneous_activities( State )

	end,

	?wooper_return_state_only( NewState ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% To be called when an Alert is received from other actor.
% An overridden method.
%
% (actor oneway)
%
-spec beAlert( wooper_state(), alert(), pid() ) ->
					 class_Actor:actor_oneway_return().
beAlert( State, Alert, _SenderPID ) ->

	NewState = case Alert of

		savage ->
			?info_fmt( "A ~s alert is received, I am preparing my deferred "
					  "termination.", [ Alert ] ),
			executeOneway( State, notifyTermination );

		famine ->
			case ?getAttr(state) of

				AState when (AState=:=weak) or (AState=:=gestation) ->

					?info_fmt( "I am in ~s state and a ~s alert is received, "
							   "I am preparing my deferred termination.",
							   [ AState, Alert ] ),

					executeOneway( State, notifyTermination );

				_otherState ->

					?info_fmt( "I received a ~s alert, but I am surviving.",
								[ Alert ] ),
					State
			end;

		reproduction ->
			case ?getAttr(state) of

				AState when (AState=:=available) ->

					class_Actor:send_actor_message( ?getAttr(forest_pid),
							beginCompetition, State ) ;

				_OtherState ->
					?info_fmt( "I received a ~s alert, but I am not available.",
							   [ Alert ] ),
					State

			end;

		_Others ->
			?info_fmt( "I received a ~s alert, but I am surviving.",
					   [ Alert ] ),
			State

	end,

	?wooper_return_state_only( NewState ).



% Called by the forest for informing the winner pid of the competition
% And the actor sends a "youAreWinner" message to the winner
%
% An updated state is returned.
%
% (actor oneway)
%
-spec theWinner( wooper_state(), pid(), pid() ) -> oneway_return().
theWinner( State, WinnerPid, _Sender ) when is_pid(WinnerPid) ->

	?info_fmt( "The winner is ~w.", [ WinnerPid ] ),

	NState = class_Actor:send_actor_message( WinnerPid, youAreWinner, State ),

	?info( "I am in gestation." ),

	NewAvailableTick = ?getAttr(current_tick_offset)
		+ ?getAttr(gestation_period),

	UpdatedState = setAttributes( NState, [
							{state,gestation},
							{available_tick,NewAvailableTick}
				   ] ),

	FinalState = executeOneway( UpdatedState, addSpontaneousTick,
							   NewAvailableTick ),

	?wooper_return_state_only( FinalState );


theWinner( State, _WinnerPid, _Sender )  ->
	?info("I am so sad that no one won the competition." ),
	?wooper_return_state_only( State ).




% Helper functions section.



% Returned breeding tick list of this female squirrel
%
% (actor oneway)
%
get_breeding_tick_list( State ) ->

	BreedingNumber = ?getAttr(lifespan) div ?getAttr(breeding_age),
	get_list( State, _BreedingTickList=[], BreedingNumber ).


get_list( _State, TickList, 0 ) ->
	TickList;

get_list( State, TickList, BreedingNumber ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	BreedingPeriod = ?getAttr(breeding_age),

	BreedingTick = CurrentTickOffset + BreedingNumber*BreedingPeriod,
	NewTickList = TickList ++ [ BreedingTick ],
	get_list( State, NewTickList, BreedingNumber-1 ).



% Updated state returned
%
% (actor oneway)
%
give_birth( State, 0 ) ->

	UpdatedNumberOfChildren = ?getAttr(number_of_offsprings) +
								  ?getAttr(offsprings_per_litter),

	NewAvailableTick = ?getAttr(current_tick_offset) + ?getAttr(nursing_period),

	UpdateState = setAttributes( State, [

						   {number_of_offsprings,UpdatedNumberOfChildren},
						   {state, nursing},
						   {available_tick,NewAvailableTick}

						   ] ),

	executeOneway( UpdateState, addSpontaneousTick, NewAvailableTick );


give_birth( State, NbBirth ) ->

	NbthOfNewChild = ?getAttr(number_of_offsprings) + 1,
	%MotherName = lists:substract("class_", )
	ChildName = ?getAttr(name) ++ "." ++ integer_to_list(NbthOfNewChild),


	ConstructParameters = [ ChildName, _defaultAge=0, ?getAttr(forest_pid) ],

	CreateNewActorState = case get_gender(State) of

		male ->
			% The squirrel actor creates a new male actor at runtime:
			class_Actor:create_actor( class_MaleRedSquirrel,
					ConstructParameters, State );

		female ->
			% The squirrel actor creates a new female actor at runtime:
			class_Actor:create_actor( class_FemaleRedSquirrel,
					ConstructParameters, State )

		end,

	UpdatedState = setAttribute( CreateNewActorState, number_of_offsprings,
					 ?getAttr(number_of_offsprings) + 1 ),

	?info_fmt( "~s has a new child, its name is ~s.",
			  [ ?getAttr(name), ChildName ] ),

	give_birth( UpdatedState, NbBirth-1 ).



% Decides a gender for a new born.
%
% For reproductivity reasons, this function assures that the first newborn
% is male, the second one is female and so on.
%
get_gender( State ) ->

	case ?getAttr(number_of_offsprings) rem 2 of

		0 ->
			male;

		1 ->
			female

	end.



% This helper function groups all spontaneous activities of this actor.
%
% An updated state is returned.
%
trigger_spontaneous_activities( State ) ->

	case ?getAttr(is_registered) of

		false ->
			BreedingList = get_breeding_tick_list( State ),
			SetState = setAttribute( State, breeding_agenda, BreedingList ),

			% Apparently fakes an actor oneway (strange):
			executeOneway( SetState, tryToRegister, [ ?MODULE, self() ] );

		true ->
			CurrentOffset = ?getAttr(current_tick_offset),
			AvailableTick = ?getAttr(available_tick),

			case ?getAttr(state) of

				weak when CurrentOffset >= AvailableTick ->
					?info( "I am proud of becoming adult." ),
					AvailState = setAttribute( State, state, available ),
					executeOneway( AvailState, scheduleNextSpontaneousTick );

				gestation when CurrentOffset >= AvailableTick ->
					?info( "I am so happy, I will have a baby." ),
					give_birth( State, ?getAttr(offsprings_per_litter) );

				nursing when CurrentOffset > AvailableTick ->
					?info( "I am so happy, all my babies are growing up." ),
					AvailState = setAttribute( State, state, available ),
					executeOneway( AvailState, scheduleNextSpontaneousTick );

				_OtherState ->
					State

			end

	end.
