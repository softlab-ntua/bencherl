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


% The purpose of this class is to create a simple periodic actor for testing
% performance tracker features (how to trace memory consumption and process
% number evolution).




% Constructs a new test actor:
%
% - ActorSettings is the AAI assigned to this actor by the load
% balancer
%
% - ActorName the name of the actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
% The Performance Tracker test actor spontaneous behaviors is: at each periodic
% tick (according to the predefined periodic attribute), every existing test
% actor creates a new performance tracker actor. And for a more memory-demanding
% test, the memory_load_loop/1 function can be activated for an increasing
% memory consumption.
%
-module(class_PerformanceTracker_TestActor).


% Determines what are the mother classes
-define( wooper_superclasses, [ class_Actor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName,
		 TerminationTickOffset ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		synchronous_new/3, synchronous_new_link/3,
		synchronous_timed_new/3, synchronous_timed_new_link/3,
		remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		construct/4, delete/1 ).



% Member method declarations:
%
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Test").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Each instance has following attributes:
%
% - periodic: is spontaneous behaviours periodic
% - created_actor_count keeps track of the actors already created



% Constructs a new test actor for the performance tracker.
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_TimeManager:tick_offset() ) ->
					   wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then these class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),

	?send_info( ActorState, "Creating a new performance tracker test actor" ),

	setAttributes( ActorState, [

		{actor_state,active},

		% Increase the period if wanting to slow down actor creations:
		{periodic,8},
		{created_actor_count,0},
		{termination_tick_offset,TerminationTickOffset},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

								] ).



% Do-nothing destructor defined to work around a Dialyzer limitation:
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	State.




% Methods section.


% Management section of the actor.

% The core of the performance tracker test actor behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),
	CurrentTick = ?getAttr(current_tick_offset),
	CreationTickOffset = ?getAttr(actor_creation_tick_offset),

	UpdatedState = case CurrentTick of

		PastOffset when PastOffset >= CreationTickOffset + TerminationOffset ->

				case ?getAttr(actor_state) of

					active ->
						ActiveState = setAttribute( State, actor_state, idle ),

						executeOneway( ActiveState,
									  scheduleNextSpontaneousTick );
						%AState = setAttribute( State, actor_state, idle ),
						%executeOneway( AState, declareTermination );

					idle ->
						executeOneway( State, declareTermination )

				end;

		_CurrentOffset ->

			UpdatedCreationCounter = ?getAttr(created_actor_count) + 1,

			% Note: as actors will create other actors, many actors are bound to
			% have the same name:
			%
			CreatedActorName = io_lib:format(
						"My Performance Tracker test actor #~B",
						[ UpdatedCreationCounter ] ),

			NewState = class_Actor:create_actor(
					_CreatedClassname=class_PerformanceTracker_TestActor,
					[ CreatedActorName, ?getAttr(termination_tick_offset) ],
					State ),

			CreatedState = setAttribute( NewState, created_actor_count,
										UpdatedCreationCounter ),

			executeOneway( CreatedState, addSpontaneousTick,
						  CurrentTick + ?getAttr(periodic) )

	end,

	?wooper_return_state_only( UpdatedState ).




% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).
