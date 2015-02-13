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



% Test class for spatialised tests.
%
-module(class_TestSpatialisedActor).


% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_SpatialisedActor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Name, InitialPosition,
		 PerceptionRadius, PerceptionPeriod, MaxSpeed, TerminationTickOffset,
		 EnvironmentPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 notifyEntitiesNearby/3 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Spatial.TestSpatialisedActor").



% Allows to use macros for spatial-aware trace sending (to be included after the
% WOOPER header):
%
-include("class_SpatialTraceEmitter.hrl").





% Attributes of an instance of a test spatialised actor are:
%
% - speed :: unit_utils:meters_per_tick() is the speed of this test actor (at
% least one upper-bound thereof)
%
% - perception_radius :: linear:radius() is the perception radius of this actor
%
% - perception_period :: class_TimeManager:tick_offset() is the period at which
% this actor will trigger a perception request
%
% - move_period :: class_TimeManager:tick_offset() is the period at which this
% actor will move (i.e. update its position)
%
% - termination_offset :: class_TimeManager:tick_offset() | 'none' is the tick
% offset at which this actor will terminate



% Creates a new test spatialised actor, in a 2D environment.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this actor
%
% - InitialPosition is the initial position of this actor in the specified
% environment
%
% - PerceptionRadius is the perception radius of this actor (in meters)
%
% - MaxSpeed is an upper-bound (if any) of the maximum speed of this actor
% (allows for better environment-level performances)
%
% - TerminationTickOffset is the tick offset at which this test actor is to
% terminate (or 'none')
%
% - EnvironmentPid is the PID of the environment this actor will live in
%

-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_SpatialisedActor:position(),
				 linear:radius(), class_TimeManager:tick_offset(),
				 class_TwoDimensionalEnvironment:max_speed(),
				 class_TimeManager:tick_offset(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	SpatialState = class_SpatialisedActor:construct( State, ActorSettings, Name,
						InitialPosition, MaxSpeed, EnvironmentPid ),

	setAttributes( SpatialState, [

			% We consider that the speed of this actor is constantly its maximum
			% one.
			%
			% Temporarily in meters per second:
			%
			{ speed, MaxSpeed },

			{ perception_radius, PerceptionRadius },

			{ perception_period, PerceptionPeriod },

			{ move_period, 5 },

			{ termination_offset, TerminationTickOffset },

			{ trace_categorization,
			  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								 ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Section for actor oneways.



% First scheduling on this test actor.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) ->
						   class_Actor:actor_oneway_return().
onFirstDiasca( State, SendingActorPid ) ->

	MetersPerSecond = ?getAttr(speed),

	TickDuration = ?getAttr(simulation_tick_duration),

	% In meters per tick:
	VirtualSpeed = case MetersPerSecond of

					   undefined ->
						   undefined;

					   _ ->
						   MetersPerSecond * TickDuration

	end,

	?info_fmt( "Overall speed of ~p meters per second, "
			   "converted to ~p meters per tick "
			   "(duration of a tick: ~p virtual seconds).",
			   [ MetersPerSecond, VirtualSpeed, TickDuration ] ),

	% First, local actions; converting to meters per tick:
	LocalState = setAttribute( State, speed, VirtualSpeed ),

	% Then calling the parent one, to declare ourself to the environment:
	ParentState = executeOnewayWith( LocalState, class_SpatialisedActor,
									 onFirstDiasca, [ SendingActorPid ] ),

	PlanState = class_Actor:scheduleNextSpontaneousTick( ParentState ),

	?wooper_return_state_only( PlanState ).



% The definition of the spontaneous behaviour of this test actor.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	TerminationTickOffset = ?getAttr(termination_offset),

	case ( TerminationTickOffset =/= none ) andalso
		( CurrentTickOffset >= TerminationTickOffset ) of

		true ->

			io:format( "~w decided to terminate (at #~p, diasca O).~n",
					   [ self(), CurrentTickOffset ] ),

			UndeclaredState = class_Actor:send_actor_message(
					?getAttr(environment_pid), undeclareEntity, State ),

			executeOneway( UndeclaredState, declareTermination );

		false ->
			act_normally( CurrentTickOffset, State )

	end.



% Helper.
%
act_normally( CurrentTickOffset, State ) ->

	% This actor moves from left to right (increasing abscissa):

	Position = ?getAttr(position),

	MovePeriod = ?getAttr(move_period),

	% Speed in meters per tick:
	NewPosition = case ?getAttr(speed) of

				% Static:
				undefined ->
						Position;

				S ->
						XOffset = S * MovePeriod,
						linear_2D:translate( Position, { XOffset, _YOffset=0 } )

	end,

	?debug_fmt( "Moving to ~p.", [ NewPosition ] ),


	PerceptionPeriod = ?getAttr(perception_period),

	% We decrement the current tick offset so that it is a multiple of 5:p
	RequestState = case ( CurrentTickOffset - 1 ) rem PerceptionPeriod of

		0 ->
			class_Actor:send_actor_message( ?getAttr(environment_pid),
					{ getTypedEntitiesWithin,
					  [ class_TestSpatialisedActor, Position,
						?getAttr(perception_radius) ] }, State );

		_ ->
			State

	end,

	NextActionOffset = CurrentTickOffset + MovePeriod,

	PlannedState = class_Actor:addSpontaneousTick( RequestState,
												   NextActionOffset ),

	MovedState = setAttribute( PlannedState, position, NewPosition ),

	?wooper_return_state_only( MovedState ).



% Called in response to the getEntitiesWithin request.
%
% (actor oneway)

-spec notifyEntitiesNearby( wooper:state(), [ pid() ], pid() ) ->
								  class_Actor:actor_oneway_return().
notifyEntitiesNearby( State, _NearbyEntities=[], _EnvironmentPid ) ->

	?debug( "No entity found in perception radius." ),

	?wooper_return_state_only( State );


notifyEntitiesNearby( State, NearbyEntities, _EnvironmentPid ) ->

	?info_fmt( "~B entities found in perception radius: ~p.",
			   [ length( NearbyEntities ), NearbyEntities ] ),

	?wooper_return_state_only( State ).
