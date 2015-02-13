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



% Class modelling an actor taking part to a spatial environment, i.e. being at a
% given position in this environment.
%
%
-module(class_SpatialisedActor).


% Determines what are the mother classes of this class (if any):
%
% (SpatialisedTraceEmitter to supersede TraceEmitter, inherited from Actor)
-define( wooper_superclasses, [ class_Actor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Name, InitialPosition,
		 MaxSpeed, EnvironmentPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 getPosition/2, notifyEnvironmentSettings/5, toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, ).


-type position() :: class_TwoDimensionalEnvironment:position().


-export_type([ position/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% For spatial-aware traces:
-include("class_SpatialTraceEmitter.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Spatial.SpatialisedActor").




% Attributes of an instance of a spatialised actor are:
%
% - position :: position() is the current position of this actor; note that
% between two calls this value must always be up to date, as it might be
% requested directly (bypassing actor messages) by the environment in some
% cases
%
% - max_speed :: class_TwoDimensionalEnvironment:max_speed() is an upper-bound
% (if any) of the maximum speed of this actor
%
% - environment_pid :: pid() is the PID of the environment this actor will live
% in
%
% - env_width :: class_TwoDimensionalEnvironment:border_extent() describes the
% abscissa extent of the environment, from its origin
%
% - env_height :: class_TwoDimensionalEnvironment:border_extent() describes the
% ordinate extent of the environment, from its origin
%
%
% - border_settings :: class_TwoDimensionalEnvironment:border_description()
% describes how border crossing shall be managed






% Creates a new spatialised actor, in a 2D environment.
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
% - MaxSpeed is an upper-bound (if any) of the maximum speed of this actor
% (allows for better environment-level performances)
%
% - EnvironmentPid is the PID of the environment this actor will live in
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), position(),
				 class_TwoDimensionalEnvironment:max_speed(), pid() ) ->
					   wooper:state().
construct( State, ActorSettings, Name, InitialPosition, MaxSpeed,
		   EnvironmentPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	setAttributes( ActorState, [

			{ position, InitialPosition },
			{ max_speed, MaxSpeed },
			{ environment_pid, EnvironmentPid },

			% Will serve as cached environment information:
			{ env_width, undefined },
			{ env_height, undefined },
			{ border_settings, undefined },

			{ trace_categorization,
			  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								 ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% We do not undeclare this spatialised actor automatically from this
	% environment, as this must be done in the course of the simulation.

	% This can be done easily thanks to:
	%SentState = class_Actor:send_actor_message( ?getAttr(environment_pid),
	%											undeclare, State ),

	% Then allow chaining:
	State.





% Section for actor oneways.



% First scheduling on this spatialised actor.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) ->
						   class_Actor:actor_oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Declaring ourself to the environment:
	SentState = class_Actor:send_actor_message(
				  ?getAttr(environment_pid),
				  { declareEntity, [ ?getAttr(position), ?getAttr(max_speed),
									 wooper:get_class_name( State ) ] },
				  State ),

	% Creates an initial deadline at the tick, to trigger the burners:
	?wooper_return_state_only( SentState ).




% The definition of the spontaneous behaviour of this incinerator.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% Nothing specific here, no futur planned spontaneous action.

	?wooper_return_state_only( State ).



% Requests this actor to return back its current position.
%
% Notably called by the environment.
%
% (actor oneway)
%
-spec getPosition( wooper:state(), pid() ) -> class_Actor:actor_oneway_return().
getPosition( State, SenderPid ) ->

	SentState = class_Actor:send_actor_message( SenderPid,
					{ notifyPosition, ?getAttr(position) }, State ),

	?wooper_return_state_only( SentState ).



-spec notifyEnvironmentSettings( wooper:state(),
			class_TwoDimensionalEnvironment:border_extent(),
			class_TwoDimensionalEnvironment:border_extent(),
			class_TwoDimensionalEnvironment:border_description(), pid() ) ->
						class_Actor:actor_oneway_return().
notifyEnvironmentSettings( State, Width, Height, BorderSettings, _EnvPid ) ->

	?wooper_return_state_only( setAttributes( State, [

			{ env_width, Width },
			{ env_height, Height },
			{ border_settings, BorderSettings }

	] ) ).



% Returns a textual description of this instance.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	io_lib:format( "Spatialised actor ~w whose position is ~p "
				   "(max speed: ~p meters per second), using environment ~w",
				   [ self(), ?getAttr(position), ?getAttr(max_speed),
					 ?getAttr(environment_pid) ] ).
