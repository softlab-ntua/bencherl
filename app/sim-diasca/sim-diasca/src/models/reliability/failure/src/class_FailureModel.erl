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


% Class modeling the failure behaviour of equipments.
% Most failure models rely on underlying random generators.
%
% Their instances must be simulation actors, so that the total ordering of their
% incoming messages is recreated, otherwise the generation of the random numbers
% will break reproducibility.
%
% Thus a failure model is a stochastic actor.
% See equipment_integration_test.erl for an integration test.
%
-module(class_FailureModel).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_StochasticActor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, FailureModelName,
		 RandomProfile ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, onFirstDiasca/2, getNextFailure/2 ).


-type mttf() :: class_Equipment:reliability_duration().


-export_type([ mttf/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.StochasticActor.FailureModel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new failure model:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - FailureModelName is its name
%
% - RandomProfile is a tuple describing the kind of randomness needed to compute
% failures (if any)
%
%
% RandomProfile can be among:
%
% - {uniform,N} for uniform laws (positive integer)
%
% - {exponential,Lambda} for exponential laws (floating-point)
%
% - {positive_integer_exponential,Lambda} for exponential laws (positive
% integer)
%
% - {gaussian,Mu,Sigma} for gaussian laws
%
% - {positive_integer_gaussian,Mu,Sigma} for gaussian laws (positive integer)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	  class_Actor:name(), class_Equipment:random_profile() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	% (this particular random list stores only one profile, the failure one)
	StochasticState = class_StochasticActor:construct( State, ActorSettings,
		FailureModelName, [ { failure_profile, RandomProfile } ] ),
	
	% Then the class-specific actions:
	StartingState = setAttribute( StochasticState, trace_categorization,
								  ?TraceEmitterCategorization ),

	?send_trace_fmt( StartingState, "Creating a new failure model "
		"whose failure profile is ~w, with, "
		"as default upper-bound of random consumption.", [ RandomProfile ] ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting failure model." ),

	?debug( "Failure model deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% Defined simply to avoid a useless warning to be issued / an exception to be
% thrown.
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).


% A failure model is purely passive, it only answers to requests, and it does
% not even override its 'actSpontaneous' method.



% Requests this model to determine (asynchronously) the tick offset of next
% failure, i.e. computes the next tick offset at which the caller equipment will
% fail (assuming it is just created or repaired).
%
% (actor oneway, but triggers back a oneway on the caller)
%
-spec getNextFailure( wooper:state(), pid() ) -> oneway_return().
getNextFailure( State, EquipmentPid ) ->

	% Uses directly stochastic mother class (automatic background refill):
	FailureDurationInSeconds = class_StochasticActor:get_random_value_from(
								 failure_profile, State ),

	% FailureDuration is in seconds, converting to ticks:
	%
	% (results in at least two ticks, so that the caller is never supposed to
	% fail at the same tick it receives that information)
	%
	FailureDurationInTicks = erlang:max( 2,
	  class_Actor:convert_seconds_to_ticks( FailureDurationInSeconds, State ) ),

	%io:format( "Failure duration: ~w seconds, i.e. ~B ticks.~n",
	%	[ FailureDurationInSeconds, FailureDurationInTicks ] ),

	FailureTickOffset = class_Actor:get_current_tick_offset( State )
		+ FailureDurationInTicks,

	?trace_fmt( "Determined next failure at tick offset #~B for ~w.",
				[ FailureTickOffset, EquipmentPid ] ),

	% Answer can be returned directly, as this method is triggered by reordered
	% actor messages, like its answer will be (at next tick):
	SentState = class_Actor:send_actor_message( EquipmentPid,
		{ setNextFailure, FailureTickOffset }, State ),

	?wooper_return_state_only( SentState ).
