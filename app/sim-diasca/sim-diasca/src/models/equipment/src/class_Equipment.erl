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



% Equipment class, models any equipment, mostly electronic ones.
%
% Equipments are affected by failures and can be repaired, thus make use of a
% failure model and a repair model.
%
% An equipment can define any specific behavior to happen in case of failure or
% reparation, either on state transitions (onFailure/onReparation) or on
% persistent state (actNominal/actInDysfunction).
%
% See class_TestEquipment.erl for an example of equipement and
% equipment_integration_test.erl for a global test.
%
-module(class_Equipment).



% Determines what are the mother classes of this class (if any):
% (the stochastic behaviour is obtained indirectly, from failure/repair models).
%
-define( wooper_superclasses, [ class_Actor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, EquipmentName,
		 FailureModelPid, RepairModelPid ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, setNextFailure/3, setNextRepair/3,
		 actSpontaneous/1, actNominal/1, actInDysfunction/1,
		 onFirstDiasca/2, onFailure/1, onReparation/1,
		 getReliabilityStatus/1, setReliabilityProbe/2 ).


% Exported to factor behaviours when overriding onFailure/onReparation:
-export([ notify_failure/1, notify_reparation/1 ]).



-type reliability_status() :: 'nominal' | 'dysfunction'.


-type reliability_duration() :: { unit_utils:days(), unit_utils:hours(),
					 unit_utils:minutes(), unit_utils:seconds() }.


-type random_profile() :: { 'uniform', pos_integer() }
						| { 'exponential', float() }
						| { 'positive_integer_exponential', pos_integer() }
						| { 'gaussian', float(), float() }
						| { 'positive_integer_gaussian', number(), number() }.


-type reliability_tick() :: class_TimeManager:tick_offset()
											   | 'uninitialized' | 'waiting'.


% Silences as well unused warning:
-export_type([ reliability_status/0, reliability_duration/0,
			   random_profile/0, reliability_tick/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Equipment").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For nominal/failure status:
-include("class_ReliabilityProbe.hrl").


% Attributes of an equipment are:
%
% - failure_model_pid :: basic_utils:maybe( pid() ) is the PID of the failure
% model in use
%
% - repair_model_pid :: basic_utils:maybe( pid() ) is the PID of the repair
% model in use
%
% - next_failure_tick :: reliability_tick() records the time of next failure (if
% any is planned)
%
% - next_repair_tick :: reliability_tick() records the time of next reparation
% (if any is planned)
%
% - current_failure_state :: 'nominal' | 'dysfunction' tells about the current
% reliability state of this equipment
%
% - reliability_listener :: basic_utils:maybe( pid() ) records the PID on the
% reliability listener (if any)
%
% - reliability_probe :: basic_utils:maybe( class_Probe:probe_pid() )




% Implementation notes:
%
% An equipment could also be modelled as a standalone stochastic actor, with no
% need for separate failure and reparation models.
%
% When making a reliability transition (ex: from nominal to dysfunction), we
% send two consecutive samples to the reliability probes, otherwise, as we are
% jumping from a transition to another, the probe would show triangles instead
% of solid, plain rectangles, i.e. without representing clearly at each tick
% what is the current reliability status.




% Constructs a new equipment actor, regarding notably failure and reparation.
%
% Parameters are:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - EquipmentName is the name of this equipment (as a string)
%
% - FailureModelPid is the PID of the failure model to be used by this equipment
%
% - RepairModelPid is the PID of the repair model to be used by this equipment
%
%
% Child classes of this Equipment class only have to override if needed the
% following two oneway methods:
%
% - the actNominal(State) oneway, called at each tick where the equipment is in
% nominal conditions (including the ticks during when it has just been repaired)
%
% - the actInDysfunction(State) oneway, called at each tick where the equipment
% is in dysfunction (including the ticks when a dysfunction just occurred)
%
% Besides, the onFailure(State) and onReparation(State) oneways will be called
% at each transition, from nominal to dysfunction, and the other way round.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, ActorSettings, EquipmentName ),

	% Then the class-specific actions:
	% Failure state can be 'nominal' or 'dysfunction'.
	%
	% Equipments are supposed tested before being installed, thus start in
	% nominal condition:
	%
	% (cannot set next_*_tick, as no knowledge of current scheduling here)
	StartingState = setAttributes( ActorState, [

		{ failure_model_pid, FailureModelPid },
		{ repair_model_pid, RepairModelPid },
		{ next_failure_tick, uninitialized },
		{ next_repair_tick, uninitialized },
		{ current_failure_state, nominal },
		{ reliability_listener, undefined },
		{ reliability_probe, undefined },
		{ trace_categorization, ?TraceEmitterCategorization } ]

								  ),

	?send_trace_fmt( StartingState,
		"Creating a new equipment whose failure model is ~w and "
		"whose repair model is ~w.", [ FailureModelPid, RepairModelPid ] ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting equipment." ),

	% reliability_probe and reliability models not deleted here, as not owned.

	?debug( "Equipment deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the equipment.


% Called by the failure model, in answer to a getNextFailure call.
%
% Third parameter of the request (sender PID, the failure model) is ignored.
%
% (actor oneway)
%
-spec setNextFailure( wooper:state(), class_TimeManager:tick_offset(), pid() )
					-> class_Actor:actor_oneway_return().
setNextFailure( State, FailureTick, _SenderPid ) ->

	%io:format( "setNextFailure, for tick #~B.~n", [ FailureTick ] ),

	% Consistency check:
	NewState = case ?getAttr(next_failure_tick) of

		waiting ->

		   % FailureTick is already a time (a tick), not a duration:
		   ?debug_fmt( "Equipment planned future failure at tick ~B.",
			   [ FailureTick ] ),

			PlannedState = executeOneway( State, addSpontaneousTick,
										  FailureTick ),

			setAttributes( PlannedState, [
						{ next_failure_tick, FailureTick },
						{ next_repair_tick, uninitialized }
										  ] ) ;

		termination_triggered ->
			State

	 end,

	?wooper_return_state_only( NewState ).



% Called by the repair model, in answer to a getNextRepair call.
%
% Third parameter (sender Pid, the repair model) is ignored.
%
% (actor oneway)
%
-spec setNextRepair( wooper:state(), class_TimeManager:tick_offset(), pid() )
					-> class_Actor:actor_oneway_return().
setNextRepair( State, RepairTick, _SenderPid ) ->

	%io:format( "setNextRepair, for tick #~B.~n", [RepairTick] ),

	% Consistency check:
	NewState = case ?getAttr(next_repair_tick) of

		waiting ->

		   % RepairTick is already a time (a tick), not a duration:
		   ?debug_fmt( "Equipment planned future repair at tick ~B.",
					   [ RepairTick ] ),

			PlannedState = executeOneway( State, addSpontaneousTick,
										  RepairTick ),

			setAttributes( PlannedState, [
						{ next_failure_tick, uninitialized },
						{ next_repair_tick, RepairTick } ] ) ;

		termination_triggered ->
			State

	 end,

	?wooper_return_state_only( NewState ).



% Management section of the equipement actor.


% The core of the equipment generic behaviour.
%
% Manages transition between failure and repair, and triggers actions associated
% for both of these states.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	%?debug( "Equipment acting." ),

	%io:format( "actSpontaneous at ~B: state is ~p.~n",
	% [ class_Actor:get_current_tick( State ),
	%  ?getAttr(current_failure_state) ] ),

	% Reliability is probed at tick begin:
	NewState = case ?getAttr(current_failure_state) of

		nominal ->
			handle_nominal( State );

		dysfunction ->
			handle_dysfunction( State );

		terminating ->
			executeOneway( State, scheduleNextSpontaneousTick )

	end,

	%?debug( "Equipment acted." ),

	% No need to schedule each tick, here we can jump to the next transition:
	%PlannedState = executeOneway( NewState, scheduleNextSpontaneousTick ),
	PlannedState = NewState,

	?wooper_return_state_only( PlannedState ).




% Default implementation of the actNominal oneway.
%
% Note: made to be overridden for actual equipments.
%
% (oneway)
%
-spec actNominal( wooper:state() ) -> oneway_return().
actNominal( State ) ->

	?warning( "Equipment actNominal/1 oneway method called." ),

	?wooper_return_state_only( State ).



% Default implementation of the actInDysfunction oneway.

% Note: made to be overridden for actual equipments.
%
% (oneway)
%
-spec actInDysfunction( wooper:state() ) -> oneway_return().
actInDysfunction( State ) ->

	?warning( "Non-overridden actInDysfunction/1 oneway method called." ),

	?wooper_return_state_only( State ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% Default implementation of the onFailure oneway.

% Note: made to be overridden for actual equipments.
%
% (oneway)
%
-spec onFailure( wooper:state() ) -> oneway_return().
onFailure( State ) ->

	?warning( "Non-overridden onFailure/1 oneway method called." ),

	?wooper_return_state_only( notify_failure(State) ).



% Default implementation of the onReparation oneway.
%
% Note: made to be overridden for actual equipments.
%
% (oneway)
%
-spec onReparation( wooper:state() ) -> oneway_return().
onReparation( State ) ->

	?warning( "Non-overridden onReparation/1 oneway method called." ),

	?wooper_return_state_only( notify_reparation( State ) ).



% Returns the current status of this equipment regarding reliability, i.e.
% either nominal or dysfunction.
%
% (const request)
%
-spec getReliabilityStatus( wooper:state()) ->
								  request_return( reliability_status() ).
getReliabilityStatus( State ) ->
	?wooper_return_state_result( State, ?getAttr(current_failure_state) ).



% Links specified reliability probe to this equipment.
%
% (request, for synchronisation purpose)
%
-spec setReliabilityProbe( wooper:state(), pid() ) ->
								 request_return( 'probe_set' ).
setReliabilityProbe( State, ProbePid ) ->

	%?trace( "setReliabilityProbe called." ),

	?wooper_return_state_result(
		setAttribute( State, reliability_probe, ProbePid ), probe_set ).




% Section for helper functions (not methods).


% Called whenever a failure happens.
%
% Returns an updated state.
%
% (helper)
%
-spec trigger_failure( wooper:state() ) -> wooper:state().
trigger_failure( State ) ->

	% Consistency check:
	nominal = ?getAttr(current_failure_state),

	FailureState = setAttribute( State, current_failure_state, dysfunction ),

	% Next repair tick not set yet, let's request it:
	RequestState = class_Actor:send_actor_message(
		?getAttr(repair_model_pid), getNextRepair, FailureState ),

	setAttribute( RequestState, next_repair_tick, waiting ).



% Called whenever a repair happens.
%
% Returns an updated state.
%
% (helper)
%
-spec trigger_repair( wooper:state() ) -> wooper:state().
trigger_repair( State ) ->

	% Consistency check:
	dysfunction = ?getAttr(current_failure_state),

	NominalState = setAttribute( State, current_failure_state, nominal ),

	% Next failure tick not set yet, let's request it:
	RequestState = class_Actor:send_actor_message(
		?getAttr(failure_model_pid), getNextFailure, NominalState ),

	setAttribute( RequestState, next_failure_tick, waiting ).




% Helper function for the actSpontaneous/1 oneway.
%
% Returns an updated state.
%
% (helper)
%
-spec handle_nominal( wooper:state() ) -> wooper:state().
handle_nominal( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	% Working correctly, thus watching for next failure:
	case ?getAttr(next_failure_tick) of

		 uninitialized->

			%io:format( "handle_nominal at #~B: no next failure set.~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, nominal, State ),

			% Failure tick not set yet, let's request it:
			RequestState = class_Actor:send_actor_message(
				?getAttr(failure_model_pid), getNextFailure, State ),

			WaitingState = setAttribute( RequestState, next_failure_tick,
										 waiting ),

			% Acts nevertheless, in a nominal way here.

			?debug( "Equipment acting normally, until knowing "
					"when the next failure will occur." ),

			% Calls directly the overridden actNominal/1 oneway:
			executeOneway( WaitingState, actNominal );


		CurrentTickOffset ->

			%io:format( "handle_nominal at #~B: failing!~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset - 1, nominal, State ),
			send_probe( CurrentTickOffset, dysfunction, State ),

			?info( "Equipment failure." ),

			% Failure happened!
			FailedState = trigger_failure( State ),

			% Calls directly overridden onFailure oneway:

			% Notifies the transition:
			FirstFailedState = executeOneway( FailedState, onFailure ),

			% And acts accordingly to this newly failed state:
			executeOneway( FirstFailedState, actInDysfunction );


		_ ->

			% Includes any other failure tick and the 'waiting' atom:

			%io:format( "handle_nominal at #~B: acting normally.~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, nominal, State ),

			?debug( "Equipment acting normally." ),
			executeOneway( State, actNominal )

	end.



% Helper function for the actSpontaneous/1 oneway.
%
% Returns an updated state.
%
% (helper)
%
-spec handle_dysfunction( wooper:state() ) -> wooper:state().
handle_dysfunction( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	send_probe( CurrentTickOffset, dysfunction, State ),

	% If out of order, watch for repair.
	% Repair tick was already set when last failure was triggered.
	case ?getAttr(next_repair_tick) of

		 uninitialized->

			%io:format( "handle_dysfunction at #~B: no next repair set.~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, dysfunction, State ),

			% Repair tick not set yet, let's request it:
			RequestState = class_Actor:send_actor_message(
				?getAttr(repair_model_pid), getNextRepair, State ),

			WaitingState = setAttribute( RequestState, next_repair_tick,
										 waiting ),

			% Acts nevertheless, in a nominal way here.

			?debug( "Equipment acting in dysfunction, until knowing "
				"when the next reparation will occur." ),

			% Calls directly overridden actNominal oneway:
			executeOneway( WaitingState, actInDysfunction );


		CurrentTickOffset ->

			?info( "Equipment reparation is over." ),

			%io:format( "handle_dysfunction at #~B: being repaired!~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset-1, dysfunction, State ),
			send_probe( CurrentTickOffset, nominal, State ),

			% Repairing over!
			RepairState = trigger_repair( State ),

			% Calls directly overridden onReparation oneway:

			% Notifies the transition:
			FirstRepairedState = executeOneway( RepairState, onReparation ),

			% And acts accordingly to this newly repaired state, returns an
			% updated state:
			executeOneway( FirstRepairedState, actNominal );


		_OtherTick ->

			%io:format( "handle_dysfunction at #~B: being out of order.~n",
			%	[ CurrentTickOffset ] ),

			send_probe( CurrentTickOffset, dysfunction, State ),

			% Includes any repair tick and the 'waiting' atom:

			?debug( "Equipment still out of order." ),

			% Returns an updated state:
			executeOneway( State, actInDysfunction )

	end.



% Notifies any reliability listener that this equipment failed.
%
% Returns an updated state.
%
% (helper)
%
-spec notify_failure( wooper:state() ) -> wooper:state().
notify_failure( State ) ->

	case ?getAttr(reliability_listener) of

		undefined ->
			State;

		ListenerPid ->
			class_Actor:send_actor_message( ListenerPid, notifyFailure,
											State )

	end.



% Notifies any reliability listener that this equipment was repaired.
%
% Returns an updated state.
%
% (helper)
%
-spec notify_reparation( wooper:state() ) -> wooper:state().
notify_reparation( State ) ->

	case ?getAttr(reliability_listener) of

		undefined ->
			State;

		ListenerPid ->
			class_Actor:send_actor_message( ListenerPid, notifyReparation,
											State )

	end.


% Sends reliability information to the probe.
%
% (helper)
send_probe( CurrentTickOffset, Status, State ) ->

	case ?getAttr(reliability_probe) of

		undefined ->
			ok;

		ProbePid ->

			SampleData = case Status of

							 nominal ->
								 ?nominal_status;

							 dysfunction ->
								 ?failed_status

						 end,

			ProbePid ! { setData, [ CurrentTickOffset, { SampleData } ] }

	end.
