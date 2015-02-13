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


% Test Equipment class.
%
% Used by the integration test defined in equipment_integration_test.erl
%
-module(class_TestEquipment).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Equipment ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, EquipmentName,
		 TerminationTickOffset, FailureModelPid, RepairModelPid ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).



% Method declarations:
%
-define( wooper_method_export, actNominal/1, actInDysfunction/1,
		 onFirstDiasca/2, onFailure/1, onReparation/1 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Equipment.TestEquipment").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new test equipment.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), class_TimeManager:tick_offset(), pid(), pid() )
			   -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	EquipmentState = class_Equipment:construct( State, ActorSettings,
		EquipmentName, FailureModelPid, RepairModelPid ),

	% Then the class-specific actions:
	% Failure state can be 'nominal' or 'dysfunction'.
	%
	% Equipments are supposed tested before being installed, thus start in
	% nominal condition:
	%
	% (cannot set next_*_tick, as no knowledge of current scheduling here)
	%
	StartingState = setAttributes( EquipmentState, [

		{ termination_tick_offset, TerminationTickOffset },
		{ trace_categorization, ?TraceEmitterCategorization }

													] ),

	?send_trace( StartingState, "Creating a new test equipment." ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting test equipment." ),

	?debug( "Test equipment deleted." ),

	% Then allow chaining:
	State.





% Methods section.


% Behaviour when being in nominal state.
%
% Note: tick termination will be handled by act_common/1.
%
% (oneway)
%
-spec actNominal( wooper:state() ) -> oneway_return().
actNominal( State ) ->

	?info( "Acting normally (test-overridden actNominal/1 called)." ),

	?wooper_return_state_only( act_common( State ) ).



% Behaviour when being in dysfunction state.
%
% Note: tick termination will be handled by act_common/1.
%
% (oneway)
%
-spec actInDysfunction( wooper:state() ) -> oneway_return().
actInDysfunction( State ) ->

	?info( "Test-overridden actInDysfunction/1 called." ),

	?wooper_return_state_only( act_common( State ) ).




% Section for helper functions (not methods).


% Manage the termination of this test equipment.
%
% Common to nominal and dysfunction.
%
% Returns an updated state.
%
-spec act_common( wooper:state() ) -> wooper:state().
act_common( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% We must prevent this equipment to further interact with other actors when
	% its end is near:
	%
	FreezeOffset = ?getAttr(termination_tick_offset) - 2,

	% Terminates if the termination offset is reached:
	case ?getAttr(current_tick_offset) of

		Offset when Offset >= FreezeOffset  ->

			% No more interactions wanted:
			TermState = executeOneway( State, scheduleNextSpontaneousTick ),

			setAttributes( TermState, [

							{ next_failure_tick, termination_triggered },
							{ next_repair_tick, termination_triggered },
							{ current_failure_state, terminating }

										  ] );

		Offset when Offset >= TerminationOffset ->

			?info( "Test Equipment preparing termination." ),

			% Returns an updated state:
			executeOneway( State, declareTermination );

		_ ->
			State

	end.



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



-spec onFailure( wooper:state() ) -> oneway_return().
onFailure( State ) ->

	?info( "Failure occurred! "
		"(test-overridden onFailure oneway method called)." ),

	?wooper_return_state_only( State ).



-spec onReparation( wooper:state() ) -> oneway_return().
onReparation( State ) ->

	?info( "Reparation occurred! "
		"(test-overridden onReparation oneway method called)." ),

	?wooper_return_state_only( State ).
