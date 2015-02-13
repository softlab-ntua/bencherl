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



% Test of the data-logger facilities, from a simulation actor.
%
-module(class_DataLoggingActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName,
		 TerminationTickOffset, ListenerPid ).



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
-define(TraceEmitterCategorization,"Actor.Test.Datalogging").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes:



% Constructs a new test actor for data-logging:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - ActorName the name of the actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
% - ListenerPid for any listener of this actor
%
% This test actor creates two probes.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), class_TimeManager:tick_offset(), pid() ) ->
					   wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% Cannot use 'output' yet (no talkative attribute):
	%io:format( "Creating a test actor with parameters:~n~p.~n",
	%	[ [ ?wooper_construct_parameters ] ] ),

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),

	% Will be used as probe name as well:
	FirstTitle = io_lib:format( "Curves A for actor ~p", [ self() ] ),

	FirstVirtualProbePair = class_DataLogger:create_virtual_probe(
			 _FirstProbeName=FirstTitle,
			 _FirstCurveNames=[ "Curve A1", "Curve A2" ],
			 _FirstZones=[],
			 _FirstTitle=FirstTitle,
			 _FirstXLabel="Simulation tick",
			 _FirstYLabel="Curve A values" ),


	SecondTitle = io_lib:format( "Curves B for actor ~p", [ self() ] ),

	SecondVirtualProbePair = class_DataLogger:create_virtual_probe(
			 _SecondProbeName=SecondTitle,
			 _SecondCurveNames=[ "Curve B1", "Curve B2", "Curve B3" ],
			 _SecondZones=[],
			 _SecondTitle=SecondTitle,
			 _SecondXLabel="Simulation tick",
			 _SecondYLabel="Curve B values" ),


	?send_info_fmt( ActorState, "Creating a new datalogging test actor, "
					"terminating no sooner than tick offset #~w.",
					[ TerminationTickOffset ] ),

	% - termination_tick_offset is the tick offset at which this test actor will
	% terminate
	%
	% - listener_pid allows to notify a process (ex: the test case) that the
	% report generation is over
	%
	setAttributes( ActorState, [
		{ termination_tick_offset, TerminationTickOffset },
		{ listener_pid, ListenerPid },
		{ first_probe_ref, FirstVirtualProbePair },
		{ second_probe_ref, SecondVirtualProbePair },
		% Useful to select console verbosity:
		{ talkative, false },
		%{ talkative, true},
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting datalogging test actor." ),

	% No specific probe deletion.

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.



% The core of the test actor behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->

			?info( "Test Actor preparing termination." ),

			TerminatingState = executeOneway( State, declareTermination ),

			output( State, io_lib:format( " - ~w terminating at #~B~n",
										  [ self(), PastOffset ] ) ),

			TerminatingState;

		CurrentOffset ->

			output( State, io_lib:format(
			   " - ~w acting spontaneously at #~B on ~s~n",
			   [ self(), CurrentOffset, net_utils:localnode() ] ) ),

			send_probe_data( CurrentOffset, State ),

			executeOneway( State, addSpontaneousTick, CurrentOffset + 7 )

	end,
	?wooper_return_state_only( NewState ).



-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Let's simply schedule this actor for the next tick:
	NewState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( NewState ).




% Section for helper functions (not methods).


% Outputs specified message in console, if talkative.
output( State, Message ) ->

	case ?getAttr(talkative) of

		true ->
			io:format( Message );

		false ->
			ok

	end.



% Sends data to the virtual probes (if any was selected).
send_probe_data( CurrentOffset, State ) ->

	% Depending on the result specification, one probe may be wanted while the
	% other not.
	%
	% In both cases, should a (virtual) probe be not wanted, automatically no
	% sample will be sent to it:

	class_DataLogger:send_data(
					?getAttr(first_probe_ref),
					CurrentOffset,
					_FirstSample={ random:uniform(50), random:uniform(30) } ),

	class_DataLogger:send_data(
					?getAttr(second_probe_ref),
					CurrentOffset,
					_SecondSample={ random:uniform(100), random:uniform(30),
								   random:uniform(150) } ).
