% Copyright (C) 2011-2014 EDF R&D

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
-module(class_DataExchangeTestActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, DataKey,
		TerminationTickOffset ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		synchronous_new/4, synchronous_new_link/4,
		synchronous_timed_new/4, synchronous_timed_new_link/4,
		remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		construct/5, delete/1 ).


% Member method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Test.DataExchange").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes:
%
% This test actor will perform data-exchanges (read/modify/write). The point is
% that it does not even to know whether the simulation is started or not (in
% this case it is an initial actor).



% Constructs a new test actor for data-logging:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - ActorName the name of the actor
%
% - DataKey is an atom corresponding to the key of a data entry of interest for
% that actor
%
% - TerminationTickOffset the duration after which this actor should terminate
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				class_Actor:name(), class_DataExchanger:key(),
				class_TimeManager:tick_offset() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% Cannot use 'output' yet (no talkative attribute):
	%io:format( "Creating a test actor with parameters:~n~p.~n",
	%	[ [?wooper_construct_parameters] ] ),

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),

	TraceState = class_TraceEmitter:set_categorization(
								   ?TraceEmitterCategorization, ActorState ),


	?send_info_fmt( TraceState, "Creating a new data-exchange test actor, "
		"terminating no sooner than tick offset #~w.",
		[ TerminationTickOffset ] ),

	% This actor will make use of the data-exchange service:
	ExchangeState = class_Actor:enable_data_exchange( TraceState ),

	% Common to all actors:
	{ 3, mutable } = class_Actor:read_qualified_data( example_key_for_actors,
														ExchangeState ),

	1 = class_Actor:read_data( other_example_key_for_actors, ExchangeState ),


	% Read-modify-write its "own" specified key:
	V = class_Actor:read_data( DataKey, ExchangeState ),
	class_Actor:modify_data( DataKey, V+1, mutable, ExchangeState ),


	% - termination_tick_offset is the tick offset at which this test actor will
	% terminate
	%
	% - exchange_settings is an opaque type with allows this actor to make use
	% of the data exchange service
	%
	% - data_key is a value of interest read and possibly modified by this
	% actor, interacting with the data-exchanger
	%
	setAttributes( ExchangeState, [

		{termination_tick_offset,TerminationTickOffset},
		{data_key,DataKey},

		% Useful to select console verbosity:
		{talkative,false},
		%{talkative,true},

		% Allows to check that the value read from the data-exchange service is
		% correct:
		{expected_value,V+1},

		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

								] ).



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	%io:format( "Test actor ~w deleted, while was still at tick offset #~p.~n",
	%		  [ self(), ?getAttr(current_tick_offset) ] ),

	% Class-specific actions:
	?info( "Deleting data-exchange test actor." ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.





% The core of the test actor behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	%io:format( "--> tick offset #~p for ~w.~n",
	%	[ ?getAttr(current_tick_offset), self() ] ),

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of


		PastOffset when PastOffset >= TerminationOffset ->

			?info( "Test Actor preparing termination." ),

			%io:format( "Test Actor ~p preparing termination at #~p.~n",
			%		  [ self(), PastOffset ] ),

			% Following two calls could also have been grouped into an
			% overloading of the default declareTermination/2 implementation:
			%
			% (we request an immediate termination here, as we should not have
			% to wait for anyone)
			%
			TerminatingState = executeOneway( State, declareTermination,
						_IntercalaryDiasca=0 ),

			output( State, io_lib:format( " - ~w terminating at #~B.~n",
										 [ self(), PastOffset ] ) ),

			TerminatingState;


		CurrentOffset ->

			output( State, io_lib:format(
			   " - ~w acting spontaneously at #~B on ~s.~n",
			   [ self(), CurrentOffset, net_utils:localnode() ] ) ),

			Key = ?getAttr(data_key),

			V = class_Actor:read_data( Key, State ),

			% Checking:
			V = ?getAttr(expected_value),

			NewV = V + CurrentOffset rem (?getAttr(actor_abstract_id) + 2 ),

			class_Actor:modify_data( Key, NewV, mutable, State ),

			ExpectedState = setAttribute( State, expected_value,NewV ),

			executeOneway( ExpectedState, addSpontaneousTick, CurrentOffset+7 )



	end,

	?wooper_return_state_only( NewState ).




% Section for actor oneways.

-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% We choose here not to do anything until the next tick:
	NewState = executeOneway( State, addSpontaneousTick,
							 ?getAttr(current_tick_offset) + 1 ),

	?wooper_return_state_only( NewState ).




% Section for helper functions (not methods).


% Outputs specified message in console, if talkative.
-spec output( wooper_state(), string() ) -> basic_utils:void().
output( State, Message ) ->

	case ?getAttr(talkative) of

		true ->
			io:format( Message );

		false ->
			ok

	end.
