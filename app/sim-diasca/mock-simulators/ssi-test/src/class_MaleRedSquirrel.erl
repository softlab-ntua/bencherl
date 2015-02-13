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

% This file is part of forest ecosystem integration test case.


% The objective of this module is to show the main features of an actor in
% multiple scheduling modes. These instances have indeed a periodic schedule and
% can be also triggered by messages.


% Class modelling a male red squirrel.
%
-module(class_MaleRedSquirrel).


% TO-DO (FIXME):
%
% - what is "savage" expected to mean?
% - weak should be replaced by young?



% Determines what are the mother classes
-define( wooper_superclasses, [ class_Squirrel ] ).


-define( wooper_construct_parameters, ActorSettings, SquirrelName, GivenAge,
		ForestPid ).


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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, beInvited/3,
		 youLose/2, youWon/2, beAlert/3 ).


% Static method declarations (to be directly called from module):
%-define( wooper_static_method_export, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization, "SSI-Test.class_MaleRedSquirrel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


-include("ssi_test_types.hrl").



% Constructs a new male red squirrel actor:
%
% - ActorSettings corresponds to the engine settings for this actor
%
% - SquirrelName is the name of the squirrel, as a plain string
%
% - GivenAge is the initial age of this squirrel
%
% - ForestPid is the PID of the forest this squirrel is in
%
-spec construct( wooper:state(), class_Actor:actor_settings(), string(), age(),
				pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First, the mother class:
	SquirrelState = class_Squirrel:construct( State, ActorSettings,
										   SquirrelName, GivenAge, ForestPid ),

	% Attribute descriptions:
	% - frame_of_mind: can be 'available' or 'weak'

	StartingState = setAttributes( SquirrelState, [

		{frame_of_mind,undefined},
		{tail_length,get_initial_tail_length()},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

	] ),

	?send_trace( StartingState, "Creating a new male red squirrel." ),

	StartingState.




% Methods section.



% The spontaneous behaviour of a male red squirrel instance.
%
% (actor oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->
			termination_relative_activities( State );

		_CurrentOffset ->
			spontaneous_activities( State )

	end,

	?wooper_return_state_only( NewState ).




% Called when this squirrel succeeded in a contest.
%
% When a youWon message is received, the squirrel switches for an arrogant
% frame_of_mind for a defined period.
%
% (actor oneway)
%
-spec youWon( wooper:state(), pid() ) -> class_Actor:actor_oneway_return().
youWon( State, _SenderPid ) ->

	?info( "I am the winner, I am the most beautiful squirrel." ),

	FrameState = setAttribute( State, frame_of_mind, arrogant ),

	NextAvailableTick = ?getAttr(current_tick_offset)
		+ ?getAttr(arrogant_period),

	FinalState = executeOneway( FrameState, addSpontaneousTick,
							   NextAvailableTick ),

	?wooper_return_state_only( FinalState ).



% Called when this squirrel failed in a contest.
%
% (actor oneway)
%
-spec youLose( wooper:state(), pid() ) -> class_Actor:actor_oneway_return().
youLose( State, _SenderPid ) ->

	?info( "I am so sad, I am not the winner." ),

	NextState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( setAttribute( NextState, frame_of_mind,
											available ) ).




% Called when an alert message is received from the forest.
%
% (actor oneway)
%
-spec beAlert( wooper:state(), alert(), pid() ) ->
					 class_Actor:actor_oneway_return().
beAlert( State, Alert, _SenderPID ) ->

	NewState = case Alert of

		savage ->
			?info( "A savage alert is received, I am preparing my deferred "
						  "termination." ),
			executeOneway( State, notifyTermination );

		famine ->
			case ?getAttr(frame_of_mind) of

				Mindset when (Mindset=:=weak) orelse (Mindset=:=gestation) ->
					?info_fmt( "I am in ~p frame of mind and "
							   "a famine alert is received, I am preparing "
							   "my deferred termination.", [ Mindset ] ),
					executeOneway( State, notifyTermination );

				_OtherMindset ->
					?info_fmt( "I received a ~p alert, but I am surviving.",
								[ Alert ] ),
					State

			end;

		_OtherAlert ->
			?info_fmt( "I received a ~p alert, but I am surviving.", 
					  [ Alert ] ),
			State

	end,

	?wooper_return_state_only( NewState ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% A competition invitation is received, the squirrel replies with its tail
% length.
%
% (actor oneway)
%
-spec beInvited( wooper:state(), pid(), pid() ) ->
					   class_Actor:actor_oneway_return().
beInvited( State, LauncherPid, SenderPid ) ->

	?info_fmt( "I, male red squirrel ~w, receive a invitation.", [ self() ] ),

	TailLength = case ?getAttr(frame_of_mind) of

		weak ->
			 ?info( "I cannot participate to the competition, I am to young." ),
		refused;

		_Others ->
			?info( "I received a competition invitation, "
				"I believe that I will win." ),
			?getAttr(tail_length)

	end,

	SentState = class_Actor:send_actor_message( SenderPid,
				 { informedParticipation, [ LauncherPid, TailLength ] },
				 State ),

  ?wooper_return_state_only( SentState ).





% Helper functions.

%youAreWinner

% Once a squirrel eats, its tail gains 0.1cm.
%
eat_nuts( State ) ->

	NewLength = math_utils:round_after( ?getAttr(tail_length) + 0.1,
									   _Digits=1 ),

	?info( "How delicious the nuts are, my tail is more beautiful." ),

	setAttribute( State, tail_length, NewLength ).



% Returns a random tail length, between 3 - 6 for a newborn.
%
% This function is called only one time when a new male is created.
%
get_initial_tail_length()->
	%random:uniform(N): Returns a random float between 0 and N
	6 - random:uniform(3).



% This helper function groups all termination related activities.
%
% Returns an updated state.
%
termination_relative_activities( State ) ->

	WaitTicks = ?getAttr(termination_waiting_ticks),
	CurrentOffset = ?getAttr(current_tick_offset),

	case ?getAttr(termination_initiated) of

		false ->
			?info( "I am preparing deferred termination." ),
			executeOneway( State, notifyTermination );

		true when WaitTicks > 0 ->

			NextState = executeOneway( State, scheduleNextSpontaneousTick ),

			NewWaitTick = ?getAttr(termination_waiting_ticks) - 1,

			setAttribute( NextState, termination_waiting_ticks, NewWaitTick );

		_Other ->
			?info_fmt( "I am terminating at tick #~B.", [ CurrentOffset ] ),
			executeOneway( State, declareTermination )

	end.




% This helper function groups all spontaneous activities of this actor.
%
% Returns an updated state.
%
spontaneous_activities( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),
	AvailableTick = ?getAttr(available_tick),

	case ?getAttr(is_registered) of

		false ->
			% Apparently fakes an actor oneway (strange):
			executeOneway( State, tryToRegister, [ ?MODULE, self() ] );

		true ->
			case ?getAttr(frame_of_mind) of

				weak when CurrentOffset >= AvailableTick ->

					?info( "I am an adult squirrel now!" ),

					FrameState = setAttribute( State, frame_of_mind,
												  available ),

					executeOneway( FrameState, scheduleNextSpontaneousTick );

				arrogant when CurrentOffset < AvailableTick ->

						?info( "I am the most beautiful squirrel." ),
						State;

				_Others ->
						eat_nuts( State )
			end

	end.
