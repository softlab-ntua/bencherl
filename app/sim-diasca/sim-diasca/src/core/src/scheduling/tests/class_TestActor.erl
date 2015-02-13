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


% Test of Actor class, regarding time management.
%
-module(class_TestActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName,
		 SchedulingPolicy, CreationPolicy, TerminationTickOffset ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 lowerTraceIntensity/1, onActorCreated/3,
		 addInitialPeer/2, addPeer/2, removePeer/2, forgetPeer/2, hello/3 ).


% Static method declarations.
-define( wooper_static_method_export, add_initial_peers/2 ).


-type scheduling_policy() ::  { 'periodic', non_neg_integer() }
							| { 'erratic',  non_neg_integer() }.


-type creation_policy() :: 'no_creation'
		| { non_neg_integer(), { scheduling_policy(), creation_policy() } }
		| class_TimeManager:tick_offset().


-export_type([ scheduling_policy/0, creation_policy/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Test").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Implementation notes:


% When a test actor A adds another test actor B as peer:
%
% - for A, B is a target peer
%
% - for B, A becomes (as soon as the first hello message is received) a source
% peer
%
% If B terminates, it has to call removePeer on A, otherwise A would keep on
% sending hello messages to a non-existing B and would block.
%
% So B has to store the Pid of A.
%
% However, if A terminated before B, then the removePeer call from B on A would
% itself block.
%
% Therefore A, when terminating, needs also to notify B, thanks to a forgetPeer
% call.


% Moreover we have to rely on a deferred termination phase. Indeed if an actor A
% determines at timestamp {T,D} that it has to terminate, it will notify the
% actors it is interacting with (B1, B2, etc.) during this {T,D} timestamp, but
% they will take this message into account only at {T,D+1}. Thus they may still
% be sending messages to the terminating actor (A) at {T,D} and even {T,D+1} (if
% the shuffling of incoming messages results into the notification sent by the
% terminating actor to be processed lately). These perfectly licit messages
% would then be processed by the terminating actor at {T,D+2}, possibly after it
% declared its termination, which is not wanted (for example, if it was already
% deleted, it could acknowledge this message and this would block the
% simulation).
%
% So the actual termination should occur here no sooner than {T,D+3}. It could
% occur then, but, as there could be a chain of interacting actors (B1 having to
% notify C that A is leaving), we could imagine that licit messages could arrive
% at {T,D+4} and later diascas.
%
% To ease the troubleshooting of models, terminating actors will be *actually*
% deleted only at the next tick, so that they can be able to detect messages
% received whereas they have terminated (in the diascas in-between).
%
% So, when an actor decides at {T,D} it will terminate, it has to call its
% declareTermination/2 oneway (or the class_Actor:declare_termination/2 helper
% function), whose second parameter is the number of intercalary diascas which
% is wanted before being considered as terminated, which is a positive number
% (possibly zero) or the atom 'unlimited'.
%
% This call will immediately set the 'next_action' attribute of this actor to
% 'terminating' (so that, for example, it can still acknowledge, but choose to
% ignore, any incoming actor message that was sent before the termination
% information propagated). Receiving an actor message while in 'terminating'
% cannot be avoided in the general case (race condition between normal exchanges
% and the termination notification) and is not an error (generally these
% messages are just to be ignored). Then as many diasca as specified will be
% requested by this actor (which thus will be scheduled as many times). Then its
% 'next_action' attribute will be set to 'terminated' (the actor could be
% deleted then, but deferring this deletion favors the debugging), and any new
% actor message that would be received will be interpreted as an error.
%
% At the beginning of the next tick, the actor will be deleted.
%
% If, instead of specifying a number of intercalary diascas, 'unlimited' was
% specified, then the next_action of this actor will be set and left to
% 'terminating' (never reaching 'terminated'); the actor will not request any
% diasca, will accept any new actor message and will be deallocated as soon as
% the next tick begins.




% Two ways of adding a peer have to be defined. Indeed, to do so initially, we
% need to perform some synchronisation, thus we need a request (thus
% addInitialPeer/2), whereas adding a peer while the simulation is running
% implies using an actor for that, thus using an (actor) oneway (addPeer/2).

% Later: add the use of a virtual probe, to test also write and overall
% scalability.



% The attributes of an instance of this test actor are:
%
% - target_peers are the other actors this actor sends messages to
%
% - source_peers are the actors that send messages to this actor
%
% - next_planned is a list of the next tick offsets at which a spontaneous
% action was requested (useful to declare them only once, even with very erratic
% scheduling plans)
%
% - scheduling_settings describes how this actor should be scheduled
%
% - creation_settings describes how this actor should create actors, ex:
% {_InterCount=10, {_SchedulingPolicy={periodic,4},_CreationPolicy=no_creation}}
%
% - creation_countdown keeps track of the number of spontaneous actions before
% the next actor creation, if not equal to 'no_creation'
%
% - created_actors keeps track of the actors already created
%
% - termination_tick_offset is the tick offset at which this test actor will
% terminate



% Constructs a new test actor:
%
% - ActorSettings corresponds to the various information (ex: AAI, seeding,
% ordering mode, etc.) that the load-balancer sets for each newly created actor
%
% - ActorName the name of the actor
%
% - SchedulingSettings describes how this test actor is to drive its scheduling,
% among:
%
%  - { periodic, P } where P is the requested number of ticks between two
%  spontaneous behaviours
%
%  - { erratic, MinRange } where MinRange allows to set up to how many ticks
%  should elapse in general between two erratic scheduling
%
% - CreationSettings describes if and how this test actor is to create other
% actors, among:
%
%  - no_creation: never creates an actor
%
%  - { InterCount, KindOfCreatedActor } where Count specifies the number of
%  spontaneous schedulings before an actor is created, and KindOfCreatedActor,
%  describes its policies, thanks to a pair { SchedulingPolicy, CreationPolicy }
%
% - TerminationTickOffset the duration after which this actor should terminate
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), scheduling_policy(), creation_policy(),
				 class_TimeManager:tick_offset() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% Cannot use 'output' yet (no talkative attribute):
	%io:format( "Creating a test actor ~p with parameters:~n~p.~n",
	%	[ self(), [ ?wooper_construct_parameters ] ] ),

	% First the direct mother classes, then this class-specific actions:
	%
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),


	InitialCountdown = case CreationPolicy of

		no_creation ->
			no_creation;

		{ InterCount, _KindOfCreatedActor } ->
			InterCount

	end,

	TraceState = setAttributes( ActorState, [

		{ target_peers, [] },
		{ source_peers, [] },
		{ scheduling_settings, SchedulingPolicy },
		{ creation_settings, CreationPolicy },
		{ creation_countdown, InitialCountdown },
		{ created_actors, [] },
		{ hello_count, 0 },
		{ next_planned, undefined },
		{ termination_tick_offset, TerminationTickOffset },
		{ termination_initiated, false },

		% Useful to select console verbosity:
		{ talkative, false },
		%{ talkative, true },

		% Allows to avoid too many traces to be sent in some heavy tests:
		{ trace_intensity, low },

		% We define here 15 extra attributes, to have a more realistic emulation
		% of a usual model, each like:
		% (attribute_example: [{1276,6016,712254}, #Ref<0.0.0.40>,
		% {1276,6016,712259},#Ref<0.0.0.41>]}.
		%
		% Each of the values of these attributes adds 96 bytes of memory
		% footprint in 32-bit mode, and 192 bytes in 64-bit mode, thus these 15
		% attributes add 1440 bytes per instance in 32-bit, and 2880 bytes in
		% 64-bit.
		{ attr_1 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_2 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_3 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_4 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_5 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_6 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_7 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_8 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_9 , [ now(), make_ref(), now(), make_ref() ] },
		{ attr_10, [ now(), make_ref(), now(), make_ref() ] },
		{ attr_11, [ now(), make_ref(), now(), make_ref() ] },
		{ attr_12, [ now(), make_ref(), now(), make_ref() ] },
		{ attr_13, [ now(), make_ref(), now(), make_ref() ] },
		{ attr_14, [ now(), make_ref(), now(), make_ref() ] },
		{ attr_15, [ now(), make_ref(), now(), make_ref() ] },

		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization )}

								] ),

	?send_info_fmt( TraceState, "Creating a new test actor, "
					"terminating no sooner than tick offset #~w.",
					[ TerminationTickOffset ] ),

	TraceState.




% Overridden destructor.
%
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	%?info( "Deleting test actor." ),
	% erlang:unlink() not used, as done manager-side.
	%?debug( "Test actor deleted." ),
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

	%?info_fmt( "actSpontaneous: local agenda is ~p.",
	%			 [ ?getAttr(next_planned) ] ),

	case ?getAttr(trace_intensity) of

		high ->
			?info_fmt( "Test Actor acting, having for peers: ~p.~n",
					   [ ?getAttr(target_peers) ] );

		_ ->
			ok

	end,

	CurrentTickOffset = ?getAttr(current_tick_offset),
	TerminationOffset = ?getAttr(termination_tick_offset),

	UpdatedPlannedState = update_plan( CurrentTickOffset, State ),

	% Terminates if the termination offset is reached or exceeded:
	NewState = case CurrentTickOffset of

		PastOffset when PastOffset >= TerminationOffset ->
			terminate( PastOffset, UpdatedPlannedState );

		CurrentOffset ->
			% Non-termination behaviour:
			behave_normally( CurrentOffset, UpdatedPlannedState )

	end,

	?wooper_return_state_only( NewState ).



% Overridden, in order to synchronise correctly the internal planning that this
% test actor maintains, and to start its behaviour.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	%io:format( "class_TestActor: onFirstDiasca called at diasca ~B.~n",
	%		  [ ?getAttr(current_diasca) ] ),

	TargetOffset = ?getAttr(current_tick_offset) + 1,

	% We are allowed to send actor messages from this very first diasca, let's
	% do it if possible, and if yes in a similarly random way:
	%
	% (thus we can reach diasca 2)
	%
	SentState = case ?getAttr(target_peers) of

					[] ->
						% Nobody to contact:
						State;

					[ TargetPeer | _T ] ->

						RandomValue = 3 * ?getAttr(actor_abstract_id)
							   + 2 * TargetOffset + ?getAttr(current_diasca),

						case RandomValue rem 2 of

							0 ->
								State;

							1 ->
								class_Actor:send_actor_message( TargetPeer,
									   { hello, [ ?getAttr(name) ] }, State )

						end

	end,

	% Update the internal information to plan a next action as well:
	PlanState = setAttribute( SentState, next_planned, [ TargetOffset ] ),

	UpdatedState = executeOneway( PlanState, addSpontaneousTick, TargetOffset ),

	?wooper_return_state_only( UpdatedState ).




% Manages the planned spontaneous actions.
%
% Returns an updated state.
%
% (helper)
%
-spec update_plan( class_TimeManager:tick_offset(), wooper:state() ) ->
						wooper:state().
update_plan( CurrentTickOffset, State ) ->

	NextPlanned = ?getAttr(next_planned),

	case lists:member( CurrentTickOffset, NextPlanned ) of

		true ->
			deleteFromAttribute( State, next_planned, CurrentTickOffset );

		false ->
			throw( { spontaneous_plan_error, CurrentTickOffset, NextPlanned } )

	end.



% Manages the termination behaviour of this actor.
%
% Returns an updated state.
%
% (helper)
%
-spec terminate( class_TimeManager:tick_offset(), wooper:state() ) ->
						wooper:state().
terminate( PastOffset, State ) ->

	case ?getAttr(termination_initiated) of

		false ->

			?info( "Test Actor preparing deferred termination." ),

			% Source and target peers must be notified here, otherwise, next
			% time they will send a message to this actor, they will hang
			% forever:
			%
			% (we are at {T,D}, termination process starts)
			%
			% (this returns a new state)
			%
			notify_termination( State );

		true ->

			% We are at least at {T,D+1}.

			TerminatingState = executeOneway( State,
											 declareTermination ),

			output( TerminatingState,
				   io_lib:format( " - ~w terminating at #~B~n",
								  [ self(), PastOffset ] ) ),

			TerminatingState

	end.



% Manages the normal, non-termination behaviour of this actor.
%
% Returns an updated state.
%
% (helper)
%
-spec behave_normally( class_TimeManager:tick_offset(), wooper:state() ) ->
						wooper:state().
behave_normally( CurrentOffset, State ) ->

	output( State, io_lib:format( " - ~w acting spontaneously at #~B on ~s~n",
					[ self(), CurrentOffset, net_utils:localnode() ] ) ),

	CreationState = manage_actor_creation( CurrentOffset, State ),

	% Erratic yet reproducible scheduling, always in the future, most actors
	% should have a different scheduling:
	%
	NextSpontaneousOffset = determine_next_spontaneous_tick( CurrentOffset,
															 CreationState ),

	TerminationOffset = ?getAttr(termination_tick_offset),

	% We should not plan a post-termination action:
	FutureState = case NextSpontaneousOffset of

					undefined ->
						  % Nothing special to schedule:
						  CreationState;

					LateOffset when LateOffset >= TerminationOffset ->
						  % Here we clamp to the termination tick and prepare
						  % for the termination:
						  InternalState = appendToAttribute( CreationState,
								  next_planned, TerminationOffset ),

						  executeOneway( InternalState, addSpontaneousTick,
										 TerminationOffset );

					NormalOffset ->

						  InternalState = appendToAttribute( CreationState,
								  next_planned, NormalOffset ),

						  executeOneway( InternalState, addSpontaneousTick,
										 NormalOffset )
	end,





	% Resets the count each tick, for the next one:
	HelloState = say_hello( FutureState ),

	setAttribute( HelloState, hello_count, 0 ).



% Determines the next spontaneous tick (if any), or 'undefined'.
%
% Returns an updated tick offset.
%
% (helper)
%
-spec determine_next_spontaneous_tick( class_TimeManager:tick_offset(),
			wooper:state() ) -> 'undefined' | class_TimeManager:tick_offset().
determine_next_spontaneous_tick( CurrentOffset, State ) ->

	TargetOffset = case ?getAttr(scheduling_settings) of

		{ periodic, Period } ->
			CurrentOffset + Period;

		{ erratic, MinRange } ->

			% Changing, between actors and between ticks of any given actor:
			VariableOffset = 2 * ?getAttr(actor_abstract_id) + CurrentOffset,

			CurrentOffset + 1 + VariableOffset rem
						( MinRange + length( ?getAttr(target_peers) ) )

	end,

	ensure_planned( TargetOffset, State ).



% Ensures that specified tick offset is locally declared in the local planning,
% exactly once.
%
% Returns an updated state.
%
% (helper)
%
-spec ensure_planned( class_TimeManager:tick_offset(), wooper:state() ) ->
				'undefined' | class_TimeManager:tick_offset().
ensure_planned( TickOffset, State ) ->

	% A given tick must not be declared more than once as a future action:
	case lists:member( TickOffset, ?getAttr(next_planned) ) of

		true ->
			undefined;

		false ->
			TickOffset

	end.



% Manages the actor creation.
%
% Returns an updated state.
%
% (helper)
%
-spec manage_actor_creation( class_TimeManager:tick_offset(),
							wooper:state() ) -> wooper:state().
manage_actor_creation( CurrentOffset, State ) ->

	case ?getAttr(creation_countdown) of

		no_creation ->
			%io:format( "(this actor does not create actors)~n" ),
			State;

		0 ->

			% Let's create a new actor:
			{ InterCount, { NewSchedulingPolicy, NewCreationPolicy } } =
				?getAttr(creation_settings),

			NewActorCount = length( ?getAttr(created_actors) ) + 1,

			NewActorName = lists:flatten( io_lib:format( "~s-~B",
									   [ ?getAttr(name), NewActorCount ] ) ),

			ActorTerminationTickOffset = CurrentOffset + 5
				+ class_RandomManager:get_uniform_value( 100 ),

			ConstructionParameters = [ NewActorName,
								_SchedulingPolicy=NewSchedulingPolicy,
								_CreationPolicy=NewCreationPolicy,
								ActorTerminationTickOffset ],

			% We perform roughly as many basic creations as placed ones:
			CreatedState = case ActorTerminationTickOffset rem 2 of

				0 ->
					output( State, io_lib:format(
							"(actor creation #~B for ~w just requested)~n",
							[ NewActorCount, self() ] ) ),

					class_Actor:create_actor( _CreatedClassname=class_TestActor,
											  ConstructionParameters, State );

				1 ->

					PlacementHint = ActorTerminationTickOffset,

					output( State, io_lib:format(
						  "(placed actor creation #~B for ~w just requested)~n",
						  [ NewActorCount, self() ] ) ),

					class_Actor:create_placed_actor(
							_CreatedClassname=class_TestActor,
							ConstructionParameters, PlacementHint, State )

			 end,

			% Resets creation timer:
			setAttribute( CreatedState, creation_countdown, InterCount );


		NonNullCount ->
			%io:format( "(this actor will create an actor later)~n" ),
			setAttribute( State, creation_countdown, NonNullCount-1 )

	end.



% Lowers the trace sending intensity of this actor.
%
% (helper)
%
-spec lowerTraceIntensity( wooper:state() ) -> wooper:state().
lowerTraceIntensity( State ) ->
	?wooper_return_state_only( setAttribute( State, trace_intensity, low ) ).



% Overridden oneway, called by the load balancer whenever it performed the
% corresponding creation request.
%
% (actor oneway)
%
-spec onActorCreated( wooper:state(),
	   { pid(), class_name(), [method_argument()] }, pid() ) -> wooper:state().
onActorCreated( State, { CreatedActorPid, ActorClassName,
				ActorConstructionParameters }, _LoadBalancerPid ) ->

	output( State, io_lib:format(
					 "### Actor ~w notified it created actor ~w.~n",
					 [ self(), CreatedActorPid ] ) ),

	?debug_fmt( "Test actor notified that actor ~w of class ~s was created "
		"on its behalf, with parameters ~p; adding it as a peer.",
		[ CreatedActorPid, ActorClassName, ActorConstructionParameters ] ),

	PeerState = add_peer( CreatedActorPid, State ),

	?wooper_return_state_only( appendToAttribute( PeerState, created_actors,
												  CreatedActorPid ) ).



% Adds specified peer to known target peers, if it is not already registered.
%
% To be called initially, from tests, before the simulation is started.
%
% (request, for synchronisation purpose).
%
-spec addInitialPeer( wooper:state(), pid() ) -> request_return( 'peer_added' ).
addInitialPeer( State, PeerPid ) ->

	NewState = add_peer( PeerPid, State ),

	?wooper_return_state_result( NewState, peer_added ).



% Adds specified peer to known target peers.
%
% To be called from an actor, while the simulation is running.
%
% (actor oneway)
%
-spec addPeer( wooper:state(), pid() ) -> oneway_return().
addPeer( State, PeerPid ) ->

	output( State, io_lib:format( "Peer ~w added to ~w.~n",
								 [ PeerPid, self() ] ) ),

	case ?getAttr(trace_intensity) of

		true ->
			?info_fmt( "Peer ~w added.~n", [ PeerPid ] );

		_ ->
			ok

	end,

	case lists:member( PeerPid, ?getAttr(target_peers) ) of

		true ->
			% Nothing to be done here, already there:
			?wooper_return_state_only( State );

		false ->
			?wooper_return_state_only(
					appendToAttribute( State, target_peers, PeerPid ) )

	end.



% Oneway called by a peer requesting this actor not to send it anymore messages,
% for example because this actor is terminating.
%
% (actor oneway)
%
-spec removePeer( wooper:state(), pid() ) -> oneway_return().
removePeer( State, PeerPid ) ->

	% A peer is expected to be registered exactly once.

	UpdatedState = deleteFromAttribute( State, target_peers, PeerPid ),

	?wooper_return_state_only( UpdatedState ).



% Oneway called by a peer telling this actor that it will never send hello
% messages any more to it, and thus that the latter can forget the former.
%
% (actor oneway)
%
-spec forgetPeer( wooper:state(), pid() ) -> oneway_return().
forgetPeer( State, PeerPid ) ->
	?wooper_return_state_only(
		deleteFromAttribute( State, source_peers, PeerPid ) ).




% Receives an 'hello' message.
%
% (oneway)
%
-spec hello( wooper:state(), class_Actor:internal_name(), pid() ) ->
				   oneway_return().
hello( State, SenderName, SenderPid ) ->

	%CurrentOffset = class_Actor:get_current_tick_offset( State ),

	%io:format( " - for ~w at #~B, instant_spontaneous_requested: ~p~n",
	%	 [ self(), CurrentOffset, ?getAttr(instant_spontaneous_requested) ] ),

	%output( State, io_lib:format(
	%	" - ~w is being said hello by ~s (i.e. ~w) at #~B.~n",
	%	[ self(), SenderName, SenderPid, CurrentOffset ] ) ),

	case ?getAttr(trace_intensity) of

		high ->
			?info_fmt( "Received an hello message from ~s (~w).",
					   [ SenderName, SenderPid ] );

		_ ->
			ok

	end,

	SourceState = case lists:member( SenderPid, ?getAttr(source_peers) ) of

		true ->
			State;

		false ->
			appendToAttribute( State, source_peers, SenderPid )

	end,

	?wooper_return_state_only( addToAttribute( SourceState, hello_count, 1 ) ).




% Static method section.



% Adds the list of specified peers to the specified target peer, synchronously.
%
% Note: define for convenience, to avoid code duplication in tests.
%
% (static)
%
-spec add_initial_peers( pid(), [pid()] ) -> basic_utils:void().
add_initial_peers( TargetPeer, Peers ) ->

	% We call a request to ensure we are synchronised (no race condition with
	% start message):
	[ TargetPeer ! { addInitialPeer, P, self() } || P <- Peers ],

	% A nice side effect is that we can run these operations in parallel:
	[ receive { wooper_result, peer_added } -> ok end ||
		_X <- lists:seq( 1, length( Peers ) ) ].




% Section for helper functions (not methods).



% Adds specified target peer to actor.
%
% Returns a new state.
%
% (helper)
%
add_peer( PeerPid, State ) ->

	case lists:member( PeerPid, ?getAttr(target_peers) ) of


		true ->
			% Already registered, nothing done:
			State;


		false ->

			output( State, io_lib:format( "Peer ~w added to ~w.~n",
										  [ PeerPid, self() ] ) ),

			case ?getAttr(trace_intensity) of

				true ->
					?info_fmt( "Peer ~w added.~n", [ PeerPid ] );

				_ ->
					ok

			end,

			appendToAttribute( State, target_peers, PeerPid )

	end.



% Says hello to all target peers.
%
% Returns an updated state.
%
% (helper)
%
say_hello( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	case ?getAttr(target_peers) of

		[] ->
			%?trace_fmt( "Test Actor acting spontaneously at tick offset #~B, "
			%			"but having no target peer to say hello to.",
			%			[ CurrentTickOffset ] ),
			State;

		Peers ->

			case ?getAttr(talkative) of

				true ->
					?info_fmt( "Test Actor acting spontaneously "
							   "at tick offset #~B, saying hello to ~w.",
							   [ CurrentTickOffset, Peers ] );

				false ->
					ok

			end,

			SendFun = fun( Peer, FunState ) ->

				output( State, io_lib:format(
								 "    ~w saying hello to ~w at #~B.~n",
								 [ self(), Peer, CurrentTickOffset ]  ) ),

				case ?getAttr(trace_intensity) of

					high ->
						?info_fmt( "Saying hello to ~w.", [ Peer ] );

					_ ->
						ok

				end,

				% Returns an updated state:
				class_Actor:send_actor_message( Peer,
					{ hello, [ ?getAttr(name) ] }, FunState )

			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, Peers )

	end.



% Notifies all source peers that this actor is terminating, thus they must not
% send any more messages to it, otherwise they will wait for ever for an answer
% and will stall the simulation.
%
% Returns an updated state.
%
% (helper)
%
notify_termination( State ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	SourcePeers = ?getAttr(source_peers),

	% First, tells the actors targeting this actor to stop, otherwise they will
	% hang:
	%
	SourceState = case SourcePeers of

		[] ->
			?trace_fmt( "Test Actor terminating at tick offset #~B, "
						"having no source peer to notify.",
						[ CurrentTickOffset ] ),
			State;

		SourcePeers ->
			?info_fmt( "Test Actor terminating at tick offset #~B, "
					   "requesting source peers ~w to stop sending messages.",
					   [ CurrentTickOffset, SourcePeers ] ),

			SourceSendFun = fun( Peer, FunState ) ->

				output( State, io_lib:format(
					"    ~w requests ~w to stop sending messages.~n",
					[ self(), Peer ] ) ),

				%?info_fmt( "Requesting ~w to stop sending messages.",
				%		  [ Peer ] ),

				% Returns an updated state:
				class_Actor:send_actor_message( Peer, removePeer, FunState )

			end,

			% Returns an updated state:
			lists:foldl( SourceSendFun, State, SourcePeers )

	end,


	TargetPeers = ?getAttr(target_peers),

	% Second, tells the targeted actors they will not be notified by this actor
	% anymore:
	TargetState = case ?getAttr(target_peers) of

		[] ->
			?trace_fmt( "Test Actor terminating at tick offset #~B, "
						"having no target peer to notify.",
						[ CurrentTickOffset ] ),
			SourceState;

		TargetPeers ->
			?info_fmt( "Test Actor terminating at tick offset #~B, "
					   "notifying target peers ~w to forget it.",
					   [ CurrentTickOffset, TargetPeers ] ),

			TargetSendFun = fun( Peer, FunState ) ->

				output( State, io_lib:format(
								 "    ~w requests ~w to forget it.~n",
								 [ self(), Peer ] ) ),

				%?info_fmt( "Requesting ~w to forget it.", [ Peer ] ),

				% Returns an updated state:
				class_Actor:send_actor_message( Peer, forgetPeer, FunState )

			end,

			% Returns an updated state:
			lists:foldl( TargetSendFun, SourceState, TargetPeers )

	end,

	TerminationDelay = case SourcePeers ++ TargetPeers of

			[] ->
					% Nothing to wait for, we can terminate immediately:
					0;

			_ ->
					% We have sent at least an actor message at this diasca D,
					% thus (depending on reordering) at D+1 the peers might
					% already send a message to this actor before processing the
					% message that was just sent; this second message would be
					% processed at D+2, thus we cannot terminate before D+3:
					3

	end,

	%TerminationDelay = unlimited,

	executeOneway( TargetState, declareTermination, TerminationDelay ).



% Outputs specified message in console, if talkative.
%
% (helper)
%
output( State, Message ) ->

	case ?getAttr(talkative) of

		true ->
			io:format( Message );

		false ->
			ok

	end.
