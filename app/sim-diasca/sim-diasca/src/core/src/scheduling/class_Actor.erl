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



% Actor base class.
%
% All elements of a simulation having to exchange messages with others and/or
% having a spontaneous behaviour to develop should be instances of this class
% (directly or not), as they need to be tightly scheduled by a TimeManager.
%
% See class_Actor_test.erl
%
-module(class_Actor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, ActorName ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, simulationStarted/3, simulationEnded/1,
		 onFirstDiasca/2, beginTick/2, beginDiasca/3, beginTerminationDiasca/3,
		 scheduleNextSpontaneousTick/1, addSpontaneousTick/2,
		 addSpontaneousTicks/2,
		 withdrawSpontaneousTick/2, withdrawSpontaneousTicks/2,
		 declareTermination/1, declareTermination/2,
		 timeManagerShutdown/1, receiveActorMessage/6,
		 acknowledgeMessage/2, actSpontaneous/1,
		 getSimulationTick/1, getSimulationTickOffset/1, getSimulationDate/1,
		 getHostingNode/1,
		 getTextualTimings/1, convertSecondsToTicks/2, convertTicksToSeconds/2,
		 nudge/2, getAAI/1, getActorInfo/1, onActorCreated/5,
		 triggerNameNotification/2,
		 relink/1 ).



% Static method declarations (to be directly called from module):
%
-define( wooper_static_method_export,
		 create_initial_actor/2, create_initial_actor/3,
		 create_initial_actors/1, create_initial_actors/2,
		 create_initial_placed_actor/3, create_initial_placed_actor/4 ).



% Helper functions.


% Helpers for scheduling, message sending, creation, etc.:
%
-export([ get_abstract_identifier/1, get_deployed_root_directory/1,
		  convert_seconds_to_ticks/2, convert_seconds_to_ticks/3,
		  convert_seconds_to_non_null_ticks/2,
		  convert_seconds_to_non_null_ticks/3,
		  convert_ticks_to_seconds/2,
		  get_current_tick/1, get_current_tick_offset/1,
		  send_actor_message/3, send_delayed_actor_message/4,
		  send_actor_messages/3,
		  add_spontaneous_tick/2, add_spontaneous_ticks/2,
		  withdraw_spontaneous_tick/2, withdraw_spontaneous_ticks/2,
		  declare_termination/2, get_name/1, is_running/1,
		  create_actor/3, create_placed_actor/4,
		  declare_probe/6 ]).



% Helpers related to the data-exchange service:
%
-export([ enable_data_exchange/1,
		  define_data/2, define_data/3, define_data/4,
		  modify_data/2, modify_data/3, modify_data/4,
		  read_data/2, read_qualified_data/2 ]).



% Exported to be shared (ex: with class_BroadcastingActor):
-export([ check_spontaneous_tick_consistency/2, validate_scheduling_outcome/1,
		  check_diasca_consistency/3, process_last_diasca_messages/3,
		  update_agenda_with/3 ]).



% Abstract Actor Identifier (AAI).
%
% Live actors have non-null AAI (starting at 1, the AAI of the load balancer); a
% null AAI designates a "zombi" actor, i.e. information about an actor that does
% not exist anymore.
%
-type aai() :: integer().


% Name of an actor, as supplied by the user:
-type name() :: string().


% Name of an actor, as stored internally:
-type internal_name() :: binary().


% Specifies how an actor organises its termination process, from its declaration
% to its realisation: either a positive count of diascas (possibly zero) to
% wait, or 'unlimited', so that the actor is deleted only at the next tick.
%
-type termination_delay() :: class_TimeManager:diasca() | 'unlimited'.


% Used by actors internally, to record the new next action they plan (in the
% next_action attribute): respectively, either nothing, or needing a new diasca
% (either to process an actor message or to go through its termination
% procedure), or having decided to terminate (either being in the course of, or
% considering having terminated for good)
%
-type next_actor_action() :: 'no_diasca_requested' | 'new_diasca_needed'
				| { 'terminating', termination_delay() } | 'terminated'.



% Used by actors in their dialog with their time manager, to notify it of the
% next action they plan: respectively, either nothing, or needing a new diasca
% to manage messages, or terminating with intermediary diascas, or terminating
% with no specific limit in terms of diascas, or having terminated for good.
%
-type next_reported_action() :: 'no_diasca_requested' | 'new_diasca_needed'
				| 'terminating' | 'terminating_unlimited' | 'terminated'.



% Ordered list of tick offsets at which this actor expects to develop a
% spontaneous behaviour:
%
-type agenda() :: [ class_TimeManager:tick_offset() ].


-type actor_oneway_name() :: oneway_name().

-type actor_oneway_argument() :: any().

-type actor_oneway_return() :: oneway_return().


% Just for documentation purpose:
%
-type actor_pid() :: pid().



% Describes a pending actor message, as stored by an actor once having received
% it.
%
-record( actor_message, {

		% The tick offset at which this actor message has been sent and is to be
		% processed:
		tick_offset :: class_TimeManager:tick_offset(),

		% The target diasca at which this actor message is to be processed (the
		% next diasca after the one of the sending):
		diasca :: class_TimeManager:diasca(),

		% The PID of the sending actor:
		sender_pid :: pid(),

		% The AAI of the sending actor:
		sender_aai :: aai(),

		% The actual actor message, which is a oneway call:
		actual_message :: oneway_call()

						 } ).


-type actor_message() :: #actor_message{}.


-type instance_creation_spec() :: { class_name(), [ method_argument() ] }
			  | { class_name(), [ method_argument() ],
				  class_LoadBalancer:placement_hint() }.


% Tells what kind of reordering of actor messages is wanted, among:
%
% - 'unordered': the actor processes messages in their arrival order; smallest
% possible overhead, but the order is dictated by the technical context, and is
% not reproducible
%
% - 'constant_arbitrary_order': messages are ordered only according to their
% content and their sender; this order is totally reproducible, but will only
% exhibit one of the many possible trajectories of the simulated system
%
% - 'constant_permuted_order': messages are first reordered like in the case of
% 'constant_arbitrary_order', then they are uniformly permuted based on the
% random seed the actor received on creation, either coming from a reproducible
% or ergodic context; all possible trajectories have a fair probability of being
% chosen by this more advanced reordering
%
-type message_ordering_mode() :: 'unordered' | 'constant_arbitrary_order'
								 | 'constant_permuted_order'.


% For #actor_settings:
-include("class_LoadBalancer.hrl").

% The record is defined in class_LoadBalancer.hrl:
-type actor_settings() :: #actor_settings{}.


-export_type([ aai/0, name/0, internal_name/0,
			   next_actor_action/0, next_reported_action/0, agenda/0,
			   actor_oneway_name/0, actor_oneway_argument/0,
			   actor_oneway_return/0, actor_pid/0, actor_message/0,
			   instance_creation_spec/0, message_ordering_mode/0,
			   actor_settings/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For time_manager_name, virtual_seconds, etc.:
-include("class_TimeManager.hrl").


% For actor_info() type:
-include("class_InstanceTracker.hrl").


% Implementation notes:


% About stochastic management.

% The initial implementation for stochastic actors was complex and added some
% constraints; a system where actors could wait for other actors and be
% waited by others was added.
%
% The newer implementation for stochastic actors is better in all aspects, and
% does not need the waiting mechanism any more.
%
% Therefore this mechanism to manage wait graphs is not used any more.



% About timing specifications.

% In terms of timings, a simulation starts at an initial tick which is known of
% all schedule-related agents. Therefore only offsets relative to the beginning
% of the simulation time are used, not absolute ticks, which would be far larger
% and would probably exceed the native integer type, therefore wasting resources
% with big integers.
%
% The actor class inherits from the trace emitter class (which itself must
% maintain the emitter time in order to timestamp traces), and the two
% attributes of the latter, 'initial_tick' and 'current_tick_offset',
% correspond, in the context of an actor, respectively to the initial tick of
% the simulation (thus this same value is shared by all actors) and the offset
% of the current tick of this actor relatively to this initial simulation tick.
%
% A new attribute is defined, 'actor_creation_tick_offset', which records when
% the actor is created, relatively to the beginning of the simulation (hence
% relatively to 'initial_tick'). This allows for example to keep track of the
% age of that actor. Therefore class_TraceEmitter:get_current_tick/1 (which
% returns initial_tick+current_tick_offset) could be used directly by
% actors. However the best practise is surely to call
% class_Actor:get_current_tick/1 or, still better,
% class_Actor:get_current_tick_offset/1 since usually offsets are more
% appropriate than absolute ticks.


% Note that a (most probably small) speed-up could be obtained by not performing
% further permutations in reproducible mode with a default seed: the basic
% arbitrary reordering is as good as one which is additionally uniformly
% permuted.


% Some attributes (namely: 'previous_schedule',
% 'expected_next_spontaneous_tick', 'current_agenda') are used only for checking
% purposes: they allow to verify whether the actual scheduling, as decided by
% the time manager, is indeed compliant with what could be forecast from the
% point of view of each actor.

% As a consequence, in release (non-debug) mode they could be removed as a
% whole, with no harm, with a certain resulting speed gain.



% About scheduling.

% An actor may not be scheduled at all during a tick (which itself may or may
% not be scheduled), or the actor may be scheduled, for a spontaneous action
% (then at diasca 0) and/or for any number of times to perform triggered actions
% (then at strictly positive diascas), each time an actor message was sent to it
% on the previous diasca, or possibly to terminate.

% Once an actor finishes its scheduling, it notifies its time manager both of
% the additional ticks on which it is to perform spontaneous actions (if any)
% and of the ticks for which it already declared such actions but that are to be
% withdrawn (if any).
%
% The withdrawal of ticks is applied before the addition, so that it is not
% possible to add and remove the same tick in the same operation.



% About parallelism.

% We try to offset as much as possible processings so that they happen once the
% actor answered to the time-manager.



% About actor creation.

% When an actor is created (either initially, i.e. from the simulation case, or
% from another actor, i.e. in the course of the simulation), the actual spawn
% triggers its constructor which in turn will trigger the subscription of this
% actor. If the simulation is already running, it will result into the execution
% of the simulationStarted/3 request (otherwise it will happen a future
% simulation start).
%
% If the simulation is already running, we want to avoid simply scheduling first
% this newly created actor thanks to its actSpontaneous/1 actor oneway, as it
% would have then to wait for the next tick: creation chains would thus suffer
% from a potentially problematic engine-induced latency, which is not wanted.
%
% So we decided that when the request for an actor creation is made at diasca D
% (in the course of the simulation), not only the actual creation (spawn and
% synchronisation, done by the load balancer) will happen at D+1 and the
% *creator* will be notified of it at D+2 (by an onActorCreated/5 call), but
% also the created actor will also be triggered, at this already scheduled
% diasca (D+2), by an onFirstDiasca/2 actor oneway call. This way, this actor
% may decide to send actor messages and/or define its next spontaneous ticks
% and/or create other actor(s), allowing for instantaneous chained creations.
%
% If the simulation was not running yet, this is an initial creation and the
% onFirstDiasca/2 actor oneway will be triggered as soon as possible, i.e. at
% diasca 1 of the initial tick. Of course no onActorCreated/5 actor oneway call
% will be made, as there is not even a creating actor in this case.



% About actor synchronization.

% In a few words: whether or not the simulation is already started, on actor
% creation the time manager calls the simulationStarted/3 request on the actor
% for pure engine-level synchronisation, while the load-balancer sends to the
% actor the onFirstDiasca/2 actor oneway for pure model-level
% initialization/synchronisation.
%
% More precisely: created actors have to initialize themselves, and not all
% relevant information are always available at their creation (ex: simulation
% starting tick). For any late model-specific set-up, knowing that their
% simulationStarted/3 method is not meant to be overridden (it is used privately
% by the engine, notably to tell the actor about the simulation initial tick and
% the current timestamp), their onFirstDiasca/2 actor oneway (which is not used
% at all internally by the engine) is called as soon as they are synchronised
% with the simulation. They can then initialize themselves at will, with all
% necessary information. As this is an actor oneway, it must be called by
% another actor, and the load balancer is the most suitable for that.
%
%
% So, if we are performing an initial creation, the actor will be created, but
% not scheduled, as long as the simulation is not started; when this happens:
%
% - its simulationStarted/3 request is first called, by its time manager
%
% - then its onFirstDiasca/2 actor oneway is called (by the load balancer),
% through the usual process of inter-actor exchanges
%
%
% Otherwise, the actor is created in the course of the simulation, and then,
% quite similarly:
%
% - it calls by itself its simulationStarted/3 request, as a part of its
% construction phase (just having suscribed to its time manager)
%
% - then its onFirstDiasca/2 actor oneway is called (by the load balancer),
% through the usual process of inter-actor exchanges



% About actor termination.

% The termination of an actor is solely decided by itself: an actor cannot
% trigger directly the deletion of another, as in the general case a termination
% involves an actor-specified coordinated tear-down phase. For example, the
% actor might have to notify other actors that it is to terminate, so that they
% know they should not send actor messages to it anymore.
%
% Termination is a three-step process, that we want, similarly to creations, to
% happen possibly during the same tick (thus leading to intermediary diascas
% being needed):
%
%  1. an actor declares at a given diasca Dt that, based on its own logic, it
%  now intends to terminate (see declareTermination/{1,2}), possibly specifying
%  the number of diascas it requests to elapse before its actual termination can
%  occur; this is useful for example when a notification of termination is to
%  propagate first into a chain of other actors, which otherwise might keep on
%  sending actor messages to this actor; this duration (in logical steps) before
%  the termination can happen is either a positive count of intermediary diascas
%  to wait (possibly 0) or the 'unlimited' atom, in which case the termination
%  is to occur no sooner than the next tick, regardless of the number of diascas
%  which happen to be needed in-between (this is useful when not needing to
%  assess that a termination is to happen in a bounded duration in diascas); not
%  specifying a duration (see declareTermination/1) defaults to 'unlimited'
%
%  As a consequence, the next_action of this actor becomes 'terminated', if D=0,
%  or { terminating, D } if D is either a strictly positive number of diascas or
%  is 'unlimited'
%
%  2. at the end of the diasca Dt (see notify_diasca_ended/1), if:
%
%   - D is 'unlimited', then the actor will stay in the 'terminating' state
%   until the end of the current tick (regardless of the number of diascas), and
%   then will be immediately deleted; in between, the actor will not be
%   specifically scheduled anymore
%
%   - D is a strictly positive integer; then the actor stays in 'terminating'
%   state, D is decremented and a new diasca is requested, at which the actor
%   will be scheduled again (only to manage its termination phase)

%   - D is zero, then the actor goes in 'terminated' state, will not be
%   scheduled anymore, and the time manager will be allowed to delete it from
%   the next diasca; in practise the actual deletion will generally be deferred
%   to the next tick, so that detecting unexpected interactions with this
%   terminated actor is easier (unlike the D=unlimited case, here the actor is
%   already in the 'terminated' case)
%
%  3. the actor is deleted for good by the time manager
%
% TO-DO: a priori, if no actor message is sent anymore, the tick can be ended
% directly, regardless of any remaining termination diasca (no need to wait for
% them).


% About serialisation.
%
% No corner case, we use the default WOOPER do-nothing hooks for the
% WOOPER-provided serialise/3 request, as each model instance is serialised
% properly thanks to the default mechanisms.



% The class-specific attributes of an actor instance are:
%
% - actor_abstract_id :: aai() corresponds to this actor abstract identifier
% (AAI), as assigned by the load balancer
%
% - load_balancer_pid :: pid() is the PID of the load balancer
%
% - actor_creation_tick_offset :: class_TimeManager:tick_offset() is the offset
% between the beginning of the simulation (initial_tick) and the tick at which
% the actor was created; it will be set once that actor is synchronized (when
% entering the simulation)
%
% - waited_acks :: [ pid() ] is a list of the PID of the actors to which this
% actor sent an actor message this diasca; it allows it to notify adequately the
% time manager that its diasca is finished indeed
%
% - previous_schedule :: maybe( class_TimeManager:logical_timestamp() )
% corresponds to the logical timestamp of the latest scheduling (spontaneous or
% triggered) of this actor (only used for scheduling checking, not strictly
% necessary)
%
% - added_spontaneous_ticks :: [ class_TimeManager:tick_offset() ] is a list of
% the future tick offsets (if any) at which this actor requests to develop
% additional spontaneous actions; a tick might be declared multiple times by an
% actor; it will then be scheduled only once
%
% - withdrawn_spontaneous_ticks :: [ class_TimeManager:tick_offset() ] is a list
% of the future tick offsets (if any) that this actor requested in the past, but
% wants to withdraw now; withdrawing a non-declared spontaneous tick is not
% allowed and will be detected
%
% - next_action :: next_actor_action() describes the next action this actor
% requests (no diasca, a new diasca, terminating or terminated)
%
% - expected_next_spontaneous_tick :: class_TimeManager:tick_offset() is the
% tick offset at which this actor expects to be scheduled next for a spontaneous
% behaviour (only used for scheduling checking, no strictly necessary)
%
% - current_agenda :: [ class_TimeManager:tick_offset() ] is the ordered list of
% tick offsets at which this actor expects to develop a spontaneous behaviour
% (only used for scheduling checking, not strictly necessary)
%
% - last_sent_schedule_trigger :: basic_utils:maybe(
% class_TimeManager:logical_timestamp() ) records whether this actor has already
% sent a schedule_trigger message to its time manager specifying timestamp {T,D}
% (corresponding to the expected processing of the message, hence for its next
% scheduling); this allows to avoid that an actor having received a large number
% of actor messages sends consequently a large number of (local) messages to its
% time manager, and also spares the need for the time manager to ensure this
% actor is registered up to once for any diasca
%
% - pending_messages :: [ actor_message() ] is the list of actor messages that
% this actor will have to reorder and then to process on the next tick; the list
% is initially built on receiving order (for what it is worth)
%
% - current_tick_offset :: class_TimeManager:tick_offset() is the current tick
% offset of this actor (actually this emitter is inherited from
% class_TraceEmitter)
%
% - current_diasca :: diasca() stores the current diasca of this actor
%
% - exchange_settings is an opaque datastructure (actually either the
% 'undefined' atom, which is the default, or a { RootDataExchangerPid,
% LocalDataExchangerPid } pair), used to enable an actor to make use of the
% data-exchange service
%
% - time_manager_pid :: pid() records the PID of the time manager which drives
% this actor directly
%
% - random_seed :: random_utils:seed() is a triplet corresponding to the seed of
% this actor, as assigned by the time manager
%
% - message_ordering_mode :: message_ordering_mode() dictates the reordering of
% messages that this actor is to performed
%
% - simulation_tick_duration :: class_TimeManager:virtual_seconds() is the
% actual duration, in floating-point milliseconds (in virtual time), between two
% simulation ticks




% Constructs a new simulation actor:
%
% - ActorSettings :: actor_settings() describes the actor abstract identifier
% (AAI) and seed of this actor, as assigned by the load balancer
%
% - ActorName :: string() is a human-readable name for that actor (as a plain
% string); it is preferably not too long and without whitespaces
%
-spec construct( wooper:state(), actor_settings(), name() ) -> wooper:state().
construct( State,
		   #actor_settings{ aai=ActorAbstractIdentifier,
							seed=ActorSeed,
							message_ordering_mode=OrderingMode },
		   ActorName ) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, ActorName ),

	% Then the class-specific actions:

	% We do not keep the seed once initialised:
	random_utils:start_random_source( ActorSeed ),

	?send_trace_fmt( TraceState,
		"Creating a new actor named '~s' and bearing abstract identifier ~B.",
		[ ActorName, ActorAbstractIdentifier ] ),


	% The load balancer being itself an actor, by construction it is not
	% registered yet, with use its reserved AAI (1) to overcome that:
	%
	BalancerPid = case ActorAbstractIdentifier of

					  1 ->
						  self();

					  _OtherAAI ->
						  class_LoadBalancer:get_balancer()

				  end,

	StartingState = setAttributes( TraceState, [

		% Set again, for clarity:
		{ initial_tick, undefined },

		{ actor_abstract_id, ActorAbstractIdentifier },
		{ load_balancer_pid, BalancerPid },
		{ actor_creation_tick_offset, undefined },
		{ waited_acks, [] },
		{ previous_schedule, undefined },
		{ added_spontaneous_ticks, [] },
		{ withdrawn_spontaneous_ticks, [] },
		{ exchange_settings, undefined },
		{ next_action, no_diasca_requested },
		{ expected_next_spontaneous_tick, undefined },
		{ current_agenda, [] },
		{ last_sent_schedule_trigger, undefined },
		{ pending_messages, [] },
		{ current_tick_offset, undefined },
		{ current_diasca, undefined },
		% exchange_settings, time_manager_pid set later.
		{ random_seed, ActorSeed },
		{ message_ordering_mode, OrderingMode },
		% simulation_tick_duration set later.
		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

												]
								  ),

	% Find the time manager and subscribe:
	TimeManagerPid = try basic_utils:wait_for_local_registration_of(
			class_TimeManager:get_registration_name() ) of

		Pid ->
			Pid

	catch

		Exception ->

			?send_error_fmt( StartingState,
				"Actor constructor failed: "
				"unable to find time manager: ~w, stopping actor.",
				[ Exception ] ),

			throw( { actor_construction_failed, time_manager_not_found,
				Exception } )

	end,

	%?send_debug( StartingState, "Time manager found, subscribing." ),

	TimeAwareState = setAttribute( StartingState, time_manager_pid,
								   TimeManagerPid ),

	BinActorName = text_utils:string_to_binary( ActorName ),

	% We want the actual class name of this actor instance, not, for example,
	% just 'class_Actor':
	%
	{ _SameState, ClassName } = executeRequest( TimeAwareState, getClassName ),

	% Registers to the time manager, allows it to associate its instance and
	% class names, and AAI to its PID and get time information:
	%
	TimeManagerPid ! { subscribe,
				 [ ActorAbstractIdentifier, BinActorName, ClassName ], self() },

	MaxWaitedDuration = get_maximum_subscription_duration(),

	% Hijacks the WOOPER main loop to force message selection:
	{ TickDuration, StartInformation } = receive

		{ wooper_result, { time_subscribed, TickDur, StartInfo } } ->
				 { TickDur, StartInfo };

		{ wooper_result, already_time_subscribed } ->
			?send_error( TimeAwareState,"Actor constructor failed: "
				"already subscribed to time manager, stopping actor." ),
			throw( { actor_construction_failed, already_time_subscribed } )

	after MaxWaitedDuration ->

		?send_error( TimeAwareState, "Actor constructor failed: "
			"time-out while waiting for time manager, stopping actor." ),

		throw( { actor_construction_failed, time_subscription_timed_out } )

	end,

	TickState = setAttribute( TimeAwareState, simulation_tick_duration,
							  TickDuration ),

	case StartInformation of

		not_started_yet ->
			% Simulation not running, the simulationStarted message will be sent
			% later when appropriate:
			TickState;

		{ ActualInitialTick, FirstScheduledOffset } ->
			% Created on the course of the simulation, calling simulationStarted
			% atomically:
			{ StartedState, { actor_started, _SelfPid } } = executeRequest(
					  TickState, simulationStarted,
					  [ ActualInitialTick, FirstScheduledOffset ] ),
			StartedState

	end.




% Overridden destructor.
%
% Unsubscribing from the time manager supposed already done, thanks to a
% termination message.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	%?trace( "Deleting actor." ),

	InstanceTrackerPid = class_InstanceTracker:get_local_tracker(),

	{ SameState, ActualClassName } = executeRequest( State, getClassName ),

	% Helps keeping track of current living actors:
	InstanceTrackerPid ! { unregisterActor, [ self(), ActualClassName ] },

	%?debug( "Actor deleted." ),

	% Then allow chaining:
	SameState.




% Methods section.


% Management section of the actor.



% Request called:
%
% - by the time manager when this actor enters the simulation while it was not
% running at the actor's subscription
%
% - directly by this actor itself if upon subscribing it is told that the
% simulation is already running
%
% Note: this simulationStarted/3 method belongs to the engine internals and
% should not be overridden. See the onFirstDiasca/2 actor oneway instead, whose
% sole purpose is to ease the implementation of models (not used by the engine).
%
% Returns { started, Pid }, Pid being the PID of this actor (the atom is used as
% a security to discriminate among answers to requests).
%
% (request)
%
-spec simulationStarted( wooper:state(), class_TimeManager:tick(),
		   class_TimeManager:logical_timestamp() ) ->
							   request_return( { 'actor_started', pid() } ).
simulationStarted( State, SimulationInitialTick,
			 _CurrentTimestamp={ CurrentTickOffset, CurrentDiasca } ) ->

	%?trace_fmt( "This initial actor is notified of simulation start, "
	%            "with a simulation initial tick of ~B, "
	%            "while current tick offset is #~B and diasca ~B.",
	%			 [ SimulationInitialTick, CurrentTickOffset, CurrentDiasca ] ),

	%io:format( "class_Actor:simulationStarted called for ~w, "
	%			"with simulation initial tick ~p, with current tick #~B "
	%			"and diasca ~B.~n",
	%			[ self(), SimulationInitialTick, CurrentTickOffset,
	%			CurrentDiasca ] ),

	?checkUndefined(initial_tick),
	?checkUndefined(current_tick_offset),
	?checkUndefined(current_diasca),
	?checkUndefined(actor_creation_tick_offset),
	false = is_running( State ),

	StartedState = setAttributes( State, [

		{ initial_tick, SimulationInitialTick },

		{ current_tick_offset, CurrentTickOffset },

		% Otherwise onFirstDiasca/2 will trigger some checking errors:
		{ current_diasca, CurrentDiasca },

		{ actor_creation_tick_offset, CurrentTickOffset }

		% current_agenda already set to [] for all actors except the
		% load-balancer (for bootstrapping purposes).

														] ),

	?wooper_return_state_result( StartedState,
								_Result={ actor_started, self() } ).



% Notifies this actor that the simulation ended.
%
% For the vast majority of actors (but unlike the load balancer for example),
% this means deletion (overridden for the load balancer, which has a different
% life cycle).
%
% (oneway)
%
-spec simulationEnded( wooper:state() ) -> oneway_return().
simulationEnded( State ) ->

	% By default, means immediate deletion:
	self() ! delete,

	?wooper_return_state_only( State ).



% This actor oneway is automatically called the next diasca after an actor is
% created or, if the simulation was not running, on diasca 1 (i.e. just after
% the spontaneous behaviours).
%
% This method is meant to be overridden, knowing that otherwise the created
% actor will be schedule only once (this time), and never again.
%
% Its purpose is to gather one-time only operations, otherwise each call of
% actSpontaneous/1 would have to test whether it is the first one or not.
%
% A typical yet simplistic implementation can be:
%
% onFirstDiasca( State, _SendingActorPid ) ->
%	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
%	?wooper_return_state_only( ScheduledState ).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	{ _IgnoredState, ActualClassname } = executeRequest( State, getClassName ),

	?warning_fmt( "The onFirstDiasca/2 actor oneway has not been overridden "
				  "for class '~s' (instances will thus remain purely passive).",
				  [ ActualClassname ] ),

	% Notably we could expect the following oneways (or their corresponding
	% helper functions) to be called here: scheduleNextSpontaneousTick/1,
	% addSpontaneousTick/2, addSpontaneousTicks/2, withdrawnSpontaneousTick/2,
	% withdrawnSpontaneousTicks/2, declareTermination/{1,2}, etc.

	% Another option is simply to force the overriding of this method:
	throw( { not_overridden, onFirstDiasca, 2 } ).

	% A last possible option would have been to have by default:
	%ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
	%?wooper_return_state_only( ScheduledState ).

	%?wooper_return_state_only( State ).



% Called by the local time manager in order to schedule this actor for a new
% tick, starting with its spontaneous behaviour (diasca 0).
%
% Returns an updated state, and triggers back a notification to the
% corresponding time manager when the spontaneous action has been completed.
%
% (oneway)
%
-spec beginTick( wooper:state(), class_TimeManager:tick_offset() ) ->
								  oneway_return().
beginTick( State, NewTickOffset ) ->

	Agenda = ?getAttr(current_agenda),

	%io:format( " beginTick for actor ~w at tick offset #~B, with agenda ~w.~n",
	%   [ self(), NewTickOffset, Agenda ] ),

	% Comment to disable these checkings:
	check_spontaneous_tick_consistency( NewTickOffset, State ),


	NewTickOffset = hd( Agenda ),
	[] = ?getAttr(added_spontaneous_ticks),
	[] = ?getAttr(withdrawn_spontaneous_ticks),

	% Removes the first entry of this agenda, this new tick offset:
	SpontaneousUpdatedState = setAttribute( State, current_agenda,
		tl( Agenda ) ),


	% Other attributes set at the end of previous scheduling:
	PreState = setAttributes( SpontaneousUpdatedState, [

			{ current_tick_offset, NewTickOffset },
			{ current_diasca, 0 }

														] ),

	SpontaneousState = executeOneway( PreState, actSpontaneous ),

	% Note: we are not checking the correctness of the engine here, we ensure
	% models are properly written (hence this should not be commented out):
	%
	validate_scheduling_outcome( SpontaneousState ),

	% The 'actSpontaneous' method might have sent actor messages:
	AckState = case getAttribute( SpontaneousState, waited_acks ) of

		[] ->
				notify_diasca_ended( SpontaneousState );

		_StillWaiting ->
				% End of tick to be determined by acknowledgeMessage/2:
				SpontaneousState

	end,

	?wooper_return_state_only( AckState ).



% Checkings, in the case of the beginning of a new tick.
%
check_spontaneous_tick_consistency( NewTickOffset, State ) ->

	% First, time must always progress:
	check_time_consistency_at_new_tick( NewTickOffset, State ),

	% Checks that actor messages are within possible past and future bounds:
	check_messages_at_new_tick( NewTickOffset, State ),

	% Then, this tick must be in the agenda, and be the first:
	check_spontaneous_indeed( NewTickOffset, State ).



% Checks that this scheduling returned a sensible result.
%
validate_scheduling_outcome( State ) ->

	AddedTicks = ?getAttr(added_spontaneous_ticks),
	WithdrawnTicks = ?getAttr(withdrawn_spontaneous_ticks),

	case ?getAttr(next_action) of


		no_diasca_requested ->
			validate_new_ticks( AddedTicks, WithdrawnTicks,
									?getAttr(current_tick_offset) );


		new_diasca_needed ->
			validate_new_ticks( AddedTicks, WithdrawnTicks,
									?getAttr(current_tick_offset) );


		{ terminating, _Delay } ->

			case AddedTicks of

				[] ->
					ok;

				_ ->
					throw( { added_ticks_while_terminating, AddedTicks } )

			end,

			case WithdrawnTicks of

				[] ->
					ok;

				_ ->
					% Might be acceptable, though.
					throw( { withdrawn_ticks_while_terminating,
							WithdrawnTicks } )

			end;


		terminated ->

			case AddedTicks of

				[] ->
					ok;

				_ ->
					throw( { added_ticks_while_terminated, AddedTicks } )

			end,

			case WithdrawnTicks of

				[] ->
					ok;

				_ ->
					throw( { withdrawn_ticks_while_terminated,
							WithdrawnTicks } )

			end

	end.



% Ensures that all specified ticks are in the future and that none is added and
% withdrawn at the same time.
%
validate_new_ticks( AddedTicks, WithdrawnTicks, CurrentTickOffset ) ->

	case AddedTicks of

		[] ->
			ok;

		_ ->
			case lists:min( AddedTicks ) > CurrentTickOffset of

				true ->
					ok;

				false ->
					FaultyAddedTicks = [ T
								   || T <- AddedTicks, T =< CurrentTickOffset ],
					throw( { added_spontaneous_ticks_must_be_in_future,
							FaultyAddedTicks, CurrentTickOffset } )

			end

	end,

	case WithdrawnTicks of

		[] ->
			ok;

		_ ->
			case lists:min( WithdrawnTicks ) > CurrentTickOffset of

				true ->
					ok;

				false ->
					FaultyWithdrawnTicks = [ T
						   || T <- WithdrawnTicks, T =< CurrentTickOffset ],
					throw( { withdrawn_spontaneous_ticks_must_be_in_future,
							FaultyWithdrawnTicks, CurrentTickOffset } )

			end

	end,

	case list_utils:intersect( AddedTicks, WithdrawnTicks ) of

		[] ->
			ok;

		Intersection ->
			throw( { no_withdrawal_of_added_spontaneous_ticks,
					AddedTicks, WithdrawnTicks, Intersection } )

	end.



% Called by the local time manager in order to schedule this actor for a new
% non-null diasca, after its spontaneous behaviour.
%
% Returns an updated state, and triggers back a notification to the
% corresponding time manager when the triggered actions have been completed.
%
% (oneway)
%
-spec beginDiasca( wooper:state(), class_TimeManager:tick_offset(),
				  class_TimeManager:diasca() ) -> oneway_return().
beginDiasca( State, TickOffset, NewDiasca ) ->

	%io:format( " beginDiasca for ~w at diasca ~B of tick offset #~B.~n",
	%		   [ self(), NewDiasca, TickOffset ] ),

	% Comment to disable these checkings:
	check_diasca_consistency( TickOffset, NewDiasca, State ),

	% Other attributes set at the end of previous scheduling:
	PreState = setAttributes( State, [

					% This is not superfluous: we might have received an actor
					% message while being still lagging in a past tick:
					{ current_tick_offset, TickOffset },

					{ current_diasca, NewDiasca }

									  ] ),

	TriggerState = process_last_diasca_messages( TickOffset, NewDiasca,
												PreState ),

	% Note: we are not checking the correctness of the engine here, we ensure
	% models are properly written (hence this should not be commented out).
	%
	validate_scheduling_outcome( TriggerState ),

	% The triggered methods might have sent actor messages:
	AckState = case getAttribute( TriggerState, waited_acks ) of

		[] ->
				notify_diasca_ended( TriggerState );

		_StillWaiting ->
				% End of diasca to be determined by acknowledgeMessage/2:
				TriggerState

	end,

	?wooper_return_state_only( AckState ).



% Checks diasca consistency.
%
% (optional checkings)
%
check_diasca_consistency( TickOffset, NewDiasca, State ) ->

	check_trigger_tick_consistency( TickOffset, NewDiasca, State ),

	% We cannot enfore these constraints, as an otherwise idle actor might be
	% awoken in any late future by the receiving of an actor message:
	% (only possible checkings already done in check_trigger_tick_consistency/3.

	%TickOffset = ?getAttr(current_tick_offset),
	%NewDiasca = ?getAttr(current_diasca) + 1,

	[] = ?getAttr(added_spontaneous_ticks),
	[] = ?getAttr(withdrawn_spontaneous_ticks),

	% If we are scheduling a non-zero diasca, it must mean that either we have
	% at least an actor message to process and/or that we are terminating:
	%
	case ?getAttr(pending_messages) of

		[] ->

			case ?getAttr(next_action) of

				% Not 'unlimited':
				{ terminating, AnyDuration } when is_integer(AnyDuration) ->
					ok;

				UnexpectedAction ->
					throw( { unexpected_diasca_scheduling, UnexpectedAction } )

			end;

		_ ->
			ok

	end.



% Checkings, in the case of a triggered tick.
%
check_trigger_tick_consistency( TickOffset, NewDiasca, State ) ->

	% First, time must always progress:
	check_time_consistency_at_new_diasca( TickOffset, NewDiasca, State ),

	% Checks that actor messages are within possible past and future bounds:
	check_messages_at_new_diasca( TickOffset, NewDiasca, State ),

	% Finally, at least one actor message (for that diasca of that tick) must be
	% available:
	%
	check_triggered_indeed( TickOffset, NewDiasca, State ).



% Helps to check whether the actor is scheduled for a new diasca as expected, by
% ensuring that the causality always progresses.
%
check_time_consistency_at_new_diasca( TickOffset, NewDiasca, State ) ->

	false = ( NewDiasca < 1 ),

	% A possible case is that simply this actor receives an actor message sent
	% at diasca 0 by another actor which was spontaneously scheduled, whereas
	% this actor was not.
	%
	% Hence the only checking we can do is to ensure that the previous schedule
	% is strictly in the past:
	%
	case ?getAttr(previous_schedule) of

		{ PreviousTickOffset, _AnyDiasca }
		  when PreviousTickOffset < TickOffset ->
			ok;

		% Same tick:
		{ TickOffset, PreviousDiasca } when PreviousDiasca < NewDiasca ->
			ok;

		undefined ->
			% When an actor is just created, it shows up directly at a non-zero
			% diasca:
			ok;

		% Same diasca:
		{ TickOffset, NewDiasca } ->
			throw( { duplicated_diasca, NewDiasca, TickOffset } );

		{ TickOffset, WrongDiasca } ->
			throw( { wrong_diasca, WrongDiasca, NewDiasca, TickOffset } );

		{ WrongTickOffset, NewDiasca } ->
			throw( { wrong_offset, WrongTickOffset, TickOffset, NewDiasca } );

		WrongTimestamp ->
			throw( { wrong_timestamp, WrongTimestamp,
					{ TickOffset, NewDiasca } } )

	end.



% Called by the time manager in order to schedule this actor a last time, so
% that it can safely terminate.
%
% (oneway)
%
-spec beginTerminationDiasca( wooper:state(), class_TimeManager:tick_offset(),
	class_TimeManager:diasca() ) -> oneway_return().
beginTerminationDiasca( State, TickOffset, NewDiasca ) ->

	% Actual termination now!
	io:format( "Actor ~w terminating now at {~p,~p}.~n",
			  [ self(), TickOffset, NewDiasca ] ),

	% First some checkings:
	terminated = ?getAttr(next_action),

	check_termination_time_consistency( TickOffset, NewDiasca, State ),

	[] = ?getAttr(current_agenda),

	case ?getAttr(pending_messages) of

		[] ->
			ok;

		Messages ->
			?warning_fmt( "Actor (AAI: ~B) is terminating at tick offset #~B"
						  " diasca ~B, but it had following pending actor "
						  "messages: ~p.",
						  [ ?getAttr(actor_abstract_id), TickOffset, NewDiasca,
						   Messages ] )

	end,


	% Then some logging:

	% (not counting terminating tick)
	LifespanInTicks = TickOffset - ?getAttr(actor_creation_tick_offset),

	LifespanInSeconds = convert_ticks_to_seconds( LifespanInTicks, State ),

	?trace_fmt( "Actual termination of actor (AAI: ~B) occurred at "
				"tick offset #~B diasca ~B, i.e. after an actual lifespan "
				"of ~B ticks (~s).~n",
				[ ?getAttr(actor_abstract_id), TickOffset, NewDiasca,
				  LifespanInTicks, text_utils:duration_to_string(
							   round( 1000 * LifespanInSeconds ) ) ] ),

	% Finally we do not delete this actor here, we defer until next tick and
	% trigger it from the time manager:
	%
	% self() ! delete,

	% Nothing more to send to the time manager:
	?wooper_return_state_only( State ).



% Checkings, in the case of a termination tick.
check_termination_time_consistency( TickOffset, NewDiasca, State ) ->

	% A termination can *only* happen just afterwards the previous diasca on the
	% same tick:
	PreviousDiasca = NewDiasca - 1,
	{ TickOffset, PreviousDiasca } = ?getAttr(previous_schedule).




% Helps to check whether the actor is scheduled for a new tick as expected, by
% ensuring time always progresses.
%
check_time_consistency_at_new_tick( NewTickOffset, State ) ->

	case ?getAttr(previous_schedule) of

		undefined when is_integer(NewTickOffset) ->
			% Actor starting, with any tick offset (0 if the simulation was not
			% started, non-null otherwise):
			ok;

		{ WrongTickOffset, _D } when WrongTickOffset > NewTickOffset ->
			throw( { backward_tick_scheduling, WrongTickOffset,
					NewTickOffset } );

		{ NewTickOffset, _AnyDiasca } ->
			throw( { duplicated_scheduling, NewTickOffset } );

		_CompatibleTickOffset ->
			% Any tick offset strictly in the past is possible:
			ok

	end.



% Checks that no pending actor message targets an impossible future for
% specified new tick.
%
% To be called just once this actor was scheduled for a new tick.
%
% This check is useful as, when an actor receives a message, it cannot check
% whether it comes from a too distant (incorrect) future, as the actor may be
% still lingering in a remote past, short of having been scheduled recently.
%
% So the only point in time where it can check messages against future is once
% being just scheduled (whatever the reason - an actor may already have received
% messages once it starts its new tick, although it is not the most common
% case).
%
check_messages_at_new_tick( CurrentTickOffset, State ) ->
	check_messages_at_new_tick_helper( ?getAttr(pending_messages),
									  CurrentTickOffset ).



% Any pending actor message can only have a very specific timestamp here (these
% can only be early messages targeting diasca 1).
%
check_messages_at_new_tick_helper( _PendingMessages=[], _CurrentTickOffset ) ->
	ok;

% The only licit case (most common put first):
check_messages_at_new_tick_helper(
		% If having received an early message, can only occur at diasca 0 and
		% then target diasca 1:
		[ #actor_message{ tick_offset=CurrentTickOffset, diasca=1 } | T ],
		CurrentTickOffset ) ->
	check_messages_at_new_tick_helper( T, CurrentTickOffset );

% Correct tick, but invalid diasca:
check_messages_at_new_tick_helper(
	  [ M=#actor_message{ tick_offset=CurrentTickOffset, diasca=MsgDiasca }
	   | _T ],
	  CurrentTickOffset ) ->
	throw( { diasca_one_expected_in_message_at_new_tick, MsgDiasca,
			CurrentTickOffset, M } );

% All other cases are errors as well:
check_messages_at_new_tick_helper( [ M | _T ], CurrentTickOffset ) ->
	throw( { invalid_logical_timestamp_in_message_at_new_tick,
			CurrentTickOffset, M } ).




% Checks that no pending actor message targets an impossible future for
% specified diasca at specified tick.
%
% To be called just once this actor was scheduled for a new diasca.
%
% This check is useful as, when an actor receives a message, it cannot check
% whether it comes from a too distant (incorrect) future, as the actor may be
% still lingering in a remote past, short of having been scheduled recently.
%
% So the only point in time where it can check messages against future is once
% being just scheduled (whatever the reason - an actor may already have received
% messages once it starts its new tick, although it is not the most common
% case).
%
check_messages_at_new_diasca( CurrentTickOffset, CurrentDiasca, State ) ->
	check_messages_at_new_diasca_helper( ?getAttr(pending_messages),
										 CurrentTickOffset, CurrentDiasca ).


% A pending actor message can only have a specific timestamp here.
%
check_messages_at_new_diasca_helper( _PendingMessages=[], _CurrentTickOffset,
							_CurrentDiasca ) ->
	ok;

% The only licit cases are where the actor and message timestamps agree or a
% message, sent by an early actor, is one diasca in the future (most common put
% first):
%
check_messages_at_new_diasca_helper(
	  [ #actor_message{ tick_offset=CurrentTickOffset, diasca=CurrentDiasca }
	   | T ], CurrentTickOffset, CurrentDiasca ) ->
	% Same diasca, most common case:
	check_messages_at_new_diasca_helper( T, CurrentTickOffset, CurrentDiasca );

check_messages_at_new_diasca_helper(
	  [ #actor_message{ tick_offset=CurrentTickOffset, diasca=NextDiasca }
	   | T ], CurrentTickOffset, CurrentDiasca )
  when NextDiasca == CurrentDiasca + 1 ->
	% Next diasca, message must have been sent by an early actor:
	check_messages_at_new_diasca_helper( T, CurrentTickOffset, CurrentDiasca );

% All other cases are errors:
check_messages_at_new_diasca_helper( [ M | _T ], CurrentTickOffset,
									CurrentDiasca ) ->
	throw( { invalid_logical_timestamp_in_message_at_new_diasca,
			{ CurrentTickOffset, CurrentDiasca }, M } ).





% Throws an exception iff the specified tick offset was actually not registered
% as a schedule tick for this actor, or if was registered, but not as the first
% deadline.
%
check_spontaneous_indeed( NewTickOffset, State ) ->

	% An actor may have received an early message from a remote actor before
	% receiving its own beginTick message, so the next checking would be wrong:
	%
	% [] = ?getAttr(pending_messages),

	case ?getAttr(next_action) of

		terminating ->
			throw( terminating_in_next_tick );

		_ ->
			ok

	end,

	case ?getAttr(current_agenda) of

		[ NewTickOffset | _T ] ->
			ok;

		Agenda ->
			throw( { faulty_spontaneous_entry, NewTickOffset, Agenda } )

	end.



% Throws an exception iff there is no pending actor messages for this diasca of
% the current tick, for this actor.
%
check_triggered_indeed( TickOffset, NewDiasca, State ) ->

	PendingMessages = ?getAttr(pending_messages),

	case find_message_for_timestamp( TickOffset, NewDiasca, PendingMessages ) of

		true ->
			ok;

		false ->
			% Only other reason to be triggered: active termination in progress.
			case ?getAttr(next_action) of

				{ terminating, D } when is_integer(D) ->
					ok;

				_ ->
					throw( { triggered_unexpectedly, TickOffset, NewDiasca,
							 PendingMessages } )

			end

	end.



% Determines whether there is at least one pending actor message for the
% specified timestamp.
%
% Intercepts as well actor messages in the past.
%
find_message_for_timestamp( _TickOffset, _Diasca, _Acc=[] ) ->
	false;

find_message_for_timestamp( TickOffset, Diasca,
	  _Acc=[ #actor_message{ tick_offset=TickOffset, diasca=Diasca } | _T ] ) ->
	true;

find_message_for_timestamp( TickOffset, Diasca,
	  [ M = #actor_message{ tick_offset=ATickOffset, diasca=ADiasca } | _T ] )
		 when ATickOffset =/= TickOffset orelse ADiasca < Diasca
			  orelse ADiasca > Diasca + 1 ->
	throw( { found_message_in_the_past, M, { TickOffset, Diasca } } );

find_message_for_timestamp( TickOffset, Diasca, [ _H | T ] ) ->
	find_message_for_timestamp( TickOffset, Diasca, T ).





% Scheduling section: adding or removing spontaneous ticks.



% Requests the next tick to be added to the future spontaneous ticks of this
% actor.
%
% (oneway)
%
-spec scheduleNextSpontaneousTick( wooper:state() ) -> oneway_return().
scheduleNextSpontaneousTick( State ) ->

	NextTick = ?getAttr(current_tick_offset) + 1,

	?wooper_return_state_only( add_spontaneous_tick( NextTick, State ) ).



% Adds the specified spontaneous tick offset to the already registered ones.
%
% (oneway)
%
-spec addSpontaneousTick( wooper:state(), class_TimeManager:tick_offset() ) ->
								oneway_return().
addSpontaneousTick( State, SpontaneousTickToAdd ) ->

	NewState = add_spontaneous_tick( SpontaneousTickToAdd, State ),

	?wooper_return_state_only( NewState ).



% Adds the specified spontaneous tick offsets to the already registered ones.
%
% (oneway)
%
-spec addSpontaneousTicks( wooper:state(),
					[ class_TimeManager:tick_offset() ]  ) -> oneway_return().
addSpontaneousTicks( State, SpontaneousTicksToAdd ) ->

	NewState = add_spontaneous_ticks( SpontaneousTicksToAdd, State ),

	?wooper_return_state_only( NewState ).



% Adds the specified spontaneous tick offset to the already registered ones, and
% returns an updated state.
%
-spec add_spontaneous_tick( class_TimeManager:tick_offset(),
						   wooper:state() ) -> wooper:state().
add_spontaneous_tick( SpontaneousTickToAdd, State ) ->

	%% case class_Actor:get_abstract_identifier( State ) of

	%% 	197 ->
	%% 		io:format( "Truck schedule at #~B: AAI=~p, name=~s, random=~p, add ~B to ~p~n",
	%% 				   [ class_Actor:get_current_tick_offset( State ),
	%% 					 197,
	%% 					 ?getAttr(name),
	%% 					 random_utils:get_random_state(),
	%% 					 SpontaneousTickToAdd,
	%% 					 ?getAttr(added_spontaneous_ticks)
	%% 				   ] );

	%% 	_ ->
	%% 		ok
	%% end,

	appendToAttribute( State, added_spontaneous_ticks,
								 SpontaneousTickToAdd ).



% Adds the specified spontaneous tick offsets to the already registered ones,
% and returns an updated state.
%
-spec add_spontaneous_ticks( [ class_TimeManager:tick_offset() ],
						   wooper:state() ) -> wooper:state().
add_spontaneous_ticks( SpontaneousTicksToAdd, State ) ->

	PreviousTicks = ?getAttr(added_spontaneous_ticks),

	%% case class_Actor:get_abstract_identifier( State ) of

	%% 	197 ->
	%% 		io:format( "Truck schedule: AAI=~p, name=~s, random=~p, add ~p to ~p~n",
	%% 				   [ 197,
	%% 					 ?getAttr(name),
	%% 					 random_utils:get_random_state(),
	%% 					 SpontaneousTicksToAdd,
	%% 					 ?getAttr(added_spontaneous_ticks)
	%% 				   ] );

	%% 	_ ->
	%% 		ok
	%% end,

	% We prefer simplifying as much as possible the future of work of the time
	% manager by removing duplicated ticks, but it will done later, on sending:
	%
	NewTicks = SpontaneousTicksToAdd ++ PreviousTicks,

	setAttribute( State, added_spontaneous_ticks, NewTicks ).




% Withdraws the specified spontaneous tick offset from the already registered
% ones.
%
% (oneway)
%
-spec withdrawSpontaneousTick( wooper:state(),
						class_TimeManager:tick_offset() ) -> oneway_return().
withdrawSpontaneousTick( State, SpontaneousTickToWithdraw ) ->

	NewState = withdraw_spontaneous_tick( SpontaneousTickToWithdraw, State ),

	?wooper_return_state_only( NewState ).





% Withdraws the specified spontaneous tick offsets from the already registered
% ones.
%
% (oneway)
%
-spec withdrawSpontaneousTicks( wooper:state(),
			[ class_TimeManager:tick_offset() ] ) -> oneway_return().
withdrawSpontaneousTicks( State, SpontaneousTicksToWithdraw ) ->

	NewState = withdraw_spontaneous_ticks( SpontaneousTicksToWithdraw, State ),

	?wooper_return_state_only( NewState ).



% Withdraws the specified spontaneous tick offset from the already registered
% ones, and returns an updated state.
%
-spec withdraw_spontaneous_tick( class_TimeManager:tick_offset(),
						   wooper:state() ) -> wooper:state().
withdraw_spontaneous_tick( SpontaneousTickToWithdraw, State ) ->

	appendToAttribute( State, withdrawn_spontaneous_ticks,
								 SpontaneousTickToWithdraw ).



% Withdraws the specified spontaneous tick offsets to the already registered
% ones, and returns an updated state.
%
-spec withdraw_spontaneous_ticks( [ class_TimeManager:tick_offset() ],
						   wooper:state() ) -> wooper:state().
withdraw_spontaneous_ticks( SpontaneousTicksToWithdraw, State ) ->

	PreviousTicks = ?getAttr(withdrawn_spontaneous_ticks),

	% We prefer simplifying as much as possible the future of work of the time
	% manager by removing duplicated ticks, but it will done later, on sending:
	%
	NewTicks = SpontaneousTicksToWithdraw ++ PreviousTicks,

	setAttribute( State, withdrawn_spontaneous_ticks, NewTicks ).



% Called by an actor when it determines it is to be removed from the simulation
% and deleted: starts its termination procedure, with no specific upper bound in
% terms of diascas until actual termination (so this actor will be actually
% deleted at the next tick (diasca 0).
%
% (oneway)
%
-spec declareTermination( wooper:state() ) -> oneway_return().
declareTermination( State ) ->

	% Defaults to 'unlimited':
	DeletionState = declare_termination( unlimited, State ),

	?wooper_return_state_only( DeletionState ).



% Called by an actor when it determines it is to be removed from the simulation
% and deleted: starts its termination procedure, waiting for exactly the
% specified number of diascas (supposedly sufficient so that all actors knowing
% it become aware of its termination) until performing its actual
% termination.
%
% Of course it is up to this actor to notify appropriately the relevant actors
% and to select a sufficient number of diascas to do so.
%
% (oneway)
%
-spec declareTermination( wooper:state(), termination_delay() ) ->
								oneway_return().
declareTermination( State, TerminationDelay ) ->

	DeletionState = declare_termination( TerminationDelay, State ),

	?wooper_return_state_only( DeletionState ).



% Called by an actor when it determines it is to be removed from the simulation
% and deleted, which could be done on the very next diasca.
%
% (helper)
%
-spec declare_termination( termination_delay(), wooper:state() ) ->
								 wooper:state().
declare_termination( _IntercalaryDiasca=0, State ) ->

	check_no_termination( State ),

	% Here we do not expect to receive any other actor message (even in this
	% diasca - beware to the unshuffling of messages, we never can be sure we
	% are processing the last message of a diasca), so we proceed directly to
	% the 'terminated' state:

	DeletionState = setAttribute( State, next_action, terminated ),

	?wooper_return_state_only( DeletionState );



% Includes the 'unlimited' case (here we will wait specified diascas before
% going in the 'terminated' state):
%
declare_termination( IntercalaryDiasca, State ) ->

	check_no_termination( State ),

	DeletionState = setAttribute( State, next_action,
						{ terminating, IntercalaryDiasca } ),

	?wooper_return_state_only( DeletionState ).



% Ensures that the actor has not already initiated its termination.
%
check_no_termination( State ) ->

	case ?getAttr(next_action ) of

		no_diasca_requested ->
			ok;

		new_diasca_needed ->
			ok;

		{ terminating, _Any } ->
			throw( termination_declared_whereas_terminating );

		terminated ->
			throw( termination_declared_whereas_terminated )

	end.



% Reacts to a notification of time manager shutdown by deleting this actor.
%
% (oneway)
%
-spec timeManagerShutdown( wooper:state() ) -> oneway_return().
timeManagerShutdown( State ) ->

	%?debug( "Received a notification of time manager shutdown, "
	%	"requesting our own deletion." ),

	self() ! delete,

	?wooper_return_state_only( State ).



% The definition of the actor spontaneous behaviour.
%
% The actSpontaneous/1 oneway is expected to update the actor state, send any
% relevant actor message(s) and/or update the spontaneous agenda of this actor.
%
% Default implementation, made to be overridden.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?trace_fmt( "Acting spontaneously (blank default behaviour) "
		"at tick offset #~B.", [ get_current_tick_offset( State ) ] ),

	% No future action defined, defaults to passive (none):
	?wooper_return_state_only( State ).



% Oneway called by another actor (A) to send to this current actor (S=self()) a
% behaviour-specific message: this S actor stores this message for later
% processing, but acknowleges it immediately to A.
%
% (oneway)
%
-spec receiveActorMessage( wooper:state(), class_TimeManager:tick_offset(),
	class_TimeManager:diasca(), oneway_call(), pid(), aai() ) ->
								 oneway_return().
receiveActorMessage( State, MessageTickOffset, MessageTargetDiasca,
					ActorOneway, SendingActorPid, SendingActorAai ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	%io:format( "receiveActorMessage '~p' for ~w: current tick offset is #~p, "
	%   "current diasca is ~B, message tick offset is #~p, "
	%   "message target diasca is ~B.~n",
	%   [ ActorOneway, self(), CurrentTickOffset, CurrentDiasca,
	%   MessageTickOffset, MessageTargetDiasca ] ),


	% First, some checkings.
	%
	% Note that this actor may not have been scheduled for a while, thus even
	% its current tick/diasca might be outdated.
	%
	% So the only impossible case is if the message targets the current local
	% stored current time or any time before; it must be in a strict future:
	%
	case MessageTickOffset < CurrentTickOffset of

		true ->
			?error_fmt( "An actor message targeting a tick in the past was "
						"received at local tick offset #~B diasca ~B; "
						"sender: ~w (AAI: ~B), "
						"message tick offset #~B diasca ~B, oneway: ~p.",
						[ CurrentTickOffset, CurrentDiasca, SendingActorPid,
						  SendingActorAai, MessageTargetDiasca,
						  MessageTickOffset, ActorOneway ] ),

			FullMessage = { MessageTickOffset, MessageTargetDiasca, ActorOneway,
						   SendingActorPid, SendingActorAai },

			throw( { message_tick_in_the_past, FullMessage, CurrentTickOffset,
					CurrentDiasca, self() } );

		false ->

			case MessageTickOffset > CurrentTickOffset of

				true ->
					% Nothing more can be checked, can be licit:
					ok;

				false ->
					% Here MessageTickOffset == CurrentTickOffset:
					case MessageTargetDiasca =< CurrentDiasca of

						true ->
							% The message must be at least one diasca in the
							% future:

							?error_fmt( "An actor message targeting a diasca "
										"in the past was received at "
										"tick offset #~B, local diasca ~B; "
										"sender: ~w (AAI: ~B), "
										"message diasca ~B, oneway: ~p.",
										[ CurrentTickOffset, CurrentDiasca,
										 SendingActorPid, SendingActorAai,
										 MessageTargetDiasca, ActorOneway ] ),

							FullMessage = { MessageTickOffset,
										   MessageTargetDiasca, ActorOneway,
										   SendingActorPid, SendingActorAai },

							throw( { message_diasca_in_the_past, FullMessage,
								 CurrentTickOffset, CurrentDiasca, self() } );

						false ->
							% In the future:
							ok

					 end

			end

	end,

	MessageTimestamp = { MessageTickOffset, MessageTargetDiasca },

	% Let's start by checking whether receiving a message is licit here:
	case ?getAttr(next_action) of

				terminated ->

					?error_fmt( "An actor message has been received "
								"at diasca ~B whereas this receiving actor "
								"has already terminated; sender is ~w (AAI: ~B)"
								", message timestamp is {~p,~p}, and is '~p'.",
								[ CurrentDiasca, SendingActorPid,
								 SendingActorAai, MessageTickOffset,
								 MessageTargetDiasca, ActorOneway ] ),

					throw( { message_received_while_terminated,
							{ CurrentTickOffset, CurrentDiasca },
							MessageTimestamp,
							{ ?getAttr(actor_abstract_id), self() },
							{ SendingActorAai, SendingActorPid },
							ActorOneway } ) ;

				_ ->
					ok

	end,

	% Note that the message sent below is not enough to ensure that the next
	% diasca will be scheduled at all, as the time manager of this receiving
	% actor may already have reported its end of diasca; this is why this is up
	% to the *calling* actor to notify, when finishing its diasca, its own time
	% manager, which by design has not finished its diasca yet.
	%
	% We have here to specify the diasca at which this actor should be
	% triggered, as the time manager of this receiving actor may not have
	% already received the 'begin' message for the diasca from which the sending
	% actor acted (ex: if the sending actor is scheduled by the root time
	% manager whereas this receiving actor is on another node).
	%
	% Indeed, when the time manager of this receiving actor receives such a
	% scheduleTrigger message while still at diasca D (i.e. not having received
	% yet a 'begin diasca' for D+1), it has no way of knowing whether this is
	% due to another time manager still in D or already in D+1. The race
	% condition between its 'begin diasca' message and the scheduleTrigger
	% message results in the need for the target diasca to be specified.
	%
	% Of course this message is strictly necessary, let alone so that the local
	% time manager knows which actors shall be triggered during next diasca.
	%
	% However this scheduleTrigger message should not be sent more than once by
	% an actor to its local time manager during the same diasca (as an actor
	% should not be triggered more than once in a given diasca); instead of
	% having time managers remove duplicates in their
	% actors_to_trigger_next_diasca list, each actor just ensures that it sends
	% only up to one scheduleTrigger message per diasca:
	%
	ScheduledState = case ?getAttr(last_sent_schedule_trigger) of

				MessageTimestamp ->
					% Only case where nothing is to be done:
					State;

				% Either 'undefined' or a different timestamp (in the past):
				_ ->

					%io:format( "~w sending scheduleTrigger to "
					%  "time manager ~w.~n",
					%  [ self(), ?getAttr(time_manager_pid) ] ),

					% A *key* point is to use the timestamp from the message,
					% knowing that this receiving actor may be lagging behind
					% the sender one:
					%
					?getAttr(time_manager_pid) ! { scheduleTrigger,
						   [ MessageTickOffset, MessageTargetDiasca ], self() },

					% Now wait for the scheduleTrigger answer:
					receive

						% A priori, having this operation be synchronous may not
						% be necessarily strictly mandatory (but it does not
						% hurt and is cheap - being purely local and happening
						% only up to once per diasca per actor):
						{ wooper_result, trigger_planned } ->
							ok

					end,
					setAttribute( State, last_sent_schedule_trigger,
								 MessageTimestamp )

			end,

	% Then acknowledges to the sender (no interleaving preferred):
	SendingActorPid ! { acknowledgeMessage, self() },


	% Stores this message for later processing:
	StoredMessage = #actor_message{

			tick_offset=MessageTickOffset,
			diasca=MessageTargetDiasca,
			sender_pid=SendingActorPid,
			sender_aai=SendingActorAai,
			actual_message=ActorOneway

								   },

	MessageState = appendToAttribute( ScheduledState, pending_messages,
		 StoredMessage ),

	?wooper_return_state_only( MessageState ).




% Callback triggered by the reception of an acknowledgement of an actor to which
% this actor sent a message.
%
% (oneway)
%
-spec acknowledgeMessage( wooper:state(), pid() ) -> oneway_return().
acknowledgeMessage( State, CalledActorPid ) ->

	Waited = ?getAttr(waited_acks),

	% Check we are indeed waiting for this ack, remove it from list, see if it
	% was the last waited one:
	%
	ShortenWaitedList = list_utils:delete_existing( CalledActorPid, Waited ),

	ShortenState = setAttribute( State, waited_acks, ShortenWaitedList ),

	NewState = case ShortenWaitedList of

				[] ->
					% Last ack received, ready to declare this actor's end of
					% diasca:
					%
					notify_diasca_ended( ShortenState );

				_ ->
					% There is still at least one waited ack, still waiting:
					ShortenState

			end,

	?wooper_return_state_only( NewState ).



% Sends to the local time manager a notification that the current diasca ended.
%
% Returns an updated state.
%
% (helper)
%
notify_diasca_ended( State ) ->

	% Checking:
	[] = ?getAttr(waited_acks),

	% Let's try to ease as much as possible the work of the time manager:
	AddedTicks = list_utils:uniquify( ?getAttr(added_spontaneous_ticks) ),

	WithdrawnTicks = list_utils:uniquify(
								   ?getAttr(withdrawn_spontaneous_ticks) ),

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	{ NextRecordedAction, NextNotifiedAction } = case ?getAttr(next_action) of

			 A={ terminating, unlimited } ->
				% No, we will not schedule this actor until end of time:
				{ A, terminating_unlimited };

			 { terminating, _DiascaCount=0 } ->
				% Termination completed; an actor is expected to send a
				% 'terminated' notification once, as it is to be deallocated
				% just afterwards (it could be deleted at the next diasca,
				% however we defer it to the next tick):
				{ terminated, terminated };

			 { terminating, NonNullDiascaCount } ->
				% Termination still in progress, we will request new diascas:
				{ { terminating, NonNullDiascaCount-1 }, terminating };

			  new_diasca_needed ->
				 % We must reset the recorded next_action attribute:
				 { no_diasca_requested, new_diasca_needed };

			  no_diasca_requested ->
				 { no_diasca_requested, no_diasca_requested };

			  terminated ->
				 { terminated, terminated }

	end,

	%io:format( "Actor ~w at {~p,~p}: next action is ~p.~n",
	%		[ self(), CurrentTickOffset, CurrentDiasca, NextNotifiedAction ] ),

	% No more actor message waited this diasca:
	NotificationMessage = case CurrentDiasca of

				  0 ->
					  { notifySpontaneousActionsCompleted,
					   [ CurrentTickOffset, self(), NextNotifiedAction,
						AddedTicks, WithdrawnTicks ] };

				  _ ->
					  { notifyTriggeredActionsCompleted,
					   [ CurrentTickOffset, CurrentDiasca, self(),
						NextNotifiedAction, AddedTicks, WithdrawnTicks ] }

	end,

	% No more actor message waited this diasca:
	?getAttr(time_manager_pid) ! NotificationMessage,

	NewAgenda = update_agenda_with( AddedTicks, WithdrawnTicks,
										   ?getAttr(current_agenda) ),

	% Prepare for next diasca, reset relevant attributes:
	setAttributes( State, [

				{ previous_schedule, { CurrentTickOffset, CurrentDiasca } },
				{ added_spontaneous_ticks, [] },
				{ withdrawn_spontaneous_ticks, [] },
				{ next_action, NextRecordedAction },
				{ current_agenda, NewAgenda }

								   ] ).



% Returns the current simulation time of this actor, expressed as an offset of
% simulation ticks relative to the beginning of the simulation.
%
% (const request)
%
-spec getSimulationTickOffset( wooper:state() ) ->
				   request_return( class_TimeManager:tick_offset() ).
getSimulationTickOffset( State ) ->
	?wooper_return_state_result( State, ?getAttr(current_tick_offset) ).



% Returns the current simulation time of this actor, expressed as an absolute
% number of simulation ticks.
%
% (const request)
%
-spec getSimulationTick( wooper:state() ) ->
					   request_return( class_TimeManager:tick() ).
getSimulationTick( State ) ->
	?wooper_return_state_result( State, get_current_tick(State) ).



% Returns the current simulation time of this actor, structured as follows:
% {{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond}}.
%
% This date might be less precise than the actual simulation tick, so the latter
% cannot be obtained from the former.
%
% (const request)
%
-spec getSimulationDate( wooper:state() ) ->
							request_return( basic_utils:timestamp() ).
getSimulationDate( State ) ->

	IntegerSeconds = round( convert_ticks_to_seconds( get_current_tick( State ),
													State ) ),

	?wooper_return_state_result( State,
		calendar:gregorian_seconds_to_datetime(IntegerSeconds) ).



% Returns a textual description of the simulation and real time, for this actor.
%
% (const request)
%
-spec getTextualTimings( wooper:state() ) -> request_return( string() ).
getTextualTimings( State ) ->
	?wooper_return_state_result( State, get_textual_timings( State ) ).



% Returns an atom corresponding to the Erlang node on which this actor runs.
%
% Note: mostly useful for the test of the placement heuristics.
%
% (const request)
%
-spec getHostingNode( wooper:state() ) ->
							request_return( net_utils:node_name() ).
getHostingNode( State ) ->
	?wooper_return_state_result( State, node() ).



% Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (rounded) number of simulation ticks,
% which is at least equal to one tick.
%
% Ex: convertSecondsToTicks( State, 0.001 )
% Note: the convert_seconds_to_ticks helper function can be used as well.
%
% (const request)
%
-spec convertSecondsToTicks( wooper:state(), unit_utils:seconds() ) ->
		   request_return( class_TimeManager:tick_offset() ).
convertSecondsToTicks( State, Seconds ) ->
	?wooper_return_state_result( State,
		convert_seconds_to_ticks( Seconds, State ) ).



% Converts the specified tick count into a fractional (floating-point) number of
% seconds.
%
% Note: the convert_ticks_to_seconds helper function can be used as well.
%
% (const request)
%
-spec convertTicksToSeconds( wooper:state(), class_TimeManager:tick_offset() )
						   -> request_return( float() ).
convertTicksToSeconds( State, Ticks ) ->
	?wooper_return_state_result( State,
		convert_ticks_to_seconds( Ticks, State ) ).



% Returns (asynchronously, to avoid deadlocks) the current list of waited actors
% (if any) for that actor.
%
% Allows the TimeManager to know why this actor may be stalling the simulation,
% and who it is.
%
% (const oneway)
%
-spec nudge( wooper:state(), pid() ) -> oneway_return().
nudge( State, SenderPid ) ->

	SenderPid ! { notifyNudged, [ self(), get_current_tick_offset( State ),
								?getAttr(waited_acks) ] },

	?wooper_return_state_only( State ).



% Returns the AAI of that instance.
%
% (const request)
%
-spec getAAI( wooper:state() ) -> request_return( aai() ).
getAAI( State ) ->
	?wooper_return_state_result( State, ?getAttr(actor_abstract_id) ).



% Returns an information record about this actor.
%
% (const request)
%
-spec getActorInfo( wooper:state() ) -> request_return( actor_info() ).
getActorInfo( State ) ->

	{ _SameState, ActualClassName } = executeRequest( State, getClassName ),

	ActorInfo = #actor_info{
		classname=ActualClassName,
		name=?getAttr(name),
		aai=?getAttr(actor_abstract_id) },

	?wooper_return_state_result( State, ActorInfo ).





% Called automatically two diascas after this actor called create_actor/3, to
% notify it the creation was done, resulting in a newly created actor.
%
% Parameters are:
%
% - CreatedActorPid the PID of the just created actor
%
% - ActorClassName the classname of that created actor
%
% - ActorConstructionParameters the parameters which were specified for its
% creation
%
% Class name and construction parameters are provided as arguments, so that the
% creating actor is able to discriminate among multiple creations it might have
% requested in the course of the same tick.
%
% Note: this is a default implementation, meant to be overridden.
%
% (actor oneway)
%
-spec onActorCreated( wooper:state(), pid(), class_name(),
					 [ method_argument() ], pid() ) -> oneway_return().
onActorCreated( State, _CreatedActorPid, _CreatedActorClassName,
				_CreatedActorConstructionParameters, _LoadBalancerPid ) ->

	%?debug_fmt(
	%	"Default non-overridden onActorCreated/5 method called: "
	%    "actor ~w was created, instance of class ~w, from parameters ~p.",
	%	[ CreatedActorPid, CreatedActorClassName,
	% CreatedActorConstructionParameters ] ),

	?wooper_return_state_only( State ).



% Requires the caller (generally the time manager) to be notified
% (asynchronously) of the name (as a binary) of this actor.
%
% (const oneway)
%
-spec triggerNameNotification( wooper:state(), pid() ) -> oneway_return().
triggerNameNotification( State, CallerPid ) ->

	CallerPid ! { notifyName, [ self(), ?getAttr(name) ] },

	?wooper_return_state_only( State ).



% Relinks this actor, supposing it was just deserialised.
%
% (request, for synchronicity)
%
-spec relink( wooper:state() ) -> request_return( { 'relinked', pid() } ).
relink( State ) ->

	?wooper_return_state_result( State, { relinked, self() } ).



% 'Static' methods.



% Section for helper functions (not methods).



% Returns the (supposed opaque) identifier (AAI) of that actor.
%
% During a given simulation, an actor bears a unique identifier, and this
% identifier will be the same from one simulation to another.
%
% (helper function)
%
-spec get_abstract_identifier( wooper:state() ) -> aai().
get_abstract_identifier( State ) ->
	?getAttr(actor_abstract_id).



% Returns a path to the root directory of the deployed elements, as a plain
% string.
%
% Useful to be able to look-up and read third-party (simulation-specific)
% deployed data.
%
% Ex: "/tmp/sim-diasca-My_Case-boudevil-2012-12-7-at-13h-56m-03s-1f79"
% "3a6ba507/deployed-elements" may be returned.
%
% (helper function)
%
-spec get_deployed_root_directory( wooper:state() ) -> string().
get_deployed_root_directory( _State ) ->

	% Relies on the fact that the working directory must not have been changed,
	% neither by models nor by the simulation case (otherwise a path would have
	% to be recorded, possibly in each actor):
	{ ok, OutputDir } = file:get_cwd(),
	file_utils:join( [ OutputDir, "..", "deployed-elements" ] ).




% This section for time conversion is directly inspired from the one offered by
% the time manager.


% Converts the specified duration in virtual seconds (expressed as an integer or
% a floating point value) into an integer (non-negative, rounded) number of
% simulation ticks.
%
% Note: this time conversion will be checked for accuracy based on the default
% threshold in terms of relative error.
%
% Ex: TickCount = convert_seconds_to_ticks( 0.001, State )
%
% (helper function)
%
-spec convert_seconds_to_ticks( number(), wooper:state() ) ->
									class_TimeManager:tick_offset().
convert_seconds_to_ticks( Seconds, State ) when is_integer( Seconds ) ->
	convert_seconds_to_ticks( erlang:float( Seconds ), State );

convert_seconds_to_ticks( Seconds, State ) ->

	% Less than 1.5% of relative error tolerated by default:
	convert_seconds_to_ticks( Seconds, _DefaultMaxRelativeError=0.015, State ).





% Converts the specified number of (floating-point) seconds into an integer
% (rounded) number of ticks, checking that any rounding error stays within
% specified maximum relative error.
%
% For example, to limit the relative error to 5%, use MaxRelativeError=0.05.
%
% (helper function)
%
-spec convert_seconds_to_ticks( number(), math_utils:percent(), wooper:state() )
							  -> class_TimeManager:tick_offset().
convert_seconds_to_ticks( Seconds, MaxRelativeError, State )
  when is_integer( Seconds ) ->

	convert_seconds_to_ticks( erlang:float( Seconds ), MaxRelativeError,
							  State );

convert_seconds_to_ticks( Seconds, MaxRelativeError, State )
  when is_float( Seconds ) andalso Seconds >= 0 ->

	TickDuration = ?getAttr(simulation_tick_duration),

	TickCount = erlang:round( Seconds / TickDuration ),

	% Converts back to measure error:
	CorrespondingSeconds = TickCount * TickDuration,

	case math_utils:are_relatively_close( Seconds, CorrespondingSeconds,
										  MaxRelativeError ) of

		true ->
			TickCount;

		false ->

			ActualError = math_utils:get_relative_difference( Seconds,
														CorrespondingSeconds ),

			throw( { too_inaccurate_duration_conversion, TickCount,
					 { Seconds, CorrespondingSeconds }, TickDuration,
					 { MaxRelativeError, ActualError } } )

	end.




% Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (strictly positive, rounded) number of
% simulation ticks, which is at least equal to one tick.
%
% Ex: TickCount = convert_seconds_to_non_null_ticks( 0.001, State )
%
% (helper function)
%
-spec convert_seconds_to_non_null_ticks( number(), wooper:state() ) ->
									class_TimeManager:tick_offset().
convert_seconds_to_non_null_ticks( Seconds, State )
  when is_integer( Seconds ) ->

	convert_seconds_to_non_null_ticks( erlang:float( Seconds ), State );

convert_seconds_to_non_null_ticks( Seconds, State ) ->
	case convert_seconds_to_ticks( Seconds, State ) of

		0 ->
			1;

		TickCount ->
			TickCount

	end.



% Converts the specified duration in seconds (expressed as an integer or a
% floating point value) into an integer (strictly positive, rounded) number of
% simulation ticks, checking that any rounding error stays within specified
% maximum relative error and then ensuring the returned duration is at least
% equal to one tick.
%
% Ex: TickCount = convert_seconds_to_non_null_ticks( 0.001, 0.01, State )
%
% (helper function)
%
-spec convert_seconds_to_non_null_ticks( number(), math_utils:percent(),
						wooper:state() ) -> class_TimeManager:tick_offset().
convert_seconds_to_non_null_ticks( Seconds, MaxRelativeError, State ) ->

	case convert_seconds_to_ticks( Seconds, MaxRelativeError, State ) of

		0 ->
			1;

		TickCount ->
			TickCount

	end.



% Converts the specified tick count into a fractional (floating-point) number of
% seconds.
%
% (helper function)
%
-spec convert_ticks_to_seconds( class_TimeManager:tick_offset(),
								wooper:state() ) -> virtual_seconds().
convert_ticks_to_seconds( Ticks, State ) ->
	Ticks * ?getAttr(simulation_tick_duration).




% Returns a textual description of the real and simulated time.
%
% (helper function)
%
-spec get_textual_timings( wooper:state() ) -> string().
get_textual_timings( State ) ->

	CurrentTick = get_current_tick_offset( State ),

	{ { SimYear, SimMonth, SimDay }, { SimHour, SimMinute, SimSecond } } =
		calendar:gregorian_seconds_to_datetime( CurrentTick ),

	{ { RealYear, RealMonth, RealDay }, { RealHour, RealMinute, RealSecond } } =
		{ date(), time() },

	io_lib:format( "actor simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, CurrentTick,
		  RealDay, RealMonth, RealYear, RealHour, RealMinute, RealSecond ] ).



% Returns the current (numerical) simulation tick offset this actor is in,
% relatively to the simulation initial time (initial_tick), expressed in
% simulation ticks.
%
% (helper function)
%
-spec get_current_tick_offset( wooper:state() ) ->
								class_TimeManager:tick_offset().
get_current_tick_offset( State ) ->
	?getAttr(current_tick_offset).



% Returns the current (numerical) absolute simulation tick this actor is in.
%
% Note: class_TraceEmitter:get_current_tick/1 could be used as well. This
% version, to be called only when this actor is synchronized, must be slightly
% faster.
%
% It has been defined in class_Actor as well as in class_TraceEmitter, as the
% former is a child class of the latter, and, from the point of view of the
% user, relying on class_Actor:get_current_tick is more intuitive than using its
% class_TraceEmitter counterpart (inheritance, which is here a technical detail,
% is hidden).
%
-spec get_current_tick( wooper:state() ) -> class_TimeManager:tick().
get_current_tick( State ) ->
	?getAttr(initial_tick) + ?getAttr(current_tick_offset).




% Sends specified message to the specified actor, records this sending to wait
% for its acknowledgement, and returns an updated state. These inter-actor
% messages exchanged during simulation are the only allowed way of communicating
% between actors.
%
% An actor message parameter describes the behaviour (actor oneway, translating
% to an Erlang function) to trigger when this message will be taken into account
% by the targeted actor, once messages will have been properly reordered.
%
% This sent message corresponds to a oneway, not a request, to avoid any
% blocking operation, as the time management service must be the only one to
% control the course of the simulation.
%
% The sender PID is automatically added, thus it does not need to be specified
% explicitly here. The sender AAI is also automatically added as well, as the
% receiver will need it to reorder its incoming actor messages.
%
% The specified tick is the one expected for the delivery, i.e. the next tick,
% hence the +1.
%
% The actor message is a oneway call: it is described by the name of the actor
% oneway to trigger on the target actor (specified as an atom, ex: 'setColor')
% on the next tick, and by a (possibly empty) list of the corresponding
% arguments; so the call is either 'my_oneway' or
% '{my_oneway,SingleNonListParameter}' or '{my_oneway,[Arg1,...]}'.
%
% In all cases, the actual call, in the case of an actor message, will be
% performed with an additional parameter, the PID of the sending actor. This
% extra parameter will be transparently added, so an actor oneway which looks
% like a call to a oneway with N parameters specified will trigger a call to a
% function whose arity is N+2: the state, then the N parameters, then the PID of
% the sending actor (i.e.: in that order).
%
% So a typical call made by an actor whose PID is P1 to an actor P2 can be made
% thanks to the following actor message:
% NewState = class_Actor:send_actor_message( P2, {setColor,[red,15]}, AState )
%
% This would trigger on the target actor, setColor/4 on the next tick, as the
% PID of the sending actor is automatically added as last parameter:
% setColor( State, red, 15, P1 ) ->
%
% Returns an updated state, appropriate to wait automatically for this call to
% be acknowledged.
%
% (helper function)
%
-spec send_actor_message( pid(), oneway_call(), wooper:state() ) ->
								wooper:state().
send_actor_message( ActorPid, ActorOneway, State ) ->

	%io:format( "  ~w sending an actor message to ~w at {~p,~p}: ~p~n",
	%		  [ self(), ActorPid, ?getAttr(current_tick_offset),
	%		   ?getAttr(current_diasca), ActorOneway ] ),

	ActorPid ! { receiveActorMessage,
				[ ?getAttr(current_tick_offset), ?getAttr(current_diasca)+1,
				 ActorOneway, self(), ?getAttr(actor_abstract_id) ] },

	NewAction = case ?getAttr(next_action) of

					Action={ terminating, _Duration } ->
						Action;

					% No test really necessary against terminated: we should not
					% even be scheduled in this case.
					%
					terminated ->
						throw( { no_message_sending_when_terminated,
								ActorPid, ActorOneway } );

					_ ->
						%io:format( "Actor ~w requesting a new diasca, "
						%			"after having sent an actor message.~n",
						%			[ self() ] ),
						new_diasca_needed

	end,

	% Here we know for sure that the next diasca will have to be scheduled,
	% since the actor message will have to be processed by the recipient.
	%
	% At least one time manager must be notified of that. The one of the actor
	% that receives the actor message should be avoided, since it may have
	% already finished its tick and answered to its parent manager (if any).
	%
	% Conversely, we know for sure that the time manager of this sending actor
	% is still waiting for the end of its tick.
	%
	% Therefore it is up to that sending actor to trigger the scheduling of the
	% next tick, once it will finish its own scheduling for this diasca.
	%
	% This works, as all time managers are notified of all diascas, regardless
	% of what they are to schedule.
	%
	setAttributes( State, [
		{ waited_acks, [ ActorPid | ?getAttr(waited_acks) ] },
		{ next_action, NewAction }
						   ] ).



% Sends an actor message exactly like send_actor_message/3, except that an
% additional delay (AdditionalDelay) is taken into account.
%
% Thus an actor message sent at tick T will be processed by its target at
% T+1+AdditionalDelay (not at T+AdditionalDelay), instead of at T+1 as usual.
%
% This may be useful on rare cases, when wanting to reproduce a specific
% behaviour, for example to respect a uniform mode of operation between
% different Sim-Diasca flavours (centralised vs distributed).
%
% Returns an updated state, appropriate to wait automatically for this call to
% be acknowledged.
%
% (helper function)
%
-spec send_delayed_actor_message( class_TimeManager:tick_offset(), pid(),
				  oneway_call(), wooper:state() ) -> wooper:state().
send_delayed_actor_message( _AdditionalDelay, _ActorPid, _ActorMessage,
						   _State ) ->
	% Probably never useful in this distributed branch:
	throw( { not_implemented, send_delayed_actor_message } ).



% Sends specified message to the specified listed actors, records these sendings
% to wait for the corresponding acknowledgements, and returns an updated
% state. These inter-actor messages exchanged during simulation are the only
% allowed way of communicating between actors.
%
% An actor message parameter describes the behaviour (actor oneway, translating
% to an Erlang function) to trigger when this message will be taken into account
% by the targeted actor, once messages will have been properly reordered.
%
% This sent message corresponds to a oneway, not a request, to avoid any
% blocking operation, as the time management service must be the only one to
% control the course of the simulation.
%
% The sender PID is automatically added, thus it does not need to be specified
% explicitly here. The sender AAI is also automatically added as well, as the
% receiver will need it to reorder its incoming actor messages.
%
% The specified tick is the one expected for the delivery, i.e. the next tick,
% hence the +1.
%
% The actor message is a oneway call: it is described by the name of the actor
% oneway to trigger on the target actor (specified as an atom, ex: 'setColor')
% on the next tick, and by a (possibly empty) list of the corresponding
% arguments; so the call is either 'my_oneway' or
% '{ my_oneway, SingleNonListParameter }' or '{ my_oneway, [ Arg1,... ] }'.
%
% In all cases, the actual call, in the case of an actor message, will be
% performed with an additional parameter, the PID of the sending actor. This
% extra parameter will be transparently added, so an actor oneway which looks
% like a call to a oneway with N parameters specified will trigger a call to a
% function whose arity is N+2: the state, then the N parameters, then the PID of
% the sending actor (i.e.: in that order).
%
% So a typical call made by an actor whose PID is P1 to actors P2 and P3 can be
% made thanks to the following actor message:
%
% NewState = class_Actor:send_actor_messages( [ P2, P3 ], {setColor,[red,15]},
% AState )
%
% This would trigger on the target actors, setColor/4 on the next tick, as the
% PID of the sending actor is automatically added as last parameter:
%
% setColor( State, red, 15, P1 ) ->
%
% Returns an updated state, appropriate to wait automatically for this call to
% be acknowledged.
%
% (helper function)
%
-spec send_actor_messages( [ pid() ], oneway_call(), wooper:state() ) ->
								wooper:state().
send_actor_messages( ActorPidList, ActorOneway, State ) ->

	%io:format( "  ~w sending an actor message to ~w at {~p,~p}: ~p~n",
	%			[ self(), ActorPidList, ?getAttr(current_tick_offset),
	%			   ?getAttr(current_diasca), ActorOneway ] ),

	ActorMessage = { receiveActorMessage,
				[ ?getAttr(current_tick_offset), ?getAttr(current_diasca)+1,
				 ActorOneway, self(), ?getAttr(actor_abstract_id) ] },

	[ ActorPid ! ActorMessage || ActorPid <- ActorPidList ],

	NewAction = case ?getAttr(next_action) of

					Action={ terminating, _Duration } ->
						Action;

					% No test really necessary against terminated: we should not
					% even be scheduled in this case.
					%
					terminated ->
						throw( { no_message_sending_when_terminated,
								ActorPidList, ActorOneway } );

					_ ->
						%io:format( "Actor ~w requesting a new diasca, "
						%			"after having sent an actor message.~n",
						%			[ self() ] ),
						new_diasca_needed

	end,

	% Here we know for sure that the next diasca will have to be scheduled,
	% since the actor message will have to be processed by the recipient.
	%
	% At least one time manager must be notified of that. The one of the actor
	% that receives the actor message should be avoided, since it may have
	% already finished its tick and answered to its parent manager (if any).
	%
	% Conversely, we know for sure that the time manager of this sending actor
	% is still waiting for the end of its tick.
	%
	% Therefore it is up to that sending actor to trigger the scheduling of the
	% next tick, once it will finish its own scheduling for this diasca.
	%
	% This works, as all time managers are notified of all diascas, regardless
	% of what they are to schedule.
	%
	setAttributes( State, [

		{ waited_acks, ActorPidList ++ ?getAttr(waited_acks) },
		{ next_action, NewAction }

						   ] ).



% Makes this actor search for its messages associated to its current tick and
% diasca, sorts them in a particular order, and requests to process them.
%
% Returns an updated state.
%
process_last_diasca_messages( CurrentTickOffset, CurrentDiasca, State ) ->

	% Messages are checked at reception, but another checking is nevertheless
	% performed here, as here we know for sure at which global tick and diasca
	% we are (no possible race condition here between an actor message and the
	% 'begin diasca' message):
	%
	{ CurrentMessages, NextMessages, PastMessages } =
		split_messages_over_time( ?getAttr(pending_messages),
								 CurrentTickOffset, CurrentDiasca ),


	% Exactly as discussed in receiveActorMessage/6, messages in the very next
	% future (i.e. the diasca just after this new one) are licit, because they
	% might have been received before this 'begin diasca' message (that we are
	% processing) if the sending actor got its own 'begin diasca' message and
	% executed quick enough to have this actor message received before.

	% Note: can be commented-out when not in debug mode:
	check_future_messages( NextMessages, CurrentTickOffset, CurrentDiasca ),

	% They come from the past (abnormal):
	case PastMessages of

		[] ->
			ok;

		_ ->
			?error_fmt( "There is at least one actor message in the past: ~p.",
					   [ PastMessages ] ),
			throw( { actor_message_in_the_past, PastMessages } )

	end,

	ReorderedMessages = apply_reordering( CurrentMessages,
										 ?getAttr(message_ordering_mode) ),

	% Executes the actor messages in the relevant order:
	ExecutedState = execute_reordered_oneways( ReorderedMessages, State ),

	% Flushes all messages except the ones in the future:
	setAttribute( ExecutedState, pending_messages, NextMessages ).



% Checks that actor messages in the future are legitimate:
%
check_future_messages( _FutureMessages=[], _CurrentTickOffset,
					  _CurrentDiasca ) ->
	ok;

% Abnormal cases:
check_future_messages( _FutureMessages=[ M=#actor_message{
   tick_offset=MessageOffset, diasca=MessageDiasca } | _T ],
					  CurrentTickOffset, CurrentDiasca )
  when MessageOffset > CurrentTickOffset
	   orelse MessageDiasca > CurrentDiasca + 1->

	throw( { message_from_unexpected_future, {MessageOffset,MessageDiasca},
			{CurrentTickOffset,CurrentDiasca}, M } );

% Only correct case is same offset, next diasca (same diasca not possible -
% these are all future messages):
check_future_messages( _FutureMessages=[ #actor_message{
   tick_offset=CurrentTickOffset, diasca=MessageDiasca } | T ],
					  CurrentTickOffset, CurrentDiasca )
  when MessageDiasca =:= CurrentDiasca + 1 ->

	check_future_messages( T, CurrentTickOffset, CurrentDiasca ).




% Sends in turn the specified oneways (here no third parameter is specified,
% like for requests, however the sender PID is specified in the parameters
% nevertheless, as last argument) to this same process. Appending it at end
% (rather than at beginning) is a bit more expensive, but it is far clearer for
% the implementor of the methods corresponding to the received actor messages.
%
% (AAI is ignored here, as not useful anymore here)
%
% Basically a three-clause fold:
%
execute_reordered_oneways( _Messages=[], State ) ->
	State;

% List parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, _SenderAAI, {OnewayName,OnewayArgList} } | MessageTuples ],
		State ) when is_list(OnewayArgList) ->

	NewState = executeOneway( State, OnewayName,
				list_utils:append_at_end( SenderPid, OnewayArgList ) ),

	execute_reordered_oneways( MessageTuples, NewState );

% Standalone parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, _SenderAAI, { OnewayName, OnewaySingleNonListArg } }
									| MessageTuples ], State ) ->

	NewState = executeOneway( State, OnewayName,
							 [ OnewaySingleNonListArg, SenderPid ] ),

	execute_reordered_oneways( MessageTuples, NewState );

% No parameter here:
execute_reordered_oneways( _Messages=[
	   { SenderPid, _SenderAAI, OnewayName } | MessageTuples ], State ) ->

	NewState = executeOneway( State, OnewayName, SenderPid ),

	execute_reordered_oneways( MessageTuples, NewState ).







% Actor-side management.


% Updates this actor's agenda for checking with specified future action.
%
% Returns an updated agenda.
%
-spec update_agenda_with( [ class_TimeManager:tick_offset() ],
			  [ class_TimeManager:tick_offset() ], agenda() ) -> agenda().
update_agenda_with( AddedTicks, WithdrawnTicks, Agenda ) ->

	% We withdraw before adding, hence if a never-specified tick is to be added
	% and withdrawn at the same diasca, the operation will fail:
	WithdrawAgenda = lists:foldl(
					fun( Tick, AccAgenda ) ->
							list_utils:delete_existing( Tick, AccAgenda ) end,
					_WithdrawAcc0=Agenda,
					_WithdrawList=WithdrawnTicks ),

	%lists:sort( list_utils:uniquify( WithdrawAgenda ++ AddedTicks ) ),

	lists:foldl(
					fun( Tick, AccAgenda ) ->
							insert_in_agenda( Tick, AccAgenda ) end,
					_AddAcc0=WithdrawAgenda,
					_AddList=AddedTicks ).



% Inserts the specified tick offset into specified agenda.
%
insert_in_agenda( TickOffset, Agenda ) ->
	insert_in_agenda( TickOffset, Agenda, _FirstAgendaEntries=[] ).


insert_in_agenda( TickOffset, _Agenda=[], FirstAgendaEntries ) ->
	% Agenda exhausted, insert at end:
	lists:reverse( [ TickOffset | FirstAgendaEntries ] );

insert_in_agenda( TickOffset, [ TickOffset | _T ]=Agenda,
				 FirstAgendaEntries ) ->
	% Previously a given actor could not register more than once to the same
	% tick; as some users felt the need for it, now an actor can specify the
	% same future tick multiple times; this will not raise an exception anymore,
	% and will trigger only one scheduling of that actor during the tick that
	% was specified multiple times:
	%
	%throw( { tick_already_in_agenda, TickOffset,
	%	lists:reverse( FirstAgendaEntries ) ++ Agenda } );

	% So now we just ignore repeated tick declarations and return the original
	% agenda:
	lists:reverse( FirstAgendaEntries ) ++ Agenda;

insert_in_agenda( TickOffset, [H|_T]=Agenda, FirstAgendaEntries )
		when H > TickOffset ->
	% We went past the point where this offset should be specified:
	lists:reverse( FirstAgendaEntries ) ++ [ TickOffset | Agenda ];

% Here TickOffset > H:
insert_in_agenda( TickOffset, [ H | T ], FirstAgendaEntries ) ->
		% (implied: 'when H < TickOffset ->')
	% We shall continue, still in lower offsets:
	insert_in_agenda( TickOffset, T, [ H | FirstAgendaEntries ] ).




% Sorts the specified list of messages into three lists (returned as a triplet):
% the messages corresponding to the specified tick, the ones in the future of
% that tick, and the ones in its past.
%
% For messages corresponding to the current tick, the tick information is
% removed: instead of a 4-element tuple, a {Pid,Aai,Message} triplet is
% returned.
%
split_messages_over_time( Messages, CurrentTickOffset, CurrentDiasca ) ->
	split_messages_over_time( Messages, CurrentTickOffset, CurrentDiasca,
					 _CurrentOnes=[], _FutureOnes=[], _PastOnes=[] ).



split_messages_over_time( _Messages=[], _CurrentTickOffset, _CurrentDiasca,
						 CurrentOnes, FutureOnes, PastOnes ) ->
	{ CurrentOnes, FutureOnes, PastOnes };

split_messages_over_time(
		[ H=#actor_message{ tick_offset=MessageTickOffset } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageTickOffset < CurrentTickOffset ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							 CurrentOnes, FutureOnes, [ H | PastOnes ] );

split_messages_over_time(
		[ H=#actor_message{ tick_offset=MessageTickOffset } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageTickOffset > CurrentTickOffset ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							 CurrentOnes, [ H | FutureOnes ], PastOnes );

% Here 'when MessageTickOffset =:= CurrentTickOffset' is implied by
% pattern-matching, now taking into account the diascas:
split_messages_over_time(
		[ H=#actor_message{ diasca=MessageDiasca } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageDiasca < CurrentDiasca ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							 CurrentOnes, FutureOnes, [ H | PastOnes ] );

split_messages_over_time(
		[ H=#actor_message{ diasca=MessageDiasca } | T ],
		CurrentTickOffset, CurrentDiasca, CurrentOnes, FutureOnes, PastOnes )
  when MessageDiasca > CurrentDiasca ->
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
							 CurrentOnes, [ H | FutureOnes ], PastOnes );

% Here 'when MessageTickOffset =:= CurrentTickOffset andalso MessageDiasca =:=
% CurrentDiasca' is implied by pattern-matching:
%
% (could be refactored to be the first clause, as it must be by far the most
% commonly used one)
%
split_messages_over_time( [ #actor_message{ sender_pid=Pid, sender_aai=Aai,
			actual_message=Message} | T ], CurrentTickOffset, CurrentDiasca,
						 CurrentOnes, FutureOnes, PastOnes ) ->
	% No timestamp kept in the 'current list':
	split_messages_over_time( T, CurrentTickOffset, CurrentDiasca,
		   [ { Pid, Aai, Message } | CurrentOnes ], FutureOnes, PastOnes ).





% Section dedicated to the creation of actors.


% Creates synchronously a new initial actor, whereas the simulation has not been
% started yet.
%
% This must be called only directly from test/simulation cases, to create the
% initial situation before the simulation time starts progressing.
%
% This is a synchronous operation, to ensure that no race condition occurs
% between the creation of initial actors and the start of the simulation.
%
% The new actor will be placed according to the default placement heuristic of
% the engine.
%
% Use create_actor/3 whenever needing to create an actor in the course of the
% simulation (in that case, actors must be created by other actors only).
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
% (static)
%
-spec create_initial_actor( class_name(), [ method_argument() ] ) ->
								  actor_pid().
create_initial_actor( ActorClassname, ActorConstructionParameters ) ->

	% No guard needed here, as will be applied in create_initial_actor/3:

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	create_initial_actor( ActorClassname, ActorConstructionParameters,
						  LoadBalancerPid ).



% Creates synchronously a new initial actor, whereas the simulation has not been
% started yet.
%
% Behaves exactly like the create_initial_actor/2 static method, except it
% relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
% (static)
%
-spec create_initial_actor( class_name(), [ method_argument() ], pid() ) ->
								  actor_pid().
create_initial_actor( ActorClassname, ActorConstructionParameters,
		LoadBalancerPid ) when is_atom( ActorClassname )
			andalso is_list( ActorConstructionParameters )
			andalso is_pid( LoadBalancerPid ) ->

	LoadBalancerPid ! { createInitialActor,
		[ ActorClassname, ActorConstructionParameters ], self() },

	receive

		{ wooper_result, ActorPid } when is_pid( ActorPid ) ->
			ActorPid

	end.



% Creates synchronously a new initial actor, whereas the simulation has not been
% started yet.
%
% This must be called only directly from test/simulation cases, to create the
% initial situation before the simulation time starts to progress.
%
% This is a synchronous operation, to ensure that no race condition occurs
% between the creation of initial actors and the start of the simulation.
%
% The new actor will be placed by the engine according to the specified
% placement hint: all actors created with a given placement hint are guaranteed
% to be placed on the same computing node, automatically selected approriately
% by the engine.
%
% This allows to ensure that the actors that are the most tightly linked are
% co-allocated and thus, being in the same node, interact with as little
% overhead as possible.
%
% Use create_actor/3 whenever needing to create an actor in the course of the
% simulation (in that case, actors must be created by other actors only).
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
% (static)
%
-spec create_initial_placed_actor( class_name(), [ method_argument() ],
			class_LoadBalancer:placement_hint() ) -> pid().
create_initial_placed_actor( ActorClassname, ActorConstructionParameters,
							PlacementHint ) ->

	% No guard needed here, as will be applied in create_initial_placed_actor/4:

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	create_initial_placed_actor( ActorClassname, ActorConstructionParameters,
						LoadBalancerPid, PlacementHint ).



% Creates synchronously a new initial actor, whereas the simulation has not been
% started yet.
%
% Behaves exactly like the create_initial_placed_actor/3 static method, except
% it relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
% This static method is specific to the Sim-Diasca distributed version (as the
% load balancer is involved).
%
% Returns the PID of the newly created (initial) actor, or throws an exception.
%
% (static)
-spec create_initial_placed_actor( class_name(), [ method_argument() ],
			pid(), class_LoadBalancer:placement_hint() ) -> pid().
create_initial_placed_actor( ActorClassname, ActorConstructionParameters,
					 LoadBalancerPid, PlacementHint )
  when is_atom( ActorClassname ) andalso is_list( ActorConstructionParameters )
	   andalso is_pid( LoadBalancerPid ) ->

	LoadBalancerPid ! { createInitialPlacedActor,
		[ ActorClassname, ActorConstructionParameters, PlacementHint ],
		  self() },

	receive

		{ wooper_result, ActorPid } when is_pid(ActorPid) ->
			ActorPid

	end.



% Creates synchronously - and in parallel - the specified list of new initial
% actors, whereas the simulation has not been started yet.
%
% Each actor is created based on the specified class name and construction
% parameters, possibly augmented of a placement hint. For example: { class_X, [
% P1, P2] } or { class_Y, [ 34 ], my_hint }
%
% Returns the list of the PID of the newly created (initial) actors (in the
% order of their specification in the input list), or throws an exception.
%
% (static)

-spec create_initial_actors( [ instance_creation_spec() ] ) -> [ actor_pid() ].
create_initial_actors( ActorConstructionList ) ->

	% No guard needed here, as will be applied in create_initial_actor/3:

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	create_initial_actors( ActorConstructionList, LoadBalancerPid ).



% Creates synchronously - and in parallel - the specified list of new initial
% actors, whereas the simulation has not been started yet.
%
% Each actor is created based on the specified class name and construction
% parameters, possibly augmented of a placement hint. For example: { class_X, [
% P1, P2] } or { class_Y, [ 34 ], my_hint }
%
% Returns the list of the PIDs of the newly created (initial) actors (in the
% order of their specification in the input list), or throws an exception.
%
% Behaves exactly like the create_initial_actors/1 static method, except it
% relies on a user-specified PID for the load balancer.
%
% It is useful whenever having a large number of initial actors to create, as it
% allows to retrieve the load balancer PID only once for all, instead of having
% each create_initial_actor/2 performing a useless look-up for it.
%
% (static)
%
-spec create_initial_actors( [ instance_creation_spec() ], pid() ) ->
								   [ actor_pid() ].
create_initial_actors( ActorConstructionList, LoadBalancerPid )
  when is_list( ActorConstructionList ) andalso is_pid( LoadBalancerPid ) ->

	LoadBalancerPid ! { createInitialActors, [ ActorConstructionList ],
						self() },

	receive

		{ wooper_result, ActorPidList } when is_list(ActorPidList) ->
			ActorPidList

	end.




% Helper functions.


% Returns the name of this actor, as a binary.
%
% Note: is never and cannot be overridden.
%
% Allows to mask the inheritance from TraceEmitter.
%
% (const helper)
%
-spec get_name( wooper:state() ) -> class_Actor:internal_name().
get_name( State ) ->
	?getAttr(name).



% Returns true iff this actor is running, i.e. if it has been synchronised to
% the simulation, that thus has already been started.
%
% (const helper)
%
-spec is_running( wooper:state() ) -> boolean().
is_running( State ) ->

	% simulationStarted/3 is the only one to set it:
	case ?getAttr(initial_tick) of

		undefined ->
			false;

		_Other ->
			true

	end.



% Triggers the creation of a new actor, while the simulation is running.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/5 method called with the PID of the newly created actor.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the load
% balancer.
%
% Helper parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% the 'class_TestActor' atom)
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{2,3} or create_initial_placed_actor/{3,4} must be used
% instead.
%
% Returns an updated state.
%
% (helper function)
%
-spec create_actor( class_name(), [ method_argument() ], wooper:state() ) ->
						wooper:state().
create_actor( ActorClassname, ActorConstructionParameters, State )
  when is_atom(ActorClassname) andalso is_list(ActorConstructionParameters) ->

	%io:format( "create_actor: creating an instance of ~p.~n",
	%		  [ ActorClassname ] ),

	% The load balancer is an actor, thus two diascas will be requested.

	% Will trigger back, on this actor, onActorCreated/5 in two diascas:
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createActor, [ ActorClassname, ActorConstructionParameters ] },
		State ).



% Triggers the creation of a new actor with a placement hint, while the
% simulation is running.
%
% This is an actor-level helper function, which takes care automatically of the
% sending (at this tick T, diasca D) of the createActor actor message to the
% load balancer, which in turn, at the next diasca (still during T, at diasca
% D+1), will effectively create the targeted actor (with a remote synchronous
% timed new) until, at diasca D+2 of the same tick T, this creating actor has
% its onActorCreated/5 method called with the PID of the newly created actor.
%
% The load balancer is to create actors based on actor messages, for
% reproducibility reasons.
%
% The actual placement of the created actor is fully determined by the specified
% placement hint.
%
% Helper parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% Note: if wanting to create an actor before the simulation is started, then
% create_initial_actor/{2,3} or create_initial_placed_actor/{3,4} must be used
% instead.
%
% Returns an updated state.
%
% (helper function)
%
-spec create_placed_actor( class_name(), [ method_argument() ],
	  class_LoadBalancer:placement_hint(), wooper:state() ) -> wooper:state().
create_placed_actor( ActorClassname, ActorConstructionParameters, PlacementHint,
					State ) when is_atom(ActorClassname)
								andalso is_list(ActorConstructionParameters) ->

	% Will trigger back, on this actor, onActorCreated/5 in two diascas:
	send_actor_message( ?getAttr(load_balancer_pid),
		{ createPlacedActor, [ ActorClassname, ActorConstructionParameters,
							PlacementHint ] }, State ).


% Allows an actor to declare a probe, with specified parameters.
%
% The probe creation may or may not be accepted by the result manager.
%
% If yes, PID of the newly created probe will be returned.
%
% If no, the 'non_wanted_probe' atom will be returned.
%
% (helper function)
%
-spec declare_probe( class_Probe:name_options(),
		 [ class_Probe:declared_curve_name() ], [ class_Probe:declared_zone() ],
		 text_utils:title(), text_utils:label(), text_utils:label() ) ->
						   class_Probe:probe_pid().
declare_probe( NameOptions, CurveNames, Title, Zones,
		XLabel, YLabel ) ->

	% From an actor, any created probe will write its files under the directory
	% for temporary data (by default under '/tmp'), the current directory of all
	% computing nodes:
	%
	class_Probe:declare_result_probe( NameOptions, CurveNames, Title, Zones,
						XLabel, YLabel ).



% Allows to enable this actor to make use of the data-exchange service.
%
% Returns an updated state.
%
% (helper function)
%
-spec enable_data_exchange( wooper:state() ) -> wooper:state().
enable_data_exchange( State ) ->
	setAttribute( State, exchange_settings,
				class_DataExchanger:get_actor_exchange_settings() ).



% Allows this actor to define a new set of data entries.
%
% A data not specifying a qualifier will be defined with the default one.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( class_DataExchanger:entries(), wooper:state() ) ->
						basic_utils:void().
define_data( EntryList, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ EntryList ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% Allows this actor to define a new data entry.
%
% The data will be defined with the default qualifier.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( class_DataExchanger:key(), class_DataExchanger:value(),
				wooper:state() ) -> basic_utils:void().
define_data( Key, Value, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ Key, Value ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% Allows this actor to define a new data entry.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the definition will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the definition will occur in-between the end of the current tick and
% the beginning of the next one.
%
-spec define_data( class_DataExchanger:key(), class_DataExchanger:value(),
	  class_DataExchanger:qualifier(), wooper:state() ) -> basic_utils:void().
define_data( Key, Value, Qualifier, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { defineData, [ Key, Value, Qualifier ], self() },
	receive

		{ wooper_result, data_defined } ->
			ok

	end.



% Allows this actor to modify a new set of data entries.
%
% A data not specifying a qualifier will be defined with the default one.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( class_DataExchanger:entries(), wooper:state() ) ->
		basic_utils:void().
modify_data( EntryList, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ EntryList ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.



% Allows this actor to define a new data entry.
%
% The data will be defined with the default qualifier.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( class_DataExchanger:key(), class_DataExchanger:value(),
		wooper:state() ) -> basic_utils:void().
modify_data( Key, Value, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ Key, Value ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.



% Allows this actor to define a new data entry.
%
% Note: this is a synchronous operation to avoid race conditions.
%
% If the simulation is not running, the modification will happen immediately and
% be available everywhere in the exchanger hierarchy. If the simulation is
% running, the modification will occur in-between the end of the current tick
% and the beginning of the next one.
%
-spec modify_data( class_DataExchanger:key(), class_DataExchanger:value(),
	class_DataExchanger:qualifier(), wooper:state() ) -> basic_utils:void().
modify_data( Key, Value, Qualifier, State ) ->

	% Setting data involves the root data-exchanger:
	{ RootDataExchangerPid, _LocalDataExchangerPid } =
		?getAttr(exchange_settings),

	RootDataExchangerPid ! { modifyData, [ Key, Value, Qualifier ], self() },
	receive

		{ wooper_result, data_modified } ->
			ok

	end.




% Returns the value associated to the specified key (an atom).
%
% Actors can read directly any number of data without any particular precautions
% on the same tick, and these operations will be as cheap as reasonably
% possible.
%
% (helper function)
%
-spec read_data( class_DataExchanger:key(), wooper:state() ) ->
					   class_DataExchanger:value().
read_data( Key, State ) ->

	% Reads are purely local (and enable_data_exchange/1 must have been called
	% beforehand):
	{ _RootExchangerPid, LocalExchangerPid } = ?getAttr(exchange_settings),

	% The data-exchanger service has been designed so that this reading can be
	% done locally and directly, with no message reordering nor latency:
	LocalExchangerPid ! { readData, Key, self() },
	receive

		{ wooper_result, V } ->
			V

	end.



% Returns the value and qualifier (as a {Value,Qualifier} pair) associated to
% the specified key (an atom).
%
% Actors can read directly any number of data without any particular precautions
% on the same tick, and these operations will be as cheap as reasonably
% possible.
%
% (helper function)
-spec read_qualified_data( class_DataExchanger:key(), wooper:state() ) ->
			class_DataExchanger:qualified_value().
read_qualified_data( Key, State ) ->

	% Reads are purely local (and enable_data_exchange/1 must have been called
	% beforehand):
	{ _RootExchangerPid, LocalExchangerPid } = ?getAttr(exchange_settings),

	% The data-exchanger service has been designed so that this reading can be
	% done locally and directly, with no message reordering nor latency:
	LocalExchangerPid ! { readQualifiedData, Key, self() },
	receive

		{ wooper_result, V } ->
			V

	end.





% Returns a reordered version of the specified list of messages for the current
% diasca, to obtain an order satisfying the expected properties for the
% simulation.
%
% Depending on the simulator settings (second parameter of this function), the
% ordering (if any is requested) will be performed either so that reproductivity
% is ensured (i.e. events are sorted according to a constant arbitrary order),
% and/or so that "ergodicity" is ensured, i.e. so that events are shuffled in an
% uniform way (random permutations).
%
% See the 'Reordering Of Actor Messages' section of documentation for more
% information.
%
apply_reordering( MessagesForCurrentDiasca, unordered ) ->
	% Nothing to do here!
	MessagesForCurrentDiasca;

apply_reordering( MessagesForCurrentDiasca, constant_arbitrary_order ) ->

	% Pending messages for this diasca are an unordered list of {Pid,AAI,Msg}
	% elements.
	%
	% Sorts the incoming messages based first on the message (element 3), then,
	% if necessary on the sender AAI (element 2), if ever two messages had an
	% identical hash (ex: same message sent by different actors), so that the
	% reproducibility is ensured (as there is a bijection between AAI and PID,
	% PID do not need to be taken into account; and if an actor sends the same
	% message to the same actor more than once at the same diasca, their
	% relative order will not matter - they will be exactly the same).
	%
	% Therefore the PID - which is only a technical identifier - is ignored
	% here.
	%
	% Ex: if having a message list L = [ {Pa,5,5}, {Pb,4,5}, {Pc,6,5}, {Pd,1,7},
	% {Pe,10,5}, {Pf,2,5}, {Pg,3,5}, {Ph,7,5}, {Ph,7,1}, {Pa,5,8}, {Pa,5,1} ]
	% then 'lists:keysort( 3, lists:keysort(2,L) )' returns:
	% [ {Pa,5,1}, {Ph,7,1}, {Pf,2,5}, {Pg,3,5}, {Pb,4,5}, {Pa,5,5}, {Pc,6,5},
	% {Ph,7,5}, {Pe,10,5}, {Pd,1,7}, {Pa,5,8}] (P stands for 'PID'), i.e. the
	% list is sorted first according to its third member (the message, here an
	% integer), then to its second (the AAI).
	%
	% Note also that the first sort (chronologically) yields:
	% lists:keysort(2,L) = [ {Pd,1,7}, {Pf,2,5}, {Pg,3,5}, {Pb,4,5}, {Pa,5,5},
	% {Pa,5,8}, {Pa,5,1}, {Pc,6,5}, {Ph,7,5}, {Ph,7,1}, {Pe,10,5} ]. i.e.
	% it is sorted in ascending AAI order.
	%
	% Note also that using keysort implies relying not on the hash value of the
	% term, but on the natural order of Erlang terms.
	%
	lists:keysort( 3, lists:keysort( 2, MessagesForCurrentDiasca ) );

apply_reordering( MessagesForCurrentDiasca, constant_permuted_order ) ->

	% Returns a reordered version of the specified list of messages for the
	% current tick, to obtain a constant order compatible to either simulation
	% modes (reproducible or ergodic), as they are determined here only by the
	% random seed they rely on.
	%
	% Here we just need to perform a random (i.e. using the actor's correctly
	% seeded random generator) shuffle of messages (uniform permutation) once
	% they have been first put into a stable order (that's all!):
	%
	list_utils:random_permute( apply_reordering( MessagesForCurrentDiasca,
												 constant_arbitrary_order ) ).




% Returns the highest acceptable idle duration, in milliseconds, for the
% completion of the subscription process.
%
-spec get_maximum_subscription_duration() -> unit_utils:milliseconds().



-ifdef(exec_target_is_production).


% In production mode here:

get_maximum_subscription_duration() ->
	% 60 seconds:
	60 * 1000.


-else.


% In development mode here:

get_maximum_subscription_duration() ->
	% 8 seconds:
	8 * 1000.


-endif.
