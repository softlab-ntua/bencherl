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



% Broadcasting actor base class.
%
% This is a specialization of the generic actor, for all models that at least
% once have to send the same actor message to a large number (ex: at least
% several thousands) of actors.
%
% This broadcasting actor manages its waited actors thanks to a ?list_impl list,
% rather than thanks to a plain list, which is a lot more efficient.
%
% If applicable, for larger numbers of actors, the sendings might be done over
% more than one diasca. See the load balancer has a typical example.
%
% Note: one must ensure that a broadcasting actor sends messages with its own
% static send_actor_message/3 and send_actor_messages/3 rather than the ones of
% class_Actor.
%
% See also class_Actor.erl
%
-module(class_BroadcastingActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



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
-define( wooper_method_export, beginTick/2, beginDiasca/3, acknowledgeMessage/2,
		 sendActorMessagesOverDiascas/4, nudge/2 ).




% Helper functions.


% Helpers for scheduling, message sending, creation, etc.:
%
-export([ send_actor_message/3, send_actor_messages/3,
		  send_actor_messages_over_diascas/3 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.BroadcastingActor").


% For #actor_settings:
-include("class_LoadBalancer.hrl").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For time_manager_name, virtual_seconds, etc.:
-include("class_TimeManager.hrl").


% For list_impl:
-include("data_types.hrl").


% Implementation notes:
%
% A broadcasting actor is basically an actor whose waited_acks attribute changed
% from [ pid() ] to ?list_impl( pid() ).
%
% This choice would induce overhead in terms of processing and memory to the
% vast majority of actors, hence the subclass defined here.
%
% We could imagine later making the same change for the pending_messages
% attribute (if ensuring it will be managed exactly the same in both classes).



% Actor-specific attributes are:
%
% - waited_acks :: ?list_impl( pid() ) is inherited from class_Actor but has
% changed of type; is now a (not plain anymore) list of the PID of the actors to
% which this actor sent an actor message this diasca; it allows it to notify
% adequately the time manager that its diasca is finished indeed; for actors
% sending a large number of actor messages (ex: the load balancer sending the
% onFirstDiasca message to all actors)
%
% - chunk_size :: basic_utils:count() is the number of actors per chunk


% Constructs a new broadcasting actor:
%
% - ActorSettings :: actor_settings() describes the actor abstract identifier
% (AAI) and seed of this actor, as assigned by the load balancer
%
% - ActorName :: string() is a human-readable name for that actor (as a plain
% string); it is preferably not too long and without whitespaces
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name() ) -> wooper:state().
construct( State, ActorSettings, ActorName ) ->

	% First the direct mother classes:
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),

	% Then the class-specific actions:
	setAttributes( ActorState, [

		{ waited_acks, ?list_impl:new() },
		{ chunk_size, 8000 },
		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								] ).



% To silence Dialyzer:
%
-spec destruct( wooper:state() ) ->  wooper:state().
destruct( State ) ->
	State.



% Methods section.



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

	% Note: exactly as the class_Actor counterpart, except for waited_acks:

	Agenda = ?getAttr(current_agenda),

	%io:format( " beginTick for actor ~w at tick offset #~B, with agenda ~w.~n",
	%   [ self(), NewTickOffset, Agenda ] ),

	% Comment to disable these checkings:
	class_Actor:check_spontaneous_tick_consistency( NewTickOffset, State ),


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
	class_Actor:validate_scheduling_outcome( SpontaneousState ),

	% The 'actSpontaneous' method might have sent actor messages:
	AckState = case ?list_impl:is_empty(
					getAttribute( SpontaneousState, waited_acks ) ) of

		true ->
				notify_diasca_ended( SpontaneousState );

		false ->
				% End of tick to be determined by acknowledgeMessage/2:
				SpontaneousState

	end,

	?wooper_return_state_only( AckState ).



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

	% Note: exactly as the class_Actor counterpart, except for waited_acks:

	%io:format( " beginDiasca for ~w at diasca ~B of tick offset #~B.~n",
	%		   [ self(), NewDiasca, TickOffset ] ),

	% Comment to disable these checkings:
	class_Actor:check_diasca_consistency( TickOffset, NewDiasca, State ),

	% Other attributes set at the end of previous scheduling:
	PreState = setAttributes( State, [

					% This is not superfluous: we might have received an actor
					% message while being still lagging in a past tick:
					{ current_tick_offset, TickOffset },
					{ current_diasca, NewDiasca }

									  ] ),

	TriggerState = class_Actor:process_last_diasca_messages( TickOffset,
														 NewDiasca, PreState ),

	% Note: we are not checking the correctness of the engine here, we ensure
	% models are properly written (hence this should not be commented out).
	%
	class_Actor:validate_scheduling_outcome( TriggerState ),

	% The triggered methods might have sent actor messages:
	AckState = case ?list_impl:is_empty(
					  getAttribute( TriggerState, waited_acks ) ) of

		true ->
				notify_diasca_ended( TriggerState );

		false ->
				% End of diasca to be determined by acknowledgeMessage/2:
				TriggerState

	end,

	?wooper_return_state_only( AckState ).



% Callback triggered by the reception of an acknowledgement of an actor to which
% this actor sent a message.
%
% (oneway)
%
-spec acknowledgeMessage( wooper:state(), pid() ) -> oneway_return().
acknowledgeMessage( State, CalledActorPid ) ->

	% Note: exactly as the class_Actor counterpart, except for waited_acks:

	Waited = ?getAttr(waited_acks),

	% Check we are indeed waiting for this ack, remove it from list, see if it
	% was the last waited one:
	%
	ShortenWaitedList = list_utils:safe_listimpl_delete( CalledActorPid,
														 Waited ),

	ShortenState = setAttribute( State, waited_acks, ShortenWaitedList ),

	NewState = case ?list_impl:is_empty( ShortenWaitedList ) of

				true ->
					% Last ack received, ready to declare this actor's end of
					% diasca:
					%
					notify_diasca_ended( ShortenState );

				false ->
					% There is still at least one waited ack, still waiting:
					ShortenState

			end,

	?wooper_return_state_only( NewState ).



% Sends specified actor message on as many diascas as needed to reach all actors
% whose PID are listed in the attribute of specified name.
%
% (actor oneway)
%
-spec sendActorMessagesOverDiascas( wooper:state(), oneway_call(),
			   attribute_name(), pid() ) -> oneway_return().
sendActorMessagesOverDiascas( State, ActorMessage, AttributeName, _SelfPid ) ->

	SentState = send_actor_messages_over_diascas( AttributeName, ActorMessage,
												  State ),

	?wooper_return_state_only( SentState ).



% Sends to the local time manager a notification that the current diasca ended.
%
% Returns an updated state.
%
% (helper)
%
notify_diasca_ended( State ) ->

	% Note: exactly as the class_Actor counterpart, except for waited_acks.

	% Checking:
	true = ?list_impl:is_empty( ?getAttr(waited_acks) ),

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

	NewAgenda = class_Actor:update_agenda_with( AddedTicks, WithdrawnTicks,
										   ?getAttr(current_agenda) ),

	% Prepare for next diasca, reset relevant attributes:
	setAttributes( State, [

				{ previous_schedule, { CurrentTickOffset, CurrentDiasca } },
				{ added_spontaneous_ticks, [] },
				{ withdrawn_spontaneous_ticks, [] },
				{ next_action, NextRecordedAction },
				{ current_agenda, NewAgenda }

								   ] ).




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

	SenderPid ! { notifyNudged, [ self(),
					class_Actor:get_current_tick_offset( State ),
					?list_impl:to_list( ?getAttr(waited_acks) ) ] },

	?wooper_return_state_only( State ).



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

		{ waited_acks,
		 ?list_impl:add_element( ActorPid, ?getAttr(waited_acks) ) },

		{ next_action, NewAction }

						   ] ).



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
% '{my_oneway,SingleNonListParameter}' or '{my_oneway,[Arg1,...]}'.
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
send_actor_messages( _ActorPidList=[], _ActorOneway, State ) ->
	% No target, no state change wanted:
	State;

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

		{ waited_acks, list_utils:listimpl_add( ?getAttr(waited_acks),
												ActorPidList ) },

		{ next_action, NewAction }

						   ] ).



% Sends specified (actor) message to the actors designated by the specified
% attribute, supposed to be a plain list (from which this function will remove
% elements). If the list is too long, the sending will be done by chunks (one
% chunk per diasca), and as many additional diascas as needed will be requested
% to exhaust the list. The actor must call this function as long as the list is
% not empty.
%
% See the load-balancer for an example.
%
% Returns an updated state.
%
% (helper function)
%
-spec send_actor_messages_over_diascas( attribute_name(), oneway_call(),
									  wooper:state() ) -> wooper:state().
send_actor_messages_over_diascas( AttributeName, ActorOneway, State ) ->

	ActorList = ?getAttr(AttributeName),

	% Gets a chunk from the actor list:
	{ FirstActors, RemainingActors } = list_utils:split_at( ActorList,
													?getAttr(chunk_size) ),

	%io:format( "Actor message ~p sent to a chunk of ~B actors at ~p.~n",
	%	[ ActorOneway, length(FirstActors),
	%	  { ?getAttr(current_tick_offset), ?getAttr(current_diasca) } ] ),

	SentState = send_actor_messages( FirstActors, ActorOneway, State ),

	case RemainingActors of

		[] ->
			% We just exhausted the list, not requesting any new action/diasca:
			SentState;

		_ ->
			% The sending must go on at the next diasca, let's send us (to
			% ourselves) an actor message for that:
			PlanState = send_actor_message( _Target=self(),
			  { sendActorMessagesOverDiascas, [ ActorOneway, AttributeName ] },
			  SentState ),

			setAttribute( PlanState, AttributeName, RemainingActors )

	end.
