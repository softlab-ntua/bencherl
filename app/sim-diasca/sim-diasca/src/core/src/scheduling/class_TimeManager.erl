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



% Distributed management of simulation time.
%
% The time manager is expected to be a singleton on each computing node.
%
% Its process is registered locally under the name returned by the
% get_registration_name/0 static method.
%
% This is a time-driven simulation, actors are expected to be synchronous.
%
% See class_TimeManager_batch_test.erl and
% class_TimeManager_interactive_test.erl.
%
-module(class_TimeManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, SimulationTickDuration, InteractivityMode,
		 ParentManagerInformation, RootInstanceTrackerPid, TroubleshootingMode,
		 Context ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, delete/1 ).



% Member method declarations.
-define( wooper_method_export,

		 declareChildManager/1, setLoadBalancerPid/2,

		 getInitialTick/1, setInitialTick/2, setInitialSimulationDate/3,
		 getFinalTick/1, setFinalTick/2, setFinalSimulationDate/3,

		 start/1, start/2, start/3, startFor/2, startFor/3, stop/1, selfStop/1,
		 suspend/1, resume/1,

		 addSimulationListener/2, removeSimulationListener/2,
		 addTimeListener/2, removeTimeListener/2,

		 registerResilienceManager/1, unregisterResilienceManager/1,

		 serialisationRequested/1, getAllLocalActors/1, mergeWith/2, relink/1,
		 restartAfterRollback/1,

		 beginTimeManagerTick/2,
		 notifySpontaneousSubtreeCompletion/5,
		 notifySpontaneousActionsCompleted/6,
		 notifySpontaneousWatchdogCompleted/2,

		 beginTimeManagerDiasca/3,
		 notifyTriggerSubtreeCompletion/6,
		 notifyTriggeredActionsCompleted/7,
		 notifyTriggeredWatchdogCompleted/3,

		 timerTickFinished/1,

		 getSimulationTick/1, getSimulationTickOffset/1,
		 getSimulationDiasca/1, getSimulationLogicalTimestamp/1,
		 getSimulationDate/1, getTextualTimings/1,

		 convertTicksToSeconds/2, convertTicksToPreciseDuration/2,
		 convertSecondsToTicks/2, convertSecondsToNonNullTickDuration/2,

		 simulationStarted/3,

		 subscribe/4, unsubscribe/1,

		 scheduleTrigger/3, notifyNudged/4,
		 onWooperExitReceived/3, onWallclockMilestone/2, onTickMilestone/2,
		 onSimulationStallDetected/1,

		 getProgressDiagnosis/1,

		 declareDataExchanger/2, requestInterDiascaNotification/1,
		 notifyOverallActorCount/2,

		 display/1, toString/1 ).



% Static method declarations (to be directly called from module):
-define( wooper_static_method_export, settings_to_string/1,
		 get_registration_name/0, get_any_manager/0,
		 merge_local_with/1 ).



% Helper functions:
-export([ get_current_tick_offset/1, get_current_tick/1,
		  get_simulation_time_and_date/1, timestamp_to_ticks/2,
		  display_waiting_reason/1 ]).



% Useful for testing directly from the shell:
-export([ test_spontaneous_lists/0, test_min_max_timestamps/0 ]).




% Simulation absolute tick (thus in virtual time):
%
-type tick() :: integer().



% Simulation tick offset (difference between two ticks):
%
-type tick_offset() :: integer().



% For list_impl and al:
-include("data_types.hrl").


% Agenda associating to tick offsets a list (actually, a list_impl) of actors.
%
% Pairs are sorted according to increasing offset order.
%
-type agenda() :: [ { tick_offset(), ?list_impl_type } ].



% A diasca is the count of logical steps gone through a given tick.
%
% At each tick, it starts at zero, and is incremented as many times as needed to
% resolve all causal exchanges that are to take place during this tick.
%
% As such, a diasca does not imply any specific duration in virtual time: we
% just know that, during a given tick T, all events generated during a diasca
% (i.e. all actor messages sent during this diasca) happened logically before
% all events generated during the next diasca (still on the same tick T)
%
% So when a diasca elapses, the overall logical clock does not progress at all.
%
-type diasca() :: non_neg_integer().



% A logical timestamp is the current time in the simulation (expressed thank to
% a tick offset) and current the number of diasca that were needed to resolve
% it (at least one if the tick is scheduled at all).
%
-type logical_timestamp() :: { tick_offset(), diasca() }.



% Used by time managers to report the next action they plan (their scheduling
% subtree included) to their parent time manager: either no action planned at
% all, or a new diasca (if at least one actor in their subtree sent an actor
% message on the current diasca), or a new (possibly distant) tick to schedule
% (the smaller they have in the subtree).
%
-type next_manager_action() :: 'no_planned_action' | 'new_diasca_needed'
							  | tick_offset().





% The tracking information sent by a time manager to its parent once a diasca is
% over.
%
% Respectively:
%
% - number of schedulings done (spontaneous actions if at diasca 0, otherwise
% triggered actors)
%
% - number of local processes
%
-type diasca_tracking_info() :: { basic_utils:count(), basic_utils:count() }.



% The main simulation events that may be listened to:
%
-type simulation_events() :: 'simulation_started'   | 'simulation_suspended'
	| 'simulation_resumed' | 'simulation_succeeded' | 'simulation_stopped'.



% Describes whether the simulation is paced according to real time (thus
% interactive), possibly with a scale factor, or runs as fast as possible (thus
% batch):
%
-type interactivity_mode() :: 'interactive' | 'batch'.


-export_type([ tick/0, tick_offset/0, diasca/0, logical_timestamp/0,
			   simulation_events/0, interactivity_mode/0 ]).



% Local (non-exported) types:


% To diagnose causes of any low performance:
%
-type diagnosis() :: 'none_waited' | [ binary() ].


% To display relevant time information on the console:
%
% (respectively: simulation date and time, tick and diasca, wallclock date and
% time)
%
-type timing_info() :: { string(), string(), tick_offset(), diasca(), string(),
					 string() }.


% To display relevant time information on the console:
%
% (respectively: total actor count, total scheduled count, total process count)
%
-type count_info() :: { basic_utils:count(), basic_utils:count(),
						basic_utils:count() }.


% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks,).


% Allows to define WOOPER base variables and methods for that class:
%
-include("wooper.hrl").




% Must be included before class_TraceEmitter header:
%
-define(TraceEmitterCategorization,"Core.TimeManagement").


% Allows to use macros for trace sending:
%
-include("traces.hrl").


% For the actor_info record:
%
-include("class_InstanceTracker.hrl").


% For talkative settings:
%
-include("simulation_settings.hrl").


% For time_manager_name, virtual_seconds, etc.:
%
-include("class_TimeManager.hrl").



% Tells how the name of a time manager should be registered.
%
% Could be local_and_global or global_only as well, but we want here exactly one
% local time manager per computing node:
%
-define( registration_type, local_only ).




% Defines how many real milliseconds the watchdog must wait before determining
% the simulation stalled.
%
% Ensure that this value is lower than the max idle duration (as defined in
% get_maximum_idle_duration/0):
%
-define( watchdog_wait_duration, 2000 ).
%-define( watchdog_wait_duration, 10000 ).

% Use this instead, on a loaded computer, to test stall detection:
%-define( watchdog_wait_duration, 1 ).



% Simulations will start by default at a base (fixed) date, on Saturday, January
% 1st, 2000 at midnight:
%
% (use setInitialTick/2 or setInitialSimulationDate/3 to change it if necessary)
%
-define( initial_simulation_date, { { 2000, 1, 1 }, { 0, 0, 0 } } ).



% Defines the threshold from which we deem that a list has too many entries to
% be displayed element per element.
%
-define( too_many_entries, 20 ).



% Implementation notes:
%
% - the general architecture relies on the fact that:
%
%   - the communication from a time manager to an actor is fully asynchronous
%   (non-blocking; ex: triggerNameNotification/2 oneway)
%
%   - the communication from an actor to its time manager can (and usually
%   should) be synchronous (once again: not true the other way round)
%
% Otherwise deadlocks could happen: if each other sent a request to the other at
% roughly the same time, each could be blocked waiting for the other, until the
% end of time or any declared time-out fires. Therefore the time manager should
% never block for an actor to answer
%
% - as much as possible, timings (simulation timestamps) are expressed as
% offsets to the simulation initial tick, rather than as absolute ticks, in
% order to preserve the performances; timing considerations in models should be
% expressed as actual durations (in virtual time), so that they do not depend on
% the chosen overall frequency
%
% - spontaneous_agenda could be an ordered_set or a gb_sets, but we suspect that
% the lighter the list will be, the more efficient it will be (only two
% operations are performed on that list: insert or modify an entry, and pop
% smallest entry); so we use a custom, plain, ordered list, knowing that in most
% cases the list should not be very long (the agenda is indexed by future tick
% offsets, not by actors); inside this list of spontaneous entries, each element
% is a {Tick,ActorList} pair, where ActorList is an advanced list (list_impl) of
% PIDs, as we may have to search for a specific PID in it
%
% - on some other lists we just have to iterate; this reason holds for the list
% of actors to be triggered on the next diasca (actors_to_trigger_next_diasca),
% which may just be plain list
%
% - all other actor-related lists (ex: waiting lists) are expected to hold
% possibly a very large number of elements (as usually one element corresponds
% to one actor), and some random access in them is required (ex: to remove a
% particular actor from a waiting list), therefore we preferred to use more
% advanced data-structures than simple lists, thinking that the number of actors
% is generally worth the overhead induced by more advanced data structures. As
% ordsets, gb_sets, sets, etc. mostly offer the same API (list of functions), we
% use the list_impl macro to define the actual type that is to be used, so that
% it can be easily changed if needed.

% We could also imagine recording, for each actor, also each spontaneous ticks
% planned, so that for example they can be removed when it is itself removed
% (instead of searching them) - but this would not scale well.



% Most if not all scheduling messages are timestamped (with the tick offset and,
% if appropriate, the current diasca), even though in some cases it is not
% strictly necessary. This is nevertheless a way of adding stronger runtime
% checkings, and the overhead is most probably tiny (a message has to be sent
% anyway).

% For example there is a potential race condition, from the point of view of a
% non-root time manager, between the receiving of a 'beginTimeManagerDiasca'
% message (sent by its parent manager) and a 'scheduleTrigger' message (sent by
% a local actor having received a message, possibly from a non-local
% actor).
%
% Indeed, the time manager at diasca D may receive a scheduleTrigger message
% targeting the next diasca D+1 (as usual when sent from an actor at diasca D
% and when the manager already received its 'begin diasca' notification) or the
% scheduleTrigger might come from an early remote actor already at diasca D+1
% (thus targeting D+2), received before the 'beginTimeManagerDiasca' message.
%
% As a consequence two different lists of actors to be triggered must be
% maintained by a time manager: the one for next diasca, and the one for the
% diasca after.



% When running in interactive mode, the root time manager is the only manager
% which makes use of a timer, all other managers are paced according to this
% same (possibly scaled) time base. Ticks are then scheduled on par with the
% real time, regardless of their need of actually being scheduled (no jump
% allowed over known idle ticks).


% A terminating actor must not terminate while being still to be scheduled later
% by the time manager (either due to an already planned spontaneous behaviour or
% because another actor is trying to interact with it): it will be scheduled
% (with a beginTermination call), directly at the next diasca after having
% declared its termination, and then be safely removed from the simulation.


% Any process can register itself as a simulation listener. It will be then
% notified of all main simulation events by the sending of appropriate messages,
% namely:
%
% - simulation_started
% - simulation_suspended
% - simulation_resumed
% - simulation_succeeded
% - simulation_stopped
%
% Of course the listener can interpret these messages as oneway calls (see also:
% the simulation_events() type).


% Regarding stochastic support.

% Time managers used to assign random seeds to actors. This had for notable
% drawback that reproducibility was guaranteed only if using a constant number
% of computing hosts (as otherwise the random series driven by the corresponding
% time managers would differ); instead, the (centralised) load-balancer takes
% care of actor seeding, with no real overhead as it has to send other
% information (ex: AAI) to them anyway.



% Listener API:
%
% A time listener should implement the two oneway methods below:
%
% - onNewTick/2, i.e.:
%
%-spec onNewTick( wooper_state(), class_TimeManager:tick_offset() ) ->
%          oneway_return().
% onNewTick( State, NewTickOffset ) -> ...
%
%
% - onNewDiasca/3, i.e.:
%
%-spec onNewDiasca( wooper_state(), class_TimeManager:tick_offset(),
%          class_TimeManager:diasca() ) -> oneway_return().
% onNewDiasca( State, TickOffset, NewDiasca ) -> ...



% Resilience section.
%
% When a resilience rollback is performed, Sim-Diasca is redeployed from scratch
% on the surviving nodes. Blank time managers (root or not) are then created,
% and the agents of the other services link to them later during this rollback;
% this requires state merges to be operated (current newly deployed managers
% being already updated by other recreated agents - notably by the load
% balancer, which is an actor, which are merged with serialised managers). Hence
% in the general cases multiple merges per node are to be performed.

% This merging design is possible now that there is no more dependency of the
% random seeds onto the number of time managers: actor seeds will be the same,
% regardless of the number of computing nodes and time managers involved, as
% only actors request seeds (from the load balancer).



% TO-DO:
%
% - tune the beginTimeManagerTick method to further minimize its duration, as
% most of the useless simulation latency comes from that critical section
% (actors are all waiting in the meantime)
%
% - maintain, thanks to the (distributed) instance tracker, a list of {
% TickOffset, TerminatedActorList } pairs, so that wrong operations due to
% life-cycle (typically trying to interact with deleted actors) can be better
% diagnosed



% Description of all the state attributes of a time manager instance:
%
% - parent_manager_pid :: basic_utils:maybe( pid() ) is the PID of the parent
% time manager of this manager (if any, otherwise set to 'undefined', in which
% case this is the time manager)
%
% - child_managers :: ?list_impl( pid() ) is a (list_impl) list of the PIDs of
% all the direct child managers of this manager (which was already defined
% previously)
%
% - known_local_actors :: ?list_impl( pid() ) is the list of the PIDs of all
% known actors that are directly managed by this time manager; useful to notify
% them for example that the simulation starts (it is a list_impl, as for example
% we have to ensure that an actor is not subscribed yet before subscribing it)
%
% - load_balancer_pid :: pid() is the PID of the load balancer (useful to keep
% track of actor overall count for example)
%
% - started :: boolean() tells whether the time manager is running
%
% - initial_tick :: tick() is the (absolute) simulation tick at which the time
% manager will start, whenever asked to start; this corresponds to the actual
% beginning of the simulation
%
% - initial_timestamp :: basic_utils:timestamp() is the wallclock timestamp
% corresponding to the moment the time manager was requested to start
%
% - current_tick_offset :: tick_offset() is the offset, expressed as a number of
% ticks, between the initial tick and the current one; we use mostly offsets
% rather than absolute ticks for efficiency reasons, Erlang will automatically
% switch from 32-bit integers to arbitrary-sized integers after roughly one year
% of simulation time, when at 50Hz (see 'math:pow(2,32)/50/3600/24/365', divided
% by 2 if signed)
%
% - current_diasca :: diasca() corresponds to the current diasca being evaluated
%
% - spontaneous_agenda :: agenda() is the agenda (actually a plain list, which
% is managed so that it remains ordered) of the next simulation tick offsets
% during which this time manager will have to send spontaneous tops to at least
% one actor; more precisely, it is an ordered list of pairs, whose first element
% is a tick offset, and second element is a list_impl-based list of actors, like
% in: [ {4,[Pid1,Pid2]}, {7,[Pid3]}, {8,[Pid1]} ]; these actor lists are not
% basic lists, even if the most usual action performed on them will be to
% iterate over them, as there will be nevertheless other random-access
% operations performed on them (testing for membership - since, at any tick, a
% given actor (PID) should not be specified more than once, or withdrawing
% spontaneous actions, etc.)
%
% - previous_timestamp :: logical_timestamp() records the previous diasca that
% was scheduled, so that tracking information can be correctly associated to the
% right simulation moment
%
% - next_timestamp :: logical_timestamp() stores what is the expected new
% simulation timestamp, from the point of view of this time manager; when being
% in {Talpha,Dalpha} it can be either 'undefined', or {Talpha,Dalpha+1} (if a
% new diasca is already known to be needed) or {Tbeta,0} with Tbeta > Talpha
% otherwise
%
% - next_action :: next_manager_action() corresponds to the soonest deadline
% which is known by this manager (based on its scheduling subtree), equal to
% 'no_diasca_requested', to 'new_diasca_needed' or to an actual tick offset to
% keep track of its soonest known deadline
%
% - actors_to_trigger_in_one_diasca :: ?list_impl( pid() ) is a (list_impl) list
% of actors that should be triggered on the next scheduled diasca (stored in
% next_timestamp, whichever it is, whether or not this time manager has already
% received its "new diasca" message) because they have received an actor message
% the current overall diasca; a local actor must be listed up to once, even if
% it received more than one actor message on the corresponding diasca (this is
% ensured actor-side, see schedule_trigger_already_sent); this is a list_impl as
% we have to remove efficiently terminating actors from it
%
% - actors_to_trigger_in_two_diascas :: ?list_impl( pid() ) is a (list_impl)
% list of actors that should be triggered on the diasca *after* the next
% scheduled one, due to the race condition described in the implementation
% notes; a local actor must be listed up to once, even if it received more than
% one actor message on the corresponding diasca (this is ensured actor-side, see
% schedule_trigger_already_sent); this is a list_impl as it will be assigned to
% actors_to_trigger_in_one_diasca afterwards
%
% - watchdog_pid :: basic_utils:maybe( pid() ) is the PID of the watchdog, if
% any, i.e. iff being the root time manager
%
% - stop_tick_offset :: maybe( tick_offset() ) is the tick offset at which the
% simulation should end (if being in the root time manager and if termination is
% based on a fixed timestamp in simulation time), otherwise 'undefined';
% only the root time manager may have a stop tick offset defined
%
% - simulation_listeners :: [ pid() ] is a (plain) list of the PID of the
% processes which are to keep track of the main simulation events, like start,
% stop, suspend, resume transitions
%
% - time_listeners :: [ pid() ] is a (plain) list of the PIDs of the processes
% which are to keep track of the logical scheduling (i.e. ticks and diascas),
% while not being actors (their onNewTick/2 and onNewDiasca/3 oneways are
% triggered appropriately)
%
% - wallclock_milestone_period :: unit_utils:milliseconds() corresponds to the
% actual (wall-clock) duration between two wallclock milestones
%
% - wallclock_milestone_listeners :: [ pid() ] is a (plain) list of the PIDs of
% the processes which are to be notified whenever wallclock-based milestones are
% met (their onWallclockMilestone/2 oneways are triggered appropriately),
% i.e. when a sufficient time in the real world elapsed
%
% - tick_milestone_period :: tick_offset() corresponds to the virtual (in
% simulation-time) duration between two tick milestones
%
% - tick_milestone_listeners :: [ pid() ] is a (plain) list of the PIDs of the
% processes which are to be notified whenever tick-based milestones are met
% (their onTickMilestone/2 oneways are triggered appropriately), i.e. when a
% sufficient time in the simulated world elapsed
%
% - suspended :: boolean() tells whether the simulation is currently suspended
%
% - simulation_tick_duration :: virtual_seconds() is the actual duration, in
% seconds (in virtual time) between two simulation ticks (floating-point value)
%
% - simulation_tick_waiting :: unit_utils:milliseconds() is, when in
% interactive mode, the real (i.e. wall-clock, user) duration, in milliseconds,
% which is expected between two simulation ticks (possibly once scaled and
% rounded)
%
% - interactivity_mode :: interactivity_mode(), to discriminate between
% 'interactive' or 'batch'
%
% - terminated_actors :: ?list_impl( pid() ) is the list of actors that have
% terminated (they have completed their termination procedure and can be deleted
% at any time; actually their deletion is triggered at the beginning of the next
% tick)
%
% - terminating_actors :: [ pid() ] is a plain list of actors that are actively
% terminating (i.e. within a bounded number of diascas) this diasca, but have
% not terminated yet; the list is established at each diasca
%
% - actors_to_delete_at_next_tick :: [ pid() ] is a list of the PID of all
% actors that are terminating and whose (deferred) deletion is to happen at next
% tick
%
% - waited_child_managers :: ?list_impl( pid() ) is a list of child managers
% that are still waited, for the current diasca to finish; it is a list_impl as
% well, since a simulation distributed over a cluster may involve, say, 300+
% nodes, if not 65000 nodes on a Bluegene/Q?
%
% - waited_spontaneous_actors :: ?list_impl( pid() ) is the list of the actors
%  whose spontaneous behaviour has been scheduled this tick (as diasca 0) and
%  that are still waited, for the current diasca to finish; it is a list_impl
%
% - waited_triggered_actors :: ?list_impl( pid() ) is the list of the actors
% whose triggered behaviour has been scheduled this diasca and that are still
% waited, for the current diasca to finish; it is a list_impl
%
% - watchdog_waited :: boolean() tells whether the watchdog is still waited on
% this diasca
%
% - waited_count :: basic_utils::count() counts the number of 'done'
% notifications, coming from scheduled actors, child managers and the watchdog,
% that are waited this diasca (spares the need of polling numerous
% list_impl-based tables each time a new 'done' message is received)
%
% - timer_pid :: maybe( pid() ) is the PID of the timer process (if any), used
% to keep track of real time when running in interactive mode; timer_pid is set
% iff being the root time manager and in interactive mode
%
% - wallclock_tracker_pid :: maybe( pid() ) is the PID of the wallclock tracker
% (only used for the root time manager)
%
% - time_tracker_pid :: maybe( pid() ) is the PID of the time tracker (if any;
% only used for the root time manager)
%
% - overall_actor_count :: basic_utils:count() is the current total number of
% actors in the simulation, as notified by the load balancer (only useful in the
% root time manager)
%
% - scheduled_tracking :: basic_utils:count() is the count of actors to be
% spontaneously scheduled (if being at diasca 0), or triggered (in later
% diascas); it is the sum for all the scheduling subtree which corresponds to
% this time manager (including itself), and it is updated at each scheduled
% diasca
%
% - process_tracking :: basic_utils:count() is the current count in terms of
% Erlang processes, for all the scheduling subtree which corresponds to this
% time manager (including itself), and it is updated at each scheduled tick
%
% - root_data_exchanger_pid :: basic_utils:maybe( pid() ) is the PID of the root
% data-exchanger (if any), with whom the root time manager may have to interact
%
% - root_instance_tracker_pid :: basic_utils:maybe( pid() ) is the PID (if any)
% of the root instance tracker, to resolve instance-level issues
%
% - local_instance_tracker_pid :: basic_utils:maybe( pid() ) is the PID (if any)
% of the local instance tracker (i.e. the one running on the same node as this
% time manager), to resolve instance-level issues
%
% - interdiasca_listeners :: [ pid() ] is a list containing the PID of all
% registered inter-diasca listeners (ex: typically the root data-exchanger)
%
% - resilience_manager_pid :: maybe( pid() ) is the PID of the resilience
% manager
%
% - serialisation_requested :: boolean() tells whether a serialisation is to
% occur once the current diasca is over



% In terms of helper processes, we may have:
%
% - timer_main_loop/2, for the interactive timing internal process
% - watchdog_main_loop/4, for the watchdog internal process
% - wallclock_tracker_main_loop/3, to schedule wall-clock milestones
% - time_tracker_main_loop/5, for the console tick tracker internal process




% Constructs a new time manager:
%
% - SimulationTickDuration designates the duration (as a strictly positive
% floating-point value), expressed in virtual seconds, of each simulation tick;
% for example, if the specified duration is 0.02s, then each simulation step
% will last for 20ms (in simulation time) and the simulation frequency will be
% 50Hz; models can be scheduled at a sub-multiple of this fundamental frequency
% if needed (ex: 16.7Hz, i.e. every 60ms, hence here every 3 ticks)
%
% - InteractivityMode, among:
%
%  - interactive: then the simulation will try to run on par with the user time
%
%  - batch: then the simulation will run as fast as possible, regardless of user
%  time
%
% - ParentManagerInformation, which can be either 'none' if this manager is the
% root one, otherwise the PID of the direct parent time manager of this one
%
% - RootInstanceTrackerPid is the PID of the root instance tracker, to help
% troubleshooting the actors
%
% - TroubleshootingMode tells whether the troubleshooting mode is enabled
%
% - Context tells here whether we are at simulation start-up or redeploying
% during a rollback
%
% Note: the initial simulation date is to be given only when this time manager
% is started.
%
-spec construct( wooper_state(), virtual_seconds(), interactivity_mode(),
	'none' | pid(), pid(), boolean(),
	class_DeploymentManager:simulation_context() ) -> wooper_state().
construct( State, SimulationTickDuration, InteractivityMode,
		  ParentManagerInformation, RootInstanceTrackerPid, TroubleshootingMode,
		  _Context ) ->

	% This manager will terminate if ever an exit signal is received, instead of
	% transforming the signal into an 'EXIT' message:
	% (a monitor could be used instead)
	%erlang:process_flag( trap_exit, true ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, "Time Manager" ),

	% We raise here the priority of all time managers, not specifically for
	% their processings, but because they are to send a large number of messages
	% (to scheduled actors), and, as a consequence, the Erlang scheduler might
	% want to retaliate:
	%
	erlang:process_flag( priority, _Level=high ),

	class_InstanceTracker:register_agent( ?time_manager_name ),

	% The child_managers are declared here, as the declareChildManager call will
	% need them to be already set:
	%
	CategorizedState = setAttributes( TraceState, [

		{ child_managers, ?list_impl:new() },
		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

	] ),

	ActualSimulationTickDuration = check_tick_duration(
									 SimulationTickDuration ),

	case InteractivityMode of

		interactive ->
			ok;

		batch ->
			ok;

		_ ->
			throw( { invalid_interactivity_mode, InteractivityMode } )

	end,

	RegistrationName = get_registration_name(),

	{ ParentManager, Description } = case ParentManagerInformation of

		none ->

			% Only one to register globally as well (even if re-deploying after
			% a recovered crash):
			%
			basic_utils:register_as( RegistrationName, local_and_global ),

			?send_debug_fmt( CategorizedState, "Root time manager "
							 "registered as '~s', locally and globally.",
							 [ RegistrationName ] ),

			{ undefined, "root time manager" };


		ParentPid when is_pid(ParentPid) ->
			% We are here a local (non-root) time manager, the children have to
			% declare to their direct parent:
			%
			% (request for synchronisation purposes)
			%
			ParentPid ! { declareChildManager, [], self() },

			% A bit of interleaving:

			?send_debug_fmt( CategorizedState, "Child time manager "
							 "registered, locally, as '~s'.",
							 [ RegistrationName ] ),

			basic_utils:register_as( RegistrationName, local_only ),

			receive

				{ wooper_result, child_manager_registered } ->
					Desc = io_lib:format( "child time manager, "
										  "whose parent manager is ~w,",
										  [ ParentPid ] ),
					{ ParentPid, Desc }

			end

	 end,

	SimulationFrequency = 1 / ActualSimulationTickDuration,

	?send_trace_fmt( CategorizedState,
		"Creating a " ++ Description ++ " in ~w mode with an actual "
		"fundamental simulation frequency of approximately ~.2fHz; "
		"in virtual time, each simulation tick will last exactly for ~fs.",
		[ InteractivityMode, SimulationFrequency,
		  ActualSimulationTickDuration ] ),

	% Instance trackers are created before time managers:
	LocalInstanceTrackerPid = basic_utils:get_registered_pid_for(
											?instance_tracker_name, local ),

	StartState = setAttributes( CategorizedState, [

		{ parent_manager_pid, ParentManager },

		% child_managers already defined.

		% As local actors may register before the simulation starts, their list
		% must be created here:
		{ known_local_actors, ?list_impl:new() },

		{ load_balancer_pid, undefined },
		{ started, false },

		% initial_tick set later in this method.
		{ initial_timestamp, basic_utils:get_timestamp() },

		% Will be set at simulation start:
		{ current_tick_offset, undefined },
		{ current_diasca, undefined },

		% As local actors may register and be scheduled before the simulation
		% starts, must be created here:
		{ spontaneous_agenda, [] },

		{ actors_to_trigger_in_one_diasca, ?list_impl:new() },
		{ actors_to_trigger_in_two_diascas, ?list_impl:new() },

		{ next_action, undefined },
		{ previous_timestamp, undefined },
		{ next_timestamp, undefined },
		{ watchdog_pid, undefined },
		{ stop_tick_offset, undefined },
		{ simulation_listeners, [] },
		{ time_listeners, [] },

		% By default once every 4 real minutes:
		{ wallclock_milestone_period, 4*60*1000 },
		{ wallclock_milestone_listeners, [] },

		% By default every 1000 ticks:
		{ tick_milestone_period, 1000 },
		{ tick_milestone_listeners, [] },

		{ suspended, false },
		{ simulation_tick_duration, ActualSimulationTickDuration },
		{ simulation_tick_waiting, round(
		   ?scale_factor_for_interactive_time * ActualSimulationTickDuration) },
		{ interactivity_mode, InteractivityMode },
		{ terminated_actors, undefined },
		{ terminating_actors, undefined },
		{ actors_to_delete_at_next_tick, [] },
		{ waited_child_managers, undefined },
		{ waited_spontaneous_actors, undefined },
		{ waited_triggered_actors, undefined },
		{ watchdog_waited, false },
		{ waited_count, undefined },
		{ timer_pid, undefined },
		{ wallclock_tracker_pid, undefined },
		{ time_tracker_pid, undefined },
		{ overall_actor_count, 0 },
		{ scheduled_tracking, 0 },
		{ process_tracking, system_utils:get_process_count() },
		{ troubleshooting_mode, TroubleshootingMode },
		{ root_data_exchanger_pid, undefined },
		{ root_instance_tracker_pid, RootInstanceTrackerPid },
		{ local_instance_tracker_pid, LocalInstanceTrackerPid },
		{ interdiasca_listeners, [] },
		{ resilience_manager_pid, undefined },
		{ serialisation_requested, false }

	] ),

	% Not starting at user time anymore, as it would break reproducibility:
	%{ Date, Time } = { date(), time() },

	% Starting by default at a base (common) date instead:
	%
	% (StartState needed, as depends on simulation_tick_duration)
	%
	DefaultInitialTick = timestamp_to_ticks( ?initial_simulation_date,
											 StartState ),

	?send_debug_fmt( StartState,
					"Time manager created, default initial tick is ~B.",
					[ DefaultInitialTick ] ),

	setAttribute( StartState, initial_tick, DefaultInitialTick ).




% Overridden destructor.
%
% All still-subscribed listeners will be warned of the manager deletion by a
% timeManagerShutdown message.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?trace( "Deleting the time manager." ),

	StoppedState = case ?getAttr(started) of

		true ->
			{ NewState, stopped } = executeRequest( State, stop ),
			NewState;

		false ->
			State

	end,

	% Recurses down the scheduling tree:
	delete_synchronously_instances(
	  ?list_impl:to_list( ?getAttr(child_managers) ) ),

	basic_utils:send_to_pid_list_impl( timeManagerShutdown,
									   ?getAttr(known_local_actors) ),

	basic_utils:unregister( get_registration_name(), ?registration_type ),

	class_InstanceTracker:unregister_agent(),

	?trace( "Time manager deleted." ),

	% Then allows chaining:
	StoppedState.






% Methods section.



% Called by all local managers that are direct children of this time manager
% (which thus is their parent), when they are created.
%
% To be called before the simulation is started.
%
% (request, for synchronisation purposes)
%
-spec declareChildManager( wooper_state() ) -> request_return(
		'child_manager_registered' ).
declareChildManager( State ) ->

	NewManagers = ?list_impl:add_element( ?getSender(),
										  ?getAttr(child_managers) ),

	NewState = setAttribute( State, child_managers, NewManagers ),

	% The interactivity mode has already been specified at construction:
	?wooper_return_state_result( NewState, child_manager_registered ).




% Called to notify the (root) time manager of the PID of the load balancer.
%
% (request, for synchronisation reasons)
%
-spec setLoadBalancerPid( wooper_state(), pid() ) ->
								request_return( 'load_balancer_set' ).
setLoadBalancerPid( State, LoadBalancerPid ) ->

	[] = ?getAttr(spontaneous_agenda),
	?checkUndefined(load_balancer_pid),


	% We want to schedule the load balancer for a spontaneous behaviour at the
	% first diasca of the first scheduled tick, so that it can itself trigger
	% the 'onFirstDiasca/2' actor oneway on all initial actors:
	%
	InitialAgenda = [ {_TickOffset=0, ?list_impl:add( LoadBalancerPid,
												   ?list_impl:new() ) } ],

	SetState = setAttributes( State, [

			{ load_balancer_pid, LoadBalancerPid },
			{ spontaneous_agenda, InitialAgenda }

									  ] ),

	?wooper_return_state_result( SetState, 'load_balancer_set' ).



% Returns the initial tick for that simulation, if it has been already set,
% otherwise the atom 'undefined'.
%
% (const request)
%
-spec getInitialTick( wooper_state() ) ->
							request_return( basic_utils:maybe( tick() ) ).
getInitialTick( State ) ->
	?wooper_return_state_result( State, ?getAttr(initial_tick) ).



% Sets the initial tick to be used by the simulation.
%
% Must not be called while the simulation is running.
%
% (oneway)
%
-spec setInitialTick( wooper_state(), tick() ) -> oneway_return().
setInitialTick( State, InitialTick ) ->

	false = ?getAttr(started),

	?wooper_return_state_only(
	   setAttribute( State, initial_tick, InitialTick ) ).



% Sets the initial tick to be used by the simulation, based on the specified
% time and date, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% (oneway)
%
-spec setInitialSimulationDate( wooper_state(), unit_utils:date(),
							   unit_utils:time() ) -> oneway_return().
setInitialSimulationDate( State, Date, Time ) ->

	false = ?getAttr(started),

	NewInitialTick = timestamp_to_ticks( { Date, Time }, State ),

	?info_fmt( "New initial simulation tick is ~B.", [ NewInitialTick ] ),

	?wooper_return_state_only( setAttribute( State, initial_tick,
											NewInitialTick ) ).



% Returns the final (absolute) tick for that simulation, or the 'undefined' atom
% if no final tick was defined.
%
% (const request)
%
-spec getFinalTick( wooper_state() ) ->
						  request_return( basic_utils:maybe( tick() ) ).
getFinalTick( State ) ->

	Res = case ?getAttr(stop_tick_offset) of

			  undefined ->
				  undefined;

			  StopOffset ->
				  ?getAttr(initial_tick) + StopOffset

	end,

	?wooper_return_state_result( State, Res ).



% Sets the final (absolute) tick to be used by the simulation.
%
% Must not be called while the simulation is running.
%
% Note: depends on the current initial tick, as is stored as a tick offset.
%
% (oneway)
%
-spec setFinalTick( wooper_state(), tick() ) -> oneway_return().
setFinalTick( State, FinalTick ) ->

	false = ?getAttr(started),

	InitialTick = ?getAttr(initial_tick),

	DurationInTicks = FinalTick - InitialTick,

	case DurationInTicks > 0 of

		true ->

			?info_fmt( "New final simulation tick is ~B.", [ FinalTick ] ),

			?wooper_return_state_only(
				   setAttribute( State, stop_tick_offset, DurationInTicks ) );

		false ->
			throw( { final_tick_on_the_past, FinalTick, InitialTick } )

	end.



% Sets the final tick to be used by the simulation, based on the specified time
% and date, expressed in virtual time.
%
% Must not be called while the simulation is running.
%
% Note: depends on the current initial tick, as is stored as a tick offset.
%
% (oneway)
%
-spec setFinalSimulationDate( wooper_state(), unit_utils:date(),
		 unit_utils:time() ) -> oneway_return().
setFinalSimulationDate( State, Date, Time ) ->

	false = ?getAttr(started),

	FinalDate = { Date, Time },

	FinalTick = timestamp_to_ticks( FinalDate, State ),

	InitialTick = ?getAttr(initial_tick),

	DurationInTicks = FinalTick - InitialTick,

	case DurationInTicks > 0 of

		true ->

			?info_fmt( "New final simulation tick is ~B.", [ FinalTick ] ),

			?wooper_return_state_only(
				   setAttribute( State, stop_tick_offset, DurationInTicks ) );

		false ->
			throw( { final_date_on_the_past, FinalDate, FinalTick,
					InitialTick } )

	end.




% Management section of the time manager.



% Starts this (root) time manager with no specific termination tick defined.
%
% (oneway)
%
-spec start( wooper_state() ) -> oneway_return().
start( State ) ->

	% Here no termination tick is set:
	?wooper_return_state_only( init( State ) ).



% Starts this (root) time manager with a specific termination tick defined,
% expressed as an offset to the initial tick.
%
% (oneway)
%
-spec start( wooper_state(), tick_offset() ) -> oneway_return().
start( State, TerminationOffset ) when is_integer(TerminationOffset) ->

	% This is an offset, no checking needed here:
	InitState = init(
				  setAttribute( State, stop_tick_offset, TerminationOffset ) ),

	StopTick = getAttribute( InitState, initial_tick ) + TerminationOffset,

	?info_fmt( "The simulation will stop no later than tick ~B "
		"(termination tick offset is #~B).", [ StopTick, TerminationOffset ] ),

	?wooper_return_state_only( InitState );


% Starts this (root) time manager with a specific termination tick defined,
% expressed as an offset to the initial tick.
%
% (oneway)
%
start( State, SimulationListenerPID ) when is_pid(SimulationListenerPID) ->
	start( appendToAttribute( State, simulation_listeners,
							 SimulationListenerPID ) ).



% Starts this (root) time manager with a specific termination tick and specified
% registered simulation listener.
%
% (oneway)
%
-spec start( wooper_state(), tick_offset(), pid() ) -> oneway_return().
start( State, TerminationOffset, SimulationListenerPID )
  when is_integer(TerminationOffset) andalso is_pid(SimulationListenerPID) ->
	start( appendToAttribute( State, simulation_listeners,
							SimulationListenerPID ),
		   TerminationOffset ).



% Starts this (root) time manager for a specified duration, expressed in
% simulation time (as a floating-point number of virtual seconds).
%
% (oneway)
%
-spec startFor( wooper_state(), number() ) -> oneway_return().
startFor( State, Duration ) when is_integer(Duration) ->

	startFor( State, erlang:float(Duration) );


startFor( State, Duration ) when is_float(Duration) andalso Duration > 0 ->

	TerminationOffset = convert_seconds_to_ticks( Duration, State ),

	InitState = init(
				  setAttribute( State, stop_tick_offset, TerminationOffset ) ),

	StopTick = getAttribute( InitState, initial_tick ) + TerminationOffset,

	?info_fmt( "The simulation will stop no later than tick ~B "
			  "(termination tick offset is #~B), corresponding to "
			  "a user-specified duration, in simulation time, of ~s.",
			  [ StopTick, TerminationOffset,
			   text_utils:duration_to_string( round( 1000 * Duration ) ) ] ),

	?wooper_return_state_only( InitState );


startFor( _State, ErrorDuration ) ->

	throw( { invalid_simulation_duration_specified, ErrorDuration } ).



% Starts this (root) time manager for a specified duration, expressed in
% simulation time (as a number of virtual seconds), with a registered stop
% listener.
%
% (oneway)
%
-spec startFor( wooper_state(), number(), pid() ) -> oneway_return().
startFor( State, Duration, SimulationListenerPID )
  when is_pid(SimulationListenerPID) ->

	startFor( appendToAttribute( State, simulation_listeners,
								SimulationListenerPID ),
			 Duration ).



% Stops this time manager.
%
% (request)
%
-spec stop( wooper_state() ) -> request_return( {'stopped',pid()} ).
stop( State ) ->

	StoppedState = case ?getAttr(started) of

		false ->
			?error( "Stop request ignored: simulation clock not running." ),
			State;


		true ->

			Timings = get_textual_timings( State ),

			?info_fmt( "Stopping simulation clock at ~s.", [ Timings ] ),

			% The engine may be stopped at any time.


			?debug( "Taking care of pending already terminated actors." ),

			%io:format( "Stopping at #~p, hence deleting actors ~p.~n",
			%		  [ ?getAttr(current_tick_offset),
			%			?getAttr(actors_to_delete_at_next_tick) ] ),

			[ A ! delete || A <- ?getAttr(actors_to_delete_at_next_tick) ],

			?debug( "Taking care of already terminating actors." ),

			% Having no more active actors is only one of the causes for the
			% time manager to stop, therefore there could be terminating actors
			% which must be removed:
			%
			TerminatedState = terminate_actors( ?getAttr(current_tick_offset),
								  ?getAttr(current_diasca) + 1, State ),


			?debug( "Taking care now of still running actors." ),

			% More generally there could be non-terminating actors still alive
			% when this stops happens, let's remove them:
			basic_utils:send_to_pid_list_impl( simulationEnded,
								  ?getAttr(known_local_actors) ),

			% known_local_actors updated later.


			?debug( "Stopping watchdog, trackers and child managers." ),

			TimeTrackerState = stop_time_tracker( TerminatedState ),

			WallclockTrackerState = stop_wallclock_tracker( TimeTrackerState ),

			WatchdogState = stop_watchdog( WallclockTrackerState ),

			TimerState = stop_timer( WatchdogState ),

			stop_child_managers( TimerState ),

			[ L ! simulation_stopped || L <- ?getAttr(simulation_listeners) ],

			flush_scheduling_messages(),

			?debug( "Stop successful." ),

			% Only one display wanted, the one of the root time manager:
			case is_root_manager( State ) of

				true ->
					class_PluginManager:notify( on_simulation_stop ),
					display_timing_information( Timings, TimerState );

				false ->
					ok

			end,

			% Prepares back a blank state, should a new simulation be started:
			setAttributes( TimerState, [

				{ initial_timestamp, undefined },
				{ started, false },
				{ actors_to_delete_at_next_tick, [] },
				{ known_local_actors, ?list_impl:new() },
				{ spontaneous_agenda, [] },
				{ overall_actor_count, 0 }

			] )

	end,

	% Returning the time manager PID allows to wait in parallel for multiple
	% stopping child managers:
	%
	?wooper_return_state_result( StoppedState, { stopped, self() } ).



% Defined so that a time manager can stop itself asynchronously.
%
% (oneway)
%
-spec selfStop( wooper_state() ) -> oneway_return().
selfStop( State ) ->

	{ StoppedState, _Result } = executeRequest( State, stop, [] ),

	?wooper_return_state_only( StoppedState ).



% Suspends the simulation until a resume request is received.
%
% (oneway)
%
-spec suspend( wooper_state() ) -> oneway_return().
suspend( State ) ->

	?info( "Simulation requested to suspend." ),

	[ L ! simulation_suspended || L <- ?getAttr(simulation_listeners) ],

	?wooper_return_state_only( setAttribute( State, suspended, true ) ).



% Resumes the simulation once it has been suspended.
%
% (const oneway)
%
-spec resume( wooper_state() ) -> oneway_return().
resume( State ) ->

	?error( "Resume request received, whereas should have been intercepted "
		"by suspend code. Ignored." ),

	?wooper_return_state_only( State ).



% Listener section.


% Registers a new simulation listener, which will be notified of all main
% simulation events (like start, stop, resume, etc.).
%
% (oneway)
%
-spec addSimulationListener( wooper_state(), pid() ) -> oneway_return().
addSimulationListener( State, ListenerPid ) ->

	%?debug_fmt( "Simulation listener ~w added.", [ ListenerPid ] ),

	?wooper_return_state_only(
		appendToAttribute( State, simulation_listeners, ListenerPid ) ).



% Unregisters a simulation listener, expected to be already registered.
%
% (oneway)
%
-spec removeSimulationListener( wooper_state(), pid() ) -> oneway_return().
removeSimulationListener( State, ListenerPid ) ->

	%?debug_fmt( "Simulation listener ~w removed.", [ ListenerPid ] ),

	?wooper_return_state_only(
		deleteFromAttribute( State, simulation_listeners, ListenerPid ) ).



% Registers a new time listener, which will be notified of all scheduled ticks
% and diascas.
%
% Note: contrary to an actor, no synchronisation will be performed, and the
% listener will not be able to trigger the scheduling of any tick.
%
% (oneway)
%
-spec addTimeListener( wooper_state(), pid() ) -> oneway_return().
addTimeListener( State, ListenerPid ) ->

	%?debug_fmt( "Time listener ~w added.", [ ListenerPid ] ),

	?wooper_return_state_only(
		appendToAttribute( State, time_listeners, ListenerPid ) ).



% Unregisters a time listener, expected to be already registered.
%
% (oneway)
%
-spec removeTimeListener( wooper_state(), pid() ) -> oneway_return().
removeTimeListener( State, ListenerPid ) ->

	%?debug_fmt( "Time listener ~w removed.", [ ListenerPid ] ),

	?wooper_return_state_only(
		deleteFromAttribute( State, time_listeners, ListenerPid ) ).



% Registers the resilience manager, so that it can be notified when a requested
% serialisation can be performed (and be also notified of the main simulation
% events, like all other simulation listeners).
%
% (request, for synchronicity)
%
-spec registerResilienceManager( wooper_state() ) ->
									   request_return( 'registered' ).
registerResilienceManager( State ) ->

	ResilienceManagerPid = ?getSender(),

	Listeners = ?getAttr(simulation_listeners),

	?wooper_return_state_result( setAttributes( State, [

			{ simulation_listeners, [ ResilienceManagerPid | Listeners ] },
			{ resilience_manager_pid, ResilienceManagerPid } ] ),

								registered ).



% Unregisters the resilience manager.
%
% (oneway)
%
-spec unregisterResilienceManager( wooper_state() ) -> oneway_return().
unregisterResilienceManager( State ) ->
	?wooper_return_state_only( setAttribute( State, resilience_manager_pid,
											 undefined ) ).



% Tells this (supposedly root) time manager that a serialisation is to happen
% once the current diasca is over (typically called by the resilience manager).
%
% (oneway)
%
-spec serialisationRequested( wooper_state() ) -> oneway_return().
serialisationRequested( State ) ->
	?wooper_return_state_only(
			setAttribute( State, serialisation_requested, true ) ).



% Returns a list of all the actors that are not simulation agents and that are
% directly managed by this time manager.
%
% (const request)
%
-spec getAllLocalActors( wooper_state() ) -> request_return( [ pid() ] ).
getAllLocalActors( State ) ->

	% We filter out the load balancer (if ever it was managed by this time
	% manager), as we want only actual, "real", actors:

	% Efficient search:
	NonAgentActors = ?list_impl:delete_any( ?getAttr(load_balancer_pid),
									?getAttr(known_local_actors) ),

	% More compact sending:
	Actors = ?list_impl:to_list( NonAgentActors ),

	?wooper_return_state_result( State, Actors ).



% Merges the specified serialisation entries into the state of this time
% manager.
%
% (request)
%
-spec mergeWith( wooper_state(), term_serialisation() ) ->
					   request_return( 'merged' ).
mergeWith( State, Entries ) ->

	ToMergeAgenda = option_list:get( spontaneous_agenda, Entries ),

	% Supposedly the current is the smallest:
	MergedAgenda = merge_agendas( ?getAttr(spontaneous_agenda),
								  ToMergeAgenda ),

	% Default pair order is *not* fine:
	MergedPreviousTimestamp = max_timestamp( ?getAttr(previous_timestamp),
			option_list:get( previous_timestamp, Entries ) ),

	MergedNextTimestamp = min_timestamp( ?getAttr(next_timestamp),
			option_list:get( next_timestamp, Entries ) ),

	MergedNextAction = merge_next_action( ?getAttr(next_action),
			option_list:get( next_action, Entries ), MergedAgenda ),

	MergedKnownActors = ?list_impl:union( ?getAttr(known_local_actors),
			option_list:get( known_local_actors, Entries ) ),

	MergedToDelete = ?getAttr(actors_to_delete_at_next_tick)
		++ option_list:get( actors_to_delete_at_next_tick, Entries ),

	MergedState = setAttributes( State, [

		{ spontaneous_agenda, MergedAgenda },
		{ previous_timestamp, MergedPreviousTimestamp },
		{ next_timestamp, MergedNextTimestamp },
		{ next_action, MergedNextAction },
		{ known_local_actors, MergedKnownActors },
		{ initial_timestamp, option_list:get( initial_timestamp, Entries ) },
		{ initial_tick, option_list:get( initial_tick, Entries ) },
		{ current_tick_offset,
		  option_list:get( current_tick_offset, Entries ) },
		{ current_diasca, option_list:get( current_diasca, Entries ) },
		{ actors_to_delete_at_next_tick, MergedToDelete }

										 ] ),

	?debug_fmt( "The merge of current state:~s~nwith specified entries:~s~n"
				"resulted in:~s~n",
				[ wooper_state_toString( State ),
				  option_list:to_string( Entries ),
				  wooper_state_toString( MergedState ) ] ),

	?wooper_return_state_result( MergedState, merged ).



% Determines the next action after the merge.
%
% (helper)
%
merge_next_action( _FirstAction=no_planned_action,
				   _SecondAction=no_planned_action,
				   _MergedAgenda=[] ) ->
	no_planned_action;

merge_next_action( _FirstAction=no_planned_action,
				   _SecondAction=no_planned_action,
				   _MergedAgenda=[ { Offset, _ActorList } | _T ] ) ->
	Offset;

merge_next_action( FirstAction, _SecondAction=no_planned_action,
				   _MergedAgenda ) ->
	FirstAction;

merge_next_action( _FirstAction=no_planned_action, SecondAction,
				   _MergedAgenda ) ->
	SecondAction;

merge_next_action( FirstAction, SecondAction, _MergedAgenda )
  when FirstAction < SecondAction ->
	FirstAction;

merge_next_action( _FirstAction, SecondAction, _MergedAgenda ) ->
	SecondAction.



% Relinks this time manager, supposing it was just deserialised.
%
% (request, for synchronicity)
%
-spec relink( wooper_state() ) -> request_return( { 'relinked', pid() } ).
relink( State ) ->

	% Let's ensure we have all proper PIDs, starting from class-specific
	% updates:

	% Otherwise the next functions could not send traces:
	InitState = class_TraceEmitter:init( State ),

	UpdatedState = InitState,

	% We have to recreate all private helper processes that were running, see
	% get_private_processes_attribute_names/0:
	%
	WatchdogState = case ?getAttr(watchdog_pid) of

		undefined ->
			UpdatedState;

		?term_restoration_marker ->
			% We restore the watchdog then (reset):
			launch_watchdog( UpdatedState )

	end,

	TimerState = case ?getAttr(timer_pid) of

		undefined ->
			WatchdogState;

		?term_restoration_marker ->
			% We restore the timer then (reset):
			launch_timer( WatchdogState )

	end,

	WallclockState = case ?getAttr(wallclock_tracker_pid) of

		undefined ->
			TimerState;

		?term_restoration_marker ->
			% We restore the wallclcok time tracker then (reset):
			launch_wallclock_tracker( TimerState )

	end,

	TimeState = case ?getAttr(time_tracker_pid) of

		undefined ->
			WallclockState;

		?term_restoration_marker ->

			% We restore the wallclcok time tracker then (reset):

			Timestamp = { ?getAttr(current_tick_offset),
						  ?getAttr(current_diasca) },

			launch_time_tracker( { resuming, Timestamp }, WallclockState )

	end,

	% Then performing generic relinking, based on an entry transformer
	% requesting the instance tracking service to convert back PIDs:


	?wooper_return_state_result( TimeState, { relinked, self() } ).



% Requests this (supposedly root) time manager to restart, after a rollback
% (hence based on a just deserialised state).
%
% (oneway)
%
-spec restartAfterRollback( wooper_state() ) -> oneway_return().
restartAfterRollback( State ) ->

	% Of course we do not want an infinite rollback loop:
	RestartedState = manage_end_of_diasca_as_root_manager(
				setAttribute( State, serialisation_requested, false ) ),

	?wooper_return_state_only( RestartedState ).




% Scheduling section.


% Subsection for the scheduling of spontaneous actions (each first diasca of a
% tick).


% Notifies this time manager that a new tick must begin (thus at diasca 0); to
% be sent by the direct parent time manager (if any).
%
% Takes care of the beginning of the specified new overall simulation tick
% (starting at diasca 0), which implies sending this message recursively through
% the whole scheduling tree, so that all relevant actors can be reached.
%
% (oneway)
%
-spec beginTimeManagerTick( wooper_state(), tick_offset() ) -> oneway_return().
beginTimeManagerTick( State, NewTickOffset ) ->

	%io:format( "beginTimeManagerTick: new tick offset is #~B, "
	%			"next action ~p.~n", [ NewTickOffset, ?getAttr(next_action) ] ),

	% Some (optional) checkings:
	check_tick_consistency( NewTickOffset, State ),

	% We cannot have messages waiting for diasca 0, by design:
	true = ?list_impl:is_empty( ?getAttr(actors_to_trigger_in_one_diasca) ),

	% There might be early actors which already targeted diasca 1 of this new
	% tick:
	% true = ?list_impl:is_empty( ?getAttr(actors_to_trigger_in_two_diascas) ),


	% In the rest of that method, we will be using for example:
	% 'class_TraceEmitter:send(info,[NewState|Args])' instead of '?info(..)' so
	% that the traces are output with a timestamp set to the new current tick
	% rather than the previous one still in ?getAttr(current_tick_offset): we
	% want the next traces to be the first of this new tick, not the last of the
	% previous one, thus the use of 'NewState' instead of the usual (implicit)
	% 'State'.

	% Here we go for this next tick:

	StopTickOffset = ?getAttr(stop_tick_offset),

	FinalState = case NewTickOffset of

		% The stop tick (if defined) may happen to be a skipped tick:
		%
		% (relies also on the fact that any integer is considered as smaller
		% than the 'undefined' atom, according to Erlang term order: number <
		% atom)
		%
		LateOffset when LateOffset >= StopTickOffset ->

			% We never enter this clause if StopTickOffset == 'undefined'.

			%io:format( "beginTimeManagerTick: stopping at tick #~B, "
			%			"as stop tick was #~B.~n",
			%			[ NewTickOffset, StopTickOffset ] ),

			% Can only happen if being the root time manager.

			% Not using this new tick (which is past the requested end),
			% specifying the user-defined stop tick instead:
			NewState = setAttributes( State, [

						{ current_tick_offset, StopTickOffset },
						{ current_diasca, 0} ,
						{ next_action, no_planned_action }
											  ] ),

			ActualCurrentTick = ?getAttr(initial_tick) + StopTickOffset,

			?send_info_fmt( NewState, "Reached termination tick ~B, ~B ticks "
				"have elapsed since the simulation started, stopping now.",
				[ ActualCurrentTick, StopTickOffset ] ),

			on_simulation_success( NewState ),

			{ StoppedState, {stopped,_SelfPid} } = executeRequest( NewState,
															   stop ),
			StoppedState;


		_NonTerminalOrUndefinedStopTickOffset ->

			%io:format( "beginTimeManagerTick: continuing at tick #~B.~n",
			%		  [ NewTickOffset ] ),

			NewState = setAttributes( State, [

						{ current_tick_offset, NewTickOffset },
						{ current_diasca, 0 },
						{ next_action, no_planned_action }

											  ] ),

			% Let's run this new tick, and possibly wait for answers:
			ManagedState = manage_new_tick( NewTickOffset, NewState ),

			% Deferred deletion of terminated actors on the course of the
			% simulation:
			%
			[ A ! delete || A <- ?getAttr(actors_to_delete_at_next_tick) ],

			setAttributes( ManagedState, [

					{ previous_timestamp, { NewTickOffset, 0 } },
					{ actors_to_delete_at_next_tick, [] }

										  ] )


	end,

	?wooper_return_state_only( FinalState ).



% Notifies this time manager that the subtree corresponding to its direct child
% time manager that sent this message finished its spontaneous scheduling (first
% diasca) for the current tick.
%
% (oneway)
%
-spec notifySpontaneousSubtreeCompletion( wooper_state(), tick_offset(), pid(),
	 next_manager_action(), diasca_tracking_info() ) -> oneway_return().
notifySpontaneousSubtreeCompletion( State, SubtreeTickOffset, ChildManagerPid,
	  NextActionInSubtree,
	  _TrackingInfo={ ChildScheduleCount, ChildProcessCount } ) ->

	WaitedManagers = ?getAttr(waited_child_managers),

	%io:format( "Time manager ~w received notifySpontaneousSubtreeCompletion "
	%		   "from ~w at #~B, reporting for next action '~p'; "
	%		   "was still waiting for ~p.~n",
	%		   [ self(), ChildManagerPid, SubtreeTickOffset,
	%			 NextActionInSubtree, ?list_impl:to_list(WaitedManagers) ] ),


	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ChildManagerPid, WaitedManagers ),
	SubtreeTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	SoonestState = update_next_action_with( NextActionInSubtree,
												   State ),

	RemainingWaitedManagers = list_utils:safe_listimpl_delete( ChildManagerPid,
		WaitedManagers ),

	CurrentScheduleCount = ?getAttr(scheduled_tracking) + ChildScheduleCount,

	CurrentProcessCount = ?getAttr(process_tracking) + ChildProcessCount,

	WaitedState = setAttributes( SoonestState, [

			{ waited_child_managers, RemainingWaitedManagers },
			{ scheduled_tracking, CurrentScheduleCount },
			{ process_tracking, CurrentProcessCount }

			 ] ),

	CountedState = subtractFromAttribute( WaitedState, waited_count, 1 ),

	?wooper_return_state_only( manage_possible_end_of_diasca( CountedState ) ).



% Notifies this time manager that the specified local actor finished its
% spontaneous actions (first diasca) for the current tick, and tells what it
% expects next in terms of scheduling.
%
% Clauses are sorted by decreasing probability.
%
% (oneway)
%
-spec notifySpontaneousActionsCompleted( wooper_state(), tick_offset(), pid(),
	 class_Actor:next_reported_action(), [ tick_offset() ],
	 [ tick_offset() ] ) -> oneway_return().
notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=no_diasca_requested, AddedSpontaneousTicks,
		  WithdrawnSpontaneousTicks ) ->

	% Here this actor does not request any diasca, so it must not have sent any
	% actor message this diasca.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	%io:format( "Time manager ~w received notifySpontaneousActionsCompleted "
	%			"from ~w at #~B, reporting no need for a new diasca; "
	%			"was still waiting for ~B spontaneous actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
							WithdrawnSpontaneousTicks, ActorPid, State ),

	NewWaitedSpontaneousActors = list_utils:safe_listimpl_delete( ActorPid,
															 WaitedActors ),

	% next_timestamp, next_action, etc. not to be changed here.

	UpdatedState = setAttributes( AgendaState, [

			{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
			{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=new_diasca_needed, AddedSpontaneousTicks,
		  WithdrawnSpontaneousTicks ) ->

	% Here the actor requests a new diasca, most probably because it sent an
	% actor message this diasca. This means the next diasca should be scheduled,
	% but not necessarily that this sending actor is to be scheduled (the
	% receiving actor must have notified its own time manager about that).

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	%io:format( "Time manager ~w received notifySpontaneousActionsCompleted "
	%			"from ~w at #~B, reporting the need for a new diasca; "
	%			"was still waiting for ~B spontaneous actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			  ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	AgendaState = update_agenda( AddedSpontaneousTicks,
						 WithdrawnSpontaneousTicks, ActorPid, State ),

	NewWaitedSpontaneousActors = list_utils:safe_listimpl_delete( ActorPid,
															  WaitedActors ),

	UpdatedState = setAttributes( AgendaState, [

			   { next_timestamp, { ActorTickOffset, 1 } },
			   { next_action, new_diasca_needed },
			   { waited_spontaneous_actors, NewWaitedSpontaneousActors } ,
			   { waited_count, ?getAttr(waited_count)-1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=terminating, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor is (actively) terminating, has specified a number of
	% intercalary diascas and has not exhausted them.
	%
	% This implies that the next diasca must be scheduled, and that this actor
	% will have to be scheduled during it.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	%io:format( "Time manager ~w received notifySpontaneousActionsCompleted "
	%			"from ~w at #~B, reporting the actor termination; "
	%			"was still waiting for ~B spontaneous actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			  ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	NewTerminating = [ ActorPid | ?getAttr(terminating_actors) ],

	NewWaitedSpontaneousActors = list_utils:safe_listimpl_delete( ActorPid,
															   WaitedActors ),

	UpdatedState = setAttributes( State, [

			{ next_timestamp, { ActorTickOffset, 1 } },
			{ next_action, new_diasca_needed },
			{ terminating_actors, NewTerminating },
			{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
			{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=terminating_unlimited, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor has for next action { terminating, unlimited }; hence it is
	% passively terminating, we do not specifically schedule it anymore, and
	% simply will delete it at the next tick, regardless of the number of diasca
	% elapsed.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	%io:format( "Time manager ~w received notifySpontaneousActionsCompleted "
	%			"from ~w at #~B, reporting this actor is passively terminating;"
	%			"was still waiting for ~B spontaneous actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			  ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	% This actor will be left as is during the current tick, then will be
	% deleted.
	%
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	NewWaitedSpontaneousActors = list_utils:safe_listimpl_delete( ActorPid,
															   WaitedActors ),

	UpdatedState = setAttributes( State, [

				% We do not change next_timestamp, next_action, etc.

				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
				{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifySpontaneousActionsCompleted( State, ActorTickOffset, ActorPid,
		  _NextActorAction=terminated, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% This actor has terminated now, it could be deleted on any next
	% diasca/tick, but we leave it live until the next tick (not depending on
	% the number of intermediary diascas), to be able to catch any unexpected
	% late actor message directed at it.

	WaitedActors = ?getAttr(waited_spontaneous_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B, reporting the actual actor termination; "
	%			"was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),


	NewWaitedSpontaneousActors = list_utils:safe_listimpl_delete( ActorPid,
															  WaitedActors ),

	% Postponing a bit the deletion:
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	UpdatedState = setAttributes( State, [

				% We do not change next_timestamp, next_action, etc.

				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_spontaneous_actors, NewWaitedSpontaneousActors },
				{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) ).





% Notifies this (root) time manager that the watchdog finished its spontaneous
% actions (first diasca) for the current tick.
%
% (oneway)
%
-spec notifySpontaneousWatchdogCompleted( wooper_state(), tick_offset() ) ->
												oneway_return().
notifySpontaneousWatchdogCompleted( State, WatchdogTickOffset ) ->

	%io:format( "Time manager ~w received notifySpontaneousWatchdogCompleted "
	%		   "from watchdog at #~B, while still waiting for ~B "
	%		   "spontaneous actor(s).~n",
	%		   [ self(), WatchdogTickOffset,
	%			?list_impl:size( ?getAttr(waited_spontaneous_actors) ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?getAttr(watchdog_waited),
	WatchdogTickOffset = ?getAttr(current_tick_offset),
	0 = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	% Nothing to do. Although it is not necessary, we prefer to force the
	% keeping in sync with the watchdog as well.

	WaitedCount = ?getAttr(waited_count),

	AcknowledgedState = setAttributes( State, [

		{ waited_count, WaitedCount-1 },
		{ watchdog_waited, false }

											   ] ),

	% Yes, indeed, it may actually happen that the watchdog is the last to
	% answer, even in a distributed context!
	%
	?wooper_return_state_only(
				 manage_possible_end_of_diasca( AcknowledgedState ) ).







% Subsection for the scheduling of all diascas that may be created after the
% first one (which is dedicated to spontaneous actions).



% Notifies this time manager that a new (strictly positive) diasca tick is to
% begin now (thus after diasca 0, which is dedicated to spontaneous actions); to
% be sent by the direct parent time manager (if any).
%
% Takes care of the beginning of the specified new diasca, which implies sending
% this message recursively through the whole scheduling tree, so that all
% relevant actors can be notified.
%
% (oneway)
%
-spec beginTimeManagerDiasca( wooper_state(), tick_offset(), diasca() ) ->
									oneway_return().
beginTimeManagerDiasca( State, TickOffset, NewDiasca ) ->

	%io:format( "beginTimeManagerDiasca: at tick offset #~B, "
	%		   "new diasca is ~B.~n", [ TickOffset, NewDiasca ] ),

	% Some (optional) checkings:
	check_diasca_consistency( TickOffset, NewDiasca, State ),

	% actors_to_trigger_in_one_diasca and/or actors_to_trigger_in_two_diascas
	% might be non-empty here.

	NewState = setAttributes( State, [

					{ current_diasca, NewDiasca },
					{ next_action, no_planned_action }

									 ] ),

	% Let's run this new diasca, and possibly wait for answers:
	ManagedState = manage_new_diasca( TickOffset, NewDiasca, NewState ),

	FinalState = setAttribute( ManagedState, previous_timestamp,
						 { TickOffset, NewDiasca } ),

	?wooper_return_state_only( FinalState ).



% Notifies this time manager that the subtree corresponding to its direct child
% time manager that sent this message finished the specified (strictly positive)
% diasca for the current tick.
%
% (oneway)
%
-spec notifyTriggerSubtreeCompletion( wooper_state(), tick_offset(),
	 diasca(), pid(), next_manager_action(), diasca_tracking_info() ) ->
										   oneway_return().
notifyTriggerSubtreeCompletion( State, SubtreeTickOffset, SubtreeDiasca,
			  ChildManagerPid, NextActionInSubtree,
			  _TrackingInfo={ ChildScheduleCount, ChildProcessCount } ) ->

	WaitedManagers = ?getAttr(waited_child_managers),

	%io:format( "Time manager ~w received notifyTriggerSubtreeCompletion "
	%			"from ~w at #~B diasca ~B, reporting for next action '~p'; "
	%			"was still waiting for ~p.~n",
	%			[ self(), ChildManagerPid, SubtreeTickOffset, SubtreeDiasca,
	%			 NextActionInSubtree, ?list_impl:to_list(WaitedManagers) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ChildManagerPid, WaitedManagers ),
	SubtreeTickOffset = ?getAttr(current_tick_offset),
	SubtreeDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	SoonestState = update_next_action_with( NextActionInSubtree,
												   State ),

	RemainingWaitedManagers = list_utils:safe_listimpl_delete( ChildManagerPid,
		WaitedManagers ),

	CurrentScheduleCount = ?getAttr(scheduled_tracking) + ChildScheduleCount,

	CurrentProcessCount = ?getAttr(process_tracking) + ChildProcessCount,

	WaitedState = setAttributes( SoonestState, [

			{ waited_child_managers,RemainingWaitedManagers },
			{ scheduled_tracking,CurrentScheduleCount },
			{ process_tracking,CurrentProcessCount }

			 ] ),

	CountedState = subtractFromAttribute( WaitedState, waited_count, 1 ),

	?wooper_return_state_only( manage_possible_end_of_diasca( CountedState ) ).



% Notifies this time manager that the specified local actor finished its current
% (non-first) diasca for the current tick, and tells what it expects next in
% terms of scheduling.
%
% Clauses are sorted by decreasing probability.
%
% (oneway)
%
-spec notifyTriggeredActionsCompleted( wooper_state(), tick_offset(), diasca(),
		 class_Actor:actor_pid(), class_Actor:next_reported_action(),
		 [ tick_offset() ], [ tick_offset() ] ) -> oneway_return().
notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
	   _NextActorAction=no_diasca_requested, AddedSpontaneousTicks,
	   WithdrawnSpontaneousTicks ) ->

	% Here this actor does not request any diasca, so it must not have sent any
	% actor message this diasca.

	WaitedActors = ?getAttr(waited_triggered_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B diasca ~B, reporting no need for a new diasca;"
	%			" was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	ActorDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
						 WithdrawnSpontaneousTicks, ActorPid, State ),

	NewWaitedTriggeredActors = list_utils:safe_listimpl_delete( ActorPid,
																WaitedActors ),

	% next_timestamp, next_action, etc. should not to be changed here (will be
	% determined just when needed, when reporting the end of this diasca).

	UpdatedState = setAttributes( AgendaState, [

				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
	   _NextActorAction=new_diasca_needed, AddedSpontaneousTicks,
	   WithdrawnSpontaneousTicks ) ->

	% Here the actor requests a new diasca, most probably because it sent an
	% actor message this diasca. This means the next diasca should be scheduled,
	% but not necessarily that this sending actor is to be scheduled (the
	% receiving actor must have notified its own time manager about that).

	WaitedActors = ?getAttr(waited_triggered_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B diasca ~B, reporting the need for a new diasca;"
	%			" was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset, ActorDiasca,
	%			  ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	ActorDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	% Note: the agenda is updated, but not the next action:
	AgendaState = update_agenda( AddedSpontaneousTicks,
						 WithdrawnSpontaneousTicks, ActorPid, State ),

	NewWaitedTriggeredActors = list_utils:safe_listimpl_delete( ActorPid,
																WaitedActors ),

	UpdatedState = setAttributes( AgendaState, [

				{ next_timestamp, { ActorTickOffset, ActorDiasca+1 } },
				{ next_action, new_diasca_needed },
				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count)-1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		  _NextActorAction=terminating, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor is (actively) terminating, has specified a number of
	% intercalary diascas and has not exhausted them.
	%
	% This implies that the next diasca must be scheduled, and that this actor
	% will have to be scheduled during it.

	WaitedActors = ?getAttr(waited_triggered_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B, reporting this actor is actively terminating; "
	%			"was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	ActorDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	NewTerminating = [ ActorPid | ?getAttr(terminating_actors) ],

	NewWaitedTriggeredActors = list_utils:safe_listimpl_delete( ActorPid,
														WaitedActors ),

	UpdatedState = setAttributes( State, [

			{ next_timestamp, { ActorTickOffset, ActorDiasca + 1 } },
			{ next_action, new_diasca_needed },
			{ terminating_actors, NewTerminating },
			{ waited_triggered_actors, NewWaitedTriggeredActors },
			{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		  _NextActorAction=terminating_unlimited, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% Here the actor has for next action { terminating, unlimited }; hence it is
	% passively terminating, we do not specifically schedule it anymore, and
	% simply will delete it at the next tick, regardless of the number of diasca
	% elapsed.

	WaitedActors = ?getAttr(waited_triggered_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B, reporting this actor is passively terminating;"
	%			" was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	ActorDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	% This actor will be left as is during the current tick, then will be
	% deleted.
	%
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	NewWaitedTriggeredActors = list_utils:safe_listimpl_delete( ActorPid,
																WaitedActors ),

	UpdatedState = setAttributes( State, [

				% We do not change next_timestamp, next_action, etc.

				{ actors_to_delete_at_next_tick, ActorsToDelete },
				{ waited_triggered_actors, NewWaitedTriggeredActors },
				{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) );


notifyTriggeredActionsCompleted( State, ActorTickOffset, ActorDiasca, ActorPid,
		  _NextActorAction=terminated, _AddedSpontaneousTicks=[],
		  _WithdrawnSpontaneousTicks=[] ) ->

	% This actor has terminated now, it could be deleted on any next
	% diasca/tick, but we leave it live until the next tick (not depending on
	% the number of intermediary diascas), to be able to catch any unexpected
	% late actor message directed at it.

	WaitedActors = ?getAttr(waited_triggered_actors),

	%io:format( "Time manager ~w received notifyTriggeredActionsCompleted "
	%			"from ~w at #~B, reporting the actual actor termination; "
	%			"was still waiting for ~B triggered actors.~n",
	%			[ self(), ActorPid, ActorTickOffset,
	%			 ?list_impl:size( WaitedActors ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?list_impl:is_member( ActorPid, WaitedActors ),
	ActorTickOffset = ?getAttr(current_tick_offset),
	ActorDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	% Postponing a bit the deletion:
	ActorsToDelete = [ ActorPid | ?getAttr(actors_to_delete_at_next_tick) ],

	NewWaitedTriggeredActors = list_utils:safe_listimpl_delete( ActorPid,
																WaitedActors ),

	UpdatedState = setAttributes( State, [

					% We do not change next_timestamp, next_action, etc.

					{ actors_to_delete_at_next_tick, ActorsToDelete },
					{ waited_triggered_actors, NewWaitedTriggeredActors },
					{ waited_count, ?getAttr(waited_count) - 1 }

						   ] ),

	?wooper_return_state_only( manage_possible_end_of_diasca( UpdatedState ) ).




% Notifies this (root) time manager that the watchdog finished its current
% (non-first) diasca for the current tick.
%
% (oneway)
%
-spec notifyTriggeredWatchdogCompleted( wooper_state(),
							tick_offset(), diasca() ) -> oneway_return().
notifyTriggeredWatchdogCompleted( State, WatchdogTickOffset, WatchdogDiasca ) ->

	%io:format( "Time manager ~w received notifyTriggeredWatchdogCompleted "
	%			"from watchdog at #~B diasca ~B, while still waiting "
	%			"for ~B triggered actor(s).~n",
	%			[ self(), WatchdogTickOffset, WatchdogDiasca,
	%			 ?list_impl:size( ?getAttr(waited_spontaneous_actors) ) ] ),

	% First, some (optional) checkings:
	check_waited_count_consistency( State ),
	true = ?getAttr(watchdog_waited),
	WatchdogTickOffset = ?getAttr(current_tick_offset),
	WatchdogDiasca = ?getAttr(current_diasca),
	0 = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	% Nothing to do. Although it is not necessary, we prefer to force the
	% keeping in sync with the watchdog as well.

	WaitedCount = ?getAttr(waited_count),

	AcknowledgedState = setAttributes( State, [

		{ waited_count, WaitedCount-1 },
		{ watchdog_waited, false }

											   ] ),

	% Yes, indeed, it may actually happen that the watchdog is the last to
	% answer, even in a distributed context!
	%
	?wooper_return_state_only(
				 manage_possible_end_of_diasca( AcknowledgedState ) ).





% Notifies this time manager that the tick timer determined that a new tick
% should occur; the manager will increase the tick of the simulation clock and
% triggers its processing.
%
% To be called by the internal timer, only if being the root time manager and
% being interactive.
%
% Note: we do not enter into spontaneous actions/diasca considerations, as the
% tick timer exists only for the interactive mode, only interested into
% ticks. We do not even record the values of ticks (we just notify that one
% elapsed), as we do not want to maintain here the actual tick offsets in spite
% of delays, drops and other events.
%
% (oneway)
%
-spec timerTickFinished( wooper_state() ) -> oneway_return().
timerTickFinished( State ) ->

	?trace( "Received a timerTickFinished notification." ),

   interactive = ?getAttr(interactivity_mode),

	% Checks whether the current tick can really be declared over:
	case ?getAttr(waited_count) of

		0 ->

			CurrentTickOffset = ?getAttr(current_tick_offset),

			% Yes, all agents reported the tick could be ended, so we are on
			% time indeed, as we suppose here that this manager is not
			% overloaded enough to process this oneway with significant delay.
			%
			% So we just suppose this call corresponds to the real time and just
			% check that actors are on time.
			%
			% A more robust approach would be to base at least the timer
			% on the real time, not on receive time-outs.
			?debug_fmt( "The simulation is on time for tick offset #~B.",
						[ CurrentTickOffset ] ),

			% Ready for next tick... if not suspended:
			SuspendedState = manage_suspension( CurrentTickOffset, State ),

			% Still supposed to be running?
			case getAttribute( SuspendedState, started ) of

				true ->
					% Yes, so continue with the next tick, using a message to
					% ensure constant space:
					self() ! { beginTimeManagerTick, CurrentTickOffset + 1 };

				false ->
					% No new tick should be begun if stopped in the meantime: a
					% stop request might have been issued whereas a
					% timerTickFinished message was still in the mailbox of this
					% manager.
					%
					% If it was the case, just do nothing, to avoid beginning a
					% new tick whereas stopped:
					%
					ok

			end,

			?wooper_return_state_only( SuspendedState );

		NonZeroCount ->

			?warning_fmt( "timerTickFinished trigger: "
						  "cannot keep up with the interactive pace, "
						  "still waiting for ~B agents, "
						  "loosing sync with hard real time.",
						  [ NonZeroCount ] ),

			% Tick not changed, still waiting:
			?wooper_return_state_only( State )

	end.





% Section for time synchronisation of operations.



% Returns the current simulation time, as an absolute tick.
%
% (const request)
%
-spec getSimulationTick( wooper_state() ) -> request_return( tick() ).
getSimulationTick( State ) ->
	?wooper_return_state_result( State, get_current_tick(State) ).



% Returns the current simulation tick offset, if the simulation has already been
% started, otherwise the atom 'undefined'.
%
% (const request)
%
-spec getSimulationTickOffset( wooper_state() ) ->
									request_return( tick_offset() ).
getSimulationTickOffset( State ) ->
	?wooper_return_state_result( State, ?getAttr(current_tick_offset) ).



% Returns the current simulation diasca, if the simulation has already been
% started, otherwise the atom 'undefined'.
%
% (const request)
%
-spec getSimulationDiasca( wooper_state() ) ->
	   request_return( basic_utils:maybe( diasca() ) ).
getSimulationDiasca( State ) ->
	?wooper_return_state_result( State, ?getAttr(current_diasca) ).



% Returns the current simulation timestamp (tick offset and diasca), if the
% simulation has already been started, otherwise the atom 'undefined'.
%
% (const request)
%
-spec getSimulationLogicalTimestamp( wooper_state() ) ->
		request_return( basic_utils:maybe( logical_timestamp() ) ).
getSimulationLogicalTimestamp( State ) ->

	Res = case ?getAttr(current_tick_offset) of

		undefined ->
			undefined;

		TickOffset ->
			{ TickOffset, ?getAttr(current_diasca) }

	end,

	?wooper_return_state_result( State, Res ).




% Returns the current simulation time, structured as follows:
% { {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} }.
%
% (const request)
%
-spec getSimulationDate( wooper_state() ) ->
							request_return( basic_utils:timestamp() ).
getSimulationDate( State ) ->

	Seconds = convert_ticks_to_rounded_seconds( get_current_tick(State),
												State ),

	?wooper_return_state_result( State,
		calendar:gregorian_seconds_to_datetime( Seconds ) ).



% Returns a textual description of the simulation and real time.
%
% (const request)
%
-spec getTextualTimings( wooper_state() ) -> request_return( string() ).
getTextualTimings( State ) ->
	?wooper_return_state_result( State, get_textual_timings(State) ).



% Converts the specified number of seconds (expressed as an integer or a
% floating-point value, i.e. any granularity below the second can be specified)
% into an integer (rounded, non-negative) number of ticks, using the target time
% manager.
%
% Ex: TimeManager ! { convertSecondsToTicks, 0.02, self() }.
% Returns the appropriate (integer) number of ticks.
%
% Note that, due to rounding, depending on the current frequency of the time
% manager and on the specified duration, the returned duration might be zero
% tick, i.e. a null duration.
%
% Note also that models are expected to call the counterpart
% class_Actor:convert_seconds_to_ticks/{2,3} helper functions, rather than
% interacting with their time manager.
%
% (const request)
%
-spec convertSecondsToTicks( wooper_state(),
 unit_utils:seconds() | virtual_seconds() ) -> request_return( tick_offset() ).
convertSecondsToTicks( State, Seconds ) ->
	?wooper_return_state_result( State,
		convert_seconds_to_ticks( Seconds, State ) ).



% Converts the specified number of seconds (expressed as an integer or a
% floating-point value, i.e. any granularity below the second can be specified)
% into an integer (rounded, strictly positive) number of ticks, using the target
% time manager and ensuring that, if ever the rounding would have led to a
% zero-tick duration, a duration of one tick is returned instead.
%
% Ex: TimeManager ! { convertSecondsToNonNullTickDuration, 0.02, self() }.
% Returns the appropriate (integer, strictly positive) number of ticks.
%
% Useful to ensure that under no circumstances a duration can be null, in order
% that planned actions will always happen in a strict future.
%
% (const request)
%
-spec convertSecondsToNonNullTickDuration( wooper_state(),
		  unit_utils:seconds() | virtual_seconds() ) ->
											 request_return( tick_offset() ).
convertSecondsToNonNullTickDuration( State, Seconds ) ->

	Count = case convert_seconds_to_ticks( Seconds, State ) of

		0 ->
			1 ;

		NonNullCount ->
			NonNullCount

	end,

	?wooper_return_state_result( State, Count ).



% Converts the specified tick count into an integer (rounded) number of seconds,
% using the specified time manager.
%
% Returns the appropriate number of seconds.
%
% (const request)
%
-spec convertTicksToSeconds( wooper_state(), tick_offset() ) ->
				   request_return( unit_utils:seconds() ).
convertTicksToSeconds( State, Ticks ) ->
	?wooper_return_state_result( State,
		convert_ticks_to_rounded_seconds( Ticks, State ) ).


-spec convertTicksToPreciseDuration( wooper_state(), tick_offset() ) ->
				   request_return( virtual_seconds() ).
convertTicksToPreciseDuration( State, Ticks ) ->
	?wooper_return_state_result( State,
		convert_ticks_to_seconds( Ticks, State ) ).



% Section dedicated to the subscribing/unsubscribing of the simulated actors.



% Requests the manager to subscribe the calling actor:
%
% - AbstractActorIdentifier is the AAI of the actor to subscribe
%
% - ActorBinName is the name of this actor (as a binary)
%
% - ClassName is the class of this actor (as an atom)
%
% Any just subscribed actor will be notified of the next simulation diasca,
% which, if the simulation is running, will be the current one plus one,
% regardless of any skip of ticks which could be already planned.
%
% Returns either:
%
% - time_subscribed if the operation succeeded (subscribed, and was not
% subscribed yet); the actor will receive the begin notification for the next
% scheduled diasca
%
% - already_time_subscribed if the caller was already subscribed (it is still
% subscribed only once)
%
% Note: this method could not be named 'register', as it is a reserved word.
%
% Ex: MyTimeManager ! { subscribe, [ MyAAI, MyBinName, MyClassName ], self() }
%
% The current timestamp of manager cannot be returned, as it may not be started
% yet (in which case there is not even a current time).
%
% (request)
%
-spec subscribe( wooper_state(), class_Actor:aai(), text_utils:bin_string(),
				class_name() ) -> request_return(
	'already_time_subscribed'
	| { 'time_subscribed', virtual_seconds(),
		 'not_started_yet' | { tick(), logical_timestamp() } } ).
subscribe( State, AbstractActorIdentifier, ActorBinName, ClassName ) ->

	% PID retrieved from request:
	CallerPid = ?getSender(),

	case ?getAttr(troubleshooting_mode) of

		true ->

			% Updates directly the right instance tracker: this time manager,
			% the subscribing actor and the instance tracker that shall be
			% targeted are by design on the same node, thus a local look-up is
			% the best approach.
			LocalInstanceTracker = class_InstanceTracker:get_local_tracker(),

			LocalInstanceTracker ! { registerActor, [ AbstractActorIdentifier,
						ActorBinName, CallerPid, ClassName ] };

		false ->
			ok

	end,

	LocalActors = ?getAttr(known_local_actors),

	case ?list_impl:is_member( CallerPid, LocalActors ) of

		true ->

			?warning_fmt( "Subscribing requested, whereas "
				"actor ~w was already time subscribed.", [ CallerPid ] ),

			?wooper_return_state_result( State, already_time_subscribed ) ;

		false ->

			%?debug_fmt( "Subscribing actor ~w.", [ CallerPid ] ),

			% Links together this manager and the calling listener, so that the
			% termination of one will result in the other receiving an exit
			% signal:
			%
			% (this is better to link actors to their own local time manager, in
			% a distributed way, rather than for example linking all actors to
			% the load balancer)
			%
			true = erlang:link( CallerPid ),

			AddedState = setAttribute( State, known_local_actors,
				?list_impl:add_element( CallerPid, LocalActors ) ),

			% If the simulation is already running, notifies directly the actor,
			% otherwise does nothing, as it will be done when starting:
			%
			% (the actor is currently blocked in its constructor, waiting for a
			% wooper result; therefore its simulationStarted/3 request could not
			% be called until it received the result of this subscribe method,
			% which would in turn create a deadlock)
			%
			% We now have to be able to:
			%
			% - subscribe an actor
			%
			% - notify it that the simulation is already running (if it is the
			% case)
			%
			% both as one atomic operation, as otherwise there could be a race
			% condition between two spawned actors, the first receiving the
			% start notification and its first top then sending a message to the
			% second actor, whereas this latter actor is not even started yet.
			%

			StartInformation = case ?getAttr(started) of

				true ->
					% Already running, notifying this actor about that.
					%
					% No need to specifically schedule this actor on the fly for
					% the very next diasca (based on the 'onFirstDiasca/2' actor
					% oneway) as it is done by the load balancer.
					%
					% We specify here the current diasca, not the next one at
					% which this actor will be scheduled, as it may receive an
					% actor message in-between (typicall onFirstDiasca/2) and
					% this would trigger a 'message in the past' error:
					%
					NewTimestamp = { ?getAttr(current_tick_offset),
									 ?getAttr(current_diasca) },

					{ ?getAttr(initial_tick), NewTimestamp };

				false ->
					not_started_yet

			end,

			% Note: the created actor is not the one which must ensure the next
			% diasca is actually scheduled, since its time manager may already
			% have reported its end of diasca; the only time manager that should
			% be used is the only one that for sure has not finished its diasca
			% yet, i.e. the one of the creating actor. As a consequence here we
			% do nothing to ensure that the next diasca is scheduled.

			% StartInformation allows to combine 'subscribe' and
			% 'simulationStarted' into one atomic operation:
			%
			?wooper_return_state_result( AddedState,
				{ time_subscribed, ?getAttr(simulation_tick_duration),
				  StartInformation } )

	end.



% Requests the manager to unsubscribe the caller from the list of time
% listeners.
%
% Returns time_unsubscribed if the operation succeeded (subscribed and was not
% subscribed yet).
%
% Ex: MyTimeManager ! { unsubscribe, [], self() }
%
% (request)
%
-spec unsubscribe( wooper_state() ) -> request_return( 'time_unsubscribed' ).
unsubscribe( State ) ->

	UpdatedState = actual_unsubscribing( ?getSender(), State ),

	?wooper_return_state_result( UpdatedState, time_unsubscribed ).





% Section dedicated to the exchanges performed in the course of a diasca.



% Called by an actor managed by this time manager and having received an actor
% message during this diasca for the first time: this call tells this time
% manager to schedule (trigger) this actor on the specified next diasca, so that
% it can process its actor message(s).
%
% Note that specifying the triggered diasca is necessary, due to a possible race
% condition for a local time manager between this message and its 'begin diasca'
% message sent by its parent time manager.
%
% (request)
%
-spec scheduleTrigger( wooper_state(), tick_offset(), diasca() ) ->
					 request_return( 'trigger_planned' ).
scheduleTrigger( State, TriggerTickOffset, TriggerDiasca ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),
	CurrentDiasca = ?getAttr(current_diasca),

	ActorToSchedule = ?getSender(),

	%io:format( "scheduleTrigger: ~w adding actor to trigger ~w at diasca ~B "
	%   "of tick offset #~B while being at diasca ~B of tick offset #~B.~n",
	%   [ self(), ActorToSchedule, TriggerDiasca, TriggerTickOffset,
	%	 CurrentDiasca, CurrentTickOffset ] ),


	% Checkings:

	% Most of the triggers should come from the same tick and diasca. However an
	% early non-local actor may already be in the next diasca (before this
	% manager receives its 'begin diasca' notification), or even in any
	% arbitrary future tick (after a jump over idle ones). So we can only test:
	%
	RecordedState = case TriggerTickOffset of


		CurrentTickOffset ->

			% Usual case: same tick; either sent from the same diasca (most
			% frequent), or being diasca-early:
			%
			UsuallyExpectedTriggerDiasca = CurrentDiasca + 1,
			EarlyBirdTriggerDiasca = CurrentDiasca + 2,

			case TriggerDiasca of

				UsuallyExpectedTriggerDiasca ->

					% Most common case, sender and manager in synch:

					CurrentDiascaActors =
						?getAttr(actors_to_trigger_in_one_diasca),

					% Checking:
					false = ?list_impl:is_member( ActorToSchedule,
												CurrentDiascaActors ),

					UpdatedDiascaActors = ?list_impl:add_element(
										ActorToSchedule, CurrentDiascaActors ),

					setAttribute( State, actors_to_trigger_in_one_diasca,
								UpdatedDiascaActors );


				EarlyBirdTriggerDiasca ->

					% Early sender, compared to this manager that must be
					% lagging behind:

					FutureDiascaActors =
						?getAttr(actors_to_trigger_in_two_diascas),

					% Checking:
					false = ?list_impl:is_member( ActorToSchedule,
												  FutureDiascaActors ),

					UpdatedDiascaActors = ?list_impl:add_element(
										ActorToSchedule, FutureDiascaActors ),

					% We now know what is the next timestamp (hopefully):
					NextTimestamp = { CurrentTickOffset,
									 EarlyBirdTriggerDiasca },

					setAttributes( State, [

					  { actors_to_trigger_in_two_diascas, UpdatedDiascaActors },
					  { next_timestamp, NextTimestamp }

								] );

				_ ->
					throw( { invalid_trigger_diasca, TriggerDiasca,
						CurrentDiasca, CurrentTickOffset, ActorToSchedule } )

			end;


		OtherTickOffset when OtherTickOffset > CurrentTickOffset ->

			% If already in the future (after a jump), cannot have gone past
			% diasca 0, thus the message must be targeting diasca 1:
			1 = TriggerDiasca,

			FutureDiascaActors = ?getAttr(actors_to_trigger_in_two_diascas),

			% Checking:
			false = ?list_impl:is_member( ActorToSchedule, FutureDiascaActors ),

			% This is another case of early actor, with a 2-diasca offset:
			UpdatedDiascaActors = ?list_impl:add_element( ActorToSchedule,
														  FutureDiascaActors ),

			% We now know what is the next timestamp (hopefully):
			NextTimestamp = { OtherTickOffset, 1 },

			setAttributes( State, [

					{ actors_to_trigger_in_two_diascas, UpdatedDiascaActors },
					{ next_timestamp, NextTimestamp }

								] )

	end,


	% Note: the sender of the actor message is expected to have notified its own
	% time manager (the only one which by design cannot have finished its
	% diasca) that the next diasca should be scheduled.

	% The purpose is not to return a specific result, but to ensure the actor
	% that received the actor message is blocked, itself blocking the calling
	% actor, so that no race condition can happen:
	%
	?wooper_return_state_result( RecordedState, trigger_planned ).




% Called by an actor managed by this time manager whenever it is nudged
% (asynchronously) *and* when the answer from that actor came after the
% time-out: in this case the hijacked WOOPER loop at the level of
% onSimulationStallDetected/1 has already given up waiting for this answer, and
% this method is called instead.
%
% (oneway)
%
-spec notifyNudged( wooper_state(), pid(), tick_offset(), [pid()] ) ->
						  oneway_return().
notifyNudged( State, _NudgedActorPid, _TickOffset, _WaitedAcks ) ->

	% Ignored, as came too late (was meant to be intercepted before):
	?wooper_return_state_only( State ).




% Section about the management of abnormal situations.


% Overriding the WOOPER default EXIT handler, not interested in 'normal' EXIT
% messages.
%
% (oneway)
%
-spec onWooperExitReceived( wooper_state(), pid(), basic_utils:exit_reason() )
						  -> oneway_return().
onWooperExitReceived( State, _Pid , normal ) ->
	?wooper_return_state_only( State );

onWooperExitReceived( State, Pid, ExitType ) ->

	?warning_fmt( "Time manager EXIT handler ignored signal '~w' from ~w.",
		[ ExitType, Pid ] ),

	?wooper_return_state_only( State ).




% Section about simulation milestones.



% This oneway is triggered periodically, in order to be able to perform
% operations like house-keeping on a regular basis (wallclock-wise).
%
% The duration, in milliseconds, is measured from the simulator start-up.
%
% Defined in order to be enriched at will.
%
% Note: each time manager (root or not) will call this method on its own, but it
% will triggered by the root one, to avoid non-synchronised waitings).
%
% (oneway)
%
-spec onWallclockMilestone( wooper_state(), unit_utils:milliseconds() ) ->
								  oneway_return().
onWallclockMilestone( State, CurrentMillisecond ) ->

	% First notifies ASAP the direct child managers:
	MilestoneMessage = { onWallclockMilestone, CurrentMillisecond },

	basic_utils:send_to_pid_list_impl( MilestoneMessage,
									  ?getAttr(child_managers) ),

	% Only the root manager will display it:
	case is_root_manager( State ) of

		true ->
			?info_fmt( "Wall-clock milestone triggered after "
					   "an elapsed duration of ~s; "
					   "current wall-clock time is ~s.~n",
					   [ text_utils:duration_to_string( CurrentMillisecond ),
						basic_utils:get_textual_timestamp() ] );

		false ->
			ok

	end,

	io:format( "Wall-clock milestone triggered on ~p (~p) "
			  "after an elapsed duration of ~s; "
			  "current wall-clock time is ~s.~n",
			  [ self(), node(),
			   text_utils:duration_to_string( CurrentMillisecond ),
			   basic_utils:get_textual_timestamp() ] ),

	class_PluginManager:notify( _Event=on_simulation_wallclock_milestone_met,
								_Parameters=CurrentMillisecond ),

	% Then propagates the milestone to the registered local processes:
	[ A ! MilestoneMessage || A <- ?getAttr(wallclock_milestone_listeners) ],


	CleanedState = perform_house_keeping( State ),

	?wooper_return_state_only( CleanedState ).



% This oneway is triggered one simulation tick out of N, in order to be able to
% perform operations like house-keeping on a regular basis (virtual-time wise).
%
% Defined in order to be enriched at will.
%
% Note: each time manager (root or not) will call this method on its own, with
% any specific synchronisation (as soon as they have finished the milestone
% tick), unlike wallclock-milestones.
%
% (oneway)
%
-spec onTickMilestone( wooper_state(), tick_offset() ) -> oneway_return().
onTickMilestone( State, TickOffset ) ->

	% Only the root manager will display it:
	case is_root_manager( State ) of

		true ->
			?info_fmt( "Simulation-time milestone triggered at tick offset #~B,"
					   " while current wall-clock time is ~s.~n",
					   [ TickOffset, basic_utils:get_textual_timestamp() ] );

		false ->
			ok

	end,

	class_PluginManager:notify( _Event=on_simulation_tick_milestone_met,
								_Parameters=TickOffset ),

	%io:format( "Simulation-time milestone triggered on ~p (~p) "
	%		  "at tick offset #~B, while current wall-clock time "
	%		  "is ~s.~n",
	%		  [ self(), node(), TickOffset,
	%		   basic_utils:get_textual_timestamp() ] ),

	% Propagates the milestone to relevant simulation agents:
	[ A ! { onTickMilestone, TickOffset }
	 || A <- ?getAttr(tick_milestone_listeners) ],

	CleanedState = perform_house_keeping( State ),

	?wooper_return_state_only( CleanedState ).



% Oneway called by the watchdog whenever it deems that the simulation is
% stalled.
%
% Only called for the root time manager, the only one using a watchdog.
%
% The upward propagation of a simulation stall does not need to be specifically
% managed, as the parent will have to wait for its child managers anyway.
%
% (const oneway)
%
-spec onSimulationStallDetected( wooper_state() ) -> oneway_return().
onSimulationStallDetected( State ) ->

	WaitedChildManagers = ?getAttr(waited_child_managers),
	WaitedChildManagerCount = ?list_impl:size( WaitedChildManagers ),

	% See whether the cause is local (a blocked actor) and/or indirect
	% (a blocked child manager):
	ChildManagerMessage = case WaitedChildManagerCount of

		0 ->
			"not waiting for any child time manager";

		ChildManagerCount when ChildManagerCount > ?too_many_entries ->
			io_lib:format( "still waiting for ~B child time managers",
						   [ ChildManagerCount ] );

		ChildManagerCount ->

			ManagerDescriptions = [
				io_lib:format( "~w (on node ~s)", [ M, node(M) ] )
					  || M <- ?list_impl:to_list( WaitedChildManagers ) ],

			io_lib:format(
				"still waiting for following ~B child time manager(s): ~s",
				[ ChildManagerCount,
				  text_utils:join( _Sep=", ",  ManagerDescriptions ) ] )

	end,

	SpontaneousActors = ?getAttr(waited_spontaneous_actors),
	SpontaneousActorCount = ?list_impl:size( SpontaneousActors ),
	SpontaneousMessage = case SpontaneousActorCount of

		0 ->
			"not waiting for any actor spontaneously scheduled";

		SpontaneousCount when SpontaneousCount > ?too_many_entries ->
			io_lib:format(
				"still waiting for ~B actors spontaneously scheduled",
				[ SpontaneousCount ] );

		SpontaneousCount ->
			io_lib:format( "still waiting for following ~B actors "
				"spontaneously scheduled: ~p",
				[ SpontaneousCount, ?list_impl:to_list(SpontaneousActors) ] )

	end,

	TriggeredActors = ?getAttr(waited_triggered_actors),
	TriggeredActorCount = ?list_impl:size( TriggeredActors ),
	TriggeredMessage = case TriggeredActorCount of

		0 ->
			"not waiting for any triggered actor";

		TriggeredCount when TriggeredCount > ?too_many_entries ->
			io_lib:format(
				"still waiting for ~B triggered actors",
				[ TriggeredCount ] );

		TriggeredCount ->
			io_lib:format( "still waiting for following ~B "
				"triggered actors: ~p",
				[ TriggeredCount, ?list_impl:to_list(TriggeredActors) ] )

	end,


	CurrentTickOffset = ?getAttr(current_tick_offset),

	WaitedCount = ?getAttr(waited_count),

	% Even the watchdog can be waited for (yes, this happens):
	WatchdogMessage = case ?getAttr(watchdog_waited) of

		false ->
			"not waiting for watchdog";

		true ->
			"still waiting for watchdog"

	end,

	Message = case WaitedCount of

		undefined ->
			"Simulation currently unable to start, "
			"or unusually long to do so..." ;

		_Defined ->
			io_lib:format( "Simulation currently stalled at tick offset #~B "
						   "(being at diasca ~p) for time manager ~w, "
						   "still waiting for a total of ~B notification(s) "
						   "of end of diasca: ~s",
			  [ CurrentTickOffset, ?getAttr(current_diasca), self(),
			   WaitedCount, text_utils:string_list_to_string(
					[ ChildManagerMessage, SpontaneousMessage, TriggeredMessage,
					WatchdogMessage ] ) ] )

	end,

	FinalState = case WaitedCount of

		undefined ->

			?error( Message ),
			io:format( "~n### " ++ Message ++ "~n" ),
			State;

		WaitedChildManagerCount ->
			% Only stalled because of children, stay rather mute:
			?warning( Message ),
			% Maybe to disable:
			io:format( "~n### Overall non-local stall detected "
					  "at tick offset #~B.~n", [ CurrentTickOffset ] ),
			State;

		_Other ->

			% We are locally stalled (and a root time manager):
			{ DiagState, Diag } = executeRequest( State, getProgressDiagnosis ),

			FormattedDiag = format_nested_diagnoses( Diag ),

			ActorExplanation = io_lib:format( text_utils:join( "~n",
				[ "~nThe diagnosis about the simulation stall is: "
					| FormattedDiag ] ) ++ "~n", [] ),

			FullMessage = Message ++ ActorExplanation,

			?error( FullMessage ),

			io:format( "~n### " ++ FullMessage ++ "~n" ),
			DiagState

	end,

	?wooper_return_state_only( FinalState ).



% Returns a four-element tuple made of:
%
% - the PID of this time manager (for its parent to discriminate between the
% answers of its children)
%
% - the name of this node (as an atom)
%
% - a diagnosis about the local actors that are waited (as a list of binaries),
% or, if none, the 'none_waited' atom
%
% - a list of nested similar recursively-determined progress diagnoses, for the
% (direct) child managers
%
% That way we go through all the scheduling hierarchy.
%
% To be called on any time manager (either root or child, at any depth).
%
% (const request)
%
-spec getProgressDiagnosis( wooper_state() ) -> request_return(
		{ pid(), net_utils:atom_node_name(), diagnosis(), [ diagnosis() ] } ).
getProgressDiagnosis( State ) ->

	% First, triggers a parallel recursive progress request:
	Children = ?getAttr(child_managers),

	basic_utils:send_to_pid_list_impl( { getProgressDiagnosis, [], self() },
									   Children ),

	LocalDiag = get_local_diagnosis( State ),

	ChildDiags = wait_for_diagnoses( Children, _Diagnoses=[] ),

	Res = { self(), net_utils:localnode(), LocalDiag, ChildDiags },

	?wooper_return_state_result( State, Res ).



% Declares that a data exchange service will be used, and provides the PID of
% its root data exchanger, so that synchronisation of data updates with regard
% to tick can be done.
%
% The root data-exchanger becomes then a simulation listener.
%
% (oneway)
%
-spec declareDataExchanger( wooper_state(), pid() ) -> oneway_return().
declareDataExchanger( State, RootDataExchangerPid ) ->

	% Checking:
	undefined = ?getAttr(root_data_exchanger_pid),

	Listeners = ?getAttr(simulation_listeners),

	?wooper_return_state_only( setAttributes( State, [

			{ simulation_listeners, [ RootDataExchangerPid | Listeners ] },
			{ root_data_exchanger_pid, RootDataExchangerPid }

													 ] ) ).



% Requests this (root) time manager to notify the caller (e.g. the root data
% exchanger) when this tick will be over, so that operations (e.g. commit
% propagation) can be done.
%
% Note: the next overall tick does not need to be scheduled, information will
% just be ready for any next tick to come.
%
% (request, for synchronisation purposes)
%
-spec requestInterDiascaNotification( wooper_state() ) ->
			 request_return( 'interdiasca_tracked' ).
requestInterDiascaNotification( State ) ->

	% Checking:
	undefined = ?getAttr(parent_manager_pid),
	[] = ?getAttr(interdiasca_listeners),

	?wooper_return_state_result(
		appendToAttribute( State, interdiasca_listeners, ?getSender() ),
		interdiasca_tracked ).




% Helper section.



% Tells whether this time manager is the root one.
%
% (helper)
%
-spec is_root_manager( wooper_state() ) -> boolean().
is_root_manager( State ) ->

	case ?getAttr(parent_manager_pid) of

		undefined ->
			true;

		_ ->
			false

	end.



% Displays all relevant timing information, when the simulation is over.
%
% (helper)
%
-spec display_timing_information( string(), wooper_state() ) ->
										basic_utils:void().
display_timing_information( Timings, State ) ->

	ElapsedTicks = get_current_tick( State ) - ?getAttr(initial_tick),

	SimDuration = convert_ticks_to_milliseconds( ElapsedTicks, State ),

	SimDurationString = text_utils:duration_to_string( SimDuration ),

	StopTimestamp = basic_utils:get_precise_timestamp(),

	RealDuration = basic_utils:get_precise_duration(
						 ?getAttr(initial_timestamp), StopTimestamp ),

	RealDurationString = text_utils:duration_to_string( RealDuration ),

	io:format( "Simulation terminated successfully at ~s, after a duration"
			   " of ~s in simulation time (~B ticks),"
			   " computed during a wall-clock duration of ~s.~n",
			   [ Timings, SimDurationString, ElapsedTicks,
				 RealDurationString ] ),

	case RealDuration of

		0 ->
			io:format( "Simulation did not last long enough to "
					  "define a clock factor.~n" );

		_ ->

			AccFactor = SimDuration / RealDuration,

			case SimDuration > RealDuration of

				true ->
					io:format( "Simulation ran faster than "
							  "the clock, with an acceleration "
							  "factor of x~.3f.~n~n", [ AccFactor ] );

				false ->
					io:format( "Simulation ran slower than "
							  "the clock, with an acceleration "
							  "factor of x~.3f.~n~n", [ AccFactor ] )

			end

	end.



% Returns a diagnosis, as a list of binaries or as the 'none_waited' atom, about
% the local actors (if any) managed by this time manager.
%
% (helper)
%
-spec get_local_diagnosis( wooper_state() ) -> diagnosis().
get_local_diagnosis( State ) ->

	WaitedActors = ?list_impl:to_list( ?getAttr(waited_spontaneous_actors) )
		++ ?list_impl:to_list( ?getAttr(waited_triggered_actors) ),

	% Never perform blocking operations with actors from the time manager,
	% otherwise deadlocks could occur (ex: with scheduleTrigger/3, which can
	% happen approximately at any time).
	%
	case length( WaitedActors ) of

				0 ->
					none_waited;

				L when L < 20 ->
					get_wait_explanation( WaitedActors, L, State );

				TooLong ->
					[ text_utils:string_to_binary( io_lib:format(
					  "is still waiting for ~B actors", [ TooLong ] ) ) ]

	end.



wait_for_diagnoses( Children, Diagnoses ) ->

	case ?list_impl:is_empty( Children ) of

		true ->
			Diagnoses;

		false ->

			receive

				{ wooper_result,
				  DiagTuple={ ChildPid, _NodeName, _LocalDiag, _SubDiags } } ->

					case ?list_impl:is_member( ChildPid, Children ) of

						true ->
							NewChildren = ?list_impl:delete( ChildPid,
															 Children ),
							wait_for_diagnoses( NewChildren,
										[ DiagTuple | Diagnoses ] );

						false ->
							throw( { unexpected_diagnosis, DiagTuple } )

					end

			end

	end.





% Sends back explanations about the local actors that are still waited.
%
% Returns a list of binaries, for a more detailed view.
%
% (helper)
%
get_wait_explanation( WaitedActors, Count, State ) ->

	% They will answer back with 'notifyNudged':
	[ ActorPid ! { nudge, self() } || ActorPid <- WaitedActors ],

	% These actors being local, we already know they are managed by the local
	% instance tracker:
	LocalTrackerPid = ?getAttr(local_instance_tracker_pid),

	% Needed for non-first level waited actors (they may not be local):
	RootTrackerPid = ?getAttr(root_instance_tracker_pid),

	case length( WaitedActors ) of

		L when L > 20 ->
			io_lib:format( "is still waiting for ~B actors (overloaded?)",
						   [ L ] );

		_ ->

			Header = case Count of

						 1 ->
							 "is still waiting for following actor:";

						 Many when Many > 1 ->
							 io_lib:format( "is still waiting for "
											"following ~B actors:", [ Count ] )

			end,

			[ text_utils:string_to_binary(Header) |
			  prepare_wait_explanation( WaitedActors, LocalTrackerPid,
					RootTrackerPid, ?getAttr(current_tick_offset), _Acc=[] ) ]

	end.



prepare_wait_explanation( _WaitedActors=[], _LocalTrackerPid, _RootTrackerPid,
						 _TickOffset, Acc ) ->
	Acc;

prepare_wait_explanation( [ ActorPid | T ], LocalTrackerPid, RootTrackerPid,
						 TickOffset, Acc ) ->

	ActorNaming = get_best_naming_for( ActorPid, LocalTrackerPid, local ),

	ActorInfo = receive

		% The next 'notifyNudged' message was sent by an actor in answer to the
		% 'nudge' one sent in get_wait_explanation/3.
		%
		% Only select what we target, other notifyNudged messages not popped at
		% the end will result in a call to that oneway, which will do nothing:
		%
		{ notifyNudged, [ ActorPid, TickOffset, WaitedActors ] } ->

			case length( WaitedActors ) of

				L when L > 5 ->
					io_lib:format( "~s, which itself is waiting for "
								   "~B other actors", [ ActorNaming, L ] );

				0 ->
					io_lib:format( "~s, not waiting for anyone (still busy?)",
						   [ ActorNaming ] );

				_ ->
					% The next actors are not necessarily local.
					%
					% Here we could either send a request the root tracker (and
					% let it ask to the right local tracker), or find ActorNode
					% = node( Pid ), and then make a local name look-up on this
					% node for the (local) tracker.
					%
					% We preferred here the first solution:
					%
					ActorNamings = [
						get_best_naming_for( Pid, RootTrackerPid, global )
							|| Pid <- WaitedActors ],

					ListedActors = text_utils:join( _Sep=", and for ",
													ActorNamings ),

					io_lib:format( "~s, which itself is waiting for ~s",
						   [ ActorNaming, ListedActors ] )

			end

	after 100 ->

			io_lib:format( "~s did not answer on time (busy?)",
						  [ ActorNaming ] )

	end,

	ActorInfoBin = text_utils:string_to_binary( ActorInfo ),

	prepare_wait_explanation( T, LocalTrackerPid, RootTrackerPid, TickOffset,
							 [ ActorInfoBin | Acc ] ).



% Returns the best textual description about specified actor, supposing
% TrackerPid is the PID of the tracker which is in charge of this actor.
%
% LookUpType is either 'local' (we know that the target instance tracker should
% be able to answer) or 'global' (it may have to perform a global look-up in its
% hierarchy - generally to be used on the root instance tracker).
%
% (helper)
%
-spec get_best_naming_for( pid(), pid(), 'local' | 'global' ) -> string().
get_best_naming_for( ActorPid, TrackerPid, LookUpType ) ->

	Request = case LookUpType of

			local ->
				getActorInformationLocal;

			global ->
				getActorInformationGlobal

	end,

	TrackerPid ! { Request, ActorPid, self() },

	receive

		{ wooper_result, { Infos, Node } }
		  when is_record( Infos, actor_info ) ->

			AAI = Infos#actor_info.aai,

			ActorName = case Infos#actor_info.name of

				undefined ->

					% Sends a oneway to this actor, which will update the root
					% instance tracker later.
					%
					% (note: we do not specify self() here, we want the instance
					% tracking service to be updated instead; as we do not know
					% which tracker shall be targeted, we specify the root one
					% instead):
					%
					ActorPid ! { triggerNameNotification, TrackerPid },

					% For this time, fall-back to a basic information:
					io_lib:format( "actor whose AAI is ~B (PID: ~w)",
								  [ AAI, ActorPid ] );

				Name ->
					% Already available, use it:
					io_lib:format( "actor named '~s' whose AAI is ~B (PID: ~w)",
								  [ Name, AAI, ActorPid ] )

			end,

			% These information are known to exist:
			ActorName ++ io_lib:format( " of class ~s on node ~s",
					[ Infos#actor_info.classname, Node ] )

	end.





% Returns a textual representation of the nested diagnoses: a list of plain
% strings.
%
format_nested_diagnoses( Diag ) ->
	% One-element list, to start:
	format_nested_diagnoses( [ Diag ], _Acc=[], _Level=0 ).


format_nested_diagnoses( _Diag=[], Acc, _Level ) ->
	Acc;

format_nested_diagnoses( _Diag=[ {TMPid,Node,LocalDiags,ChildDiags} | T ],
						Acc, Level ) ->

	TMText = get_prefix_for( Level ) ++
		io_lib:format( "Time manager on ~s (~w):", [ Node, TMPid ] ),

	PrefixBullet = get_prefix_for( Level+1 ),

	LocalTextList = case LocalDiags of

					none_waited ->
						[ PrefixBullet ++ "has no waited local actor" ];

					[ Header | ActorList ] ->
						PrefixSecondBullet = get_prefix_for( Level+2 ),
						TranslatedBullets = [ PrefixSecondBullet
						  ++ text_utils:binary_to_string(B) || B <- ActorList ],
						[ PrefixBullet ++ text_utils:binary_to_string( Header )
						 | TranslatedBullets ]

	end,

	StartTextList = [ TMText | LocalTextList ],
	ChildTextList = case ChildDiags of

					[] ->
						 [ PrefixBullet ++ "has no child time manager" ];

					DiagList ->
						% Recurses (depth-first)
						[ PrefixBullet ++ "has for child managers: " |
							format_nested_diagnoses( DiagList, Acc, Level+3 ) ]

	end,

	% Continues on this "top-level" list:
	format_nested_diagnoses( T,  Acc ++ StartTextList ++ ChildTextList, Level ).



% Returns a suitable indentation prefix for specific nesting level, as a plain
% string.
get_prefix_for( _Level=0 ) ->
	" - ";

get_prefix_for( _Level=1 ) ->
	"   * ";

get_prefix_for( _Level=2 ) ->
	"     + ";

get_prefix_for( _Level=3 ) ->
	"       # ";

get_prefix_for( _Level=4 ) ->
	"         ~~ ";

get_prefix_for( _Level=5 ) ->
	"           + ";

get_prefix_for( Level ) ->
	lists:flatten( lists:duplicate( Level*2, " " ) ) ++ " - ".




% Generic interface.



% Called by the load balancer to update the overall actor count.
%
% (oneway)
%
-spec notifyOverallActorCount( wooper_state(), basic_utils:count() ) ->
									oneway_return().
notifyOverallActorCount( State, NewValue ) ->

	?wooper_return_state_only(
		setAttribute( State, overall_actor_count, NewValue ) ).



% Displays the state in the console.
%
% (const oneway)
%
-spec display( wooper_state() ) -> oneway_return().
display( State ) ->

	wooper_display_instance( State ),

	?wooper_return_state_only( State ).



% Returns a textual description of this manager.
%
% (const request)
%
-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, wooper_state_toString(State) ).





% 'Static' methods (module functions):



% Returns a textual description of specified simulation settings record.
%
% (static)
%
-spec settings_to_string( #simulation_settings{} ) -> string().
settings_to_string( #simulation_settings{
		simulation_name    = Name,
		tick_duration      = SubmittedTickDuration,
		interactivity_mode = Interactivity,
		evaluation_mode    = EvaluationMode }  ) ->

	NameString = io_lib:format( "simulation name is '~s'.", [ Name ] ),

	TickDuration = float( SubmittedTickDuration ),

	Frequency = 1 / TickDuration,

	DurationString = io_lib:format( "simulation tick duration is exactly "
		"~f (virtual) seconds, which corresponds to a fundamental frequency of "
		"approximately ~.3f Hz.", [ TickDuration, Frequency ] ),

	InteractivityString = io_lib:format( "simulation will run in ~s mode.",
		[ Interactivity ] ),

	EvaluationModeString = case EvaluationMode of

		fastest->
			"evaluation will be done in fastest mode, "
			"with no message reordering, and using default seed." ;

		reproducible ->
			"evaluation will be totally reproducible, using default seed." ;

		{ reproducible, Seed } ->
			io_lib:format( "evaluation will be totally reproducible, "
						   "using user-specified seed ~p.", [ Seed ] ) ;

		ergodic ->
			"evaluation will be done in ergodic mode."

	end,

	text_utils:string_list_to_string( [ NameString, DurationString,
		InteractivityString, EvaluationModeString ] ).



% Returns the atom corresponding to the name this time manager should be
% registered as.
%
% Note: executed on the caller node.
%
% (static)
%
-spec get_registration_name() -> basic_utils:registration_name().
get_registration_name() ->

	% We used to prefer using unique names (even if they actually remained,
	% each, node-local).
	%
	% Ex: sim_diasca_time_manager_for_testSimulation@myhost.mydomain.org
	%list_to_atom( atom_to_list( ?time_manager_name ) ++ "_for_"
	%	++ atom_to_list( node() ) ).

	% Simpler and sufficient in a purely local context:
	?time_manager_name.



% Returns the PID of the current time manager if it exists, otherwise the
% 'time_manager_not_available' atom.
%
% Waits a bit before giving up: useful when client and manager processes are
% launched almost simultaneously.
%
% (static)
%
-spec get_any_manager() -> pid() | 'time_manager_not_available'.
get_any_manager() ->

	% Waits gracefully for the time manager to exist:
	try basic_utils:wait_for_global_registration_of( get_registration_name() )
			of

		TimeManagerPid ->
			TimeManagerPid

	catch

		_Exception ->
			time_manager_not_available

	end.





% Section for helper functions (not methods).


% Checks that specified simulation duration is correct, and returns a
% standardized version of it.
%
check_tick_duration( SpecifiedSimulationDuration )
  when is_integer(SpecifiedSimulationDuration) ->
	check_tick_duration( erlang:float( SpecifiedSimulationDuration ) );

check_tick_duration( D ) when is_float(D) andalso D > 0 ->
	D;

check_tick_duration( D ) ->
	throw( { invalid_specified_tick_duration, D } ).




% Updates the recorded next action for this manager, depending on incoming new
% information from a child manager.
%
% Returns an updated state.
%
% (helper)
%
-spec update_next_action_with( next_manager_action(), wooper_state() )
									 -> wooper_state().
update_next_action_with( no_planned_action, State ) ->
	% No change here, already set to the same value:
	State;

update_next_action_with( new_diasca_needed, State ) ->
	% Nothing sooner can exist, overriding blindly:
	setAttribute( State, next_action, new_diasca_needed );

update_next_action_with( NextTickOffset, State ) ->

	SoonestDeadline = case ?getAttr(next_action) of

		new_diasca_needed ->
			new_diasca_needed;

		undefined ->
			NextTickOffset;

		FirstNextOffset when FirstNextOffset > NextTickOffset ->
			NextTickOffset;

		SmallerOffset ->
			SmallerOffset

	end,
	setAttribute( State, next_action, SoonestDeadline ).



% Internal timer functions, for interactive mode.


% Ticks at a regular pace (without maintaining any particular tick count), for
% interactive mode:
%
-spec timer_main_loop( pid(), unit_utils:milliseconds() ) -> no_return().
timer_main_loop( TimeManagerPid, TimeOut ) ->

	receive

		delete ->
			%io:format( "Timer: timer_main_loop requested to stop.~n" ),
			TimeManagerPid ! timer_stopped
			% Ended.

	% After following real milliseconds, sends the timer top, and recurses:
	after TimeOut ->

		% No diasca shall be considered here:
		TimeManagerPid ! timerTickFinished,

		timer_main_loop( TimeManagerPid, TimeOut )

	end.



% Manages the stopping of the timer.
%
% Returns an updated state.
%
-spec stop_timer( wooper_state() ) -> wooper_state().
stop_timer( State ) ->

	UpdatedState = case ?getAttr(interactivity_mode) of

		interactive ->

			% If interactive, the timer has to be stopped:
			case ?getAttr(timer_pid) of

				undefined ->
					% Nothing to stop:
					State;

				TimerPid ->

					?debug( "Waiting for the simulation timer to stop." ),
					TimerPid ! delete,

					receive

						timer_stopped ->
							?debug( "Timer stopped." ),
							setAttribute( State, timer_pid, undefined )

					end

			end;


		batch ->
			% In batch mode, no timer to stop:
			%
			% (note that a self-sent timer_top message might be still sitting in
			% the TimeManager mail box)
			%
			State


	end,

	% Flushes any remaining timer-top oneway calls/messages (Y-combinator):
	F = fun( Fun ) ->
		receive

			timerTickFinished ->
				Fun( Fun )

		after 0 ->

			ok

		end

	end,

	F( F ),

	UpdatedState.



% Ensures that watchdog-originating end of tick/diasca notifications are sent
% regularly, otherwise requests the specified time manager to display some
% information explaining the possible causes of the simulation stall.
%
% Note: apparently, loosing network connectivity (ex: putting the local network
% interface down in the course of the simulation) does not lead to having a
% non-suspended watchdog ever kicking in (stale VM?).
%
% This would just result into a 'noconnection' exception, quite later.
%
-spec watchdog_main_loop( pid(), unit_utils:milliseconds(),
	   unit_utils:milliseconds(), unit_utils:milliseconds() ) -> no_return().
watchdog_main_loop( TimeManagerPid, Period, AccumulatedDuration, MaxIdle ) ->

	%io:format( "Watchdog ~w running (period: ~B ms, accumulated: ~B ms).~n",
	%		  [ self(), Period, AccumulatedDuration ] ),

	receive


		{ beginWatchdogTick, _NewTickOffset=0 } ->

			%io:format( "Watchdog received a notification of tick begin "
			%   "for tick offset #0.~n" ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifySpontaneousWatchdogCompleted, 0 },

			% We start with an initial additional margin, so that all the
			% initial actors, even if they are are quite numerous, can
			% nevertheless complete their first diasca (to overcomed the
			% typical initial burst load):
			%
			% (unit: milliseconds)
			%
			CreationDurationMargin = 10 * get_maximum_idle_duration(),

			% Resets accordingly the idle timer:
			watchdog_main_loop( TimeManagerPid, Period,
					_AccumulatedDuration= - CreationDurationMargin, MaxIdle );


		{ beginWatchdogTick, NewTickOffset } ->

			%io:format( "Watchdog received a notification of tick begin "
			%   "for tick offset #~B.~n", [ NewTickOffset ] ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifySpontaneousWatchdogCompleted,
							  NewTickOffset },

			% Resets the idle timer:
			watchdog_main_loop( TimeManagerPid, Period,
							   _AccumulatedDuration=0, MaxIdle );


		{ beginWatchdogDiasca, [ TickOffset, NewDiasca ] } ->

			%io:format( "Watchdog received a notification of diasca begin "
			%  "for tick offset #~B diasca ~B.~n",
			%  [ TickOffset, NewDiasca ] ),

			% Acknowledges it immediately:
			TimeManagerPid ! { notifyTriggeredWatchdogCompleted,
							  [ TickOffset, NewDiasca ] },

			% Resets the idle timer:
			watchdog_main_loop( TimeManagerPid, Period,
							   _AccumulatedDuration=0, MaxIdle );


		% Useful, for example to let a serialisation occur:
		suspendWatchdog ->

			%io:format( "Watchdog suspended.~n" ),

			?notify_debug( "Watchdog suspended." ),

			receive

				resumeWatchdog ->
					%io:format( "Watchdog resumed.~n" ),
					?notify_debug( "Watchdog resumed." ),
					watchdog_main_loop( TimeManagerPid, Period,
									   _AccumulatedDuration=0, MaxIdle )

			end;


		timeManagerShutdown  ->
			%io:format( "Watchdog deleted.~n" ),
			?notify_debug( "Watchdog removed (shutdown)." );


		delete ->
			%io:format( "Watchdog deleted.~n" ),
			?notify_debug( "Watchdog removed (deletion)." );


		{ synchronous_delete, ListenerPid } ->
			%io:format( "Watchdog synchronously deleted.~n" ),
			?notify_debug( "Watchdog removed (synchronous deletion)." ),
			ListenerPid ! watchdog_deleted;


		Other ->

			?notify_warning_fmt(
				"Watchdog received an unexpected message (~p), ignored.",
				[ Other ] ),

			watchdog_main_loop( TimeManagerPid, Period, AccumulatedDuration,
							MaxIdle )


	% After following real (actual) milliseconds:
	after Period ->

		case AccumulatedDuration of

			% Deemed to be an orphaned node (thus needing to halt automatically)
			% if more than MaxIdle milliseconds elapsed since last known event:

			TooLong when TooLong > MaxIdle ->

				io:format( "Warning: watchdog on node '~s' detected "
						   "a too long idle duration (more than ~s), "
						   "this node is thus deemed orphaned, "
						   "shutting it down now at ~s.~n",
						   [ net_utils:localnode(),
							 text_utils:duration_to_string( MaxIdle ),
							 basic_utils:get_textual_timestamp() ] ),

				erlang:halt( 95 );


			_OtherDuration ->

				% Output disabled, as anyway the onSimulationStallDetected
				% method will display a message both on the console and in the
				% traces:
				io:format( " ### Watchdog detected a stalled simulation "
						   "at ~s, see simulation traces for diagnosis.~n",
						   [ basic_utils:get_textual_timestamp() ] ),

				TimeManagerPid ! onSimulationStallDetected,
				watchdog_main_loop( TimeManagerPid, Period,
						  AccumulatedDuration + Period, MaxIdle )

		end

	end.



% Manages the stopping of the watchdog.
%
% Returns an updated state.
%
-spec stop_watchdog( wooper_state() ) -> wooper_state().
stop_watchdog( State ) ->

	case ?getAttr(watchdog_pid) of

		undefined ->
			State;

		WatchdogPid ->

			WatchdogPid ! { synchronous_delete, self() },

			% Otherwise the watchdog could send 'done' messages after the next
			% flushing:
			receive

				watchdog_deleted ->
					ok

			end,

			?debug_fmt( "Flushing then any notification(s) "
				"sent by the watchdog (~w) in-between.", [ WatchdogPid ] ),

			% Flushes any remaining messages (Y-combinator):
			F = fun( Fun ) ->

				receive

					{ notifySpontaneousWatchdogCompleted, _TickOffset } ->
						Fun( Fun );

					{ watchdogDiascaFinished, _TickOffset, _Diasca } ->
						Fun( Fun )

				after 0 ->

					ok

				end

			end,

			F( F ),

			setAttribute( State, watchdog_pid, undefined )

	end.



% The wallclock tracker allows to monitor the simulation progress over real
% time, and to trigger wallclock milestones.
%
% One such tracker is used for a whole simulation (ex: not one per node), and is
% connected to the root time manager.
%
% Period is in wall-clock milliseconds.
%
-spec wallclock_tracker_main_loop( pid(), unit_utils:milliseconds(),
			unit_utils:milliseconds() ) -> basic_utils:void().
wallclock_tracker_main_loop( RootTimemanagerPid, Period, TotalDuration ) ->

	receive

		delete ->
			ok

	after Period ->

		NewTotalDuration = TotalDuration + Period,

		RootTimemanagerPid ! { onWallclockMilestone, NewTotalDuration },

		wallclock_tracker_main_loop( RootTimemanagerPid, Period,
									NewTotalDuration )

	end.



% The time tracker allows to output on the console the current time schedule
% information regularly, no more than once per second.
%
% This prevents very fast simulations from being slowed down by the mere console
% output of tick progress (flow control).
%
% Parameters are:
%
% - LatestDisplayedSecond corresponds to the latest second during which an
% output was performed (on the console)
%
% - LatestMeasuredSecond corresponds to the latest measured second, as reported
% by the root time manager
%
% - LatestTopInfo is {Timings,Counts}, as specified by the root time manager,
% i.e. Timings={ SimDateString, SimTimeString, CurrentTickOffset, CurrentDiasca,
% RealDateString, RealTimeString } and Counts={ TotalActorCount,
% TotalScheduleCount, TotalProcessCount }
%
% Note: the tick tracker output (i.e. mainly the tick table) can be displayed so
% that it respects the RST syntax (see 'Uncomment for RST output').
%
% Therefore a PDF file can be generated from it, for example thanks to the
% generate-pdf-from-rst.sh script.
%
-spec time_tracker_main_loop( unit_utils:seconds() | 'undefined',
							  unit_utils:seconds() | 'not_undefined',
							 { timing_info(), count_info() },
							 pid(),
							 pid() ) -> no_return().
time_tracker_main_loop( LatestDisplayedSecond, LatestMeasuredSecond,
						LatestTopInfo={ Timings, Counts }, LoadBalancerPid,
						RootTimeManagerPid ) ->

	% Most of the time-tracking messages are dropped (roughly one per second is
	% kept), but messages nevertheless arrive mostly already (expensively)
	% pre-formatted.
	%
	% This is not necessarily a waste of resources, as these formattings and
	% conversions are anyway needed by the traces at each tick and diasca, see
	% record_progress_message/4.
	%
	receive

		{ delete, CallerPid } ->

			% Not recursing anymore, final values before tear-down:
			display_console_line( Timings, Counts ),

			% Comment for RST output:
			display_top_row(),

			io:format( "~n" ),

			%io:format( "(tick tracker stopped)~n" ),
			CallerPid ! { stopped, self() };


		{ NewMeasuredSecond, NewLatestTopInfo } ->
			% We eat all progress messages ASAP, we want to pick latest
			% information:
			%
			% (uncomment next line to see all progress lines on the console, in
			% that case comment the one of the clause above):
			%
			%display_console_line( Timings, Counts ),
			time_tracker_main_loop( LatestDisplayedSecond, NewMeasuredSecond,
						 NewLatestTopInfo, LoadBalancerPid, RootTimeManagerPid )

	after 0 ->

			% We exhausted the timestamped messages, so the function parameters
			% now correspond to the most up-to-date information:

			% (lazy asynchronous update of the manager is programmed)
			%
			LoadBalancerPid ! { getOverallInstanceCount, RootTimeManagerPid },

			% We have eaten them all, so LatestTopInfo is indeed the latest:

			case LatestMeasuredSecond of

				LatestDisplayedSecond ->

					% Time not yet elapsed, recursing with the previous latest
					% timestamp:

					%io:format( "(still in second ~B).~n", [ LatestSecond ] ),
					timer:sleep( 200 ),
					time_tracker_main_loop( LatestDisplayedSecond,
									   LatestMeasuredSecond, LatestTopInfo,
									   LoadBalancerPid, RootTimeManagerPid );

				NewSecond ->
					%io:format( "latest = ~w, current = ~w.~n",
					%		  [ LatestDisplayedSecond, NewSecond ] ),
					display_console_line( Timings, Counts ),
					timer:sleep( 200 ),
					time_tracker_main_loop( NewSecond, NewSecond, LatestTopInfo,
										   LoadBalancerPid, RootTimeManagerPid )

			end

	end.



% Displays the bar, above and below the per-tick lines, so that the whole looks
% like an array.
%
display_top_row() ->
	io:format( "+----------------------+----------------+--------+"
			   "----------------------+--------------+--------------+"
			   "----------------+~n" ).


% Displays a double bar, so that the whole looks like the header of an array.
%
display_top_row_heavy() ->
	io:format( "+======================+================+========+"
			   "======================+==============+==============+"
			   "================+~n" ).



% Displays the final progress line on the console.
%
% We defer as much as possible the string construction as very messages are
% actually displayed, hence needed.
%
display_console_line( _Timings={ SimDateString, SimTimeString,
		   CurrentTickOffset, CurrentDiasca, RealDateString, RealTimeString },
					  _Counts={ TotalActorCount, TotalScheduleCount,
							 TotalProcessCount } ) ->

	% Ex: '|S:       (not started)|T:             0|D:     0|
	%  R:   3/9/2012 17:47:39|A:           0|S:           0|P:           34 |

	% Room for 999999999999 actors, i.e. 10^12 actors!

	TimestampString = io_lib:format(
		   "|S: ~10.s ~8.s|T: ~13.B|D: ~5.B|R: ~10.s ~8.s|",
		   [ SimDateString, SimTimeString, CurrentTickOffset, CurrentDiasca,
			RealDateString, RealTimeString ] ),

	% We have to compensate for the load-balancer (if any), which is an actor
	% too:
	%
	ActualActorCount = case TotalActorCount of

						   0 ->
							   0;

						   StrictlyPositive ->
							   StrictlyPositive - 1

	end,

	io:format( "~sA: ~11.B|S: ~11.B|P: ~12.B |~n", [ TimestampString,
					ActualActorCount, TotalScheduleCount, TotalProcessCount ] ).

	% Uncomment for RST output: display_top_row().



% Manages the stopping of the wall-clock tracker.
%
% Returns an updated state.
%
% (helper)
%
-spec stop_wallclock_tracker( wooper_state() ) -> wooper_state().
stop_wallclock_tracker( State ) ->

	% Only the root manager has such a tracker:
	case ?getAttr(wallclock_tracker_pid) of

		undefined ->
			ok;

		Pid ->
			Pid ! delete

	end,

	% Not synchronous.

	setAttribute( State, wallclock_tracker_pid, undefined ).



% Manages the stopping of the tick tracker.
%
% Returns an updated state.
%
% (helper)
%
-spec stop_time_tracker( wooper_state() ) -> wooper_state().
stop_time_tracker( State ) ->

	case ?getAttr(time_tracker_pid) of

		undefined ->
			State;

		TrackerPid ->

			% Blocking to avoid having its last message be displayed after the
			% "simulation stopped" one:
			TrackerPid ! { delete, self() },

			receive

				{ stopped, TrackerPid } ->
					ok

			end,

			setAttribute( State, time_tracker_pid, undefined )

	end.



% Returns the current (numerical) simulation tick.
%
% Note: the time manager must be started.
%
-spec get_current_tick_offset( wooper_state() ) -> tick_offset() | 'undefined'.
get_current_tick_offset( State ) ->
	?getAttr(current_tick_offset).



% Returns the current (numerical) simulation tick.
%
% Note: the time manager must be started.
%
-spec get_current_tick( wooper_state() ) -> tick().
get_current_tick( State ) ->
	%io:format( "get_current_tick called.~n" ),
	?getAttr(initial_tick) + ?getAttr(current_tick_offset).



% Returns the full date and time of the simulation, i.e.:
% {{SimYear,SimMonth,SimDay},{SimHour,SimMinute,SimSecond,SimMicrosecond}}.
%
% (helper)
%
-spec get_simulation_time_and_date( wooper_state() ) -> basic_utils:timestamp().
get_simulation_time_and_date( State ) ->

	CurrentTick = get_current_tick( State ),

	RoundedSeconds = convert_ticks_to_rounded_seconds( CurrentTick, State ),

	calendar:gregorian_seconds_to_datetime( RoundedSeconds ).



% Converts specified timestamp (date and time, expressed in virtual time) into
% the corresponding absolute tick.
%
% (helper)
%
-spec timestamp_to_ticks( basic_utils:timestamp(), wooper_state() ) ->
								tick().
timestamp_to_ticks( Timestamp, State ) ->
	convert_seconds_to_ticks(
	  calendar:datetime_to_gregorian_seconds( Timestamp ), State ).



% Returns a textual description of the real and simulated time.
%
% (helper)
%
-spec get_textual_timings( wooper_state() ) -> string().
get_textual_timings( State ) ->

	CurrentTick = get_current_tick( State ),

	{ {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} } =
		calendar:gregorian_seconds_to_datetime(
			convert_ticks_to_rounded_seconds( CurrentTick, State ) ),

	{ {RealYear,RealMonth,RealDay}, {RealHour,RealMinute,RealSecond} } =
		{ date(), time() },

	io_lib:format( "simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), "
		"real time: ~B/~B/~B ~B:~2..0B:~2..0B",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, CurrentTick,
		RealDay, RealMonth, RealYear, RealHour, RealMinute, RealSecond ] ).




% Returns { DetailedDescription, CompactDescription, Second }, of textual
% description of the specified timing information.
%
% DetailedDescription and CompactDescription are respectively a detailed and
% compact textual description of the real and simulated time, for example to be
% used respectively in traces and on the console (through the tick tracker).
%
-spec get_full_textual_timings( tick_offset(), diasca(), wooper_state() ) ->
				{ string(), timing_info(), unit_utils:seconds() }.
get_full_textual_timings( TickOffset, Diasca, State ) ->

	Tick = ?getAttr(initial_tick) + TickOffset,

	% gregorian_seconds_to_datetime/1 is probably quite expensive:
	{ {SimYear,SimMonth,SimDay}, {SimHour,SimMinute,SimSecond} } =
		calendar:gregorian_seconds_to_datetime(
			convert_ticks_to_rounded_seconds( Tick, State ) ),

	{ RealDateString, RealTimeString, RealSecond } = format_real_time_date(),

	Detailed = io_lib:format( "simulation time: "
		"~B/~B/~B ~B:~2..0B:~2..0B (tick ~B), real time: ~s ~s",
		[ SimDay, SimMonth, SimYear, SimHour, SimMinute, SimSecond, Tick,
		 RealDateString, RealTimeString ] ),

	SimDateString = io_lib:format( "~B/~B/~B", [ SimDay, SimMonth, SimYear ] ),

	SimTimeString = io_lib:format( "~B:~2..0B:~2..0B",
							[ SimHour, SimMinute, SimSecond ] ),


	% Invests as little as possible in string formatting, as anyway only one
	% compact message ("exactly") will be displayed per second (on the console):
	%
	Compact = { SimDateString, SimTimeString, TickOffset, Diasca,
			RealDateString, RealTimeString },

	{ Detailed, Compact, RealSecond }.



% Returns a triplet describing the current wall-clock time.
%
% (helper)
%
-spec format_real_time_date() -> { string(), string(), unit_utils:seconds() }.
format_real_time_date() ->

	{ RealYear, RealMonth, RealDay } = date(),
	RealDateString = io_lib:format( "~B/~B/~B",
								   [ RealDay, RealMonth, RealYear ] ),

	{ RealHour, RealMinute, RealSecond } = time(),
	RealTimeString = io_lib:format( "~B:~2..0B:~2..0B",
							[ RealHour, RealMinute, RealSecond ] ),

	{ RealDateString, RealTimeString, RealSecond }.



% Converts the specified number of (floating-point or integer) seconds into an
% integer (rounded) number of ticks.
%
% (helper)
%
-spec convert_seconds_to_ticks( virtual_seconds() | unit_utils:seconds(),
							   wooper_state() ) ->  tick_offset().
convert_seconds_to_ticks( Seconds, State ) ->

	% Less than 2% of relative error tolerated by default:
	convert_seconds_to_ticks( Seconds, _DefaultMaxRelativeError=0.02, State ).



% Converts the specified number of (floating-point or integer) seconds into an
% integer (rounded) number of ticks, checking that any rounding error stays
% within specified maximum relative error.
%
% For example, to limit the relative error to 5%, use MaxRelativeError=0.05.
%
% (helper)
%
-spec convert_seconds_to_ticks( virtual_seconds() | unit_utils:seconds(),
			  math_utils:percent(), wooper_state() ) -> tick_offset().
convert_seconds_to_ticks( Seconds, MaxRelativeError, State )
  when Seconds >= 0 ->

	TickDuration = ?getAttr(simulation_tick_duration),

	TickCount = erlang:round( Seconds / TickDuration ),

	% Converts back to measure error:
	CorrespondingSeconds = TickCount * TickDuration,

	case math_utils:are_relatively_close( Seconds, CorrespondingSeconds,
										  MaxRelativeError ) of

		true ->
			TickCount;

		false ->
			throw( { too_inaccurate_duration_conversion, TickCount, Seconds,
					 CorrespondingSeconds, TickDuration } )

	end.



% Converts the specified tick count into a (floating-point) number of virtual
% seconds.
%
% (helper)
%
-spec convert_ticks_to_rounded_seconds( tick_offset(), wooper_state() ) ->
									unit_utils:seconds().
convert_ticks_to_rounded_seconds( Ticks, State ) ->
	round( convert_ticks_to_seconds( Ticks, State ) ).


% Converts the specified tick count into a fractional (floating-point) number of
% seconds.
%
% (helper)
%
-spec convert_ticks_to_seconds( tick_offset(), wooper_state() ) ->
									  virtual_seconds().
convert_ticks_to_seconds( Ticks, State ) ->
	Ticks * ?getAttr(simulation_tick_duration).



% Converts the specified tick count into an integer (rounded) number of
% milliseconds.
%
% Note: currently the most precise evaluation of simulated durations.
%
% (helper)
%
-spec convert_ticks_to_milliseconds( tick(), wooper_state() ) ->
										   unit_utils:milliseconds().
convert_ticks_to_milliseconds( Ticks, State ) ->
	% We want (integer) milliseconds:
	erlang:round( Ticks * ?getAttr(simulation_tick_duration) * 1000 ).




% Init helper function, used by all start methods.
%
% Note: only the root time manager is started, thus we know that this time
% manager is not a child one.
%
% Returns an updated state.
%
-spec init( wooper_state() ) -> wooper_state().
init( State ) ->

	% Only first place when we are sure that the initialisation is over:
	class_PluginManager:notify( on_case_initialisation_stop ),

	% Initial checkings:
	case ?getAttr(started) of

		false ->
			ok;

		true ->
			throw( simulation_already_started )

	end,

	?checkUndefined(wallclock_tracker_pid),
	?checkUndefined(time_tracker_pid),
	?checkUndefined(watchdog_pid),

	% We must be the root time manager:
	undefined = ?getAttr(parent_manager_pid),

	% Let's take care of the watchdog now:

	WatchdogState = launch_watchdog( State ),

	InitialTimestamp = { _FirstTickOffset=0, _FirstDiasca=0 },


	{ StartedState, { time_manager_started, _Self } } = executeRequest(
				WatchdogState, simulationStarted,
				[ ?getAttr(initial_tick), InitialTimestamp ] ),


	% stop_tick_offset left as is, might have been updated beforehand.
	PostStartedState = setAttributes( StartedState, [

		{ started, true },
		{ initial_timestamp, basic_utils:get_precise_timestamp() },
		% Useless: { next_action, no_planned_action },
		% No '{ overall_actor_count, 0 }', as actors may be created before
		% simulation start.
		{ scheduled_tracking, 0 }

													 ] ),


	StopString = case ?getAttr(stop_tick_offset) of


					 undefined ->
						 "Warning: no specific simulation duration (stop tick) "
							 "was specified.";

					 StopOffset when StopOffset =< 0 ->
						  throw( { unreachable_stop_tick_offset, StopOffset } );


					 StopOffset ->

						 AbsoluteStop = ?getAttr(initial_tick) + StopOffset,

						 Duration = convert_ticks_to_seconds( StopOffset,
												PostStartedState ),

						 % Expecting milliseconds:
						 TextDuration = text_utils:duration_to_string(
										round( Duration * 1000 ) ),

						 io_lib:format( "Simulation will stop no later than "
									   "tick ~B (i.e. after ~B ticks, which is "
									   "a duration of ~s in virtual time).",
									   [ AbsoluteStop, StopOffset,
										TextDuration ] )

	end,

	TickDuration = ?getAttr(simulation_tick_duration),

	% duration_to_string/1 expects milliseconds:
	TickDurationString = text_utils:duration_to_string(
		 erlang:round( TickDuration * 1000 ) ),

	Frequency = 1 / TickDuration,

	% We must record current_tick_offset=InitialTick-1 for the engine, but we
	% want the start outputs to show InitialTick instead:
	%
	Timings = get_textual_timings( addToAttribute( PostStartedState,
						current_tick_offset, 1 ) ),

	?info_fmt( "Simulation started at ~s with a simulation frequency "
			   "of approximately ~fHz (period of exactly ~s). ~s",
			   [ Timings, Frequency, TickDurationString, StopString ] ),


	% The load balancer is indeed a technical component, but it is an actor,
	% thus counted as such.
	%
	io:format( "Simulation started at ~s with a simulation frequency "
			   "of approximately ~fHz (period of exactly ~s). ~s~n~n"
			   "Meaning of the console tracker columns:~n"
			   " - S: overall [S]imulation time (full time and date)~n"
			   " - T: overall simulation [T]ick (virtual time)~n"
			   " - D: overall simulation [D]iasca "
			   "(in-tick causality progress)~n"
			   " - R: [R]eal (wall-clock) time~n"
			   " - A: total (distributed) [A]ctor count~n"
			   " - S: actor [S]chedulings on last diasca "
			   "(spontaneous/triggered behaviours)~n"
			   " - P: total (distributed) [P]rocess count~n~n",
			   [ Timings, Frequency, TickDurationString, StopString ] ),

	display_top_row(),

	io:format( "| Simulation Time      | Tick Offset    | Diasca"
			  " | Real Time           "
			  " | Actor Count  | Schedulings  | Process Count  |~n" ),

	display_top_row_heavy(),

	% Launching the wallclock and tick trackers:

	WallclockTrackerState = launch_wallclock_tracker( PostStartedState ),

	TimeTrackerState = launch_time_tracker( starting, WallclockTrackerState ),

	?debug( "Notifying time listeners." ),

	class_PluginManager:notify( on_simulation_start ),

	[ L ! simulation_started || L <- ?getAttr(simulation_listeners) ],

	case ?getAttr(interactivity_mode) of

		interactive ->

			case ?getAttr(timer_pid) of


				undefined ->
					launch_timer( TimeTrackerState );

				_TimerPid ->

					?warning( "Start request ignored: "
						"simulation clock already running." ),
					% State, not TimeTrackerState:
					State

			end;


		batch ->

			?notify_by_speak( "Starting simulation clock in batch mode." ),

			?notify_mute_fmt( "Starting simulation clock in batch "
				"(non-interactive mode) at ~s with a simulation frequency "
				"of ~fHz (period of exactly ~s).",
				[ get_textual_timings( TimeTrackerState ), Frequency,
					TickDurationString ] ),

			% Sends the first top:
			self() ! { beginTimeManagerTick, _CurrentTickOffset=0 },

			TimeTrackerState

	end.



% Notifies this time manager that the simulation started.
%
% It will in turn recurse in its own child managers, if any.
%
% This message must be acknowledged (it is a request), to avoid a race
% condition: otherwise if two actors were spawned at the same tick/diasca or
% both before the overall simulation start, then the first could receive its
% first top and send an actor message to the second even before it was itself
% started, thus synchronized (thus not having a current tick/diasca yet).
%
% We specify a logical timestamp, whereas currently it can be only {T=0,D=0} (as
% simulation just starts), but later we could imagine that a time manager could
% join the simulation dynamically (i.e. whereas it is already running).
%
% (request)
%
-spec simulationStarted( wooper_state(), tick(), logical_timestamp() ) ->
				request_return( { 'time_manager_started', pid() } ).
simulationStarted( State, SimulationInitialTick,
		  InitialTimestamp={ InitialTick, InitialDiasca } ) ->

	?info_fmt( "Time manager to start simulation at tick offset #~B diasca ~B, "
			   "defined relatively to its initial tick, which was ~B.",
			   [ InitialTick, InitialDiasca, SimulationInitialTick ] ),

	% Will be sent to the child managers then to all local actors:
	% (expressed as a oneway call)
	StartMessage = { simulationStarted,
					[ SimulationInitialTick, InitialTimestamp ], self() },

	?debug( "Notifying all child managers that the simulation starts." ),

	ChildManagers = ?getAttr(child_managers),

	basic_utils:send_to_pid_list_impl( StartMessage, ChildManagers ),

	InitialActors = ?getAttr(known_local_actors),

	?debug_fmt( "Notifying, based on chunks, all ~B initial actors "
				"that the simulation starts.",
				[ ?list_impl:size( InitialActors ) ] ),

	% We shall break larger lists into chunks, otherwise the select receive used
	% for the waiting of acknowlegments will be awfully long:
	%
	ActorCount = start_actors_by_chunks( InitialActors, StartMessage ),

	?debug_fmt( "All ~B actors start acknowledgements received.",
			   [ ActorCount ] ),

	% To avoid a potential race condition:

	case ?list_impl:is_empty( ChildManagers ) of

		true ->
			?debug( "No start acknowledgement from child manager "
					"to be waited." ),
			ok;

		false ->
			?debug_fmt( "Waiting for the start acknowledgements of the ~B "
					"child managers.", [ ?list_impl:size( ChildManagers ) ] ),

			% We could imagine a large number of managers as well:
			wait_for_start_acknowlegments( time_manager_started, ChildManagers )

	end,

	?debug( "At this level the simulation is now ready to start." ),

	% Should already be set here: known_local_actors, spontaneous_agenda:
	StartedState = setAttributes( State, [

		{ started, true },
		{ actors_to_trigger_in_one_diasca, ?list_impl:new() },
		{ actors_to_trigger_in_two_diascas, ?list_impl:new() },
		{ terminating_actors, [] },
		{ terminated_actors, ?list_impl:new() },
		{ waited_child_managers, ?list_impl:new() },
		{ waited_spontaneous_actors, ?list_impl:new() },
		{ waited_triggered_actors, ?list_impl:new() },
		{ initial_tick, SimulationInitialTick },

		% Next action is to begin a new tick at InitialTick, this means the
		% current tick is the one just before:

		% (otherwise checkings will fail):
		{ current_tick_offset, InitialTick-1 },

		% We also need a valid diasca:
		{ current_diasca, InitialDiasca }

	] ),

	?wooper_return_state_result( StartedState,
								{ time_manager_started, self() } ).



% Starts (synchronously) actors, based on chunks if they are to numerous.
%
% InitialActors is a ?list_impl we will iterate on, rather than transforming it
% into a plain list.
%
% We build a ?list_impl waited list, as we need random access in it, as answers
% will come unordered.
%
% Supposedly maintaining a separate count avoids many size/1 (potentiall
% expensive) computations.
%
% Returns the actor count.
%
start_actors_by_chunks( InitialActors, StartMessage ) ->

	%io:format( "start_actors_by_chunks for ~B actors.~n",
	%			[ ?list_impl:size( InitialActors ) ] ),

	Iterator = ?list_impl:iterator( InitialActors ),

	start_actors_by_chunks( ?list_impl:next( Iterator ),
		 _WaitedList=?list_impl:new(), _WaitedCount=0, StartMessage,
		 _TotalCount=0 ).



% End of actor list reached:
%
start_actors_by_chunks( _InitialActorsIterator=none, WaitedList, _WaitedCount,
					   _StartMessage, TotalCount ) ->

	wait_for_start_acknowlegments( actor_started, WaitedList ),
	TotalCount ;


% End of chunk reached (not wanting to wait for too many actors):
%
start_actors_by_chunks( InitialActorsIterator, WaitedList, _MaxWaitedCount=2000,
					   StartMessage, TotalCount ) ->

	wait_for_start_acknowlegments( actor_started, WaitedList ),

	% Then continue with the next chunk:
	start_actors_by_chunks( InitialActorsIterator,
		   _NewWaitedList=?list_impl:new(), _NewWaitedCount=0, StartMessage,
		   TotalCount ) ;


% Still in chunk:
start_actors_by_chunks( _InitialActorsIterator={ ActorPid, NewIterator },
					   WaitedList, WaitedCount, StartMessage, TotalCount ) ->

	ActorPid ! StartMessage,

	start_actors_by_chunks( ?list_impl:next( NewIterator ),
							?list_impl:add_element( ActorPid, WaitedList ),
							WaitedCount + 1,
							StartMessage,
							TotalCount + 1 ).




% This can be used for actors and for child time managers.
%
% WaitedType is either actor_started or time_manager_started.
%
wait_for_start_acknowlegments( WaitedType, WaitedList ) ->

	%io:format( "wait_for_start_acknowlegments for ~B waited processes.~n",
	%   [ ?list_impl:size( WaitedList ) ] ),

	case ?list_impl:is_empty( WaitedList ) of

		true ->
			ok;

		false ->
			receive

				{ wooper_result, { WaitedType, Pid } } ->

					DelList = list_utils:safe_listimpl_delete( Pid,
															   WaitedList ),

					wait_for_start_acknowlegments( WaitedType, DelList )

			end

	end.



% Returns the minimum of the two specified timestamps.
%
min_timestamp( _First=undefined, Second ) ->
	Second;

min_timestamp( First, _Second=undefined ) ->
	First;

min_timestamp( First={ F1, _ }, _Second={ S1, _} ) when F1 < S1 ->
	First;

min_timestamp( First={ F1, F2 }, _Second={ _S1=F1, S2 } ) when F2 < S2 ->
	First;

min_timestamp( _First, Second ) ->
	Second.



% Returns the maximum of the two specified timestamps.
%
max_timestamp( _First=undefined, Second ) ->
	Second;

max_timestamp( First, _Second=undefined ) ->
	First;

max_timestamp( _First={ F1, _ }, Second={ S1, _} ) when F1 < S1 ->
	Second;

max_timestamp( _First={ F1, F2 }, Second={ _S1=F1, S2 } ) when F2 < S2 ->
	Second;

max_timestamp( First, _Second ) ->
	First.



% Stops all child time managers.
%
% Tries to be parallel.
%
stop_child_managers( State ) ->

	ChildManagers = ?getAttr(child_managers),

	basic_utils:send_to_pid_list_impl( { stop, [], self() }, ChildManagers ),

	wait_stop_of_child_managers( ChildManagers, State ).



% Waits for all child managers to stop.
%
wait_stop_of_child_managers( ChildManagers, State ) ->

	case ?list_impl:is_empty( ChildManagers ) of

		true ->
			ok;

		false ->

			WaitDuration = get_maximum_stopping_duration(),

			receive

				{ wooper_result, { stopped, ManagerPid } } ->

					RemainingManagers = list_utils:safe_listimpl_delete(
													ManagerPid, ChildManagers ),

					wait_stop_of_child_managers( RemainingManagers, State )

			after WaitDuration ->

					?error_fmt( "Following child time managers failed "
								"to report on time (after ~B milliseconds) "
								"that they stopped: ~p.",
								[ WaitDuration,
								  ?list_impl:to_list( ChildManagers ) ] )

			end

	end.



% Flushes all scheduling messages which could be already sitting in the process
% mailbox.
%
flush_scheduling_messages() ->

	receive

		{ beginTimeManagerTick, _TickOffset } ->
			flush_scheduling_messages();

		{ beginTimeManagerDiasca, [ _TickOffset, _Diasca ] } ->
			flush_scheduling_messages()

	after 0 ->
		ok

	end.



% Detects and takes care of any end of diasca.
%
% Returns an updated state.
%
% (helper)
%
manage_possible_end_of_diasca( State ) ->

	case is_current_diasca_over( State ) of

		true ->

			case ?getAttr(parent_manager_pid) of

				undefined ->

					% We are the root time manager here:
					manage_end_of_diasca_as_root_manager( State );


				_ParentManagerPid ->

					manage_end_of_diasca_as_child_manager( State )

			end;


		false ->

			% Nothing to do (diasca not finished), still having to wait:
			%io:format( "Time manager ~w still having to wait.~n", [ self() ] ),

			State

	end.



% Manages an end of diasca, when being a root time manager, which leads to
% either a new diasca or a new tick.
%
% Returns an updated state.
%
manage_end_of_diasca_as_root_manager( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	CurrentDiasca = ?getAttr(current_diasca),

	% We insert here the resilience preparation as, if it was implemented among
	% the inter-diasca listeners above, it would be done on parallel with the
	% others, hence on a system whose state is still changing.
	ResilienceState = manage_resilience( CurrentTickOffset, CurrentDiasca,
										State ),

	% Moved after the resilience action, so that a rollback can restart more
	% easily:
	%
	InterState = manage_inter_diasca( ResilienceState ),

	SuspendState = manage_suspension( CurrentTickOffset, InterState ),

	% Either we have a new diasca to plan, or this tick is finished for good and
	% we can go to the next:
	%
	case ?getAttr(next_action) of

		new_diasca_needed ->

			% Next diasca to be scheduled, regardless of the interactivity mode:
			%
			% (a message is sent to ensure tail-recursiveness)
			%
			NewDiasca = CurrentDiasca + 1,

			%io:format( "Root time manager creates a new diasca, {~B,~B}.~n",
			%		  [ CurrentTickOffset, NewDiasca ] ),

			self() ! { beginTimeManagerDiasca,
					  [ CurrentTickOffset, NewDiasca ] },

			SuspendState;


		no_planned_action ->

			%io:format( "Root time manager does not have a planned action.~n" ),

			% We may have to jump to next tick then:
			case ?getAttr(spontaneous_agenda) of


				[] ->

					% Even locally, no next event, time to stop:
					?info_fmt( "At the global level, "
							   "there is no actor to trigger anymore "
							   "after this diasca (~B) nor spontaneous"
							   " action recorded after this tick "
							   "(offset #~B), therefore no future "
							   "event could possibly occur. Stopping "
							   "thus now the whole simulation.",
							   [ ?getAttr(current_diasca),
								CurrentTickOffset ] ),

					on_simulation_success( SuspendState ),

					% However there could be actors terminating, stop will
					% manage them:
					%
					% (we cannot use the stop/1 request on ourselves, as the
					% result would itself be interpreted as a call)
					%
					self() ! selfStop,

					SuspendState;


				[ { NextOverallTick, _ActorList } | _T ] ->

					schedule_new_tick( NextOverallTick, SuspendState ),
					SuspendState

			end;


		% Useless guard:
		TickOffset when TickOffset > CurrentTickOffset ->

			%io:format( "Root time manager selecting next scheduling.~n" ),

			% As the end of diasca of local actors does not update the next
			% local action, let's take it into account now:
			SoonestTickOffset = case ?getAttr(spontaneous_agenda) of

				[ { FirstAgendaTick, _ActorList } | _T ]
								  when FirstAgendaTick < TickOffset ->
					FirstAgendaTick;

				% Either [] or the first agenda tick is in the future:
				_ ->
					TickOffset

			end,

			% Simple jump:
			schedule_new_tick( SoonestTickOffset, SuspendState ),

			SuspendState

	end.




% Schedules the specified tick (root time manager only).
%
% Does not return anything of interest.
%
% (helper)
%
schedule_new_tick( NextTickOffset, State ) ->

	case ?getAttr(interactivity_mode) of

		interactive ->

			% Do nothing if interactive, as we will be triggered by the timer,
			% with a timerTickFinished call:
			%
			% (next tick will be the incremented current one)
			%
			ok;


		batch ->

			%io:format( "Root time Manager ~w at tick offset #~B "
			%   "determined that the next tick should be #~p.~n", [ self(),
			%   ?getAttr(current_tick_offset), NextTickOffset ] ),

			% A message is sent to ensure tail-recursiveness:
			self() ! { beginTimeManagerTick, NextTickOffset }

	end.



% Manages a possible end of diasca, when being a local, non-root time manager.
%
% Returns an updated state.
%
manage_end_of_diasca_as_child_manager( State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% We have to update the next action with the information from the agenda:
	%
	UpdatedNextAction = case ?getAttr(next_action) of

		new_diasca_needed ->
			% Cannot be beaten:
			new_diasca_needed;

		% We might have merged the two cases below (based on the Erlang term
		% ordering between integers and atoms), but it would have been a lot
		% less clear:
		no_planned_action ->
			case Agenda of

				[] ->
					no_planned_action;

				[ { SoonestAgendaTickOffset, _ActorList } | _T ] ->
					SoonestAgendaTickOffset

			end;

		TickOffset ->
			case Agenda of

				[] ->
					TickOffset;

				[ { SoonestAgendaTickOffset, _ActorList } | _T ]
				  when SoonestAgendaTickOffset < TickOffset ->
					SoonestAgendaTickOffset;

				_ ->
					% Here we expect SoonestAgendaTickOffset >= TickOffset:
					TickOffset

			end

	end,

	%io:format(
	%	"Child time manager reporting end of diasca ~B for tick offset #~B, "
	%	"next action in subtree is ~p.~n",
	%	[ ?getAttr(current_diasca), ?getAttr(current_tick_offset),
	%   UpdatedNextAction ] ),

	ParentManagerPid = ?getAttr(parent_manager_pid),

	% We are a child time manager, we just report the end of diasca for this
	% subtree:
	case ?getAttr(current_diasca) of

		0 ->
			% We have to declare an end of tick here:
			ParentManagerPid ! { notifySpontaneousSubtreeCompletion,
						[ ?getAttr(current_tick_offset),
						  self(),
						  UpdatedNextAction,
						  _TrackingInfo={ ?getAttr(scheduled_tracking),
										  ?getAttr(process_tracking) }
						 ] };

		CurrentDiasca ->
			% We have to declare an end of tick here:
			ParentManagerPid ! { notifyTriggerSubtreeCompletion,
						[ ?getAttr(current_tick_offset),
						  CurrentDiasca,
						  self(),
						  UpdatedNextAction,
						  _TrackingInfo={ ?getAttr(scheduled_tracking),
										  ?getAttr(process_tracking) }
						 ] }

	end,

	% No need to update next_action.
	State.



% Manages an inter-diasca transition.
%
% Returns an updated state.
%
% (helper)
%
manage_inter_diasca( State ) ->

	% We are the root time manager, deciding what to do next once having taken
	% care of inter-diasca listeners (if any):
	InterDiascaListeners = ?getAttr(interdiasca_listeners),

	% This is the first time we know the current tick is over;
	% we need to notify any intertick listener (typically the
	% root data-exchanger) as soon as possible:
	InterDiascaMessage = { onInterDiascaBegin, [], self() },

	[ L ! InterDiascaMessage || L <- InterDiascaListeners ],

	% Note: these requests are processed in parallel.

	basic_utils:wait_for( _Msg={ wooper_result, interdiasca_ended },
						  _MsgCount=length( InterDiascaListeners ) ),

	setAttribute( State, interdiasca_listeners, [] ).



% Manages the resilience mechanisms: meant to be called while the system state
% is stable and will not change until this function did its work.
%
% Returns an updated state.
%
% (helper)
%
manage_resilience( CurrentTickOffset, CurrentDiasca, State ) ->

	% Note: see the resilience manager to better understand the message
	% exchange.

	% We are the root time manager here and the whole simulation is
	% frozen. Let's act quickly!
	%
	case ?getAttr(serialisation_requested) of

		false ->
			State;

		true ->
			% We are just out of the latency-based critical path:

			% We do not want longer serialisations to trigger spurious
			% simulation stalls:
			%
			?getAttr(watchdog_pid) ! suspendWatchdog,

			?getAttr(resilience_manager_pid) ! { triggerSerialisation,
						[ CurrentTickOffset, CurrentDiasca ], self() },

			% Will resume the watchdog:
			wait_for_serialisation_end( _ActorsRequested=false,
									   _Serialised=false, State )

	end.



% Waits for the serialisations to end.
%
% Returns an updated state.
%
% (helper)
%
wait_for_serialisation_end( _ActorsReturned=true, _Serialised=true, State ) ->

	% Just having to wait passively then:
	%
	io:format( "Waiting for serialisation_done.~n" ),

	receive

		{ wooper_result, serialisation_done } ->

			io:format( "Serialisation over, resuming simulation.~n" ),

			% Simulation to continue now:
			?getAttr(watchdog_pid) ! resumeWatchdog,

			setAttribute( State, serialisation_requested, false )

	end;


% At least one request is still expected:
wait_for_serialisation_end( ActorsReturned, Serialised, State ) ->

	% Waiting fully idle for the serialisation to finish...
	%
	% However, all time managers (including this root one) must take part to the
	% serialisation action, for example by sending the list of all their local
	% actors; therefore we must be able to answer them, before waiting for the
	% serialisation to be performed. Not to mention that this time manager must
	% also be itself serialised, hence must answer to the corresponding request.
	%
	% For that, we hijack the WOOPER main loop of this root time manager:
	%
	receive

	   % Meant to be sent by the local instance tracker (first received):
	   { getAllLocalActors, [], CallerPid } ->

		   { NewState, Res } = executeRequest( State, getAllLocalActors ),

			CallerPid ! { wooper_result, Res },

			wait_for_serialisation_end( _ActorsReturned=true, Serialised,
									   NewState );


		% Meant to be sent by the resilience agent (second received):
		{ serialise, [ EntryTransformer, UserData ], CallerPid } ->

			{ NewState, Res } = executeRequest( State, serialise,
											[ EntryTransformer, UserData ] ),

			CallerPid ! { wooper_result, Res },

			wait_for_serialisation_end( ActorsReturned, _Serialised=true,
									   NewState )

	end.




% Manages simulation suspension: if the simulation must be suspended, waits
% until not suspended any more.
%
% To be preferably called at the end of a tick.
%
% Returns an updated state.
%
% (helper)
%
manage_suspension( CurrentTickOffset, State ) ->

	% TO-DO: manage correctly the suspension, with regard to batch/interactive
	% mode, suspending timer, time tracker, watchdog, etc. and flushing their
	% relevant messages.

	case ?getAttr(suspended) of

		false ->
			State;

		true ->

			?info_fmt( "Simulation suspended at {~B,~B}, waiting for a resume "
				"request.", [ CurrentTickOffset, ?getAttr(current_diasca) ] ),

			% Blocks as long as necessary:
			receive

				resume ->
					?info( "Simulation resumed." ),

					[ L ! simulation_resumed ||
						L <- ?getAttr(simulation_listeners) ],

					setAttribute( State, suspended, false )

			end

	end.



% Returns true iff this time manager can safely determine that its current
% diasca is over.
%
is_current_diasca_over( State ) ->

	check_waited_count_consistency( State ),

	% Precomputed, not to perform useless checkings of list_impl and al:
	case ?getAttr(waited_count) of

		0 ->
			true;

		%% NonNullCount when NonNullCount < 20 ->

		%%	%?debug_fmt( "(still waiting for a total of ~B "
		%%	%	"end-of-tick notifications of all sorts)", [ NonNullCount ] ),

		%%	%io:format( "(~w still waiting for a total of ~B "
		%%	%		   "end-of-tick notifications of all sorts).~n",
		%%	%		   [ self(), NonNullCount ] ),

		%%	%display_waiting_reason( State ),

		%%	false;

		_NonNullCount ->
			false

	end.



% Ensures that specified tick offset is compatible with the previous one, in the
% context of a new tick.
%
% Returns nothing useful, just throws an exception if an inconsistency is
% detected.
%
check_tick_consistency( NewTickOffset, State ) ->

	PreviousTickOffset = ?getAttr(current_tick_offset),

	JumpDuration = NewTickOffset - PreviousTickOffset,

	InitialTick = ?getAttr(initial_tick),

	PreviousTick   = InitialTick + PreviousTickOffset,
	NewCurrentTick = InitialTick + NewTickOffset,

	% Traces sent with the previous timestamp:
	case JumpDuration of

		PositiveOffset when PositiveOffset > 0 ->

			% Normal case, more checkings:
			?info_fmt( "Tick ~B (tick offset #~B, at diasca ~p) was over, "
					   "having jumped forward of ~B tick(s) to reach "
					   "the new current tick ~B, at diasca 0 "
					   "(corresponding tick offset: #~B).",
					   [ PreviousTick, PreviousTickOffset,
						?getAttr(current_diasca),
						JumpDuration, NewCurrentTick, NewTickOffset ] );

		NegativeOrNullOffset ->
			?fatal_fmt( "Tick ~B ended (tick offset: #~B, at diasca ~p), "
						"but specified new tick ~B (tick offset #~B) "
						"is not in its future "
						"(offset in the past of ~B tick(s)).",
						[ PreviousTick, PreviousTickOffset,
						 ?getAttr(current_diasca),
						 NewCurrentTick, NewTickOffset,
						 - NegativeOrNullOffset ] ),

			throw( { abnormal_tick_transition, PreviousTick, NewCurrentTick } )

	end.



% Ensures that specified tick offset and diasca are compatible with the previous
% ones, in the context of a new diasca.
%
% Returns nothing useful, just throws an exception if an inconsistency is
% detected.
%
check_diasca_consistency( TickOffset, NewDiasca, State ) ->

	PreviousDiasca = ?getAttr(current_diasca),

	% Checkings:
	TickOffset = ?getAttr(current_tick_offset),
	NewDiasca = PreviousDiasca + 1,
	{ TickOffset, PreviousDiasca } = ?getAttr(previous_timestamp).

	% We are on a non-zero diasca, thus there must be at least one child manager
	% with at least one actor to schedule (triggered or terminating), but it
	% cannot be checked here.



% Ensures that the current waiting count is accurate, or throws an exception.
%
check_waited_count_consistency( State ) ->

	WaitedSpontaneous = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	WaitedTriggered   = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	% Either we are at diasca 0 or not:
	case WaitedSpontaneous =/= 0 andalso WaitedTriggered =/= 0 of

		true ->
			throw( { spontaneous_trigger_mismatch, WaitedSpontaneous,
					WaitedTriggered } );

		false ->
			ok

	end,

	WaitedChildren = ?list_impl:size( ?getAttr(waited_child_managers) ),

	WaitedWatchdog = case ?getAttr(watchdog_waited) of

		true ->
			1;

		false ->
			0

	end,


	RealSum = WaitedSpontaneous + WaitedTriggered + WaitedChildren
		+ WaitedWatchdog,

	%io:format( "~w waiting for C=~B, S=~B, T=~B, W=~B.~n", [ self(),
	%	WaitedChildren, WaitedSpontaneous, WaitedTriggered,
	%	WaitedWatchdog ] ),

	case ?getAttr(waited_count) of

		RealSum->
			ok;

		WrongCount ->
			throw( { inconsistent_waited_count, WrongCount,
				{ RealSum, { WaitedChildren, WaitedSpontaneous, WaitedTriggered,
					WaitedWatchdog } } } )

	end.



% Outputs in console the current status regarding waiting of this time manager.
%
-spec display_waiting_reason( wooper_state() ) -> basic_utils:void().
display_waiting_reason( State ) ->

	WaitedSpontaneous = ?list_impl:size( ?getAttr(waited_spontaneous_actors) ),

	WaitedTriggered = ?list_impl:size( ?getAttr(waited_triggered_actors) ),

	WaitedChildren = ?list_impl:size( ?getAttr(waited_child_managers) ),

	WaitedWatchdog = case ?getAttr(watchdog_waited) of

		true ->
			1;

		false ->
			0

	end,

	io:format( "At tick offset #~B, time manager ~w waiting for "
			   "~B spontaneous actor(s), ~B triggered actor(s), "
			   "~B child manager(s) and ~B watchdog(s).~n",
			   [ ?getAttr(current_tick_offset), self(), WaitedSpontaneous,
				WaitedTriggered, WaitedChildren, WaitedWatchdog ] ),

	case WaitedSpontaneous of

		L when L > 0 andalso L < 5 ->

			io:format( "Waiting for following spontaneous actors: ~p.",
				[ ?list_impl:to_list( ?getAttr(waited_spontaneous_actors) ) ] );

		_ ->
			ok

	end.




% Manages the new current tick.
%
% Note that a given time manager may have nothing to schedule at one tick, as
% all ticks are propagated downward the full scheduling hierarchy, regardless of
% whether a given time manager has something to schedule (this has to be that
% way, otherwise a race condition could occur; and currently a time manager
% cannot know whether there are actors to schedule in its subtree).
%
% We delay the console/trace output as much as possible, to reduce the critical
% path, as these operations take some time. However it may lead to some actors
% being scheduled and sending console/trace output before the 'new tick'
% notification.
%
% Returns an updated state.
%
-spec manage_new_tick( tick_offset(), wooper_state() ) -> wooper_state().
manage_new_tick( NewTickOffset, State ) ->

	%io:format( "~n-- scheduling new tick, #~B (hence diasca 0), on ~w~n",
	%  [ NewTickOffset, self() ] ),

	ChildManagers = ?getAttr(child_managers),

	% First of all, recurses in all the scheduling hierarchy, regardless of
	% whether a child manager has any actor to schedule for this new tick:
	%
	ChildManagerCount = notify_child_managers_of_tick( ChildManagers,
													   NewTickOffset ),

	{ SpontaneousActorsImpl, SpontaneousCount, NewAgenda } =
		notify_spontaneous_actors( NewTickOffset, State ),

	% Here we should have escaped from most of the critical path.

	% Notifies the time listeners:
	[ P ! { onNewTick, NewTickOffset } || P <- ?getAttr(time_listeners) ],

	{ WatchedState, TickHeader, WaitedWatchdog } =
			case is_root_manager( State ) of

		true ->
			% Only the root time manager has a watchdog:
			?getAttr(watchdog_pid) ! { beginWatchdogTick, NewTickOffset },
			{ setAttribute( State, watchdog_waited, true ),
				"Root overall tick", _Waited=1 };

		false ->
			{ State, "Child local tick", _Waited=0 }

	end,

	record_progress_message( TickHeader, ChildManagerCount,
							_ScheduledCount=SpontaneousCount, WatchedState ),

	WaitedCount = SpontaneousCount + ChildManagerCount + WaitedWatchdog,

	%io:format( "~n~s for ~w: ~s is sent to ~B child time manager(s) "
	%		  "and to ~B local actor(s) that are scheduled for their "
	%		  "spontaneous behaviour (waited count, "
	%		  "including any watchdog: ~B).~n",
	%		  [ TickHeader, self(), get_textual_timings( State ),
	%		   ChildManagerCount, SpontaneousCount, WaitedCount ] ),

	% Actor termination never occurs at diasca 0.

	% We are now at {NewTickOffset,0}.
	% We came from {PastTickOffset,Dpast}.
	%
	% Some early non-local actors may already have sent actor messages to local
	% actors; in this case next_timestamp is already at {NewTickOffset,1} and
	% this should be left as is. Otherwise it must be either 'undefined' or the
	% previous timestamp, and no next timestamp is thus known. All other cases
	% mean a scheduling error.
	%
	PreviousTimestamp = ?getAttr(previous_timestamp),

	% Let's reset next_timestamp:
	NewNextTimestamp = case ?getAttr(next_timestamp) of

		PreviousTimestamp ->
			% Was not changed yet:
			undefined;

		undefined ->
			undefined;

		EarlyEntry={ NewTickOffset, 1 } ->
			% Was already set by an early actor, must be kept:
			EarlyEntry;

		WrongNextTimestamp ->
			throw( { scheduling_inconsistency_at_new_manager_tick,
				NewTickOffset, PreviousTimestamp, WrongNextTimestamp } )

	end,

	EarlyActorsToTrigger = ?getAttr(actors_to_trigger_in_two_diascas),

	KnownNextAction = case ?list_impl:is_empty( EarlyActorsToTrigger ) of

			  true ->

					% Checking:
					undefined = NewNextTimestamp,

					% Default:
					%
					% (we use the new agenda, otherwise we would pick the
					% current tick)
					%
					case NewAgenda of

						[] ->
							no_planned_action;

						[ { TickOffset, _ActorList } | _T ] ->
							TickOffset

					end;

			  false ->

				% Actors already at diasca 0 managed to send a message before
				% this time manager even received the new tick notification
				% (possibly after a jump). Checking:
				%
				{ NewTickOffset, 1 } = NewNextTimestamp,

				new_diasca_needed

	end,


	% Newer tick offset and diasca already set:
	%
	ResetState = setAttributes( WatchedState, [

		{ spontaneous_agenda, NewAgenda },

		% previous_timestamp already set.

		{ next_timestamp, NewNextTimestamp },

		{ next_action, KnownNextAction },

		% actors_to_trigger_in_one_diasca kept as is (may have already been
		% updated by early remote actors)

		% We know we always wait for them all, using most efficient list:
		{ waited_child_managers, ?getAttr(child_managers) },

		{ waited_spontaneous_actors, SpontaneousActorsImpl },

		{ waited_count, WaitedCount },

		% We initialize these two total values with the current local ones, as
		% values reported by child managers will be added during this diasca:
		{ scheduled_tracking, SpontaneousCount },

		{ process_tracking, system_utils:get_process_count() },

		% Ready for next diasca (1):
		{ actors_to_trigger_in_one_diasca, EarlyActorsToTrigger },

		{ actors_to_trigger_in_two_diascas, ?list_impl:new() }

	] ),

	TickPeriod = ?getAttr(tick_milestone_period),

	% Each 1000 ticks (ex: after 20 seconds of simulated time at 50 Hz), let's
	% trigger a simulation milestone:
	%
	% FIXME_WALLCLOCK: we can miss deadlines if jumping over them
	case NewTickOffset rem TickPeriod of

		0 ->
			self() ! { onTickMilestone, NewTickOffset };

		_ ->
			ok

	end,

	% From now, spontaneous actors, child managers and the watchdog are expected
	% to trigger 'notifySpontaneous*Complet*' methods... if there is at least
	% one of such agents:
	case WaitedCount of

		0 ->
			% Nobody will ever answer, we must already report an end of tick:
			manage_possible_end_of_diasca( ResetState );

		_NonNull ->
			% Answers will trigger back an end of diasca when appropriate:
			%
			% (due to the watchdog, the root time manager will always go that
			% route)
			%
			ResetState

	end.



% Manages the new current diasca.
%
% Note that a given time manager may have nothing to schedule at one diasca, as
% all diasca are propagated downward the full scheduling hierarchy, regardless
% of whether a given time manager has something to schedule (this has to be that
% way, otherwise a race condition could occur; and currently a time manager
% cannot know whether or not there are actors to trigger in its subtree).
%
% We delay the console/trace output as much as possible, to reduce the critical
% path, as these operations take some time. However it may lead to some actors
% being triggered and sending console/trace output before the 'new diasca'
% notification.
%
% Returns an updated state.
%
-spec manage_new_diasca( tick_offset(), diasca(), wooper_state() ) ->
							   wooper_state().
manage_new_diasca( TickOffset, NewDiasca, State ) ->

	%io:format( "~n  + scheduling new diasca, tick offset #~B diasca ~B "
	%			"on ~w~n", [ TickOffset, NewDiasca, self() ] ),

	% Using the plain list here:
	ChildManagers = ?getAttr(child_managers),

	% First of all, recurses in all the scheduling hierarchy, regardless of
	% whether a child manager has any actor to schedule for this new tick:
	%
	ChildManagerCount = notify_child_managers_of_diasca( ChildManagers,
														TickOffset, NewDiasca ),

	TerminatingActors = ?getAttr(terminating_actors),

	ScheduledActors = ?getAttr(actors_to_trigger_in_one_diasca),

	check_diasca_triggered( TerminatingActors, ScheduledActors ),

	% Smaller (plain) list on the left:

	% A perfectly licit situation is to have a terminating (not terminated)
	% actor which, during its active termination, receives actor messages;
	% however we want to have it scheduled only once, so we have to avoid
	% duplicates:
	UniqueTriggeredActors = list_utils:add_if_not_existing( TerminatingActors,
											  ScheduledActors ),

	TriggeredCount = notify_triggered_actors( TickOffset, NewDiasca,
											UniqueTriggeredActors ),

	% Here we should have escaped from most of the critical path.

	% Notifies the time listeners:
	[ P ! { onNewDiasca, [ TickOffset, NewDiasca ] } ||
		P <- ?getAttr(time_listeners) ],

	{ WatchedState, DiascaHeader, WaitedWatchdog } =
			case is_root_manager( State ) of

		true ->
			% Only the root time manager has a watchdog:
			?getAttr(watchdog_pid) ! { beginWatchdogDiasca,
									  [ TickOffset, NewDiasca ] },
			{ setAttribute( State, watchdog_waited, true ),
			  io_lib:format( "Root overall diasca ~p", [ NewDiasca ] ),
			  _Waited=1 };

		false ->
			{ State,
			  io_lib:format( "Child local diasca ~p", [ NewDiasca ] ),
			  _Waited=0 }

	end,

	%io:format( "Triggered count at {~p,~p}: ~B~n",
	%		  [ TickOffset, NewDiasca, TriggeredCount ] ),

	record_progress_message( DiascaHeader, ChildManagerCount,
							_ScheduledCount=TriggeredCount, WatchedState ),

	WaitedCount = TriggeredCount + ChildManagerCount + WaitedWatchdog,

	%io:format( "~n~s for ~w: ~s is sent to ~B child time manager(s) "
	%			"and to ~B local actor(s) that are triggered (waited count, "
	%			"including any watchdog: ~B).~n",
	%			[ DiascaHeader, self(), get_textual_timings( State ),
	%			  ChildManagerCount, TriggeredCount, WaitedCount ] ),

	TerminateState = terminate_actors( TickOffset, NewDiasca, WatchedState ),

	% We are now at { TickOffset, D }.
	% We came from { TickOffset, D-1 }.
	%
	% Some early non-local actors may already have sent actor messages to local
	% actors; in this case next_timestamp is already at {TickOffset,D+1} and
	% this should be left as is. Otherwise it must be either 'undefined' or the
	% previous timestamp, and no next timestamp is thus known. All other cases
	% mean a scheduling error.

	CurrentTimestamp = { TickOffset, NewDiasca },

	EarlyTimestamp = { TickOffset, NewDiasca + 1 },

	% Let's reset next_timestamp:
	NewNextTimestamp = case ?getAttr(next_timestamp) of

		CurrentTimestamp ->
			% Was not changed yet:
			undefined;

		undefined ->
			undefined;

		EarlyTimestamp ->
			% Was already set by an early actor, must be kept:
			EarlyTimestamp;

		WrongNextTimestamp ->
			throw( { scheduling_inconsistency_at_new_manager_diasca,
					TickOffset, NewDiasca, ?getAttr(previous_timestamp),
					CurrentTimestamp, WrongNextTimestamp } )

	end,

	EarlyActors = ?getAttr(actors_to_trigger_in_two_diascas),

	% Either we already know there will be a next diasca or not:
	KnownNextAction = case ?list_impl:is_empty( EarlyActors ) of


			  true ->

					% Checking, cannot have been set by an early actor:
					undefined = NewNextTimestamp,

					% Updates the information if needed:
					%
					% (will be overwritten as soon as one actor message is sent
					% this diasca)
					%
					case ?getAttr(spontaneous_agenda) of

						[] ->
							no_planned_action;

						[ { NextTickOffset, _ActorList } | _T ] ->
							NextTickOffset

					end;


			 false ->

				  %io:format( "Early actor detected, new diasca needed!~n" ),

				  % Checking, must have been set by an early actor:


				  EarlyTimestamp = NewNextTimestamp,

				  new_diasca_needed

	end,

	% Newer tick offset and diasca already set:
	%
	ResetState = setAttributes( TerminateState, [

		% previous_timestamp already set.

		{ next_timestamp, NewNextTimestamp },

		{ next_action, KnownNextAction },

		% This list is rebuilt each diasca:
		{ terminating_actors, [] },

		% The only place, once processed, where we can reset it without taking
		% the risk of forgetting early schedule-trigger notifications:
		{ actors_to_trigger_in_one_diasca, EarlyActors },

		{ actors_to_trigger_in_two_diascas, ?list_impl:empty() },

		% We know we always wait for them all, using most efficient list:
		{ waited_child_managers, ?getAttr(child_managers) },

		{ waited_triggered_actors, UniqueTriggeredActors },

		{ waited_count, WaitedCount },

		% We initialize these two total values with the current local ones, as
		% values reported by child managers will be added during this diasca:
		{ scheduled_tracking, TriggeredCount },

		{ process_tracking, system_utils:get_process_count() }

	] ),


	% No 'onDiascaMilestone' event deemed useful yet.

	% From now, triggered actors, child managers and the watchdog are expected
	% to trigger 'notifyTrigger*Complet*' methods... if there is at least one
	% of such agents:
	case WaitedCount of

		0 ->
			% Nobody will ever answer, we must already report an end of tick:
			manage_possible_end_of_diasca( ResetState );

		_NonNull ->
			% Answers will trigger back an end of diasca when appropriate:
			ResetState

	end.



% Performs some checking on the actors being triggered this diasca.
%
check_diasca_triggered( TerminatingActors, ScheduledActors ) ->

	% Ensures that there is no intersection in either list:
	false = list_utils:has_duplicates( TerminatingActors ),
	false = list_utils:has_duplicates( ?list_impl:to_list( ScheduledActors ) ).



% Takes care of writing in the traces and to the time tracker the latest
% progress information.
%
% To be executed by all time managers.
%
% Note that this progress information is neither authoritative nor even
% consistent, as it gathers various information that may not be synchronised.
%
% Does not return anything useful.
%
% (helper)
%
record_progress_message( Header, ChildManagerCount, ScheduledCount, State ) ->

	% We suppose that the latest progress information we gathered referred to
	% previous diasca:
	%
	{ ActualTickOffset, ActualDiasca } = case ?getAttr(previous_timestamp) of

				undefined ->
					% We just started here, let's use closest meaningful
					% timestamp:
					{ ?getAttr(current_tick_offset), ?getAttr(current_diasca) };

				T ->
					T

	end,

	% We have delayed these processings once the 'begin diasca' message has been
	% sent (i.e. when we are no more on the critical path), while still hoping
	% to *display* this 'top' message first, at each diasca:
	%
	% (date conversion in messages might be a bit expensive, but, if the console
	% tracker only uses actually very few of them, all may be used in traces,
	% hence are useful)
	%
	{ DetailedTopMessage, CompactTopMessageElements, Second } =
		get_full_textual_timings( ActualTickOffset, ActualDiasca, State ),


	% Here we use the information gathered on the last diasca, relative to the
	% scheduling subtree of this manager:
	%
	OverallSchedulingCount = ?getAttr(scheduled_tracking),
	OverallProcessCount    = ?getAttr(process_tracking),


	% Only the root manager generally has a time tracker:
	case ?getAttr(time_tracker_pid) of

		undefined ->
			ok;

		TrackerPid ->

			% Toggle the two comments, if wanting to display *all* top
			% notifications:

			% (expecting to be on the root time manager here)
			%

			Timings = CompactTopMessageElements,

			OverallActorCount = ?getAttr(overall_actor_count),

			Counts = { OverallActorCount, OverallSchedulingCount,
							OverallProcessCount },

			NewTopInfo = { Timings, Counts },

			TrackerPid ! { Second, NewTopInfo }

	end,

	% We probably should give overall information like total actor count in the
	% root time manager only, as the child ones are not notified of that actual
	% value.

	?info_fmt(
		"~s: sent at ~s to ~B child time manager(s) and to ~B local actor(s). "
		"During last diasca, for this scheduling subtree, "
		"we had a total of ~B actors that were scheduled, "
		"with ~B processes alive.",
		[ Header, DetailedTopMessage, ChildManagerCount, ScheduledCount,
		   OverallSchedulingCount, OverallProcessCount ] ).






% Updates the specified spontaneous agenda accordingly.
%
% Returns an updated state.
%
% (helper)
%
update_agenda( AddedSpontaneousTicks, WithdrawnSpontaneousTicks, ActorPid,
			  State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% We withdraw before adding, hence if a never-specified tick is to be added
	% and withdrawn *at the same diasca*, the operation will fail:
	WithdrawAgenda = withdraw_from_agenda( ActorPid, WithdrawnSpontaneousTicks,
										  Agenda ),

	AddAgenda = add_to_agenda( ActorPid, AddedSpontaneousTicks,
							  WithdrawAgenda ),

	setAttribute( State, spontaneous_agenda, AddAgenda ).



% Withdraws specified tick offsets for the specified actor from specified
% agenda.
%
% (helper)
%
withdraw_from_agenda( _ActorPid, _WithdrawnSpontaneousTicks=[], Agenda ) ->
	Agenda;

withdraw_from_agenda( ActorPid, _WithdrawnSpontaneousTicks=[Tick|T], Agenda ) ->
	NewAgenda = withdraw_from_agenda( ActorPid, Tick, Agenda, _Acc=[] ),
	withdraw_from_agenda( ActorPid, T, NewAgenda ).



% Withdraw specified actor at specified offset from agenda.
%
% Returns an updated agenda.
%
% (helper)
%
withdraw_from_agenda( ActorPid, TickOffset, _Agenda=[], _Acc ) ->
	% Agenda exhausted, tick not found:
	throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } );

withdraw_from_agenda( ActorPid, TickOffset,
					 _Agenda=[ { TickOffset, ActorList } | T ], Acc ) ->

	% Tick found, was already declared:
	case ?list_impl:is_member( ActorPid, ActorList ) of

		false ->
			throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } );

		true ->
			% The list might end up being empty, let's remove it in this case:
			NewActorList = ?list_impl:delete( ActorPid, ActorList ),

			% Agenda is sorted by ascending offsets:
			case ?list_impl:is_empty( NewActorList ) of

				true ->
					lists:reverse( Acc ) ++ T;

				false ->
					lists:reverse( Acc ) ++ [ { TickOffset, NewActorList } | T ]

			end

	end;

withdraw_from_agenda( ActorPid, TickOffset,
	  _Agenda=[ E={TOffset,_ActorList} | T ], Acc ) when TOffset < TickOffset ->

	% Offset not reached yet, continue iterating:
	withdraw_from_agenda( ActorPid, TickOffset, T, [ E | Acc ] );

withdraw_from_agenda( ActorPid, TickOffset, _Agenda, _Acc ) ->
	% Tick not found (neither higher nor equal here):
	throw( { no_spontaneous_tick_to_withdraw, TickOffset, ActorPid } ).




% Adds specified tick offsets for the specified actor to specified agenda.
%
% (helper)
%
add_to_agenda( _ActorPid, _AddedSpontaneousTicks=[], Agenda ) ->
	Agenda;

add_to_agenda( ActorPid, _AddedSpontaneousTicks=[ Tick | H ], Agenda ) ->
	NewAgenda = add_to_agenda( ActorPid, Tick, Agenda, _Acc=[]  ),
	add_to_agenda( ActorPid, H, NewAgenda ).



add_to_agenda( ActorPid, TickOffset, _Agenda=[], Acc ) ->
	% Agenda exhausted, tick to be added last:
	NewActorList = ?list_impl:add_element( ActorPid, ?list_impl:new() ),
	lists:reverse( [ { TickOffset, NewActorList } | Acc ] );

add_to_agenda( ActorPid, TickOffset, _Agenda=[ { TickOffset, ActorList } | T ],
			  Acc ) ->

	% Tick found, as was already declared: actor just to be added to the
	% pre-existing list here.
	%
	% We allow here an actor to declare the same spontaneous tick more than
	% once; in any case its PID will be listed only once (if any); we could have
	% also checked the presence of the PID before adding it, just to notify
	% (warning or error) that it was included multiple times, but we allow for
	% multiple declarations (more convenient for model developers).
	%
	NewActorList = ?list_impl:add_element( ActorPid, ActorList ),
	lists:reverse( Acc ) ++ [ { TickOffset, NewActorList } | T ];

add_to_agenda( ActorPid, TickOffset,
			  _Agenda=[ E={ SmallerOffset, _ActorList } | T ], Acc )
  when TickOffset > SmallerOffset ->
	% Still in smaller offsets here, let's continue iterating:
	add_to_agenda( ActorPid, TickOffset, T, [ E | Acc ] );

add_to_agenda( ActorPid, TickOffset,
			  % Clearer: Agenda=[ { _HigherOffset, _ActorList } | _T ], Acc ) ->
			  Agenda, Acc ) ->

	% Implicitly here we went past the last smaller (i.e. HigherOffset >
	% TickOffset), so we have to add a new list, at the relevant place
	% (i.e. just before):
	%
	NewActorList = ?list_impl:add_element( ActorPid, ?list_impl:new() ),
	lists:reverse( Acc ) ++ [ { TickOffset, NewActorList } | Agenda ].



% Notifies all specified child managers that the specified tick is to begin.
%
% Returns the number of child managers.
%
notify_child_managers_of_tick( ChildManagers, NewTickOffset ) ->

	%io:format( "Notifying at tick offset #~B following child managers: ~p.~n",
	%  [ NewTickOffset, ChildManagers ] ),

	%% case ChildManagers of

	%	[] ->
	%		io:format( "No child manager to notify of new tick offset #~B.",
	%					[ NewTickOffset ] );

	%	_Children ->
	%		io:format( "Notifying child managers ~w of new tick offset #~B.",
	%					[ ChildManagers, NewTickOffset ] )

	%% end,

	basic_utils:send_to_pid_list_impl( { beginTimeManagerTick, NewTickOffset },
						   ChildManagers ).



% Notifies all specified child managers that the specified diasca is to begin.
%
notify_child_managers_of_diasca( ChildManagers, TickOffset, NewDiasca ) ->

	%io:format( "Notifying at diasca ~B in tick offset #~B "
	%  "following child managers: ~p.~n",
	%  [ NewDiasca, TickOffset, ChildManagers ] ),

	%% case ChildManagers of

	%	[] ->
	%		io:format( "No child manager to notify of new diasca ~B "
	%					"at tick offset #~B.", [ NewDiasca, TickOffset ] );

	%	_Children ->
	%		io:format( "Notifying child managers ~w of new diasca ~B "
	%					"at tick offset #~B.",
	%					[ ChildManagers, NewDiasca, TickOffset ] )

	%% end,

	% Returns the number of child managers:
	basic_utils:send_to_pid_list_impl(
		{ beginTimeManagerDiasca, [ TickOffset, NewDiasca ] },
		ChildManagers ).



% Notifies all local actors that they are expected to develop their spontaneous
% behaviour that a new tick is to be scheduled now.
%
% Returns a { SpontaneousActors, ActorCount, NewAgenda } triplet made of the
% corresponding spontaneous actors (as a list_impl), their count and of the new
% agenda.
%
notify_spontaneous_actors( NewTickOffset, State ) ->

	Agenda = ?getAttr(spontaneous_agenda),

	% Then only, manages the local actors that must be scheduled:
	case get_spontaneous_for( NewTickOffset, Agenda ) of

		none ->
			%io:format( "No spontaneous actor to notify at tick offset #~B.~n",
			%  [ NewTickOffset ] ),
			{ ?list_impl:new(), 0, Agenda };

		{ ActorListImpl, NewSpontaneousAgenda } ->

			%io:format( "Notifying at tick offset #~B "
			%  "following spontaneous actors: ~p.~n",
			%  [ NewTickOffset, ?list_impl:to_list(ActorListImpl) ] ),

			% Oneway:
			Count = basic_utils:send_to_pid_list_impl(
					  { beginTick, NewTickOffset },	ActorListImpl ),

			{ ActorListImpl, Count, NewSpontaneousAgenda }

	end.



% Notifies all local actors that received an actor message last diasca, or that
% are actively terminating, that a new diasca began.
%
% Returns the number of triggered actors.
%
notify_triggered_actors( TickOffset, NewDiasca, TriggeredActors ) ->

	basic_utils:send_to_pid_list_impl(
	  { beginDiasca, [ TickOffset, NewDiasca ] }, TriggeredActors ).




% Called whenever the simulation terminates on success.
%
% Does not return anything useful.
%
on_simulation_success( State ) ->
	[ L ! simulation_succeeded || L <- ?getAttr(simulation_listeners) ].




% Terminates the actors that notified this time manager on the previous diasca
% that they were terminating.
%
% Returns an updated state and the list of terminated actors.
%
% (helper)
%
terminate_actors( TickOffset, NewDiasca, State ) ->

	ActorList = ?list_impl:to_list( ?getAttr(terminated_actors) ),

	%io:format( "Terminated actors: ~w.~n", [ ActorList ] ),

	% Rush for maximum parallelism:
	TerminationMessage = { beginTerminationDiasca, [ TickOffset, NewDiasca ] },
	[ Actor ! TerminationMessage || Actor <- ActorList ],

	% Then take care of the internal administrative details:
	TerminatedState = lists:foldl(
					fun( Actor, FoldedState ) ->
							actual_unsubscribing( Actor, FoldedState )
					end,
					_InitialAcc=State,
					ActorList ),

	ActorsToDelete = ActorList ++ ?getAttr(actors_to_delete_at_next_tick),

	setAttributes( TerminatedState, [

			{ terminated_actors, ?list_impl:new() },
			{ actors_to_delete_at_next_tick, ActorsToDelete }

									 ] ).



% Performs the actual unsubscription of the specified actor.
%
% Note: placed in a dedicated function, as used from more than one place.
%
% Returns an updated state.
%
% (helper)
%
actual_unsubscribing( ActorPid, State ) ->

	%io:format( "############ Unsubscribing actor ~w.~n", [ ActorPid ] ),

	LocalActors = ?getAttr(known_local_actors),

	case ?list_impl:is_member( ActorPid, LocalActors ) of

		true ->

			?debug_fmt( "Unsubscribing actor ~w.", [ ActorPid ] ),

			PurgedState = ensure_actor_never_scheduled_any_more( ActorPid,
																State ),

			UpdatedLocalActors = ?list_impl:del_element( ActorPid,
														 LocalActors ),

			setAttribute( PurgedState, known_local_actors, UpdatedLocalActors );

		false ->

			throw( { unknown_actor_to_unsubscribe, ActorPid } )

	end.



% Ensures that specified actor may not be scheduled anymore.
%
% A mere (expensive) checking that can be disabled as a whole.
%
% (helper)
%
ensure_actor_never_scheduled_any_more( ActorPid, State ) ->

	% We go through all the agenda:
	%
	% (we could as well maintain a list of terminated actors to better spot
	% life-cycle errors, based on the instance tracker)
	%
	[ check_not_in_slot( ActorPid, S ) || S <- ?getAttr(spontaneous_agenda) ],

	State.



% Checks that specified actor is not in specified slot.
%
% (helper)
%
check_not_in_slot( ActorPid, { TickOffset, ActorList } ) ->

	case ?list_impl:is_member( ActorPid, ActorList ) of

		true ->
			throw( { future_schedule_for_terminating_actor, ActorPid,
					TickOffset } );

		false ->
			ok

	end.



% Spontaneous tick management.
%
% The 'next spontaneous ticks' list is to be ordered according to the first
% member of its pairs, which is a simulation tick, from smallest to latest.



% Returns a pair made of any actor (list_impl) list registered for the specified
% tick in the specified agenda (otherwise an empty list_impl list) and of a
% corresponding new updated spontaneous agenda.
%
% Relies on the fact that the spontaneous list is ordered by increasing first
% element of the pair (i.e. by increasing ticks) and that therefore we always
% pop its head.
%
% Note: the third clause and the guard of the second could be removed.
%
get_spontaneous_for( TickOffset,
		_SpontaneousAgenda=[ { TickOffset, ActorList } | T ] ) ->
	% Found, and returned popped:
	{ ActorList, _NewSpontaneousAgenda=T };

get_spontaneous_for( _TickOffset, _SpontaneousAgenda=[] ) ->
	% Nothing is planned at all (local agenda exhausted), but it does not mean
	% the simulation will remain idle until the end of time, as other time
	% managers may have a non-empty agenda.
	none;

get_spontaneous_for( TickOffset,
		_SpontaneousAgenda=[ { OtherTickOffset, _ActorList } | _T ] )
			when OtherTickOffset > TickOffset ->
	% Other tick already in the future, thus nothing to do currently:
	none;

get_spontaneous_for( TickOffset, SpontaneousAgenda ) ->
	% here we must have found in the agenda a tick offset smaller than the
	% specified one: spontaneous entry in the past, abnormal!
	throw( { spontaneous_entry_in_the_past, TickOffset, SpontaneousAgenda } ).



% Inserts specified actor (PID) in the list of actors to be spontaneously
% scheduled on specified tick offset, if not already present.
%
% Returns an updated schedule agenda.
%
schedule_as_spontaneous_for( TickOffset, Actor, SpontaneousAgenda ) ->
	insert_as_spontaneous_for( TickOffset, Actor, SpontaneousAgenda,
		_ReversedEndList=[] ).



% Here we deal actually with tick offsets, not ticks.
%
% Example:
%    BeginList   ReversedEndList
% 1: [A,B,C,D,E] []
% 1: [B,C,D,E]   [A]
% 1: [C,D,E]     [B,A]
% 1: [D,E]       [C,B,A]
% so to rebuild the list we use: lists:reverse(ReversedEndList) ++ BeginList
insert_as_spontaneous_for( Tick, Actor, _BeginList=[], ReversedEndList ) ->
	%io:format( "A~n" ),
	% We arrived at the end of the list, not found, insert at last position:
	lists:reverse( [ { Tick, ?list_impl:add( Actor, ?list_impl:new() ) }
					| ReversedEndList ] );

insert_as_spontaneous_for( Tick, Actor,
		CurrentList=[ { Tick, Actorlist } | RemainderOfBeginList ],
		ReversedEndList ) ->

	% The tick has already an entry; adding this actor, iff not already
	% registered:
	case ?list_impl:is_member( Actor, Actorlist ) of

		true ->
			% Returns the original list:
			lists:reverse( ReversedEndList ) ++ CurrentList;

		false ->
			% Adds the actor:
			lists:reverse( [ { Tick, ?list_impl:add( Actor, Actorlist ) }
							| ReversedEndList ] ) ++ RemainderOfBeginList

	end;

insert_as_spontaneous_for( Tick, Actor,
		BeginList=[ { CurrentTick, _Actorlist } | _RemainderOfBeginList ],
		ReversedEndList ) when CurrentTick > Tick ->
	%io:format( "C~n" ),
	% Here we just went past the correct tick, which had no actor list yet,
	% adding it:
	lists:reverse( [ { Tick, ?list_impl:add( Actor, ?list_impl:new() ) }
		| ReversedEndList ] ) ++ BeginList;

insert_as_spontaneous_for( Tick, Actor,
		   [ Entry | BeginList ], ReversedEndList ) ->
	%io:format( "D~n" ),
	% Here implicitly CurrentTick < Tick, therefore just recursing:
	insert_as_spontaneous_for( Tick, Actor, BeginList,
		[ Entry | ReversedEndList ] ).



% Merges specified agendas into a unique one.
%
% Preferably, the agenda having the smaller number of pairs should be the first
% specified one.
%
-spec merge_agendas( agenda(), agenda() ) -> agenda().
merge_agendas( _FirstAgenda=[], SecondAgenda ) ->
	SecondAgenda;

% We iterate over the first, and complement the second:
%
% Entries are { Tick, ActorList } elements.
%
merge_agendas( _FirstAgenda=[ Entry | T ], SecondAgenda ) ->

	UpdatedSecondAgenda = insert_schedule_list_for( Entry, SecondAgenda ),

	merge_agendas( T, UpdatedSecondAgenda ).



% Adds the specified (list_impl-based) list of actors on specified agenda at
% specified tick, and returns the resulting agenda.
%
% Quite similar to insert_as_spontaneous_for/4.
%
% (helper)
%
insert_schedule_list_for( Entry, Agenda ) ->
	insert_schedule_list_for( Entry, Agenda, _ReversedEndAgenda=[] ).


insert_schedule_list_for( Entry, _Agenda=[], ReversedEndAgenda ) ->
	% We arrived at the end of the list, not found, insert at last position:
	lists:reverse( [ Entry | ReversedEndAgenda ] );

insert_schedule_list_for( _Entry={ Tick, ActorList },
						  _Agenda=[ { Tick, CurrentList } | T ],
						  ReversedEndAgenda ) ->
	% Complementing that tick with both lists:
	MergedList = ?list_impl:union( ActorList, CurrentList ),
	lists:reverse( [ { Tick, MergedList } | ReversedEndAgenda ] ) ++ T;

insert_schedule_list_for( Entry={ ETick, _ActorList },
						  Agenda=[ { ATick, _CurrentList } | _T ],
						  ReversedEndAgenda ) when ATick > ETick ->
	% Here we just went past the correct tick, which had no actor list yet,
	% adding it:
	lists:reverse( [ Entry | ReversedEndAgenda ] ) ++ Agenda;

insert_schedule_list_for( Entry, _Agenda=[ AEntry | T ], ReversedEndAgenda ) ->
	% Here implicitly ATick < ETick, therefore just recursing:
	insert_schedule_list_for( Entry, T, [ AEntry | ReversedEndAgenda ] ).



% Launches the watchdog.
%
% Returns an updated state.
%
% (helper)
%
launch_watchdog( State ) ->

	WatchdogDuration = ?watchdog_wait_duration,

	% Needed, as a self() in a closure would be evaluated by the spawned
	% process:
	RootTimeManagerPid = self(),

	% Depends on the execution target:
	MaxIdle = get_maximum_idle_duration(),

	% The watchdog ensures the manager does not get stuck:
	% (closure used to avoid exporting the function)
	WatchdogPid = spawn_link( fun() -> watchdog_main_loop( RootTimeManagerPid,
			  WatchdogDuration, _AccumulatedDuration=0,
			  MaxIdle ) end ),

	?debug_fmt( "Watchdog ~w created and running, "
				"with a time-out duration of ~B ms; notifying now "
				"initial actors that the simulation started.",
				[ WatchdogPid, WatchdogDuration ] ),

	setAttributes( State, [

			{ watchdog_pid, WatchdogPid },
			{ watchdog_waited, false }

						   ] ).


% Launches the timer.
%
% Returns an updated state.
%
% (helper)
%
launch_timer( State ) ->

	?notify_by_speak( "Starting simulation clock in interactive mode." ),

	TickDuration = ?getAttr(simulation_tick_duration),

	% duration_to_string/1 expects milliseconds:
	TickDurationString = text_utils:duration_to_string(
		 erlang:round( TickDuration * 1000 ) ),

	Frequency = 1 / TickDuration,

	?notify_mute_fmt( "Starting global simulation clock in interactive mode "
					  "at ~s with a requested simulation frequency "
					  "of approximately ~fHz (period of exactly ~s).",
					  [ get_textual_timings( State ), Frequency,
					   TickDurationString ] ),

	% Closure used to avoid exporting the function:

	TimerTimeOut = ?getAttr(simulation_tick_waiting),

	RootTimeManagerPid = self(),

	TimerPid = spawn_link( fun() -> timer_main_loop( RootTimeManagerPid,
													TimerTimeOut )
						   end ),

	setAttribute( State, timer_pid, TimerPid ).



% Launches the wallclock time tracker.
%
% Returns an updated state.
%
% (helper)
%
launch_wallclock_tracker( State ) ->

	RootTimeManagerPid = self(),

	WallclockTrakerPid = spawn_link( fun() ->
		   wallclock_tracker_main_loop( RootTimeManagerPid,
				 ?getAttr(wallclock_milestone_period), _TotalDuration=0 )
									 end ),

	setAttribute( State, wallclock_tracker_pid, WallclockTrakerPid ).



% Launches the (simulation) time tracker.
%
% Returns an updated state.
%
% (helper)
%
launch_time_tracker( starting, State ) ->
	launch_time_tracker( " (not", "started)", _CurrentTickOffset=0,
						_CurrentDiasca=0, State );

launch_time_tracker( { resuming, { Tick, Diasca } }, State ) ->
	launch_time_tracker( " (resuming)", "", Tick, Diasca, State ).



launch_time_tracker( SimDateString, SimTimeString, CurrentTickOffset,
					CurrentDiasca, State ) ->

	RootTimeManagerPid = self(),

	{ RealDateString, RealTimeString, _RealSecond } = format_real_time_date(),

	InitialTimings = { SimDateString, SimTimeString, CurrentTickOffset,
					  CurrentDiasca, RealDateString, RealTimeString },

	ActorCount = 0,
	ScheduleCount = 0,
	ProcessCount = ?getAttr(process_tracking),

	InitialCounts = { ActorCount, ScheduleCount, ProcessCount },

	InitialTopInfo = { InitialTimings, InitialCounts },


	?debug( "Creating simulation-time tracker." ),

	TimeTrakerPid = spawn_link( fun() -> time_tracker_main_loop(
				_LatestDisplayedSecond=undefined,
				_LatestMeasuredSecond=not_undefined,
				_LatestTopInfo=InitialTopInfo,
				?getAttr(load_balancer_pid),
				RootTimeManagerPid )
								end ),

	setAttribute( State, time_tracker_pid, TimeTrakerPid ).





% Hooks for serialisation/deserialisation, used by WOOPER-specified serialise/3.

% We do not want the serialiser function to see the PIDs of the local worker
% processes, as of course they are not meant to be resolved by the instance
% tracker (this would fail). We only cherry-pick the relevant information.



% Triggered just before serialisation.
%
-spec pre_serialise_hook( wooper_state() ) -> wooper_state().
pre_serialise_hook( State ) ->

	% In this state forged for serialisation, we keep only the interesting bits
	% (for example private processes are silenced, as they could not be resolved
	% by instance trackers):
	%
	wooper_mute_attributes( [

			parent_manager_pid,
			time_tracker_pid,
			timer_pid,
			wallclock_tracker_pid,
			watchdog_pid

											 ], State ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
-spec post_serialise_hook( class_name(), term_serialisation(), wooper_state() )
						 -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% The two hooks below are never used, as we do not deserialise time managers as
% they are, we merge them with redeployed, local ones.

% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.


% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper_state() ) -> wooper_state().
post_deserialise_hook( State ) ->
	State.



% Merges specified entries coming from deserialisation into the local time
% manager.
%
-spec merge_local_with( term_serialisation() ) -> basic_utils:void().
merge_local_with( SerialisedEntries ) ->

	RegistrationName = get_registration_name(),

	LocalManagerPid = basic_utils:get_registered_pid_for( RegistrationName,
														  local ),

	LocalManagerPid ! { mergeWith, [ SerialisedEntries ], self() },

	receive

		{ wooper_result, merged } ->
			ok

	end.



% Helper function to test the management of spontaneous lists:
%
% Using atoms instead of PID.
%
% (any() is used in return type, as ?list_impl:?list_impl() would not be
% correct: we would need gb_sets:gb_set() - i.e. no plural for type name)
%
-spec test_spontaneous_lists() -> basic_utils:void().
test_spontaneous_lists() ->

	% Starts with an empty list:
	L1 = schedule_as_spontaneous_for( 5, first, [] ),
	io:format( "Result is ~p.~n", [ L1 ] ),

	L2 = schedule_as_spontaneous_for( 4, second, L1 ),
	io:format( "Result is ~p.~n", [ L2 ] ),

	L3 = schedule_as_spontaneous_for( 6, third,  L2 ),
	io:format( "Result is ~p.~n", [ L3 ] ),

	L4 = schedule_as_spontaneous_for( 5, fourth, L3 ),
	io:format( "Result is ~p.~n", [ L4 ] ),

	L5 = schedule_as_spontaneous_for( 10, fifth,  L4 ),
	io:format( "Result is ~p.~n", [ L5 ] ),

	L6 = schedule_as_spontaneous_for( 0, sixth,  L5 ),
	io:format( "Result is ~p.~n", [ L6 ] ),

	% Check:
	L6 = [

		   { 0,  ?list_impl:from_list( [ sixth ] ) },
		   { 4,  ?list_impl:from_list( [ second ] ) },
		   { 5,  ?list_impl:from_list( [ fourth, first ] ) },
		   { 6,  ?list_impl:from_list( [ third ] ) },
		   { 10, ?list_impl:from_list( [ fifth ] ) }

			].



% Helper function to test the minimum and maximum comparisons over timestamps.
%
-spec test_min_max_timestamps() -> basic_utils:void().
test_min_max_timestamps() ->

	Z = { 0, 0 },
	O = { 1, 1 },

	A = { 1, 0 },
	B = { 0, 1 },

	io:format( "Testing min.~n" ),

	undefined = min_timestamp( undefined, undefined ),
	Z = min_timestamp( undefined, Z ),
	Z = min_timestamp( Z, undefined ),
	Z = min_timestamp( Z, Z ),
	Z = min_timestamp( Z, O ),
	Z = min_timestamp( O, Z ),
	Z = min_timestamp( Z, B ),
	B = min_timestamp( B, A ),
	A = min_timestamp( A, O ),

	io:format( "Testing max.~n" ),

	undefined = max_timestamp( undefined, undefined ),
	Z = max_timestamp( undefined, Z ),
	Z = max_timestamp( Z, undefined ),
	Z = max_timestamp( Z, Z ),
	O = max_timestamp( Z, O),
	O = max_timestamp( O, Z ),
	B = max_timestamp( Z, B ),
	A = max_timestamp( B, A ),
	O = max_timestamp( O, A ).



% Performs some house-keeping, to enhance the mode of operation of this manager.
%
perform_house_keeping( State ) ->

	% Not much to be done here currently.

	% In no way necessary, but maybe useful:
	erlang:garbage_collect(),

	State.



% Returns the highest acceptable idle duration, in milliseconds, before deciding
% a tick is lasting for too long.
%
-spec get_maximum_idle_duration() -> unit_utils:milliseconds().


% Returns the highest acceptable idle duration, in milliseconds, for the
% stopping of the simulation.
%
-spec get_maximum_stopping_duration() -> unit_utils:milliseconds().



-ifdef(exec_target_is_production).

% In production mode here:

get_maximum_idle_duration() ->
	%io:format( "(in production mode, thus ~w will be using "
	%		  "extended tick time-out)~n", [ self() ] ),

	% Up to 10 hours for one single tick (!) should be enough in general:

	% (note: such a huge duration might actually be useful, notably for
	% large-scale executions of instances whose first tick (tick offset #1)
	% involves a significantly lengthy initialization; we have seen this tick
	% last for more than 4 hours on an HPC cluster)
	%
	10 * 60 * 60 * 1000.


get_maximum_stopping_duration() ->
	60 * 1000.


-else.


% In development mode here:

get_maximum_idle_duration() ->
	% 12 seconds is quite tight, but it helps the debugging:
	120 * 1000.

get_maximum_stopping_duration() ->
	5000.


-endif.
