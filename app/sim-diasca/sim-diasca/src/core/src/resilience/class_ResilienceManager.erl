% Copyright (C) 2012-2014 EDF R&D

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




% Resilience manager.


% Its role is to enhance the capacity of the simulator to overcome faults and
% technical issues, like the loss of computing hosts in the course of the
% simulation.
%
% It allows to implement the "k-crash resistance" feature, see, in
% class_DeploymentManager.hrl, the 'crash_resilience' field of the
% deployment_settings record for more information.
%
% It drives the resilience agents.
%
% See also class_ResilienceAgent.erl and class_ResilienceManager_test.erl.
%
-module(class_ResilienceManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, FullSettings, AllComputingNodes,
		 RootTimeManagerPid, ResultManagerPid, StartTimestamp, RootDir,
		 AvailableComputingHosts ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/7, new_link/7,
		 synchronous_new/7, synchronous_new_link/7,
		 synchronous_timed_new/7, synchronous_timed_new_link/7,
		 remote_new/8, remote_new_link/8, remote_synchronous_new/8,
		 remote_synchronous_new_link/8, remote_synchronisable_new_link/8,
		 remote_synchronous_timed_new/8, remote_synchronous_timed_new_link/8,
		 construct/8, destruct/1 ).



% Member method declarations.
-define( wooper_method_export,

		 % Simulation listener API:
		 simulation_started/1, simulation_suspended/1, simulation_resumed/1,
		 simulation_succeeded/1, simulation_stopped/1,

		 serialisationRequested/1, triggerSerialisation/3,

		 nodedown/2,

		 toString/1 ).



% Static methods declarations.
-define( wooper_static_method_export,  ).


% Testing-purpose exports:
-export([ k_map_to_string/1, build_k_map/2 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Resilience.Manager").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For user_process_name:
-include("sim_diasca.hrl").


% For simulation_settings and time_manager_name:
-include("class_TimeManager.hrl").


% For deployment_settings, computing_host_info, etc:
-include("class_DeploymentManager.hrl").


% For load_balancing_settings:
-include("class_LoadBalancer.hrl").


% For resilience_manager_name, records and al:
-include("class_ResilienceManager.hrl").



% Where the resilience manager should be registered.
% Could be local_and_global or global_only as well:
%-define( registration_type, local_only ).
-define( registration_type, local_and_global ).



% A k-map registers all node-related information to prepare for crashes and
% overcome them.
%
% To each node N is associated a k-record, storing:
%
% - a list of the k nodes that a node N will back-up ("secured nodes") for N,
% i.e. whose information will be backed-up by N
%
% - a list of the k nodes that will be used as back-up nodes ("securing nodes")
% for N, i.e. that will store information on the behalf of N
%
%
% Ex: with a resilience level of k=3, to node 'a' may be associated the secured
% nodes [ g, h, i ] (they rely on 'a'), while the securing nodes may be [ 'b',
% 'c', 'd' ] ('a' rely on them).


% Record defined in corresponding header:
-type k_record() :: #k_record{}.



% The associative table for resilience: for each node, a k-record.
%
%-type k_map() :: ?hashtable_type:?hashtable_type( node_name(), k_record() ).
-type k_map() :: ?hashtable_type:?hashtable_type().


% To associate the PID of a resilience agent to its node:
%
%-type node_table() :: ?hashtable_type:?hashtable_type( node_name(), pid() ).
-type node_table() :: ?hashtable_type:?hashtable_type().





% Implementation notes.


% The exchanges with the time manager are a bit complex:
%
% - the resilience manager is the one to track the duration between two
% serialisations, and thus to know when time has come to trigger a new one (time
% is tracked before the simulation is started, to secure also the creation of
% initial actors - which might be long; however the first serialisation will not
% occur before the simulation is started)
%
% - however a serialisation must happen only on a stable state, hence between
% two diascas: the (root) time manager must thus manage it
%
% - during the inter-diasca period, the root manager could ask the resilience
% manager if a serialisation is to occur; however, latency-wise, it would be in
% the critical path, which would hurt performances; hence instead the resilience
% manager tells in advance the root time manager that, at next inter-diasca, a
% serialisation shall be triggered, and there is no exchange about resilience in
% the critical path if not serialisation is to be done between these two diascas

% This resilience manager is created by the deployment manager, but they should
% not be linked (as any crash of the former should be resisted by the
% latter). However, there should be a link between the user process (the one of
% the simulation case) and this resilience manager, as the latter should not
% fail silently (and its failing should stop the whole simulation). As none of
% the two creates the other, an (atomic) new_*link form cannot be used. We
% simply create as soon as possible a (non-atomic) link between these two.



% The class-specific attributes of the resilience manager are:
%
% - resilience_level :: basic_utils:count() corresponds to the parameter k
% (maximum number of simultaneously failing hosts)
%
% - serialisation_period :: unit_utils:seconds() is the minimal number of
% (wall-clock) seconds that should elapse before the end of a serialisation and
% the beginning of the next one
%
% - spof_nodes :: [ node_name() ] is a list of the single-point of failure nodes
%
% - protected_nodes :: [ node_name() ] is a list of all the nodes that are
% protected by this resilience mechanism
%
% - k_map :: k_map() is a map allowing to keep track of the resilence
% information for each node
%
% - node_table :: node_table() is an associative table, whose keys are the name
% of each computing node and whose values are the PID of the resilience agent in
% charge of that node
%
% - last_serialisation_timestamp :: basic_utils:timestamp() records when the
% last serialisation (if any) was done, in order to know when the next one is to
% happen
%
% - serialisation_history :: [ class_TimeManager:logical_timestamp() ] is the
% list of the { Tick, Diasca } pairs at which serialisations already occurred,
% in reverse chronological order (most recent first)
%
% - root_time_manager_pid :: pid() is the PID of the root time manager, notably
% to notify it whenever a serialisation must take place
%
% - result_manager_pid :: pid() is the PID of the result manager, needed so that
% resilience agents can know what are their local probes that they must manage,
% serialisation-wise
%
% - time_tracker_pid :: maybe( pid() ) the PID of the internal process to watch
% for serialisation periods to be elapsed
%
% - serialisation_enabled :: boolean() tells whether serialisations can occur
% (for example none should happen before the simulation is started)
%
% - lost_nodes :: [ net_utils:atom_node_name() ] is a list of all computing
% nodes that have been currently reported as down, since the simulation started
%
% - net_tick_time :: unit_utils:milliseconds() is the duration in milliseconds
% of the inter-VM tick, in order to detect node losses
%
% - simulation_settings :: simulation_settings() stores the user-specified
% simulation settings, to be kept here if needing to perform a rollback
%
% - deployment_settings :: deployment_settings() stores the user-specified
% deployment settings, to be kept here if needing to perform a rollback
%
% - load_balancing_settings :: load_balancing_settings() stores the
% user-specified settings, to be kept here if needing to perform a rollback
%
% - start_timestamp :: basic_utils:timestamp() records a timestamp corresponding
% to the simulation start
%
% - engine_root_dir :: file_utils:path() corresponds to the absolute root
% directory of the sources of a Sim-Diasca installation or check-out, i.e. to
% the directory which contains directly common, sim-diasca, traces, wooper, etc.
%
% - available_hosts :: [ computing_host_info() ] stores the up-to-date
% information about the computing hosts involved in the simulation



% Constructs a new resilience manager, from following parameters:
%
% - FullSettings allows this resilience manager to record all the initial
% settings so that, if a redeployment becomes necessary, all the services can be
% created again
%
% - AllComputingNodes is a list of the computing nodes that have to be secured
% by this manager and thus that must host a resilience agent
%
% - RootTimeManagerPid is the PID of the root time manager
%
% - ResultManagerPid is the PID of the result manager
%
% - ResultManagerPid is the PID of the result manager
%
% - StartTimestamp is a timestamp corresponding to the simulation start
%
% - RootDir is the path to the root directory of the engine
% (i.e. SIM_DIASCA_TOP)
%
% - AvailableHosts lists the information of the available computing hosts
%
-spec construct( wooper:state(),
				 { simulation_settings(), deployment_settings(),
				  load_balancing_settings() },
				 [ net_utils:atom_node_name() ],
				 pid(),
				 pid(),
				 basic_utils:timestamp(),
				 file_utils:directory_name(),
				 [ computing_host_info() ] ) -> wooper:state().
construct( State, _FullSettings={ SimulationSettings,
	DeploymentSettings=#deployment_settings{ crash_resilience=ResilienceLevel },
	LoadBalancingSettings }, AllComputingNodes, RootTimeManagerPid,
	ResultManagerPid, StartTimestamp, RootDir, AvailableHosts )
  when ResilienceLevel =:= none orelse ResilienceLevel =:= 0 ->

	% Here no resilience is requested; we nevertheless set up this resilience
	% manager, as anyway its induced overhead is tiny:
	%
	CommonState = construct_common( State ),

	% This clause can be only be called during the first (and only) deployment
	% phase of a non-resilient simulation:
	%
	case RootTimeManagerPid of

		Pid when is_pid( Pid ) ->
			ok

	end,

	FinalState = setAttributes( CommonState, [

		{ resilience_level, 0 },
		{ serialisation_period, undefined },
		{ spof_nodes, AllComputingNodes },
		{ protected_nodes, [] },
		{ k_map, undefined },
		{ node_table, undefined },
		{ last_serialisation_timestamp, undefined },
		{ serialisation_history, [] },
		{ root_time_manager_pid, RootTimeManagerPid },
		{ result_manager_pid, ResultManagerPid },
		{ time_tracker_pid, undefined },
		{ serialisation_enabled, false },
		{ lost_nodes, [] },
		{ net_tick_time,
		 class_DeploymentManager:get_inter_node_tick_time_out() * 1000 },
		{ simulation_settings, SimulationSettings },
		{ deployment_settings, DeploymentSettings },
		{ load_balancing_settings, LoadBalancingSettings },
		{ start_timestamp, StartTimestamp },
		{ engine_root_dir, RootDir },
		{ available_hosts, AvailableHosts },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

											   ] ),

	?send_info( FinalState, to_string( FinalState ) ),

	FinalState;



construct( State, _FullSettings={ SimulationSettings,
	DeploymentSettings=#deployment_settings{ crash_resilience=ResilienceLevel },
	   LoadBalancingSettings }, AllComputingNodes, RootTimeManagerPid,
	ResultManagerPid, StartTimestamp, RootDir, AvailableHosts )
  when is_integer( ResilienceLevel ) andalso ResilienceLevel > 0 ->

	% Here some resilience was required.

	% We will wait for any node crash from now then:
	%
	% (we prefer not collecting the exact reason - usually 'net_tick_timeout' -
	% as then the received message, like:
	% '{ nodedown, '<CASE>-<USER>@<HOST>', [ { nodedown_reason, Reason } ] }.'
	% could not easily be mapped to a WOOPER message without an intermediary
	% process; so we chose not receive the reason)
	%
	%ok = net_kernel:monitor_nodes( _NewSubscription=true,
	%  [ nodedown_reason ] ),
	%
	% Will trigger the nodedown/2 oneway:
	%
	ok = net_kernel:monitor_nodes( _NewSubscription=true, [] ),

	%[ monitor_node( Node, _MonitoringOn=true ) || Node <- AllComputingNodes ],

	% We need to register to the root time manager, to know when the resilience
	% is to be activated (implies becoming a simulation listener); request,
	% hence result waited, at the end of that constructor:
	RootTimeManagerPid ! { registerResilienceManager, [], self() },

	CommonState = construct_common( State ),

	% Some basic checkings (no duplicates expected):
	case length( AllComputingNodes ) - length(
								 list_utils:uniquify( AllComputingNodes ) ) of

		0 ->
			ok;

		_ ->
			throw( { duplicate_in_node_list, AllComputingNodes } )

	end,

	SerialisationPeriod =
		DeploymentSettings#deployment_settings.serialisation_period,

	% We have to protect all computing nodes (including the SPOF ones):
	%
	% (at start-up, we strictly demand that we have enough nodes)
	%
	KMap = build_k_map( ResilienceLevel, AllComputingNodes ),

	TmpDir = class_DeploymentManager:determine_temporary_directory(
												  DeploymentSettings ),

	DeploymentBaseDir =
		class_DeploymentManager:get_deployment_base_directory_for(
				SimulationSettings#simulation_settings.simulation_name,
				TmpDir, StartTimestamp ),

	ResilienceDirBin = text_utils:string_to_binary(
				file_utils:join( DeploymentBaseDir, "resilience-snapshots" ) ),

	% Creates the resilience agents as well:
	NodeTable = create_node_table( AllComputingNodes, KMap, ResilienceDirBin ),

	% Knowing that, on larger simulations, the creation of the initial situation
	% may be very long, we take this action into account when protecting from a
	% crash (thus we record the starting time at soon as possible, so that the
	% first serialisation does not happen too late):
	%
	InitialSerialisationTimestamp = StartTimestamp,

	NetTickTime = class_DeploymentManager:get_inter_node_tick_time_out(),

	FinalState = setAttributes( CommonState, [

		{ resilience_level, ResilienceLevel },
		{ serialisation_period, SerialisationPeriod },
		{ spof_nodes, [ node() ] },
		{ protected_nodes, AllComputingNodes },
		{ k_map, KMap },
		{ node_table, NodeTable },

		% Starts tracking time from now:
		{ last_serialisation_timestamp, InitialSerialisationTimestamp },

		{ serialisation_history, [] },
		{ root_time_manager_pid, RootTimeManagerPid },
		{ result_manager_pid, ResultManagerPid },
		{ time_tracker_pid, undefined },
		{ serialisation_enabled, false },
		{ lost_nodes, [] },
		{ net_tick_time, NetTickTime * 1000 },
		{ simulation_settings, SimulationSettings },
		{ deployment_settings, DeploymentSettings },
		{ load_balancing_settings, LoadBalancingSettings },
		{ start_timestamp, StartTimestamp },
		{ engine_root_dir, RootDir },
		{ available_hosts, AvailableHosts },
		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

											   ] ),

	?send_info( FinalState, to_string( FinalState ) ),

	% Answer to registerResilienceManager, for synchronisation:
	receive

		{ wooper_result, registered } ->
			ok

	end,

	FinalState.



% Elements shared by all constructors.
%
% Returns an updated state.
%
% (helper)
%
construct_common( State ) ->

	% We want the user process to be aware of any failure of this resilience
	% manager:
	%
	erlang:link( basic_utils:get_registered_pid_for( ?user_process_name,
													global ) ),

	TraceState = class_TraceEmitter:construct( State, "Resilience Manager" ),

	% Ensures also it is a singleton indeed:
	basic_utils:register_as( ?resilience_manager_name, ?registration_type ),

	class_InstanceTracker:register_agent( ?resilience_manager_name ),

	TraceState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting resilience manager." ),

	?getAttr(root_time_manager_pid) ! unregisterResilienceManager,

	% For symmetry with construction:
	[ monitor_node( Node, _MonitoringOn=false )
	  || Node <- ?getAttr(protected_nodes) ],

	case ?getAttr(time_tracker_pid) of

		undefined ->
			ok;

		Pid ->
			Pid ! delete

	end,

	basic_utils:unregister( ?resilience_manager_name, ?registration_type ),

	class_InstanceTracker:unregister_agent(),


	?debug( "Resilience manager deleted." ),

	% Then allow chaining:
	State.





% Methods section.


% Simulation listener section.
%
% The resilience manager being a simulation listener (registered to the root
% time manager), it must implement the following API.




% Callback triggered by the root time manager, as we are a simulation listener.
%
% Note: called iff an actual resilience was requested.
%
% (oneway)
%
-spec simulation_started( wooper:state() ) -> oneway_return().
simulation_started( State ) ->

	% From now on, serialisations can occur.

	% Let's start counting time until next serialisation.

	% In this method the last timestamp actually corresponds to the simulation
	% initial timestamp (always defined):
	%
	InitialSerialisationTimestamp = ?getAttr(last_serialisation_timestamp),

	CurrentTimestamp = basic_utils:get_timestamp(),

	Period = ?getAttr(serialisation_period),

	% It is a bit convoluted, but we need the last timestamp to create the time
	% tracker, which is itself needed for trigger_serialisation/3:
	%
	IsTimeForSnapshot = basic_utils:get_duration(
	   InitialSerialisationTimestamp, CurrentTimestamp ) > Period,

	LastTimestampWatchdog = case IsTimeForSnapshot of

								true ->
									serialisation_in_progress;

								false ->
									InitialSerialisationTimestamp

	end,

	% Now, let's start the time-tracking: we will then know when an inter-tick
	% duration will be appropriate for a serialisation.

	% Closures...
	ResilienceManagerPid = self(),

	% The watchdog ensures the manager does not get stuck:
	% (closure used to avoid exporting the function)
	TimeTrackerPid = spawn_link( fun() -> time_tracker_main_loop(
				  ResilienceManagerPid, LastTimestampWatchdog, Period ) end ),

	EnabledState = setAttributes( State, [

						{ serialisation_enabled, true },
						{ time_tracker_pid, TimeTrackerPid }

										  ] ),


	NewState = case IsTimeForSnapshot of

		true ->

			% Time for the first serialisation (we want to be able to back-up
			% even initial actor creations, which might also be arbitrarily
			% long); we are necessarily at {0,0}:
			%
			% (note: trigger_serialisation/3 requires time_tracker_pid to be
			% already set)
			%
			trigger_serialisation( _InitialTickOffset=0,_InitialDiasca=0,
								   EnabledState );

		false ->
			EnabledState

	end,

	?wooper_return_state_only( setAttribute( NewState, time_tracker_pid,
											 TimeTrackerPid ) ).



% Callback triggered by the root time manager, as we are a simulation listener.
%
% (oneway)
%
-spec simulation_suspended( wooper:state() ) -> oneway_return().
simulation_suspended( State ) ->
	?wooper_return_state_only( State ).



% Callback triggered by the root time manager, as we are a simulation listener.
%
% (oneway)
%
-spec simulation_resumed( wooper:state() ) -> oneway_return().
simulation_resumed( State ) ->
	?wooper_return_state_only( State ).



% Callback triggered by the root time manager, as we are a simulation listener.
%
% (oneway)
%
-spec simulation_succeeded( wooper:state() ) -> oneway_return().
simulation_succeeded( State ) ->
	% Exact same code:
	simulation_stopped( State ).



% Callback triggered by the root time manager, as we are a simulation listener.
%
% (oneway)
%
-spec simulation_stopped( wooper:state() ) -> oneway_return().
simulation_stopped( State ) ->

	case ?getAttr(time_tracker_pid) of

		undefined ->
			ok;

		Pid ->
			Pid ! delete

	end,

	?wooper_return_state_only( setAttributes( State, [

					{ time_tracker_pid, undefined },
					{ serialisation_enabled, false }

													 ] ) ).




% Notifies that a serialisation is requested (expected to be called by the
% internal tracker process).
%
% (oneway)
%
-spec serialisationRequested( wooper:state() ) -> oneway_return().
serialisationRequested( State ) ->

	case ?getAttr(serialisation_enabled) of

		true ->
			?debug( "Serialisation requested to the root time manager." ),

			% We will thus be notified as soon as the current diasca is over:
			?getAttr(root_time_manager_pid) ! serialisationRequested;

		false ->
			ok

	end,

	% We cannot proceed instantly, as the serialisation must be during an
	% inter-diasca (the next to happen, thus) to rely on a stable, unchanging
	% state:
	%
	?wooper_return_state_only( State ).



% Called (presumably by the root time manager) whenever we are in an
% inter-diasca moment and when a serialisation was previously requested, so that
% it can be performed immediately.
%
% Note: we are thus blocking the whole simulation during this call - beware to
% induced latency.
%
% (request)
%
-spec triggerSerialisation( wooper:state(), class_TimeManager:tick_offset(),
	   class_TimeManager:diasca() ) -> request_return( 'serialisation_done' ).
triggerSerialisation( State, Tick, Diasca ) ->

	SerialisedState = trigger_serialisation( Tick, Diasca, State ),

	?wooper_return_state_result( SerialisedState, serialisation_done ).



% Called automatically by net_kernel (since net_kernel:monitor_nodes/2 was used)
% whenever a computing node is deemed lost, so that a simulation rollback is
% performed immediately.
%
% (oneway)
%
nodedown( State, Node ) ->

	%?info_fmt( "Node '~s' reported as lost.", [ Node ] ),

	NewState = case ?getAttr(serialisation_history) of

				   [] ->
					   throw( { unrecoverable_node_crash,
							   no_previous_serialisation_done } );

				   [ _Latest={ Tick, Diasca } | _RestOfHistory ] ->

					   % As multiple 'nodedown' messages may be received almost
					   % at once (ex: a part of the network may have collapsed),
					   % we try first to establish the full list of down nodes
					   % before going for a rollback, in order to reduce the
					   % risk that it fails or must be performed multiple times
					   % because of 'node down' messages not processed yet:
					   %
					   CrashedNodes = collect_crash_reports( [ Node ] ),

					   perform_rollback( Tick, Diasca, CrashedNodes, State )

	end,

	?wooper_return_state_only( NewState ).



% Performs a rollback to specified simulation timestamp.
%
% Returns an updated state.
%
% (helper)
%
perform_rollback( Tick, Diasca, CrashedNodes, State ) ->

	CrashCount = length( CrashedNodes ),

	ResilienceLevel = ?getAttr(resilience_level),

	% Here we check we simply *can* overcome the past crash:
	case ResilienceLevel of

		L when CrashCount > L ->

			?fatal_fmt( "A crash of ~B computing nodes (~p) was detected. "
						"The selected resilience level (~B) cannot cope with "
						"so many crashes, no simulation rollback can "
						"overcome that.", [ CrashCount, CrashedNodes, L ] ),

			throw( { insufficient_resilience_level, L, CrashCount } );

		_ ->
			ok

	end,

	?error_fmt( "Infrastructure failure detected; ~B computing node(s) "
				"reported as lost:~s~nPerforming now a rollback to "
				"last completed serialisation timestamp "
				"(tick offset ~B, diasca ~B) to overcome the crash, "
				"since the selected resilience level (~B) allows for that.",
				[ CrashCount, text_utils:atom_list_to_string( CrashedNodes ),
				  Tick, Diasca, ResilienceLevel ] ),


	% Here, most processes on most nodes have been terminated (for example, all
	% time managers are linked to the deployment manager, hence all actors, all
	% probes, most simulation agents have been terminated in turn).
	%
	% Actually, apart the processes of the lower layers (like Erlang, WOOPER and
	% Traces), almost no (Sim-Diasca-related) process survived, except this one
	% (the resilience manager) and the resilience agents. We are starting again
	% from a rather blank state.

	% We here have first to secure the serialisation content that was previously
	% held by the lost nodes.

	AllNodes = ?getAttr(protected_nodes),

	% First, we list the securing nodes that are still alive:
	SurvivingNodes = lists:subtract( AllNodes, CrashedNodes ),

	% Now we have to determine where are the replicas corresponding to the lost
	% nodes; as a surviving node may replicate several lost nodes but,
	% conversely, a given lost node may be replicated on multiple surviving
	% nodes, we try to smooth the load on survivors (one of them should not take
	% care of too many lost nodes while the other survivors would remain mostly
	% idle).
	%
	% For that we create a list of { SurvivingNode, LostNodes } where
	% SurvivingNode is a node that survived and LostNodes is a non-empty list of
	% the lost nodes it is from now responsible of.

	% We will count, for each surviving node, the number of nodes it is to
	% secure, so that none is too loaded:
	%
	WeightedNodes = [ { Node, _SecuredList=[] } || Node <- SurvivingNodes ],

	DispatchedNodes = dispatch_serialisations( CrashedNodes, WeightedNodes,
											   ?getAttr(k_map) ),

	SurvivorCount = length( SurvivingNodes ),

	?info_fmt( "On a total of ~B nodes, ~B node(s) crashed:~s"
			   "~B node()s survived:~s"
			   "Crashed nodes have been dispatched onto surviving ones "
			   "this way:~s",
			   [ length( AllNodes ), length( CrashedNodes ),
				 text_utils:atoms_to_string( CrashedNodes ),
				 SurvivorCount,
				 text_utils:atoms_to_string( SurvivingNodes ),
				 text_utils:strings_to_string(
				   [ io_lib:format( "surviving node '~s' taking care of '~p'",
						[ Survivor, SecuredList ] )
					 || { Survivor, SecuredList } <- DispatchedNodes ] ) ] ),

	% We keep only the information relative to surviving hosts:
	SurvivingHosts = [ Entry || Entry <- ?getAttr(available_hosts),
			not lists:member( Entry#computing_host_info.node_name,
							  CrashedNodes ) ],

	ListedState = setAttributes( State, [

			{ lost_nodes, CrashedNodes ++ ?getAttr(lost_nodes) },
			{ available_hosts, SurvivingHosts }

										 ] ),

	ServiceState = recreate_simulation_services( SurvivingNodes, CrashedNodes,
												 ListedState ),

	% To be able to retrieve the PID of each surviving resilience agent:
	NodeTable = ?getAttr(node_table),

	% All surviving nodes are to work in parallel:

	SurvivorAgentPidList = [

			begin

				SurvivorAgentPid = ?hashtable_type:getEntry( _K=Node,
															NodeTable ),

				SurvivorAgentPid ! { recoverNodes,
									[ SecuredList, Tick, Diasca ], self() },

				SurvivorAgentPid

			end || { Node, SecuredList } <- DispatchedNodes ],

	% Now that the deserialisation work is triggered, performing in parallel
	% other tasks, like rebuilding the k-map (for any next crash):

	PreparedState = prepare_next_crash( ResilienceLevel, SurvivingNodes,
							NodeTable, ServiceState ),

	% Up to two hours of waiting:
	MaxDurationInSeconds = 3600 * 2,

	InitialTimestamp = basic_utils:get_timestamp(),

	case wait_for_recoveries( SurvivorAgentPidList, InitialTimestamp,
							  MaxDurationInSeconds ) of

		success ->

			FinalTimestamp = basic_utils:get_timestamp(),

			Duration = basic_utils:get_duration( InitialTimestamp,
												 FinalTimestamp ),

			?debug_fmt( "All ~B involved nodes notified that their recovery "
					   "succeeded, the operation took ~s.",
					   [ SurvivorCount,
						text_utils:duration_to_string( Duration ) ] ),

			% Instances have been just loaded now, we had to wait for all of
			% them to have notified the instance trackers, before starting the
			% relinking:
			%
			relink_instances( SurvivorAgentPidList, PreparedState );


		failure->

			?error_fmt( "The recovery over ~B nodes failed, we suspect "
						"that we lost at least one more node in its course, "
						"trying to perform a new rollback then.",
						[ SurvivorCount ] ),

			% We do not forget what we did in this failed rollback, but we
			% expect the next received message(s) will be nodedown, leading to a
			% new rollback:
			%
			PreparedState

	end.



% Repopulates the surviving nodes with adequate agents.
%
% Returns an updated state.
%
recreate_simulation_services( SurvivingNodes, _CrashedNodes, State ) ->

	?debug_fmt( "Recreating the simulation agents onto the set of "
				"surviving nodes: ~p.", [ SurvivingNodes ] ),

	% All services but the resilience one disappeared due to the crash, hence
	% shall be deployed again:
	%
	% (we use a link here too, even if setting it now the other round - which
	% does not change anything: we want to resist to next crashes as before, it
	% is dealt with trap_exit, see sim_diasca:init/3)
	%
	% (not remote: this manager and the deployment one shall be on the user
	% node)
	%
	% (of course we do not want to go through selecting the hosts again,
	% connecting to them with SSH, as it is already done; we must thus use a
	% specific constructor for that)

	% To adapt to current known system state:
	Context = { SurvivingNodes, ?getAttr(start_timestamp),
				?getAttr(engine_root_dir), ?getAttr(available_hosts) },

	_DeploymentManagerPid = class_DeploymentManager:synchronous_new_link(
			?getAttr(simulation_settings), ?getAttr(deployment_settings),
			?getAttr(load_balancing_settings), Context ),

	State.



% Reconfigures the resilience service so that any next crash can be best
% resisted.
%
% Returns an updated state.
%
% (helper)
%
prepare_next_crash( ResilienceLevel, SurvivingNodes, NodeTable, State ) ->

	NodeCount = length( SurvivingNodes ),

	MaxResilienceLevel = NodeCount - 1,

	case ResilienceLevel > MaxResilienceLevel of

		true ->

			% Thus we go for a level of MaxResilienceLevel here.

			case MaxResilienceLevel of

				0 ->

					?warning_fmt( "Not enough remaining nodes (~B) "
								  "after the crash in order to sustain "
								  "the requested resilience level (~B);"
								  " the simulation will nevertheless attempt "
								  "to continue, without any resilience margin.",
								  [ NodeCount, ResilienceLevel ] ),

					setAttributes( State, [

						   { resilience_level, 0 },
						   { protected_nodes, [] },
						   { k_map, undefined },
						   { node_table, undefined },
						   { serialisation_enabled, false }

										   ] );

				NewResilienceLevel ->
					?warning_fmt( "Not enough remaining nodes (~B) "
								  "after the crash in order to sustain "
								  "the requested resilience level (~B);"
								  " the simulation will nevertheless attempt "
								  "to continue with resilience level ~B.",
								  [ NodeCount, ResilienceLevel,
								   NewResilienceLevel ] ),

					NewKMap = build_k_map( NewResilienceLevel, SurvivingNodes ),

					UpdatedNodeTable = update_node_table( SurvivingNodes,
														 NodeTable, NewKMap ),

					setAttributes( State, [

						{ resilience_level, NewResilienceLevel },
						{ protected_nodes, SurvivingNodes },
						{ k_map, NewKMap },
						{ node_table, UpdatedNodeTable }

												 ] )

			end

	end.






% Waits for all resilience agents to report their recovery actions.
%
% (helper)
%
wait_for_recoveries( _WaitedAgents=[], _InitialTimestamp, _MaxDuration ) ->
	success;

wait_for_recoveries( WaitedAgents, InitialTimestamp, MaxDuration )  ->

	receive

		{ wooper_result, { nodes_recovered, AgentPid } } ->

			case lists:member( AgentPid, WaitedAgents ) of

				true ->
					RemainingAgents = lists:delete( AgentPid, WaitedAgents ),
					wait_for_recoveries( RemainingAgents, InitialTimestamp,
										 MaxDuration );

				false ->
					throw( { unexpected_resilience_agent, AgentPid } )

			end

	% Update every second:
	after 1000 ->

			NewDuration = basic_utils:get_duration( InitialTimestamp,
											  basic_utils:get_timestamp() ),

			case NewDuration > MaxDuration of

				true ->
					% We want a chance of recovering nevertheless:
					%throw( { recovery_time_out, WaitedAgents } );
					failure;

				false ->
					% Still waiting then:
					wait_for_recoveries( WaitedAgents, InitialTimestamp,
										 MaxDuration )

			end

	end.




% A weighted node is a { Node, AssignedCrashedNodes } pair.
%
% (helper)
%
dispatch_serialisations( _CrashedNodes=[], WeightedNodes, _KMap ) ->

	% All crash nodes assigned.
	%
	% Remove unassigned survivors:
	[ { Node, SecuredList } || { Node, SecuredList } <- WeightedNodes,
							   SecuredList =/= [] ];

dispatch_serialisations( _CrashedNodes=[ N | T ], WeightedNodes, KMap ) ->

	% We must find the surviving nodes that secure N:
	Candidates = ( ?hashtable_type:getEntry( _K=N, KMap ) )#k_record.secured_by,

	% Subset of the weighted nodes that can take care of N, others:
	{ CandidatesWeightedNodes, OtherWeightedNodes } = lists:partition(
			fun( { Node, _List } ) ->
					lists:member( Node, Candidates )
			end,
			WeightedNodes ),

	UpdatedWeightedNodes = select_least_loaded( CandidatesWeightedNodes, N ),

	dispatch_serialisations( T, UpdatedWeightedNodes ++ OtherWeightedNodes,
							 KMap ).




% Selects, among the specified weighted nodes, the one that shall be used to
% retrieve the serialisation information for the specified crashed node.
select_least_loaded( WeightedNodes, N ) ->

	ElectedNode = find_min( WeightedNodes, _Acc=undefined ),

	update_weighted_nodes( ElectedNode, N, WeightedNodes, [] ).




% We store the length to avoid computing it multiple times:
find_min( _WeightedNodes=[], undefined ) ->
	throw( no_weighted_node_found );

% We have { Node, List } in input, but we accumulate { Node, Len }:
find_min( _WeightedNodes=[], _Found={ Node, _Len } ) ->
	Node;

find_min( _WeightedNodes=[ { Node, List } | T ], undefined ) ->
	find_min( T, { Node, length( List ) } );

find_min( _WeightedNodes=[ { Node, List } | T ],
		  CurrentEntry={ _CurrentNode, CurrentLen } ) ->

	% Better than using guards (to compute length only once):
	Len = length( List ),

	case Len < CurrentLen of

		true ->
			find_min( T, _NewBest={ Node, Len } );

		false ->
			% Unchanged:
			find_min( T, CurrentEntry )

	end.



% Adds specified node N into the entry of the selected node.
%
update_weighted_nodes( ElectedNode, N,
					   _WeightedNodes=[ { ElectedNode, List } | T ],  Acc ) ->
	% Found!
	[ { ElectedNode, [ N | List ] } | T ] ++ Acc;

update_weighted_nodes( ElectedNode, N, _WeightedNodes=[ H | T ], Acc ) ->
	% Keep on recursing:
	update_weighted_nodes( ElectedNode, N, T, [ H | Acc ] ).



% Replaces, in each deserialised instance, the restoration markers by the
% corresponding newer PIDs.
%
% Returns an updated state.
%
% (helper)
%
relink_instances( SurvivorAgentPidList, State ) ->

	SurvivorCount = length( SurvivorAgentPidList ),

	% Up to one hour of waiting:
	MaxDurationInMilliseconds = 3600 * 1000,

	case wooper:send_requests_and_wait_acks( _Req=relinkInstances, _Args=[],
			SurvivorAgentPidList, MaxDurationInMilliseconds,
			_AckAtom=node_relinked ) of

		success ->

			?debug_fmt( "All ~B nodes notified that their relinking succeeded.",
						[ SurvivorCount ] ),

			State;


		{ failure, FailedPid } ->

			?error_fmt( "The relinking over ~B nodes failed (~B of them did "
						"not answer, namely: ~p), we suspect that we lost "
						"at least one more node in its course, "
						"trying to perform a new rollback then.",
						[ SurvivorCount, length( FailedPid ), FailedPid ] ),

			% We do not forget what we did in this failed rollback, but we
			% expect the next received message(s) will be nodedown, leading to a
			% new rollback:
			%
			State

	end.




% Returns a textual description of this manager.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).




% Static methods section.



% Section for helper functions (not methods).



% Loop of the internal process to wait for serialisation to be needed.
%
% We want the period to be enforced before the end of serialisation N and the
% beginning of serialisation N+1, not between the beginning of the two
% serialisations (i.e. the period should not be decreased of the duration of the
% serialisation itself, as it may last for long, even longer than the period).
%
time_tracker_main_loop( ResilienceManagerPid,
					   _LastTimestamp=serialisation_in_progress, Period ) ->

	% A serialisation is in progress; we are waiting for it to end here:
	receive

		delete ->
			% Dies by non-recursion, then:
			ok;

		serialisation_finished ->
			% We can begin again tracking time for the next serialisation:
			time_tracker_main_loop( ResilienceManagerPid,
				   _NewLastTimestamp=basic_utils:get_timestamp(), Period )

	end;


time_tracker_main_loop( ResilienceManagerPid, LastTimestamp, Period ) ->

	CurrentTimestamp = basic_utils:get_timestamp(),

	ElapsedDuration = basic_utils:get_duration( LastTimestamp,
												CurrentTimestamp ),

	RemainingDuration = Period - ElapsedDuration,

	case RemainingDuration > 0 of

				true ->
					% Still waiting then:
					receive

						delete ->
							% Dies by non-recursion, then:
							ok

					% Milliseconds:
					after RemainingDuration * 1000 ->
							% Next recursion expected to be a serialisation one:
							time_tracker_main_loop( ResilienceManagerPid,
													LastTimestamp, Period )

					end;

				false ->
					% Time for a new serialisation:
					ResilienceManagerPid ! serialisationRequested,

					% No ending timestamp yet to record:
					time_tracker_main_loop( ResilienceManagerPid,
							_LastTimestamp=serialisation_in_progress, Period )

	end.



% Returns a textual description of this instance.
%
% (helper)
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	SPOFNodes = ?getAttr(spof_nodes),
	ProtectedNodes = ?getAttr(protected_nodes),

	ResilienceLevel = ?getAttr(resilience_level),

	DurationString = case ?getAttr(serialisation_period) of

		undefined ->
			 "(no serialisation period applies)";

		D ->
			io_lib:format( ", with a serialisation period of ~s",
						   [ text_utils:duration_to_string( 1000 * D ) ] )

	end,

	io_lib:format( "Resilience manager set to level k=~B ~s,"
				   "managing ~B protected node(s):~s"
				   "having ~B single-point of failure node(s):~s"
				   "and using following k-map:~n~s. Serialisation history: ~p",
				   [ ResilienceLevel,
					 DurationString,
					 length( ProtectedNodes ),
					 text_utils:atom_list_to_string( ProtectedNodes ),
					 length( SPOFNodes ),
					 text_utils:atom_list_to_string( SPOFNodes ),
					 k_map_to_string( ?getAttr(k_map) ),
					 lists:reverse( ?getAttr(serialisation_history) )
					] ).



% Returns a textual description of this k-record.
%
% (helper)
%
-spec k_record_to_string( k_record() ) -> string().
k_record_to_string( #k_record{ securing=Securing,
							   secured_by=SecuredBy } ) ->

	SecuringString = case Securing of

						 [] ->
							 "no node";

						 _ ->
							 io_lib:format( "nodes ~p", [ Securing ] )

	end,


	SecuredByString = case SecuredBy of

						  [] ->
							  "no node";

						  _ ->
							  io_lib:format( "nodes ~p", [ SecuredBy ] )

	end,

	io_lib:format( "~n  - securing ~s~n  - being secured by ~s~n",
				   [ SecuringString, SecuredByString ] ).



% Returns a textual description of this k-map.
%
% (helper)
%
-spec k_map_to_string( k_map() ) -> string().
k_map_to_string( _KMap=undefined ) ->
	"no k-map defined";

k_map_to_string( KMap ) ->
	% Creates a reproducible order:
	KPairs = lists:keysort( _Index=1, ?hashtable_type:enumerate( KMap ) ),

	KString = text_utils:string_list_to_string( [
		io_lib:format( "for node ~s: ~s", [ N, k_record_to_string(R) ] )
				  || { N, R } <- KPairs ] ),

	io_lib:format( "k-map for ~B nodes: ~s",
				   [ length( KPairs ), KString ] ).





% Builds a k-map for specified resilience level.
%
% (state specified for traces)
%
% (helper)
%
-spec build_k_map( basic_utils:count(), node_list() ) -> k_map().
build_k_map( _ResilienceLevel, _ProtectedNodes=[] ) ->
	% Empty by design:
	?hashtable_type:new();

build_k_map( ResilienceLevel, _ProtectedNodes )
  when ResilienceLevel =:= 0 orelse ResilienceLevel =:= none ->
	% Empty by design as well:
	?hashtable_type:new();

build_k_map( ResilienceLevel, ProtectedNodes ) ->

	NodeCount = length( ProtectedNodes ),

	MaxResilienceLevel = NodeCount - 1,

	case ResilienceLevel > MaxResilienceLevel of

		true ->

			throw( { too_few_nodes_for_requested_resilience, NodeCount,
							ResilienceLevel } );

		false ->

			% Here we still have enough nodes, so we stick to ResilienceLevel.

			% We want to correctly associate secured and securing nodes, in a
			% fair, systematic way.
			%
			% Let's suppose we have 5 nodes L = [ a, b, c, d, e ] and k=3.
			%
			% a would be secured by [ b, c, d ], b by [ c, d, e ] and so on, and
			% reverse dependencies (ex: c securing a, b and another node) would
			% be recorded at the same time.
			%
			% For that we simply, given a node (ex: a) select the k next
			% elements in L, transforming the list into a ring, whose next
			% element after its last is its first again, and so one (as if the
			% list was actually the infinite one [ a, b, c, d, e, a, b, ... ]).
			%
			RingNodes = list_utils:list_to_ring( ProtectedNodes ),

			% We remove here the head, as, taking L as an example, a must be
			% associated to [ b, c, d ], not to [ a, b, c]:
			{ _FirstNodeName, InitialRingNodes } = list_utils:head( RingNodes ),

			register_nodes( ProtectedNodes, InitialRingNodes, ResilienceLevel,
							_InitialKMap=?hashtable_type:new( NodeCount ) )

	end.



% Registers nodes one by one in the k-map, thanks to k-records.
%
% For each node, we determine the nodes that it secures, and we let it know to
% each of them, to establish reverse dependencies.
%
% (helper)
%
register_nodes( _ProtectedNodes=[], _RingNodes, _ResilienceLevel, KMap ) ->
	KMap;

register_nodes( _ProtectedNodes=[ N | T ], RingNodes, ResilienceLevel, KMap ) ->

	% The ResilienceLevel nodes that will be secured by node N:
	{ SecuredNodes, _UselessRing } = list_utils:get_next(
										 _Count=ResilienceLevel, RingNodes ),

	KRecord = obtain_k_record_for( N, KMap ),
	NewKRecord = KRecord#k_record{ securing=SecuredNodes },

	% Reverse registering:
	SecuredKMap = add_securing_node( N, SecuredNodes, KMap ),

	UpdatedKMap = ?hashtable_type:addEntry( _K=N, _V=NewKRecord, SecuredKMap ),

	% We progressed of one element in the ring:
	{ _NextN, NewRingNodes } = list_utils:head( RingNodes ),

	register_nodes( T, NewRingNodes, ResilienceLevel, UpdatedKMap ).



% Registers the securing node into all its secured ones.
%
% (helper)
%
add_securing_node( SecuringNode, SecuredNodes, KMap ) ->

	lists:foldl( fun( SecuredNode, AccKMap ) ->

		% We must tell SecuredNode that it is secured by SecuringNode:
		KRecord = obtain_k_record_for( SecuredNode, AccKMap ),

		NewSecuringNodes = [ SecuringNode | KRecord#k_record.secured_by ],
		NewKRecord= KRecord#k_record{ secured_by=NewSecuringNodes },
		?hashtable_type:addEntry( _K=SecuredNode, _V=NewKRecord, AccKMap )

				 end,
				 _InitialAcc=KMap,
				 _List=SecuredNodes ).



% Returns any pre-existing k-record for specified node, otherwise a newly
% created one.
%
obtain_k_record_for( Node, KMap ) ->

	case ?hashtable_type:lookupEntry( _K=Node, KMap ) of

		hashtable_key_not_found ->
			% Return a blank, new k-record then (first access):
			#k_record{};

		{ value, KRecord }->
			KRecord

	end.



% Creates the node table, which includes creating the corresponding resilience
% agents, on each node.
%
-spec create_node_table( [ node_name() ], k_map(), text_utils:bin_string() ) ->
							   node_table().
create_node_table( AllComputingNodes, KMap, ResilienceDirBin ) ->

	lists:foldl( fun( Node, AccTable ) ->

		NodeKRecord = ?hashtable_type:getEntry( _K=Node, KMap ),

		Securing = NodeKRecord#k_record.securing,
		SecuredBy = NodeKRecord#k_record.secured_by,

		% No process-link wanted: we want to resist to node losses!
		AgentPid = class_ResilienceAgent:remote_synchronous_new( Node,
			   self(), Securing, SecuredBy, ResilienceDirBin ),

		?hashtable_type:addEntry( _Key=Node, _Value=AgentPid, AccTable )

				 end,
				 _Acc0=?hashtable_type:new(),
				 _List=AllComputingNodes ).



% Updates the node table (i.e. create a new table and update the corresponding
% resilience agents).
%
-spec update_node_table( [ node_name() ], node_table(), k_map() )
					   -> node_table().
update_node_table( SurvivingNodes, PreviousNodeTable, KMap ) ->

	lists:foldl( fun( Node, AccTable ) ->

		% Retrieving first the PID of the corresponding agent:
		AgentPid = ?hashtable_type:getEntry( _K=Node, PreviousNodeTable ),

		% Fetching the new k-record for this node:
		NodeKRecord = ?hashtable_type:getEntry( _K=Node, KMap ),

		Securing = NodeKRecord#k_record.securing,
		SecuredBy = NodeKRecord#k_record.secured_by,

		% Notifying the agent:
		AgentPid ! { updateResilienceMapping, [ Securing, SecuredBy ] },

		% By design failed nodes are not referenced anymore:
		?hashtable_type:addEntry( _Key=Node, _Value=AgentPid, AccTable )

				 end,
				 _Acc0=?hashtable_type:new(),
				 _List=SurvivingNodes ).



% Triggers an actual full serialisation of the simulation state, for a later
% re-use in case of computing node crash(es).
%
% Returns an updated state.
%
% (helper)
%
-spec trigger_serialisation( class_TimeManager:tick_offset(),
	   class_TimeManager:diasca(), wooper:state() ) -> wooper:state().
trigger_serialisation( Tick, Diasca, State ) ->

	case ?getAttr(serialisation_enabled) of

		false ->
			throw( { serialisation_triggered_whereas_disabled } );

		true ->
			ok

	end,

	SerialisationCount = length( ?getAttr(serialisation_history) ) + 1,

	NodeAgents = [ AgentPid || { _Node, AgentPid } <-
						   ?hashtable_type:enumerate( ?getAttr(node_table) ) ],

	% Each resilience agent will need to know the list of the local probes it
	% should serialise; this dispatching is done by the result manager (the only
	% agent to manage that information), let's trigger this dispatching as soon
	% as possible (the answer will be waited for later):
	%
	% (note: only managing basic probes, not virtual ones)
	%
	?getAttr(result_manager_pid) !
		{ notifyResilienceAgentsOfProbes, [ NodeAgents ], self() },

	NodeCount = length( NodeAgents ),

	?info_fmt( "Serialisation #~B triggered at tick ~p, diasca ~p on ~B nodes.",
			   [ SerialisationCount, Tick, Diasca, NodeCount ] ),


	SerialisationString = io_lib:format( "(serialisation #~B triggered "
		"at tick ~p, diasca ~p, on ~B nodes)",
		[ SerialisationCount, Tick, Diasca, NodeCount ] ),

	% A bit of ASCII-art now:
	DisplayedString = text_utils:pad_string( SerialisationString, 116 ),


	io:format( "+----------------------+----------------+--------+------------"
			   "----------+--------------+--------------+----------------+~n"
			   "| ~s |~n"
			   "+----------------------+----------------+--------+------------"
			   "----------+--------------+--------------+----------------+~n",
			   [ DisplayedString ] ),


	?debug_fmt( "~B resilience agents will be contacted for serialisation.",
				[ length( NodeAgents ) ] ),

	% Node losses during a serialisation are to be detected thanks to its then
	% too long duration (i.e. at least 2 hours):
	%
	MaxDurationInMilliseconds = max( 5 * ?getAttr(net_tick_time),
									 2 * 3600 * 1000 ),

	% In answer to notifyResilienceAgentsOfProbes:
	receive

		{ wooper_result, probes_notified } ->
			ok

	end,

	?debug( "Probes have been notified, serialisation is thus triggered now." ),

	% To be sent to all resilience agents, that now must know their probes:
	% (serialisations, peer-to-peer exchange of the resulting data, etc. will
	% then be performed with no specific need for coordination from here)
	%
	case wooper:send_requests_and_wait_acks( _Req=serialiseNode,
			_Args=[ Tick, Diasca ], NodeAgents, MaxDurationInMilliseconds,
			_AckAtom=node_serialised ) of

		success ->

			?debug( "All resilience agents answered, "
					"serialisation succeeded." ),

			% Resets the timer loop, to track future inter-serialisation time
			% later:
			%
			?getAttr(time_tracker_pid) ! serialisation_finished,

			NewHistory = [ { Tick, Diasca } | ?getAttr(serialisation_history) ],

			setAttributes( State, [

			  { last_serialisation_timestamp, basic_utils:get_timestamp() },
			  { serialisation_history, NewHistory }

								   ] );


		{ failure, WaitedAgents } ->

			Nodes = [ node( AgentPid ) || AgentPid <- WaitedAgents ],

			?error_fmt( "Serialisation interrupted, apparently ~B node(s) "
						"have been lost in its course:~s",
						[ length( Nodes ),
						  text_utils:atom_list_to_string( Nodes ) ] ),

			% We do not retain this failed serialisation attempt; before
			% processing any message sent here, nodedown ones will be handled
			% first; hence no need to send a specific message, the nodedown
			% should trigger by themselves a rollback to any ultimately relevant
			% past serialisation:
			%
			State

	end.



% Collects the various crash reports that may happen in a row (during a short
% duration).
%
% (helper)
%
collect_crash_reports( CrashedNodes ) ->

	receive

		{ nodedown, NewCrashedNode } ->
			collect_crash_reports( [ NewCrashedNode | CrashedNodes ] )

	after 2000 ->

			% No other 'node down' message expected anymore:
			CrashedNodes

	end.
