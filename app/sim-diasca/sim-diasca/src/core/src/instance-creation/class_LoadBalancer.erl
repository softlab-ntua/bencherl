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



% Load balancer.


% Its role is to manage all creation requests of simulation actors.
%
% It will ensure that the corresponding processes are dispatched according to
% specified placement policy and spawned in a reproducible order.
%
% The actors will be assigned unique and reproducible abstract actor
% identifiers, as opposed to their PID, with is a technical non-reproducible
% identifier.
%
% The actor identifier is simply an incrementing counter managed by the load
% balancer, knowing that this agent is expected to be a singleton (only one
% instance of the load balancer should run at any time for a given simulation).
%
% Moreover the load balancer is able to perform conversions between actor
% identifiers and PID, in both directions, in order to answer to look-up
% requests.
%
% The load balancer is itself an actor, as it needs to perform a reproducible
% reordering of the creation requests it receives.
%
% A shorthand for Abstract Actor Identifier is AAI.
%
% The load balancer is usually created by the deployment manager.
%
% See also class_LoadBalancer_test.erl.
%
-module(class_LoadBalancer).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_BroadcastingActor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, PlacementPolicy, Nodes,
		 NodeAvailabilityTolerance, SimulationMode, TroubleshootingMode,
		 InitialisationFiles ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, simulationEnded/1, timeManagerShutdown/1,
		 getComputingNodes/1,
		 createInitialInstancesFromFiles/3, onInstancesLoaded/1,
		 createInitialActor/3, createInitialPlacedActor/4,
		 createInitialActors/2, createActor/4, createPlacedActor/5,
		 getActorCreationInformation/4, getActorCreationInformationFromHint/4,
		 getNodeForUserIdentifier/2, registerInitialActors/2,
		 actSpontaneous/1, notifyDeletion/4, getInstanceCounts/1,
		 getOverallInstanceCount/2 ).


% Static methods declarations.
-define( wooper_static_method_export, settings_to_string/1,
		 get_registration_name/0, get_balancer/0 ).



% Hint for actor placement (co-allocation):
-type placement_hint() :: any().

-type placement_policy() :: 'round_robin' | 'select_least_loaded_first'.


-type node_availability_tolerance() :: 'fail_on_unavailable_node'
									 | 'allow_unavailable_nodes'.



-export_type([ placement_hint/0, placement_policy/0,
			  node_availability_tolerance/0 ]).


% Exported helpers:
-export([ wait_for_created_actors/2 ]).



% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.LoadBalancing").


% For notify_debug_fmt:
-include("traces.hrl").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For load_balancer_name:
-include("class_LoadBalancer.hrl").


% For evaluation_mode(), evaluation_requested_properties():
-include("class_TimeManager.hrl").


% Where the load balancer should be registered.
% Could be local_and_global or global_only as well:
%-define( registration_type, local_only ).
-define( registration_type, local_and_global ).


% Select the type you prefer:
-define( hashtable_type, lazy_hashtable ).


% Defines the default constant seed used in reproducible mode.
%
% Note that {0,0,0} does not seem to be a proper seed in pre-R15B versions.
%
-define( default_reproducible_seed, {1,7,11} ).




% Implementation notes.



% Actor identifier section.

% An actor identifier is a reproducible abstract (non technically-dependent)
% identifier of a simulation actor, a.k.a. AAI, for 'Abstract Actor Identifier'.
%
% It is a strictly positive integer, starting at 1 and incremented by the load
% balancer each time it creates an actor.
%
% During a simulation, there is a bijection between actor identifiers and the
% PID of their corresponding Erlang processes.
%
% From a simulation to another, actor PID will most probably change (ex: if the
% number of computing nodes varies, or if the placement policy in use takes
% dynamically into account the load of the computers), but a given actor should
% always have the same actor identifier assigned to it.


% The load balancer has no real life of its own, it is mainly triggered whenever
% an actor is to be created, so it can remain mostly passive.


% Previously the load balancer had to send information about created actor
% instances to the relevant instance tracker and to maintain a node-based
% tracker look-up table to target the right one.
%
% The load balancer does not communicate anymore with an instance tracker, as it
% could not manage at least one useful actor-level information: its name. Thus
% the actor had, one way or another, to update anyway a tracker with its name
% (preferentially when subscribing to its time manager). Having to send its
% name, the actor can go a little further and send all the information needed at
% once (ex: classname), and the load balancer does not need anymore to be
% involved there.


% Previously the seeding of each actor was done by its local time manager. Now,
% as anyway the load balancer is able to send information to each actor when
% creating it (ex: its AAI), its seed is specified as well.
%
% This design is simpler, and easily allows for a seeding which does not depend
% on the number of available computing nodes, as it is centralised by the
% load-balancer (instead of being distributed among time managers, which would
% make the outcome of the simulations depend on the number of involved computing
% hosts).
%
% Hence to any AAI a unique, reproducible seed must correspond. This is an issue
% when loading instances from file, as they are presented to the load balancer
% in any order. As a result, the loader balancer must maintain a table of
% pre-generated random seeds, indexed by the corresponding AAI. As this table
% may grow and shrink (depending on the order of the actor presentations), a
% hashtable is more convenient (see seed_table).


% The load balancer is both a simulation agent and an actor, which requires, in
% some cases, extra care (ex: when deserialising a simulation).



% Regarding initialisation:
%
% Creating instances before the simulation starts can be done either
% programmatically (typically directly from the simulation case, from the method
% of a scenario, etc.) or based on data (typically from a set of initialisation
% files).
%
% The 'base_actor_identifier' attribute is set depending on these modes,
% respectively to 'undefined' or to the value at which the
% 'next_actor_identifier' attribute was set when switching from code-based (the
% default) to file-based: the AAI assigned to instances read from file then
% increment from base_actor_identifier onward, based on the line number of their
% definition in the read file. As not all lines of these files correspond to
% creation requests (ex: blank line or comment), some AAI may never be assigned
% (that is not a problem).



% Describes a computing node for the load-balancing:
%
-record( compute_node, {

	% The name of this computing node, as an atom:
	name :: net_utils:atom_host_name()

} ).


-type compute_node() :: #compute_node{}.



% All state attributes of the load balancer are explained here:
%
% - placement_policy describes what is the current placement policy
%
% - node_availability_tolerance tells how we should react, should a
% computing node be unavailable
%
% - next_actor_identifier :: class_Actor:aai() corresponds to the identifier
% that will be assigned to the *next* created actor (if any); it starts at 2, as
% the load balancer itself is an actor, having the first AAI (1); as a
% consequence, the total number of actual created actors, as long as no
% initialisation file is loaded and no instance is deleted, is
% 'next_actor_identifier - 2'
%
% - base_actor_identifier :: class_Actor:aai() is always set to 'undefined'
% unless initialisation files are read, in which case it is set to the then
% current next_actor_identifier minus 1; then created instances will have for
% AAI the addition of this base AAI and their line number in the creation file;
% as line are numbered from 1 onward, the first instance will have thus, for
% AAI, this next_actor_identifier, the next one will have this
% next_actor_identifier + 1, etc.
%
% - current_actor_count keeps track of the current number of living actors
% in the simulation (contrary to next_actor_identifier, it can decrease)
%
% - instances_per_class :: ?hashtable_type:?hashtable_type( class_name(), {
% basic_utils:count(), basic_utils:count() } ) is an hashtable whose keys are
% class names (as atoms) and whose associated values are pairs where
% CreationCounter keeps track of the overall instance creation count for that
% class (regardless of deletions), and DeletionCounter keeps track of the
% overall instance deletion count for that class
%
% - instances_per_node :: ?hashtable_type:?hashtable_type(
% net_utils:atom_node_name(), basic_utils:count() ) is an hashtable whose keys
% are names of computing nodes and whose associated values are the number of
% current actors on this computing node
%
% - initial_actors :: list( pid() ) is a list of the PID of all initial actors;
% it is used so that, on simulation start, this load balancer can notify all of
% them that their first diasca is happening (onFirstDiasca actor oneway)
%
% - initialisation_files :: [ file_utils:path() ] is a list of absolute paths
% (as the current directory has to change over time) to initialisation files,
% from which initial instances will be created
%
% - deployment_manager_pid :: maybe( pid() ) allows telling the deployment
% manager that the initialisation data for instances has been processed
%
% - compute_nodes :: list( compute_node() ) is a list of compute_node records,
% describing the available Erlang nodes on which actors should be created
%
% - placement_policy_data is:
%
%   - with the round-robin policy, a pair of lists, the first one, never
% changing, containing all the available computing nodes and the second one
% containing the nodes that were not popped yet by the policy, until the
% list is replenished based on the original one
%
% - seed :: basic_utils::seed() corresponds to the root seed this load-balancer
% begins with, depending on the simulation settings
%
% - seed_table :: ?hashtable_type:?hashtable_type( class_Actor::aai(),
% basic_utils:seed() ) is an hashtable whose keys are AAIs of instances being
% loaded and whose values are the seed corresponding to each of these AAIs
%
% - troubleshooting_mode :: boolean() tells whether the troubleshooting mode is
% enabled





% Constructs a new load balancer, from following parameters:
%
% - PlacementPolicy describes which heuristic should be used to dispatch created
% actors onto computing nodes; following placement policies are specified:
%
%  - round_robin: one of the simplest scheduling algorithms, which assigns
%  actors to computing nodes in equal portions and in order, handling all
%  creation requests without priority. Round-robin scheduling is both simple and
%  easy to implement, and starvation-free. It relies on the hypothesis that all
%  actors consume on average a similar amount of resource and that all computing
%  nodes provide on average a similar amount of resource See also:
%  http://en.wikipedia.org/wiki/Round-robin_scheduling
%
%  - select_least_loaded_first: the load balancer will evaluate the current load
%  of the computing nodes, and then will choose create any new actor on the
%  least loaded node (not implemented yet)
%
% - Nodes :: [ net_utils:atom_node_name() ] is a list of Erlang nodes (as atoms)
% that are to take part to the simulation, i.e. that are eligible as running
% environments for actors, like 'MyNode@computer_a.foo.org'
%
% - NodeAvailabilityTolerance can be:
%
%  - fail_on_unavailable_node: the construction of the load balancer will fail
%  if at least one of the specified node is not available
%
%  - allow_unavailable_nodes: all nodes found not available will be rejected,
%  and the simulation will rely only on the remaining ones
%
% - EvaluationMode :: evaluation_requested_properties() provides the load
% balancer with all information to properly seed each actor
%
% - TroubleshootingMode :: boolean() tells whether the troubleshooting mode is
% activated
%
% - InitialisationFiles :: [ file_utils:path() ] is a list of initialisation
% files, from which initial instances will be created
%
% A node might be unavailable because its host is unavailable, or because the
% node cannot be run on its available host.
%
% A load balancer is also an actor (to create reproducibly other actors), and
% thus is linked to a time manager. As the load balancer is created on the same
% node as the root time manager, its time manager is the root one.
%
-spec construct( wooper:state(), placement_policy(),
				 [ net_utils:atom_node_name() ], node_availability_tolerance(),
				 evaluation_mode(), boolean(), [ file_utils:path() ] ) ->
					   wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% The load-balancer is a potential bottleneck of the architecture, insofar
	% as, for example, it will have to interact with the initial actors, which
	% may be *very* numerous (onFirstDiasca):
	%
	erlang:process_flag( priority, _Level=high ),

	{ Seed, SeedInfoString, OrderingMode } = manage_seeding( SimulationMode ),

	% The load balancer is the only actor that is created in an ad hoc way:
	BalancerSeed = random_utils:get_random_seed(),

	BalancerSettings = #actor_settings{
					 aai=1,
					 seed=BalancerSeed,
					 message_ordering_mode=OrderingMode
				   },

	% By convention the load balancer assigns to itself the first AAI, 1:
	InitialState = class_BroadcastingActor:construct( State, BalancerSettings,
													  "Load Balancer" ),

	% Then the class-specific actions:

	% We must create empty slots for nodes, so that the performance tracker does
	% not have to wait for an instance to be created on a node to see this node
	% listed:
	InitialInstancesPerNode = ?hashtable_type:addEntries(
		 [ { N, _InstanceCount=0 } || N <- Nodes ], ?hashtable_type:new() ),


	% Checking, as currently only this random module can be seeded apparently:
	random = random_utils:get_random_module_name(),

	TraceState = setAttributes( InitialState, [

		{ placement_policy, PlacementPolicy },
		{ node_availability_tolerance, NodeAvailabilityTolerance },
		{ seed, Seed },
		{ seed_table, undefined },
		{ message_ordering_mode, OrderingMode },
		{ next_actor_identifier, 2 },
		{ base_actor_identifier, undefined },
		{ current_actor_count, 1 },
		{ instances_per_class, ?hashtable_type:new() },
		{ instances_per_node, InitialInstancesPerNode },
		{ initial_actors, [] },
		{ initialisation_files, InitialisationFiles },
		{ deployment_manager_pid, undefined },
		{ troubleshooting_mode, TroubleshootingMode },

		% For bootstrapping purposes, the load balancer is the only actor that
		% starts with a non-empty agenda:
		{ current_agenda, [ 0 ] },

		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

											   ] ),

	?send_info( TraceState, SeedInfoString ),

	SelectedComputingNodeRecords = inspect_computing_nodes( Nodes,
		NodeAvailabilityTolerance, TraceState ),

	% Anticipated checking (otherwise a function clause is raised):
	PlacementPolicyData = case PlacementPolicy of

		round_robin ->
			NodeList = get_node_list_from( SelectedComputingNodeRecords ),
			{ NodeList, NodeList }

	end,

	SelectedCount = length( SelectedComputingNodeRecords ),

	% From a constructor, send_X macros should be used instead of X macros:
	?send_info_fmt( TraceState,
		"Creating a new load balancer whose placement policy is ~p, "
		"whose node tolerance is ~p, "
		"whose ~B validated computing nodes are:~n~s",
		[ PlacementPolicy, NodeAvailabilityTolerance, SelectedCount,
			 compute_nodes_to_string( SelectedComputingNodeRecords ) ] ),

	% Commented out, as this information is already given by the deployment
	% manager:

	%	case SelectedCount of

	%		1 ->
	%			io:format( "The only validated computing node is ~s.~n",
	%		 [ compute_node_to_string( hd( SelectedComputingNodeRecords ) ) ] );

	%		_More ->
	%			io:format( "The ~B validated computing nodes are:~n~s~n",
	%			  [ SelectedCount, compute_nodes_to_string(
	%                                  SelectedComputingNodeRecords ) ] )

	%	end,


	StartingState = setAttributes( TraceState, [

		{ compute_nodes, SelectedComputingNodeRecords },
		{ placement_policy_data, PlacementPolicyData }

	] ),

	% Ensures also it is a singleton indeed:
	basic_utils:register_as( ?load_balancer_name, ?registration_type ),

	class_InstanceTracker:register_agent( State ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%io:format( "Deleting load balancer ~w.~n", [ self() ] ),

	% Class-specific actions:
	?trace( "Deleting load balancer." ),

	class_InstanceTracker:unregister_agent(),

	basic_utils:unregister( ?load_balancer_name, ?registration_type ),

	?debug( "Load balancer deleted." ),

	% Then allow chaining:
	State.





% Methods section.



% Notifies this load balancer that the simulation ended.
%
% For the vast majority of actors (but unlike the load balancer), this means
% deletion (overridden for the load balancer, which has a different life cycle).
%
% (oneway)
%
-spec simulationEnded( wooper:state() ) -> oneway_return().
simulationEnded( State ) ->

	% Do not trigger a delete here.

	?wooper_return_state_only( State ).



% Reacts to a notification of time manager shutdown.
%
% Overridden from class_Actor, not wanting for this very particular actor to be
% deleted then (deletion to be managed by the deployment manager).
%
% (oneway)
%
-spec timeManagerShutdown( wooper:state() ) -> oneway_return().
timeManagerShutdown( State ) ->

	% Do not trigger a delete here.

	?wooper_return_state_only( State ).



% Returns the list of the records corresponding to the actual selected computing
% nodes.
%
% (const request)
%
-spec getComputingNodes( wooper:state() ) -> request_return( compute_node() ).
getComputingNodes( State ) ->
	?wooper_return_state_result( State, ?getAttr(compute_nodes) ).





% Section about actor creations, initial or not.


% Requests this load balancer to trigger the actual creation of the initial
% instances that were specified in the file(s) listed in the simulation
% settings.
%
% In all cases, the caller expects a 'instances_created_from_files' message to
% be ultimately returned.
%
% (oneway, to remain responsive to placement requests)
%
-spec createInitialInstancesFromFiles( wooper:state(), file_utils:path(),
									   pid() ) -> oneway_return().
createInitialInstancesFromFiles( State, DeploymentManagerPid, EngineRootDir ) ->

	?debug( "Creation of initial instances from files." ),

	% Checkings:
	undefined = ?getAttr(base_actor_identifier),
	undefined = ?getAttr(seed_table),

	DeployState = setAttribute( State, deployment_manager_pid,
								DeploymentManagerPid ),

	LastState = case ?getAttr(initialisation_files) of


		[] ->
			% No need to create a useless instance loading process then:
			?debug( "No initialisation files specified." ),

			% The asynchronous loading notification is immediate in this case:
			self() ! onInstancesLoaded,

			DeployState;


		InitialisationFiles ->

			% Previously we were accessing the initialisation files from the
			% deployed archive. This allowed to create the load balancer on any
			% node, yet the compressing, sending and decompressing of these data
			% could become way too long; so now we assume again that the load
			% balancer is created on the user host, hence that it can access the
			% initialisation files readily, directly from the disk:
			%
			%RootDir = class_Actor:get_deployed_root_directory( State ),
			RootDir = EngineRootDir,

			ActualInitPaths = [ file_utils:join( RootDir, F )
								|| F <- InitialisationFiles ],

			%io:format( "Creating initial instances from files:~s",
			%		   [ text_utils:strings_to_string( ActualInitPaths ) ] ),

			% To trigger back the onInstancesLoaded oneway:
			spawn_link( _M=instance_loading,
						_F=manage_initialisation,
						_A=[ ActualInitPaths,
							 _NodeCount=length( ?getAttr(compute_nodes) ),
							 self() ] ),

			NextAAI = ?getAttr(next_actor_identifier),

			BaseAAI = NextAAI - 1,

			%io:format( "Setting base actor id to ~B (next AAI: ~B).~n",
			%		   [ BaseAAI, NextAAI ] ),

			setAttributes( DeployState, [

				{ base_actor_identifier, BaseAAI },
				{ seed_table, ?hashtable_type:new() }

										 ] )

	end,

	?wooper_return_state_only( LastState ).



% Called (either by itself or by the spawned instance loader) whenever all
% instances have been loaded from files (if any).
%
% (oneway, allowing the load balancer not to be blocked in
% createInitialInstancesFromFiles/2)
%
-spec onInstancesLoaded( wooper:state() ) -> oneway_return().
onInstancesLoaded( State ) ->

	?debug( "All instances loaded from files (if any)." ),

	% Unblocks in turn the deployment manager:
	?getAttr(deployment_manager_pid) ! instances_created_from_files,

	% Switch back to normal AAI mode if necessary:
	%
	% (next_actor_identifier needs no update; current random seed already in
	% final, correct state)
	%
	% We do not check seed_table as some {AAI,Seed} entries may be still there
	% (corresponding to empty lines or comments).
	%
	FinalState = setAttributes( State, [

					{ base_actor_identifier, undefined },
					{ seed_table, undefined }

									   ] ),

	?wooper_return_state_only( FinalState ).



% Creates specified actor on an automatically selected computing node, while the
% simulation is not running (i.e. not to be called by actors wanting to create
% other actors while the simulation is running), see createActor/4 instead.
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for bootstrap.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Returns the PID of the created actor. Its AAI is not returned anymore, as this
% information is of no use to the creating actor.
%
% See class_Actor:getAAI/1.
%
% (request)
%
-spec createInitialActor( wooper:state(), class_name(),
		 [ method_argument() ] ) -> request_return( class_Actor:actor_pid() ).
createInitialActor( State, ActorClassName, ActorConstructionParameters ) ->

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	{ UpdatedState, ActorPid } = create_actor( ActorClassName,
	   ActorConstructionParameters, SelectedNode, SelectedState ),

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	RecordedState = appendToAttribute( UpdatedState, initial_actors, ActorPid ),

	?wooper_return_state_result( RecordedState, ActorPid ).



% Creates specified actor on a computing node which is entirely determined by
% the specified placement hint, while the simulation is not running yet
% (i.e. not to be called by actors wanting to create other actors while the
% simulation is running - see createPlacedOtherActor/4 instead).
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for bootstrap.
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
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Returns the PID of the created actor. Its AAI is not returned anymore, as this
% information is of no use to the creating actor.
% See class_Actor:getAAI/1.
%
% (request)
%
-spec createInitialPlacedActor( wooper:state(), class_name(),
		  [ method_argument() ], placement_hint() ) ->
								  request_return( class_Actor:actor_pid() ).
createInitialPlacedActor( State, ActorClassName, ActorConstructionParameters,
						 PlacementHint ) ->

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	{ UpdatedState, ActorPid } = create_actor( ActorClassName,
	   ActorConstructionParameters, SelectedNode, State ),

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	RecordedState = appendToAttribute( UpdatedState, initial_actors, ActorPid ),

	?wooper_return_state_result( RecordedState, ActorPid ).



% Creates the specified list of (initial) actors, each on an automatically
% selected computing node, while the simulation is not running yet (i.e. not to
% be called by actors wanting to create other actors while the simulation is
% running - see createActor/4 instead).
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for bootstrap.
%
% The ActorConstructionList parameter is a list of specifications for actor
% creation (each made of a tuple containing an actor class name, a list of
% construction parameters and, possibly, a placement hint).
%
% Actor creations will be done as much as possible in parallel, over the
% available computing nodes: this request is to be used for bulk actor
% creations.
%
% Returns the list of the PIDs of the created actors, in the same order as the
% one of the corresponding construction specifications.
%
% (request)
%
-spec createInitialActors( wooper:state(),
						   [ class_Actor:instance_creation_spec() ] )
						 -> request_return( [ class_Actor:actor_pid() ] ).
createInitialActors( State, InstanceCreationSpecs ) ->

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	% We could imagine as well sending batches of creations to each node
	% (i.e. aggregating a set of creations, sent in one message to each node,
	% instead of sending one message per creation):

	% Next enhancement: creates instances directly when placement is determined.
	{ CreationInfos, NewState } = place_and_prepare_creations(
									   InstanceCreationSpecs, State ),

	NewActorPids = [
					begin

		%:format( " - constructing an instance of ~s with ~p~n",
		%		 [ Classname, ConstructionParams ] ),

		apply( Classname, remote_synchronisable_new_link, ConstructionParams )

					end

					 || { Classname, ConstructionParams } <- CreationInfos ],

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	NewInitialActors = ?getAttr(initial_actors) ++ NewActorPids,

	FinalState = setAttribute( NewState, initial_actors, NewInitialActors ),

	CreationCount = length( NewActorPids ),

	%io:format( "Spawned ~B actor(s)", [ CreationCount ] ),

	TargetNodeCount = length( ?getAttr(compute_nodes) ),

	MaxTotalWaitDuration = get_parallel_creation_time_out() *
		math_utils:ceiling( CreationCount / TargetNodeCount ),

	% While still in the parallel section:

	InitialTimeStamp = basic_utils:get_precise_timestamp(),

	%io:format( "Waiting for up to ~p ms, from ~p "
	%           "(roughly ~B ms per instance).~n",
	%			[ MaxTotalWaitDuration, InitialTimeStamp,
	%			 round( MaxTotalWaitDuration/ CreationCount ) ] ),

	wait_for_created_actors( NewActorPids, InitialTimeStamp,
							 MaxTotalWaitDuration ),

	?wooper_return_state_result( FinalState, NewActorPids ).




% Places the instances as specified by the creation specs, and returns all
% information needed to create them immediately, with an updated state that
% considers that these creations are done.
%
% (helper)
%
-spec place_and_prepare_creations( [ class_Actor:instance_creation_spec() ],
   wooper:state() ) ->
			{ [ { class_name(), [ method_argument() ] } ], wooper:state() }.
place_and_prepare_creations( InstanceCreationSpecs, State ) ->

	FirstAai = ?getAttr(next_actor_identifier),

	InstancesPerClass = ?getAttr(instances_per_class),
	InstancesPerNode = ?getAttr(instances_per_node),

	place_and_prepare_creations( InstanceCreationSpecs, _CurrentAAI=FirstAai,
			InstancesPerClass, InstancesPerNode, _AccCreationInfos=[], State ).



% (helper)
%
place_and_prepare_creations( _InstanceCreationSpecs=[], LastAAI,
		 InstancesPerClass, InstancesPerNode, AccCreationInfos, State ) ->

	% All creations managed, updating the state now:

	% Last minus previous current one:
	CreationCount = LastAAI - ?getAttr(next_actor_identifier),

	NewActorCount = ?getAttr(current_actor_count) + CreationCount,

	NewState = setAttributes( State, [

		{ next_actor_identifier, LastAAI },
		{ current_actor_count, NewActorCount },
		{ instances_per_class, InstancesPerClass },
		{ instances_per_node, InstancesPerNode }

							  ] ),

	% The contract is to preserve the order of creation information:
	{ lists:reverse( AccCreationInfos ), NewState };



place_and_prepare_creations( _InstanceCreationSpecs=[
				{ Classname, ConstructionParameters } | T ] ,
							 CurrentAAI,
							 InstancesPerClass, InstancesPerNode,
							 AccCreationInfos, State ) ->

	% No placement hint specified here, hence using default policy:
	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	prepare_creations( Classname, ConstructionParameters, SelectedNode,
					   T, CurrentAAI, InstancesPerClass, InstancesPerNode,
					   AccCreationInfos, SelectedState );

place_and_prepare_creations( _InstanceCreationSpecs=[
				{ Classname, ConstructionParameters, PlacementHint } | T ] ,
							 CurrentAAI, InstancesPerClass, InstancesPerNode,
							 AccCreationInfos, State ) ->

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	prepare_creations( Classname, ConstructionParameters, SelectedNode,
					   T, CurrentAAI, InstancesPerClass, InstancesPerNode,
					   AccCreationInfos, State ).



% Registers creation and recurses.
%
% (helper, gathering creations that are placed or not)
%
prepare_creations( Classname, ConstructionParameters, SelectedNode,
		InstanceCreationSpecs, AAI, InstancesPerClass, InstancesPerNode,
		CreationInfos, State ) ->

	display_synthetic_reporting( AAI, Classname, SelectedNode ),

	ActorSettings = get_actor_settings( AAI, State ),

	NewInstancesPerClass = record_creation_in_class_table( Classname,
														   InstancesPerClass ),

	NewInstancesPerNode = record_creation_in_node_table( SelectedNode,
														 InstancesPerNode ),

	CreationParams = [ SelectedNode, ActorSettings | ConstructionParameters ],

	CreationInfo = { Classname, CreationParams },

	place_and_prepare_creations( InstanceCreationSpecs, AAI + 1,
								 NewInstancesPerClass, NewInstancesPerNode,
								 [ CreationInfo | CreationInfos ], State ).





% Waits until all actors are reported as created.
%
% (exported helper)
%
-spec wait_for_created_actors( [ pid() ], basic_utils:count() ) ->
									 basic_utils:void().
wait_for_created_actors( ActorPids, NodeCount ) ->

	CreationCount = length( ActorPids ),

	MaxTotalWaitDuration = get_parallel_creation_time_out() *
			math_utils:ceiling( CreationCount / NodeCount ),

	InitialTimeStamp = basic_utils:get_precise_timestamp(),

	%io:format( "Waiting for up to ~p ms, from ~p "
	%           "(roughly ~B ms per instance).~n",
	%			[ MaxTotalWaitDuration, InitialTimeStamp,
	%			  round( MaxTotalWaitDuration/ CreationCount ) ] ),

	wait_for_created_actors( _WaitedPidList=ActorPids, InitialTimeStamp,
							 MaxTotalWaitDuration ).



% Waits until all actors are reported as created.
%
wait_for_created_actors( _WaitedPidList=[], _InitialTimeStamp,
						 _MaxTotalWaitDuration ) ->
	% All are ready, perfect:
	ok;

wait_for_created_actors( WaitedPidList, InitialTimeStamp,
						 MaxTotalWaitDuration ) ->

	CurrentTimeStamp = basic_utils:get_precise_timestamp(),

	Elapsed = basic_utils:get_precise_duration( InitialTimeStamp,
												CurrentTimeStamp ),

	ToWait = MaxTotalWaitDuration - Elapsed,

	case ToWait of

		D when D > 0 ->
			   ok;

		_ ->
			throw( { too_long_parallel_creation, WaitedPidList } )

	end,

	receive

		{ spawn_successful, ActorPid } ->

			% One traversal too many, just for the sake of checking:
			case lists:member( ActorPid, WaitedPidList ) of

				true ->

					NewWaitedPidList = lists:delete( ActorPid, WaitedPidList ),

					wait_for_created_actors( NewWaitedPidList, InitialTimeStamp,
						 MaxTotalWaitDuration );

				false ->
					throw( { unexpected_spawn, ActorPid, WaitedPidList } )

			end

	after ToWait ->

			% Just forces to re-check duration:
			wait_for_created_actors( WaitedPidList, InitialTimeStamp,
									 MaxTotalWaitDuration )

	end.




% Creates specified actor on an automatically selected computing node, while the
% simulation is running (i.e. to be called by actors wanting to create other
% actors in the course of their behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_actor/3 helper function.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - SenderPid is the PID of the sender
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/3 actor oneway, to notify the creating actor that
% the requested actor was created.
%
% (actor oneway)
%
-spec createActor( wooper:state(), class_name(), [method_argument()], pid() ) ->
						 oneway_return().
createActor( State, ActorClassName, ActorConstructionParameters, SenderPid ) ->

	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	{ UpdatedState, ActorPid } = create_actor( ActorClassName,
		ActorConstructionParameters, SelectedNode, SelectedState ),

	FirstSentState = class_BroadcastingActor:send_actor_message( SenderPid,
		{ onActorCreated, [ ActorPid, ActorClassName,
							ActorConstructionParameters ] }, UpdatedState ),

	SecondSentState = class_BroadcastingActor:send_actor_message( ActorPid,
											 onFirstDiasca, FirstSentState ),

	% No need to schedule the next diasca explicitly, as it is a by-product of
	% the sending of an actor message.

	?wooper_return_state_only( SecondSentState ).



% Creates specified actor on a computing node which is entirely determined by
% the specified placement hint, while the simulation is running yet (i.e. to be
% called by actors wanting to create other actors in the course of their
% behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_placed_actor/4 helper function.
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
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/3 actor oneway, to notify the creating actor that
% the requested actor was created.
%
% (actor oneway)
%
-spec createPlacedActor( wooper:state(), class_name(), [ method_argument() ],
						placement_hint(), pid() ) -> oneway_return().
createPlacedActor( State, ActorClassName, ActorConstructionParameters,
				  PlacementHint, SenderPid ) ->

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	{ UpdatedState, ActorPid } = create_actor( ActorClassName,
		ActorConstructionParameters, SelectedNode, State ),

	FirstSentState = class_BroadcastingActor:send_actor_message( SenderPid,
		{ onActorCreated,
		  [ ActorPid, ActorClassName, ActorConstructionParameters ] },
		UpdatedState ),

	SecondSentState = class_BroadcastingActor:send_actor_message( ActorPid,
											onFirstDiasca, FirstSentState ),

	% No need to schedule the next tick explicitly, as it is a by-product of the
	% sending of an actor message.

	?wooper_return_state_only( SecondSentState ).



% Returns, based on specified identifier-related information, the node on which
% the corresponding instance must be created, and which actor settings should be
% used for that.
%
% The specified identifier information may or may not be an actual identifier
% and, if yes, it may or may not be the identifier of this particular instance;
% a specified identifier is meant to be a mere placement guideline.
%
% Note: the load balancer does not create the corresponding instance, but
% considers that is will be created afterwards.
%
% (request)
%
-spec getActorCreationInformation( wooper:state(),
		instance_loading:identifier_info(), instance_loading:line_number(),
		class_name() ) -> request_return(
			{ net_utils:atom_node_name(), class_Actor:actor_settings() } ).
getActorCreationInformation( State, _IdentifierInfo=none, LineNumber,
							 Classname ) ->

	% Here, no specific node can be deduced, we just rely on the default
	% placement policy:
	%
	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, SelectedState ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } );


getActorCreationInformation( State, _IdentifierInfo=UserIdentifier, LineNumber,
							 Classname ) ->

	% Here we have a user identifier (not necessarily the one of the instance to
	% create), which we use as a placement hint:
	%
	SelectedNode = select_node_based_on_hint( _PlacementHint=UserIdentifier,
											  State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, State ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } ).



% Returns, based on specified placement hint, the node on which the
% corresponding instance must be created, and which actor settings should be
% used for that.
%
% Note: the load balancer does not create the corresponding instance, but
% considers that is will be created afterwards.
%
% (request)
%
-spec getActorCreationInformationFromHint( wooper:state(),
		placement_hint(), instance_loading:line_number(),
		class_name() ) -> request_return(
			{ net_utils:atom_node_name(), class_Actor:actor_settings() } ).
getActorCreationInformationFromHint( State, PlacementHint, LineNumber,
							 Classname ) ->

	% Here, supposedly no user identifier applies, we thus solely rely on the
	% placement hint instead:
	%
	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, State ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } ).



% Returns the computing node on which the instance corresponding to the
% specified user identifier shall be created.
%
% (const request)
%
-spec getNodeForUserIdentifier( wooper:state(),
			instance_loading:user_identifier() ) ->
							request_return( net_utils:atom_node_name()).
getNodeForUserIdentifier( State, UserIdentifier ) ->

	SelectedNode = select_node_based_on_hint( _PlacementHint=UserIdentifier,
											  State ),

	?wooper_return_state_result( State, SelectedNode ).



% Registers specified initial actors, from their PID.
%
% (request)
%
-spec registerInitialActors( wooper:state(), [ pid() ] ) ->
							   request_return( 'initial_actors_registered' ).
registerInitialActors( State, AdditionalInitialActors ) ->

	NewInitialActors = AdditionalInitialActors ++ ?getAttr(initial_actors),

	% Actor count and class/node table expected to be already updated.

	NewState = setAttribute( State, initial_actors, NewInitialActors ),

	?wooper_return_state_result( NewState, initial_actors_registered ).



% Registers a (probably initial) instance created externally (typically while
% loading them from file).
%
% Returns an updated state and the actor's creation settings.
%
% (helper)
%
register_created_instance( TargetNode, LineNumber, Classname, State ) ->

	% Check would have no effect: undefined =/= ?getAttr(base_actor_identifier),
	case ?getAttr(base_actor_identifier) of

		undefined ->
			throw( invalid_loading_condition );

		_ ->
			ok

	end,

	% LineNumber > 0, base derived from next AAI:
	ActorAai = ?getAttr(base_actor_identifier) + LineNumber,

	NextAAI = ?getAttr(next_actor_identifier),

	% Because of chunks and parallelism, lines are processed in an arbitrary
	% order, not according to their line number:
	%
	NewNextAAI = max( NextAAI, ActorAai + 1 ),

	%io:format( "register_created_instance: assigned AAI ~B, "
	%			"new next AAI is ~B.~n", [ ActorAai, NewNextAAI ] ),

	%?debug_fmt( "Registration of the creation of actor of "
	%			"class ~s on ~w, with AAI ~B.",
	%			[ Classname, TargetNode, ActorAai ] ),

	{ ActorSettings, LoadState } = get_loaded_actor_settings( ActorAai, State ),

	NewActorCount = ?getAttr(current_actor_count) + 1,

	NewClassTable = record_creation_in_class_table( Classname,
											 ?getAttr(instances_per_class) ),

	NewNodeTable = record_creation_in_node_table( TargetNode,
											 ?getAttr(instances_per_node) ),

	% We have not the PID here, hence we cannot updated initial_actors.

	FinalState = setAttributes( LoadState, [

				   { next_actor_identifier, NewNextAAI },
				   { current_actor_count, NewActorCount },
				   { instances_per_class, NewClassTable },
				   { instances_per_node, NewNodeTable }

										   ] ),

	{ FinalState, ActorSettings }.



% Overridden so that initial actors can be triggered for their first diasca,
% with their onFirstDiasca/2 actor oneway.
%
% This method is itself called because the load balancer is always scheduled for
% a spontaneous behaviour at tick offset 0 (diasca 0).
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% Initial actors may be very numerous (potentially, millions). So we use the
	% class_BroadcastingActor implementation to rely on more efficient larger
	% lists (?list_impl-based ones); but, even with these, large simulations
	% would be too demanding; so we prefer smoothing the load and creating as
	% many diascas as needed for that, notifying initial actors of their first
	% diasca by chunks of, say, a few thousands actors.

	%InitialActorsCount = length( ?getAttr(initial_actors) ) ,

	%?debug_fmt( "Notifying the ~B initial actors of their first diasca.",
	%			[ InitialActorsCount ] ),

	class_PluginManager:notify( on_simulation_bootstrap_start ),

	?debug( "Notifying the initial actors of their first diasca." ),

	%io:format( "Notifying all ~B initial actors of their first diasca "
	%		   "at ~s.~n",
	%		   [ InitialActorsCount, basic_utils:get_textual_timestamp() ] ),

	TriggeredState = class_BroadcastingActor:send_actor_messages_over_diascas(
						initial_actors, onFirstDiasca, State ),

	?debug( "All initial actors just notified of their first diasca, "
			"waiting for their processing." ),

	%io:format( "All actors notified of their first diasca at ~s.~n",
	%		 [ basic_utils:get_textual_timestamp() ] ),

	% No more spontaneous schedulings planned for the load balancer.

	class_PluginManager:notify( on_simulation_bootstrap_stop ),

	?wooper_return_state_only( TriggeredState ).



% Allows to keep track of actor deletion as well, in this single, centralized
% place.
%
% (oneway)
%
-spec notifyDeletion( wooper:state(), class_Actor:actor_pid(), class_name(),
					 net_utils:atom_node_name() ) -> oneway_return().
notifyDeletion( State, _ActorPid, ActorClassname, Node ) ->

	%io:format( "## Deletion of actor ~p (~p) on node ~p.~n",
	%		  [ ActorPid, ActorClassname, Node ] ),

	NewActorCount = ?getAttr(current_actor_count) - 1,

	NewClassTable = record_deletion_in_class_table( ActorClassname,
										?getAttr(instances_per_class) ),

	NewNodeTable = record_deletion_in_node_table( Node,
										?getAttr(instances_per_node) ),

	?wooper_return_state_only( setAttributes( State, [

				{ current_actor_count, NewActorCount },
				{ instances_per_class, NewClassTable },
				{ instances_per_node,  NewNodeTable }

											   ] ) ).



% Returns the instance counts, per class and per node.
%
% Note: this involves operations that may be a bit expensive (enumeration and
% sending of the result) but this request is called only when the performance
% tracking is activated, and this load balancer might be, if needed, created on
% the same node as the performance tracker.

% (const request)
%
-spec getInstanceCounts( wooper:state() ) -> request_return(
		{ 'instance_counts', hashtable:entries(), hashtable:entries() } ).
getInstanceCounts( State ) ->

	%io:format( "instances_per_node = ~p.~n",
	%		  [ ?hashtable_type:enumerate( ?getAttr(instances_per_node) ) ] ),

	% The first atom is to allow for easier discrimination in terms of parallel
	% messages received by the performance tracker:
	TimedCounts = { instance_counts,
					?hashtable_type:enumerate( ?getAttr(instances_per_class) ),
					?hashtable_type:enumerate( ?getAttr(instances_per_node) )
				   },

	?wooper_return_state_result( State, TimedCounts ).



% Returns (asynchronously) the overall number of model instances.
%
% Used notably by the root time manager so that the console tracker can display
% actor counts.
%
% (oneway)
%
getOverallInstanceCount( State, CallerPid ) ->

	CallerPid ! { notifyOverallActorCount, ?getAttr(current_actor_count) },

	?wooper_return_state_only( State ).




% Static methods section.


% Returns a textual description of specified load balancing settings record.
%
% (static)
%
-spec settings_to_string( #load_balancing_settings{} ) -> string().
settings_to_string( LoadBalancingSettings ) ->

	#load_balancing_settings{

		placement_policy = Placement

	} = LoadBalancingSettings,

	PlacementString = "placement policy will be " ++ case Placement of

		round_robin ->
			"round-robin."

	end,

	text_utils:string_list_to_string( [ PlacementString ] ).



% Returns the atom corresponding to the name the load balancer should be
% registered as.
%
% Note: executed on the caller node.
%
% (static)
%
-spec get_registration_name() -> basic_utils:registration_name().
get_registration_name() ->
	% Ex: 'sim_diasca_load_balancer':
	?load_balancer_name.



% Returns the PID of the (unique) load balancer.
%
% (static method, to be used by clients of the load balancer)
%
-spec get_balancer() -> pid().
get_balancer() ->
	basic_utils:wait_for_global_registration_of( get_registration_name() ).






% Section for helper functions (not methods).


% Interprets the specified seeding and reordering information, for
% initialisation.
%
-spec manage_seeding( evaluation_requested_properties() ) ->
		 { random_utils:seed(), string(), class_Actor:message_ordering_mode() }.
manage_seeding( fastest ) ->

	% Using the default (constant) seed here, even if no reordering will be
	% performed: stochastic actors need a seed anyway.
	%
	% (even in fastest mode, a random seed is needed as otherwise stochastic
	% actors would all behave the same)
	%
	DefaultSeed = ?default_reproducible_seed,

	random_utils:start_random_source( DefaultSeed ),

	% No hash-based sorting, no permutation:
	{ DefaultSeed, io_lib:format( "Simulation will run in fastest mode, "
								  "with no message reordering, and using "
								  "default seed (~p).", [ DefaultSeed ] ),
	  unordered } ;


manage_seeding( reproducible ) ->

	% A random seed is needed for stochastic actors, even if in reproducible
	% mode.

	% Root seed is a default (constant) seed here:
	DefaultSeed = ?default_reproducible_seed,

	random_utils:start_random_source( DefaultSeed ),

	% Hash-based sorting, no permutation (using the 'identity' here which is not
	% any less true than others):
	%
	{ DefaultSeed, io_lib:format( "Simulation will be totally reproducible, "
					  "using default seed (~p).", [ DefaultSeed ] ),
	  constant_arbitrary_order } ;


manage_seeding( { reproducible, SetSeed={A,B,C} } ) ->

	% Random seed used for uniform permutations of messages *and* for stochastic
	% variables.

	% Using the user-specified seed:
	random_utils:start_random_source( A, B, C ),

	% Hash-based sorting and permutation needed here:
	{ SetSeed, io_lib:format( "Simulation will be totally reproducible, "
							  "using user-specified seed ~p.", [ SetSeed ] ),
	  constant_permuted_order } ;


manage_seeding( ergodic ) ->

	% Random seed used for uniform permutations of messages *and* for stochastic
	% variables:

	% Use a time-based seed, i.e. a seed that should not be the same twice
	% (since it is based on wallclock-time):
	%
	% (not using the time_based_seed parameter, as we want to report what is the
	% actual seed used)
	TimeSeed = { A, B, C } = erlang:now(),

	random_utils:start_random_source( A, B, C ),

	% Hash-based sorting and permutation needed here:
	{ TimeSeed, io_lib:format( "Simulation will run in ergodic mode, "
					 "using time-based seed ~p.", [ TimeSeed ] ),
	  constant_permuted_order }.



% Returns the actor settings corresponding to the next actor to be created, in
% the context of a direct, programmatic creation.
%
% (helper)
%
-spec get_actor_settings( class_Actor:aai(), wooper:state() ) ->
								class_Actor:actor_settings().
get_actor_settings( AAI, State ) ->

	% Picks up a new seed:
	ActorSeed = random_utils:get_random_seed(),

	#actor_settings{
					 aai=AAI,
					 seed=ActorSeed,
					 message_ordering_mode=?getAttr(message_ordering_mode)
				   }.



% Returns the actor settings corresponding to the specified actor to be created,
% in the context of a loading-based creation, and an updated state.
%
% (helper)
%
-spec get_loaded_actor_settings( class_Actor:aai(), wooper:state() ) ->
							{ class_Actor:actor_settings(), wooper:state() }.
get_loaded_actor_settings( AAI, State ) ->

	% Here we have to manage the fact that AAIs might be requested in any order,
	% whereas we want to associate them reproducible random seeds.
	%
	% As a consequence, we record in seed_table the AAI -> Seed associations for
	% the AAIs that are smaller than the specified one - AAIs whose seeds must
	% be generated so that we can determine the seed for the current AAI of
	% interest.

	SeedTable = ?getAttr(seed_table),

	NextAAI = ?getAttr(next_actor_identifier),

	%io:format( "get_loaded_actor_settings: requesting AAI ~B, while table "
	%			"is: ~p.~n",
	%		   [ AAI, lists:sort( ?hashtable_type:keys( SeedTable ) ) ] ),

	{ NewSeedTable, ActorSeed } = case ?hashtable_type:lookupEntry( _K=AAI,
															   SeedTable ) of

		hashtable_key_not_found ->

			% Seed for this AAI not computed yet, we will thus create all
			% intermediary ones between the last computed one and this one:
			%
			expand_seed_table( _From=NextAAI, _To=AAI, SeedTable );

		{ value, AlreadyAvailableSeed } ->
			LightenTable = ?hashtable_type:removeEntry( _Key=AAI, SeedTable ),
			{ LightenTable, AlreadyAvailableSeed }

	end,

	ActorSettings = #actor_settings{
					 aai=AAI,
					 seed=ActorSeed,
					 message_ordering_mode=?getAttr(message_ordering_mode)
					  },

	{ ActorSettings, setAttribute( State, seed_table, NewSeedTable ) }.



% Returns { NewSeedTable, ActorSeed }, where:
%
% - NewSeedTable is an expanded seed table, recording for all AAIs in [FromAAI,
% ToAAI] their seed (we include ToAAI as later we will probably have to expand
% again that table)
%
% - ActorSeed is the seed of actor whose AAI is ToAAI
%
expand_seed_table( _From=AAI, _To=AAI, SeedTable ) ->
	add_seed_for( AAI, SeedTable );

expand_seed_table( FromAAI, ToAAI, SeedTable ) ->
	{ NewSeedTable, _ActorSeed } = add_seed_for( FromAAI, SeedTable ),
	expand_seed_table( FromAAI + 1, ToAAI, NewSeedTable ).



% Adds a seed for specified AAI, and returns { NewSeedTable, ActorSeed }.
%
add_seed_for( AAI, SeedTable ) ->

	% Check:
	case ?hashtable_type:hasEntry( _K=AAI, SeedTable ) of

		true ->
			throw( { not_overriding_aai, AAI } );

		false ->
			ok

	end,

	ActorSeed = random_utils:get_random_seed(),

	NewSeedTable = ?hashtable_type:addEntry( _K=AAI, _V=ActorSeed, SeedTable ),

	{ NewSeedTable, ActorSeed }.




% Inspects the already launched nodes that can be used for the simulation.
%
% Nodes are specified by their names (strings).
%
% Returns a list of compute_node records, corresponding to available and running
% named Erlang nodes.
%
% The State variable is needed, to be able to send traces.
inspect_computing_nodes( NodeNames, NodeAvailabilityTolerance, State ) ->
	inspect_computing_nodes( NodeNames, NodeAvailabilityTolerance, _Acc=[],
							State ).



inspect_computing_nodes( _NodeNames=[], _NodeAvailabilityTolerance, Acc,
						_State ) ->
	Acc;

inspect_computing_nodes( [ NodeName| OtherNodes ], NodeAvailabilityTolerance,
		Acc, State ) ->

	case net_utils:check_node_availability( NodeName, with_waiting ) of

		{ true, _Duration } ->
			NewRecord = create_compute_node_record_for( NodeName ),
			inspect_computing_nodes( OtherNodes, NodeAvailabilityTolerance,
				[ NewRecord | Acc ], State );

		{ false, _Duration } ->
			case NodeAvailabilityTolerance of

				fail_on_unavailable_node ->
					?fatal_fmt( "Node named ~s "
						"not found available, hence not selected, "
						"and the load balancer settings do not allow that.",
						[ NodeName ] ),
					throw( { unavailable_computing_node, NodeName } );

				allow_unavailable_nodes ->
					?warning_fmt( "Node named ~s "
						"not found available, hence not selected, but "
						"the load balancer settings allow that.",
						[ NodeName ] ),
					inspect_computing_nodes( OtherNodes,
						NodeAvailabilityTolerance, Acc, State )

			end

	end.



compute_nodes_to_string( ComputeNodes ) ->
	compute_nodes_to_string( ComputeNodes, [] ).


compute_nodes_to_string( _ComputeNodes=[], Acc ) ->
	Acc;

compute_nodes_to_string( _ComputeNodes=[ H | T ], Acc ) ->
	compute_nodes_to_string( T,
		" + " ++ compute_node_to_string( H ) ++ "\n" ++ Acc ).


% Helper:
compute_node_to_string( ComputeNode ) ->
	% Extracts all fields:
	#compute_node{ name=Name } = ComputeNode,
	atom_to_list( Name ).



% Determines on which node the next actor should be created, according to the
% current placement policy.
%
% Returns an updated state and the determined node.
%
select_node_by_heuristic( State ) ->

	case ?getAttr(placement_policy) of

		round_robin ->
			select_node_with_round_robin( State )

	end.



% Determines on which node the next actor should be created, according to the
% round-robin placement policy.
%
% Returns an updated state and the determined node.
%
select_node_with_round_robin( State ) ->

	{ NodeList, SelectedNode, NewCurrentList } =
			case ?getAttr(placement_policy_data) of

		{ OriginalNodeList, [] } ->
			% Here we exhausted the current list, let's replenish it:
			{ OriginalNodeList, hd(OriginalNodeList), tl(OriginalNodeList) };

		{ OriginalNodeList, [ Node | Nodes ] } ->
			{ OriginalNodeList, Node, Nodes }

	end,

	NewState = setAttribute( State, placement_policy_data,
		 { NodeList, NewCurrentList } ),

	{ NewState, SelectedNode }.



% Returns the node which corresponds to specified placement hint.
%
% (const state, hence not returned)
%
select_node_based_on_hint( PlacementHint, State ) ->

	{ OriginalNodeList, _CurrentList } = ?getAttr(placement_policy_data),

	NodeCount = length( OriginalNodeList ),

	% Hash depends only on the hint, and will be in [1,NodeCount]:
	Hash = erlang:phash2( PlacementHint, NodeCount ) + 1,

	% Returns the selected node:
	lists:nth( Hash, OriginalNodeList ).



% Returns the list of node names (as atoms) extracted from the list of computing
% node records.
%
get_node_list_from( ComputingNodeRecords ) ->
	get_node_list_from( ComputingNodeRecords, [] ).


get_node_list_from( _ComputingNodeRecords=[], Acc ) ->
	Acc;

get_node_list_from( _ComputingNodeRecords=[ #compute_node{ name=Nodename } | T],
				   Acc ) ->
	get_node_list_from( T, [ Nodename | Acc ] ).



% To be called when needing to create a new actor, on specified node.
%
% Returns a pair made of an updated state and of the PID of the newly actor,
% throws an exception on failure.
%
% Note: used both for initial and non-initial actor creations.
%
% (internal helper function)
%
-spec create_actor( class_name(), [ method_argument() ],
	net_utils:atom_node_name(), wooper:state() ) -> { wooper:state(), pid() }.
create_actor( ActorClassName, ActorConstructionParameters, Node, State ) ->

	%io:format( "Will create now an actor ~w with parameters ~p "
	%  "on node ~w.~n", [ ActorClassName, ActorConstructionParameters, Node ] ),

	ActorAai = ?getAttr(next_actor_identifier),

	ActorSettings = get_actor_settings( ActorAai, State ),

	display_synthetic_reporting( ActorAai, ActorClassName, Node ),

	try apply( ActorClassName, remote_synchronous_timed_new_link,
			[ Node, ActorSettings | ActorConstructionParameters ] ) of

		ActorPid ->

			NewActorCount = ?getAttr(current_actor_count) + 1,

			% ?debug_fmt(
			%	"Creation of actor of class ~s with parameters ~p resulted in "
			%	"the process ~w being spawn on ~w, with AAI ~B.",
			%	[ ActorClassName, ActorConstructionParameters, ActorPid,
			%	Node, ActorAai ] ),

			%io:format( "## Creation of actor ~p (~p) on node ~s.~n",
			%		  [ ActorPid, ActorClassName, Node ] ),

			NewClassTable = record_creation_in_class_table( ActorClassName,
											 ?getAttr(instances_per_class) ),

			NewNodeTable = record_creation_in_node_table( Node,
											 ?getAttr(instances_per_node) ),

			%basic_utils:display( "NewClassTable = ~s",
			%					[ ?hashtable_type:toString(NewClassTable) ] ),

			%basic_utils:display( "NewNodeTable = ~s",
			%					[ ?hashtable_type:toString(NewNodeTable) ] ),


			NewState = setAttributes( State, [

				{ next_actor_identifier, ActorAai+1 },
				{ current_actor_count, NewActorCount },
				{ instances_per_class, NewClassTable },
				{ instances_per_node, NewNodeTable }

											 ] ),

			{ NewState, ActorPid }

	catch

		throw:{ remote_synchronous_linked_time_out, Node, ActorClassName }->

			%io:format( "create_actor: time-out.~n" ),

			?fatal_fmt( "Time-out while waiting for the creation "
				"of an actor of class ~s on node ~w with parameters:~n~p.",
				[ ActorClassName, Node, ActorConstructionParameters ] ),

			throw( { actor_creation_failed, ActorClassName,
				ActorConstructionParameters, Node } )

	end.



% Records the creation of an instance of specified class, in specified class
% table.
%
% Returns an updated table.
%
% (helper)
%
record_creation_in_class_table( ActorClassName, ClassTable ) ->

	case ?hashtable_type:lookupEntry( ActorClassName, ClassTable ) of

		hashtable_key_not_found ->
			% New class to register:
			?hashtable_type:addEntry( _K=ActorClassName,
						_V={ _CreationCount=1, _DeletionCount=0 }, ClassTable );

		{ value, { CreationCount, DeletionCount } } ->
			% Just an update here:
			?hashtable_type:addEntry( _K=ActorClassName,
					_V={ CreationCount+1, DeletionCount }, ClassTable )

	end.



% Records the deletion of an instance of specified class, in specified class
% table.
%
% Returns an updated table.
%
% (helper)
%
record_deletion_in_class_table( ActorClassName, ClassTable ) ->

	%io:format( "ActorClassName = ~s, ClassTable =~n~s~n",
	%		[ ActorClassName, ?hashtable_type:toString(ClassTable) ] ),

	% Exists necessarily already:
	{ CreationCount, DeletionCount } = ?hashtable_type:getEntry(
												ActorClassName, ClassTable ),

	% Just an update here:
	?hashtable_type:addEntry( _K=ActorClassName,
							_V={ CreationCount, DeletionCount+1 }, ClassTable ).




% Records the creation of an instance on specified node, in specified node
% table.
%
% Returns an updated table.
%
% (helper)
%
record_creation_in_node_table( ActorNode, NodeTable ) ->

	% No node expected to be discovered at runtime:
	?hashtable_type:addToEntry( ActorNode, _Increment=1, NodeTable ).



% Records the deletion of an instance on specified node, in specified node
% table.
%
% Returns an updated table.
%
% (helper)
%
record_deletion_in_node_table( ActorNode, NodeTable ) ->
	?hashtable_type:addToEntry( ActorNode, _Increment=-1, NodeTable ).




% Hooks for serialisation/deserialisation.
%
% Note: the load balancer being an actor, it will be for example serialised with
% no specific request.



% Triggered just before serialisation.
%
% The state used here is dedicated to serialisation (i.e. it is not the actual,
% resulting state).
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	% Some terms are impacted by serialisation:
	setAttribute( State, compute_nodes, ?term_restoration_marker ).


% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
-spec post_serialise_hook( class_name(),
		 wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  wooper_serialisation: term_serialisation().
pre_deserialise_hook( _SerialisedEntries={ _Classname, Entries }, _UserData ) ->

	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->

	% We need to update the computing nodes this balancer knows:
	%
	% (note: possibly the user node is now eligible while previously it was not)
	%
	NewComputingNodes = nodes(),

	NodeRecords = [ create_compute_node_record_for( NodeName )
				   || NodeName <- NewComputingNodes ],

	setAttribute( State, compute_nodes, NodeRecords ).



% Returns a computing node record corresponding to specified node.
%
% (helper)
%
create_compute_node_record_for( NodeName ) ->
	#compute_node{ name=NodeName }.



% Displays a trace to allow for the monitoring of the creation of larger actor
% populations.
%
-spec display_synthetic_reporting( class_Actor:aai(), class_name(),
		   net_utils:atom_node_name() ) -> basic_utils:void().


% Expected creation duration, per instance and per node:
-spec get_parallel_creation_time_out() -> unit_utils:milliseconds().





% Displays, in production mode, a notification once 500 new actors have been
% created. Useful for large-scale runs.

-ifdef(exec_target_is_production).


% In (safer, with real-life simulation sizes) production mode here:


display_synthetic_reporting( ActorAai, ActorClassName, Node ) ->

	case ActorAai rem 500 of

		0 ->
			io:format( " + creating actor #~B, of class ~p, on node ~s, "
					   "at ~s~n",
					   [ ActorAai, ActorClassName, Node,
						basic_utils:get_textual_timestamp() ] );

		_ ->
			ok

	end.


% Expected duration is up to 5s per instance per node here:
get_parallel_creation_time_out() ->
	5000.



-else.



% In development mode here:


display_synthetic_reporting( ActorAai, ActorClassName, Node ) ->

	case ActorAai rem 500 of

		0 ->
			io:format( " + creating actor #~B, of class ~p, on node ~s, "
					   "at ~s~n",
					   [ ActorAai, ActorClassName, Node,
						 basic_utils:get_textual_timestamp() ] );

		_ ->
			ok

	end.
	%ok.


% Expected duration is up to 500ms per instance per node here:
get_parallel_creation_time_out() ->
	500.



-endif.
