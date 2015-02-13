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




% Instance tracker.


% Its role is mainly to track some information about all kinds of
% simulation-related instances, typically of models (actors) and of services
% (simulation agents). It provides as well some minor other distributed
% services.

% It is a distributed service across all computing nodes, with a root instance
% tracker, and one local instance tracker per other node, for scalability
% purposes.
%
% This is the first simulation service to be deployed; as a consequence, all
% other simulation services can assume the instance tracking is readily
% available whenever needed.
%
% The purpose of the instance tracking service is twofold:
%
% - to help managing the instances, by maintaing a two-way table allowing to
% convert PIDs into more abstract references; for model instances, this involves
% being able to convert actor PIDs to and from AAIs, for simulation services the
% conversion is between PIDs and references of a service agent; this is notably
% useful for serialization and debugging purposes
%
% - if the troubleshooting mode is enabled, then additional (redundant)
% information are gathered, in tracker-specific instance records; the purpose of
% that is to help the fixing of faulty models, which might have become
% unresponsive while we still need to access information about them (ex: AAI,
% class and instance name, etc.)
%
-module(class_InstanceTracker).




% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ParentTrackerPid, TroubleshootingMode ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, declareTrackers/2, setLoadBalancerPid/2,

		 registerChildTracker/1,

		 registerActor/5, registerActor/3, unregisterActor/3,
		 registerAgent/2, registerThirdPartyAgent/3, unregisterAgent/2,
		 registerResultProducer/2, unregisterResultProducer/2,

		 getActorInformationLocal/2, getActorInformationGlobal/2,

		 % Look-up:
		 resolvePid/3, resolveLocalPid/3,

		 % Reverse look-up:
		 %resolveAgentReference/2, resolveAAI/2, resolveResultProducerName/2,

		 setCrashedNodeMapping/2,

		 getStaticResourceInformation/1, getDynamicResourceInformation/1,

		 toString/1 ).



% Static methods:
-define( wooper_static_method_export, get_registration_name/0,
		 get_root_tracker/0, get_local_tracker/0,

		 register_agent/1, register_agent/2, unregister_agent/0,
		 unregister_agent/1,

		 get_identifier_for/2,

		 create_mockup_environment/0 ).


% For the actor_info record and al:
-include("class_InstanceTracker.hrl").


% For host_static_info and al:
-include("system_utils.hrl").



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Tracker.Instances").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% Determines the type of hashtable to be used:
-define( hashtable_type, lazy_hashtable ).


% Any kind of reference resolved:
%
-type instance_id() :: class_Actor:aai() | agent_ref() | producer_ref().



% Implementation notes.

% The root instance tracker uses the exact same code and conventions as the
% other instance trackers (ex: it is the only instance tracker of its node); it
% just happens that it is the single one which does not have a parent tracker.

% A simulation agent is declared as a class name (as an atom) and a PID (from
% which the node of the agent can be deduced). So an agent reference is declared
% as a class name, but stored and later returned as a { ClassName, NodeName }
% pair.

% Some special processes (typically, the load balancer) can be seen as both an
% actor and an agent, and thus are registered twice in trackers.



% Class-specific attributes of an instance tracker instance are:
%
% - parent_tracker_pid :: basic_utils:maybe( pid() ) is the PID of the parent
% instance tracker of the parent of this tracker (if any, otherwise set to
% 'undefined')
%
% - child_trackers :: [ pid() ] is a list of the PID of all the (direct) child
% trackers (if any) of this tracker
%
% - trackers :: ?hashtable_type:?hashtable_type( net_utils:atom_node_name(),
% pid() ) allows to tell, for a given node, what is the PID of its local
% instance tracker
%
% - troubleshooting_mode :: boolean() tells whether the troubleshooting mode is
% activated
%
% - actor_table :: ?hashtable_type:?hashtable_type( pid(), actor_info() )
% is an hashtable which allows to obtain from a PID the information kept about
% the corresponding (supposedly still living) local actor
%
% - zombi_table :: ?hashtable_type:?hashtable_type( pid(), actor_info() ) is an
% hashtable, only filled in troubleshooting mode, which allows to keep the
% information of actors that were deleted, to better diagnose issues with
% life-cycle management (on deletion, the instance information goes from
% actor_table to this zombi_table)
%
% - agent_table :: ?hashtable_type:?hashtable_type( pid(), agent_ref() ) is an
% hashtable which allows to obtain from a PID the corresponding reference of the
% simulation service agent
%
% - producer_table :: ?hashtable_type:?hashtable_type( pid(), producer_ref() )
% is an hashtable which allows to convert a PID into the reference of a result
% producer
%
% - pid_resolution_enabled :: boolean() tells whether this tracker should
% prepare in order to be able to resolve PID into AAI or agent references
% efficiently (uses in that case more RAM)

% - aai_to_pid :: ?hashtable_type:?hashtable_type( class_Actor:aai(), pid() ) is
% an (optional) reverse table, which allows to translate any AAI into a PID
%
% - agent_ref_to_pid :: ?hashtable_type:?hashtable_type( agent_ref(), pid() ) is
% an (optional) reverse table, which allows to translate any AAI into a PID
%
% - producer_ref_to_pid :: ?hashtable_type:?hashtable_type( producer_ref(),
% pid() ) is an (optional) reverse table, which allows to translate any
% reference of a result producer into a PID
%
% - crashed_node_table :: ?hashtable_type:?hashtable_type(
% net_utils:atom_node_name(), net_utils:atom_node_name() ) allows to convert the
% name of a crashed node into the name of the node that is taking it in charge
% after a rollback
%
% - last_cpu_counters :: system_utils:cpu_usage_info() records the last counters
% that were measured (is never undefined)
%
% - load_balancer_pid :: pid() is the PID of the load balancer, kept so that it
% can be notified of all instance deletions and hence record their full life
% cycle



% Constructs a new instance tracker, from following parameters:
%
% - ParentTrackerPid :: pid() | 'none' is the PID of the parent instance
% tracker of the parent of this tracker (if any, otherwise set to 'none')
%
% - TroubleshootingMode :: boolean() tells whether the troubleshooting mode is
% enabled
%
-spec construct( wooper:state(), pid() | 'none', boolean() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% We raise here the priority of all instance trackers, otherwise, when a
	% system is overloaded enough, instance trackers will not be able to cope
	% with the demand and delay the stall diagnosis which may happen after the
	% various watchdog-related time-outs:
	%
	erlang:process_flag( priority, _Level=high ),

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, "Instance tracker" ),

	{ ParentString, ParentPid } = case ParentTrackerPid of

					none ->
							% So that the root tracker can be easily found:
							basic_utils:register_as( get_registration_name(),
													 _Scope=global_only ),
							{ "root", undefined };

					_Pid ->
							% Here we are a child tracker; by design the parent
							% one should already be deployed, we have to declare
							% to it:
							%
							ParentTrackerPid ! { registerChildTracker, [],
												 self() },

							receive

								{ wooper_result, child_registered } ->
									ok

							end,
							{ "child", ParentTrackerPid }

	end,

	StartingState = setAttributes( TraceState, [

		{ parent_tracker_pid, ParentPid },
		{ child_trackers, [] },
		{ trackers, ?hashtable_type:new() },
		{ troubleshooting_mode, TroubleshootingMode },
		{ actor_table, ?hashtable_type:new() },
		{ zombi_table, ?hashtable_type:new() },
		{ agent_table, ?hashtable_type:new() },
		{ producer_table, ?hashtable_type:new() },
		{ pid_resolution_enabled, true },
		{ aai_to_pid, ?hashtable_type:new() },
		{ agent_ref_to_pid, ?hashtable_type:new() },
		{ producer_ref_to_pid, ?hashtable_type:new() },
		{ crashed_node_table, undefined },
		{ last_cpu_counters, system_utils:get_cpu_usage_counters() },

		% Cannot be known at construction-time:
		{ load_balancer_pid, undefined },

		{ trace_categorization,
		  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

												] ),

	TroubleString = case TroubleshootingMode of

		true ->
			"enabled";

		false ->
			"disabled"

	end,

	?send_info_fmt( StartingState, "Creating a new ~s instance tracker "
					"whose troubleshooting mode is ~s.",
					[ ParentString, TroubleString ] ),

	% Ensures also it is a singleton indeed:
	basic_utils:register_as( get_registration_name(), local_only ),

	% Let's now register ourselves in our own tables:
	ThisAgentClassname = ?MODULE,

	register_agent_helper( ThisAgentClassname, self(), StartingState ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting instance tracker." ),

	% Recurses down the tracker tree:
	wooper:delete_synchronously_instances( ?getAttr(child_trackers) ),

	?debug( "Instance tracker deleted." ),

	% No need to unregister from our own table.

	basic_utils:unregister( get_registration_name(), local_only ),

	case ?getAttr(parent_tracker_pid) of

		undefined ->
			% Root tracker was also globally registered:
			basic_utils:unregister( get_registration_name(), global_only );

		_ ->
			ok

	end,

	% Then allow chaining:
	State.






% Member methods section.



% Registers the specified trackers, so that this tracker can request them
% directly.
%
% Expected to be called by the deployment manager.
%
% (request, for synchronization purposes)
%
-spec declareTrackers( wooper:state(), [ pid() ] ) ->
								   request_return( 'trackers_declared' ).
declareTrackers( State, Trackers ) ->

	% First, as the caller may have included (at most once) the PID of this
	% tracker into the sent list, we filter it out:
	%
	FilteredTrackers = lists:delete( self(), Trackers ),

	% Then we create an associative table, to be able later to convert a node
	% name into the PID of its local instance tracker:

	TrackerTable = lists:foldl(

				 fun( TrackerPid, AccTable ) ->
					TrackerNode = node( TrackerPid ),
					?hashtable_type:addEntry( _K=TrackerNode, _V=TrackerPid,
										AccTable )
				 end,

				 _Acc0=?hashtable_type:new(),

				 _List=FilteredTrackers ),

	?wooper_return_state_result(
			setAttribute( State, trackers, TrackerTable ),
			trackers_declared ).



% Notifies this tracker about the PID of the load balancer.
%
% (request, for synchronization reasons)
%
-spec setLoadBalancerPid( wooper:state(), pid() ) ->
								request_return( 'load_balancer_set' ).
setLoadBalancerPid( State, LoadBalancerPid ) ->

	?checkUndefined( load_balancer_pid ),

	?wooper_return_state_result( setAttribute( State, load_balancer_pid,
							LoadBalancerPid ), 'load_balancer_set'  ).



% Registers specified child tracker.
%
% (request, for synchronization reasons)
%
registerChildTracker( State ) ->

	ChildPid = ?getSender(),

	NewState = appendToAttribute( State, child_trackers, ChildPid ),

	?wooper_return_state_result( NewState, child_registered ).



% Declares a newly created model instance (actor), expected to run on the node
% this instance tracker runs on.
%
% Expected to be called notably by the time manager local to the node this
% tracker runs on and by a reader process in the context of a deserialisation.
%
% (oneway)
%
-spec registerActor( wooper:state(), class_Actor:aai(),
		class_Actor:internal_name(), pid(), class_name() ) -> oneway_return().
registerActor( State, ActorAai, ActorBinName, ActorPid, ActorClassName ) ->

	ActorInfo = #actor_info{
	  classname=ActorClassName,
	  name=ActorBinName,
	  aai=ActorAai },

	registerActor( State, ActorPid, ActorInfo ).



% Declares a newly created model instance (actor), expected to run on the node
% this instance tracker runs on.
%
% Expected to be called notably by the time manager local to the node this
% tracker runs on and by a reader process in the context of a deserialisation.
%
% (oneway)
%
-spec registerActor( wooper:state(), pid(), actor_info() ) -> oneway_return().
registerActor( State, ActorPid, ActorInfo ) ->

	%io:format( "Declaring creation of actor ~w on tracker ~w.~n",
	%		  [ ActorPid, self() ] ),

	check_pid_and_get_node( ActorPid ),

	Table = ?getAttr(actor_table),

	% Node is implicitly the one this tracker runs on.

	NewActorTable = ?hashtable_type:addEntry( _K=ActorPid, _V=ActorInfo,
											 Table ),

	ReverseState = case ?getAttr(pid_resolution_enabled) of

			true ->
				% addKeyValueToAttribute not to be used here (risk of mixing
				% hashtable types):
				ReverseTable = ?hashtable_type:addEntry(
									_Key=ActorInfo#actor_info.aai,
									_Value=ActorPid,
									_Table=?getAttr(aai_to_pid) ),

				setAttribute( State, aai_to_pid, ReverseTable );

			false ->
				State

	end,

	?wooper_return_state_only( setAttribute( ReverseState, actor_table,
											 NewActorTable ) ).



% Called by a local actor to notify this tracker about the unregistration (this
% corresponds actually to a deletion) of this actor.
%
% (oneway)
%
-spec unregisterActor( wooper:state(), pid(), class_name() ) -> oneway_return().
unregisterActor( State, ActorPid, ActorClassname ) ->

	%io:format( "Declaring deletion of instance ~w on tracker ~w.~n",
	%			[ ActorPid, self() ] ),

	ActorTable = ?getAttr(actor_table),

	case ?hashtable_type:lookupEntry( ActorPid, ActorTable ) of

		hashtable_key_not_found ->
			throw( { unexpected_actor_unregistration, ActorPid } );

		{ value, ActorInfo } ->

			% Only one to maintain the instance count:
			?getAttr(load_balancer_pid) ! { notifyDeletion, [ ActorPid,
										   ActorClassname, node() ] },

			AAI = ActorInfo#actor_info.aai,

			ReverseState = case ?getAttr(pid_resolution_enabled) of

					true ->
						% Does nothing if the key (AAI) is not found:
						ReverseTable = ?hashtable_type:removeEntry( AAI,
													?getAttr(aai_to_pid) ),
						setAttribute( State, aai_to_pid, ReverseTable );

					false ->
						State

			end,

			NewActorTable = ?hashtable_type:removeEntry( ActorPid,
														 ActorTable ),

			ZombiTable = ?getAttr(zombi_table),

			NewZombiTable = case ?getAttr(troubleshooting_mode) of

				true ->
					?hashtable_type:addEntry( _K=ActorPid, _V=ActorInfo,
											  ZombiTable );

				false ->
					ZombiTable

			end,

			?wooper_return_state_only( setAttributes( ReverseState, [

					{ actor_table, NewActorTable },
					{ zombi_table, NewZombiTable }

															  ] ) )

	end.



% Registers specified agent (the caller) in this instance tracker.
%
% (request, for synchronisation purposes)
%
-spec registerAgent( wooper:state(), wooper:class_name() ) ->
						   request_return( 'agent_registered' ).
registerAgent( State, AgentClassname ) ->

	AgentPid = ?getSender(),

	NewState = register_agent_helper( AgentClassname, AgentPid, State ),

	?wooper_return_state_result( NewState, agent_registered ).



% Registers specified third-party agent (i.e. not the caller) in this instance
% tracker.
%
% (request, for synchronisation purposes)
%
-spec registerThirdPartyAgent( wooper:state(), wooper:class_name(), pid() ) ->
						   request_return( 'agent_registered' ).
registerThirdPartyAgent( State, AgentClassname, AgentPid ) ->

	NewState = register_agent_helper( AgentClassname, AgentPid, State ),

	?wooper_return_state_result( NewState, agent_registered ).



% Unregisters the specified agent (based on its PID) from this instance tracker.
%
% (oneway)
%
-spec unregisterAgent( wooper:state(), pid() ) -> oneway_return().
unregisterAgent( State, AgentPid ) ->

	AgentTable = ?getAttr(agent_table),

	ReverseState = case ?getAttr(pid_resolution_enabled) of

		true ->
			% Find first the reference to remove:
			AgentRef = ?hashtable_type:getEntry( AgentPid, AgentTable ),
			ReverseTable = ?hashtable_type:removeEntry( AgentRef,
										   ?getAttr(agent_ref_to_pid) ),
			setAttribute( State, agent_ref_to_pid, ReverseTable );

		false ->
			State

	end,

	NewAgentTable = ?hashtable_type:removeEntry( AgentPid, AgentTable ),

	NewState = setAttribute( ReverseState, agent_table, NewAgentTable ),

	?wooper_return_state_only( NewState ).





% Registers specified result producer (the caller) in this instance tracker.
%
% (request, for synchronisation purposes)
%
-spec registerResultProducer( wooper:state(), producer_ref() ) ->
						   request_return( 'result_producer_registered' ).
registerResultProducer( State, ProducerRef ) ->

	ProducerPid = ?getSender(),

	ReverseState = case ?getAttr(pid_resolution_enabled) of

			true ->
				NewReverseTable = ?hashtable_type:addEntry( ProducerRef,
						ProducerPid, ?getAttr(producer_ref_to_pid) ),
				setAttribute( State, producer_ref_to_pid, NewReverseTable );

			false ->
				State

	end,

	NewProducerTable = ?hashtable_type:addEntry( _K=ProducerPid, _V=ProducerRef,
											 ?getAttr(producer_table) ),


	NewState = setAttribute( ReverseState, producer_table, NewProducerTable ),

	?wooper_return_state_result( NewState, result_producer_registered ).



% Unregisters the specified result producer (based on its PID) from this
% instance tracker.
%
% (oneway)
%
-spec unregisterResultProducer( wooper:state(), pid() ) -> oneway_return().
unregisterResultProducer( State, ProducerPid ) ->

	ProducerTable = ?getAttr(producer_table),

	ReverseState = case ?getAttr(pid_resolution_enabled) of

		true ->
			% Find first the reference to remove:
			ProducerRef = ?hashtable_type:getEntry( ProducerPid,
												   ProducerTable ),
			ReverseTable = ?hashtable_type:removeEntry( ProducerRef,
										   ?getAttr(producer_ref_to_pid) ),
			setAttribute( State, producer_ref_to_pid, ReverseTable );

		false ->
			State

	end,

	NewProducerTable = ?hashtable_type:removeEntry( ProducerPid,
												   ProducerTable ),

	NewState = setAttribute( ReverseState, producer_table, NewProducerTable ),

	?wooper_return_state_only( NewState ).





% Returns information about the specified actor.
%
% (const request)
%
-spec getActorInformationLocal( wooper:state(), pid() ) -> request_return(
					{ actor_info(), net_utils:atom_node_name() } ).
getActorInformationLocal( State, ActorPid ) ->

	ActorInfo = get_local_actor_info( ActorPid, State ),

	Node = node(),

	?wooper_return_state_result( State, { ActorInfo, Node } ).



% Returns information about the specified actor, either found locally or
% (otherwise) found in the tracker hierarchy.
%
% Generally called on the root instance tracker.
%
% (const request)
%
-spec getActorInformationGlobal( wooper:state(), pid() ) ->
	request_return( { actor_info(), net_utils:atom_node_name() } ).
getActorInformationGlobal( State, ActorPid ) ->

	LocalNode = node(),

	ActorNode = node( ActorPid ),

	% Rather than performing recursive look-ups among child trackers, let's
	% directly choose the right one:
	%
	ActorInfo = case LocalNode of

		ActorNode ->
			% This actor is managed locally:
			get_local_actor_info( ActorPid, State );


		_OtherNode ->

			% Let's find the tracker that manages the actor node:
			TrackerPid = select_tracker_for_node( ActorNode,
										  ?getAttr(child_trackers) ),


			 % We could have this tracker respond automatically to the caller,
			 % but this way is probably clearer:
			 %
			 TrackerPid ! { getActorInformationLocal, ActorPid, self() },

			 receive

						{ wooper_result, { ActInfo, _Node } } ->
							ActInfo

			 end

	end,

	?wooper_return_state_result( State, { ActorInfo, ActorNode } ).


% Returns the PID of the tracker, among the specified list of tracker PID, that
% corresponds to the specified node.
%
select_tracker_for_node( Node, _Trackers=[] ) ->
	throw( { tracker_not_found_for, Node } );

select_tracker_for_node( Node, _Trackers=[ TrackerPid | T ] ) ->

	case node( TrackerPid ) of

		Node ->
			% Found!
			TrackerPid;

		_ ->
			select_tracker_for_node( Node, T )

	end.





% Resolves specified PID: transforms it into a reproducible information, for
% example for resilience purposes.
%
% (const oneway)
%
% Note: this is not a request, as the answer may come from another instance
% tracker (to avoid too many messages), therefore in this case no result would
% have to be sent by this tracker, and as a consequence this cannot be then a
% request.
%
% So the caller shall only expect in return a { notifyResolvedPid, Id }
% message, where Id :: instance_id(); this message can be interpreted as a
% oneway.
%
-spec resolvePid( wooper:state(), pid(), pid() ) -> oneway_return().
resolvePid( State, PidToResolve, CallerPid ) ->

	LocalNode = node(),

	% The requester is expected to be often (not always) local, hence the PID
	% might be remote:
	%
	case node( PidToResolve ) of

		LocalNode ->

			Resolved = resolve_locally( PidToResolve, State ),
			CallerPid ! { notifyResolvedPid, Resolved };


		RemoteNode ->

			% Only the instance tracker on this remote node should be able to
			% answer. To avoid too many messages being sent, it will answer
			% directly to the caller:
			%
			RemoteTrackerPid = ?hashtable_type:getEntry( RemoteNode,
													 ?getAttr(trackers) ),

			% No fake request can be used, we must rely on separate oneways:
			RemoteTrackerPid ! { resolveLocalPid, [ PidToResolve, CallerPid ] }

	end,

	% Const oneway:
	?wooper_return_state_only( State ).



% Resolves specified PID, assumed to be local to this node: transforms it into a
% reproducible information, for example for resilience purposes.
%
% (const oneway)
%
-spec resolveLocalPid( wooper:state(), pid(), pid() ) -> oneway_return().
resolveLocalPid( State, PidToResolve, CallerPid ) ->

	Resolved = resolve_locally( PidToResolve, State ),

	% To comply with the local case of resolvePid/2:
	CallerPid ! { notifyResolvedPid, Resolved },

	?wooper_return_state_only( State ).



% Sets (recursively) the mapping allowing to convert the name of a crashed node
% into the name of the node that is taking it in charge.
%
% (request, for synchronicity)
%
-spec setCrashedNodeMapping( wooper:state(), ?hashtable_type:?hashtable_type() )
						   -> request_return( 'crashed_node_mapping_set' ).
setCrashedNodeMapping( State, NodeTable ) ->

	TargetInstancePIDs = ?getAttr(child_trackers),

	% Go recursive first:
	wooper:send_requests( _RequestName=setCrashedNodeMapping,
						  _RequestArgs=[ NodeTable ],
						  TargetInstancePIDs ),

	NewState = setAttribute( State, crashed_node_table, NodeTable ),

	wooper:wait_for_request_answers( TargetInstancePIDs,
									 _AckAtom=crashed_node_mapping_set ),

	?wooper_return_state_result( NewState, crashed_node_mapping_set ).



% Returns some static information relative to the node this tracker runs on, as
% { Infos, Pid } where Infos is a node_static_info record, and Pid is the PID of
% this tracker (to help discriminating between simultaneous tracker answers,
% that might be requested in parallel).
%
% (const request)
%
-spec getStaticResourceInformation( wooper:state() ) ->
	 request_return(
	   { net_utils:node_name(), system_utils:host_static_info(), pid() } ).
getStaticResourceInformation( State ) ->

	{ _UsedRAM, TotalRAM } = system_utils:get_total_memory_used(),

	{ _UsedSwap, TotalSwap } = system_utils:get_swap_status(),

	Res = #host_static_info{

		total_ram=TotalRAM,

		total_swap=TotalSwap,

		core_count=system_utils:get_core_count(),

		erlang_version=system_utils:get_interpreter_version()

							},

	% PID specified, so that requests can be done in parallel:
	?wooper_return_state_result( State, { node(), Res, self() } ).




% Returns some dynamic information relative to the node this tracker runs on, as
% { Infos, Pid } where Infos is a node_dynamic_info record and Pid is the PID of
% this tracker (to help discriminating between simultaneous tracker answers,
% that might be requested in parallel).
%
% (const request)
%
-spec getDynamicResourceInformation( wooper:state() ) ->
			 request_return( { system_utils:host_dynamic_info(), pid() } ).
getDynamicResourceInformation( State ) ->

	% In bytes:
	{ UsedSwap, _TotalSwap } = system_utils:get_swap_status(),

	% GiB:
	UsedSwapInGiB = erlang:round( UsedSwap / 1024 / 1024 / 1024 ),

	{ UsedRAM, TotalRAM } = system_utils:get_total_memory_used(),

	SimulationSize = system_utils:get_memory_used_by_vm(),

	RoundDigitCount = 1,

	PercentRamUsedBySimulation = math_utils:round_after(
					  100 * SimulationSize / TotalRAM,
					  RoundDigitCount ),

	PercentRamUsedByOthers = math_utils:round_after(
					  100 * ( UsedRAM - SimulationSize ) / TotalRAM,
					  RoundDigitCount ),

	LastCounters = ?getAttr(last_cpu_counters),

	% As late as possible, to avoid getting unchanged counters:
	NewCounters = system_utils:get_cpu_usage_counters(),

	Res = #host_dynamic_info{

		node_name = node(),

		swap_used=UsedSwapInGiB,

		ram_use={ PercentRamUsedBySimulation, PercentRamUsedByOthers },

		cpu_usage=system_utils:compute_detailed_cpu_usage( LastCounters,
														   NewCounters ),

		process_count=system_utils:get_process_count()

					 },

	% PID specified, so that requests can be done in parallel:
	?wooper_return_state_result(
				setAttribute( State, last_cpu_counters, NewCounters ),
				{ Res, self() } ).



% Returns a textual description of this tracker.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->

	Desc = case ?getAttr(parent_tracker_pid) of

			   undefined ->
				   "Root instance tracker";

			   ParentPid ->
				   io_lib:format( "Instance tracker having for parent ~p",
								  [ ParentPid ] )

	end,

	Children = ?getAttr(child_trackers),

	FullDesc = Desc ++ io_lib:format( ", having ~B direct child trackers (~p)",
									  [ length( Children ), Children ] ),

	ResolutionString = case ?getAttr(pid_resolution_enabled) of

						   true ->
							   "PID resolution enabled";

						   false ->
							   "PID resolution disabled"

	end,

	Res = io_lib:format( "~s on node '~s', with ~s:~n"
				   " - actor table: ~s~n"
				   " - zombi table: ~s~n"
				   " - agent table: ~s~n"
				   " - result producer table: ~s",
				   [ FullDesc, node(), ResolutionString,
					 actor_table_to_string(    ?getAttr(actor_table) ),
					 actor_table_to_string(    ?getAttr(zombi_table) ),
					 agent_table_to_string(    ?getAttr(agent_table) ),
					 producer_table_to_string( ?getAttr(producer_table) )
					] ),

   ?wooper_return_state_result( State, Res ).






% Static methods section.



% Returns the atom corresponding to the name this instance tracker should be
% registered as.
%
% Note: executed on the caller node.
%
% (static)
%
-spec get_registration_name() -> basic_utils:registration_name().
get_registration_name() ->
	% Ex: sim_diasca_instance_tracker
	?instance_tracker_name.



% Returns the PID of the (unique) root instance tracker.
%
% (static method, to be used by clients of the instance tracker)
%
-spec get_root_tracker() -> pid().
get_root_tracker() ->
	basic_utils:get_registered_pid_for( get_registration_name(), global ).



% Returns the PID of the (unique) local instance tracker.
%
% (static method, to be used by clients of the instance tracker)
%
-spec get_local_tracker() -> pid().
get_local_tracker() ->
	basic_utils:get_registered_pid_for( get_registration_name(), local ).



% Registers the caller as a node-local agent of specified simulation service to
% its corresponding local instance tracker.
%
% (helper)
%
-spec register_agent( wooper:class_name() | wooper:state() ) ->
							basic_utils:void().
register_agent( AgentClassName ) when is_atom( AgentClassName ) ->

	TrackerPid = get_local_tracker(),

	TrackerPid ! { registerAgent, AgentClassName, self() },

	receive

		{ wooper_result, agent_registered } ->
			ok

	end;

register_agent( State ) ->

	{ _State, ActualClassName } = executeRequest( State, getClassName ),

	register_agent( ActualClassName ).



% Registers the specified PID as a node-local agent of specified simulation
% service to its corresponding local instance tracker.
%
% (static)
%
-spec register_agent( wooper:class_name(), pid() ) -> basic_utils:void().
register_agent( AgentClassName, AgentPid ) ->

	% The specified PID must be on the local node:
	check_pid_and_get_node( AgentPid ),

	TrackerPid = get_local_tracker(),

	AgentRef = AgentClassName,

	TrackerPid ! { registerThirdPartyAgent, [ AgentRef, AgentPid ], self() },

	receive

		{ wooper_result, agent_registered } ->
			ok

	end.



% Unregisters the caller, an agent of a simulation service, from its
% corresponding local instance tracker.
%
% (static)
%
-spec unregister_agent() -> basic_utils:void().
unregister_agent() ->

	TrackerPid = get_local_tracker(),

	% Oneway:
	TrackerPid ! { unregisterAgent, self() }.



% Unregisters the specified process, an agent of a simulation service, from the
% corresponding local instance tracker.
%
% (static)
%
-spec unregister_agent( pid() ) -> basic_utils:void().
unregister_agent( AgentPid ) ->

	TrackerPid = get_local_tracker(),

	% Oneway:
	TrackerPid ! { unregisterAgent, AgentPid }.



% Returns the identifier (ex: the AAI) of the instance corresponding to
% specified PID, using specified tracker.
%
% (static)
%
-spec get_identifier_for( pid(), pid() ) ->  instance_id().
get_identifier_for( InstancePid, InstanceTrackerPid ) ->

	% We call a oneway here:
	InstanceTrackerPid ! { resolvePid, [ _PidToResolve=InstancePid, self() ] },

	receive

		{ notifyResolvedPid, Id } ->
			Id

	end.



% Creates a mock-up environment suitable to emulate the instance tracking
% service (i.e. without creating it for real).
%
% See also: class_ResultManager:create_mockup_environment/0.
%
% (static)
%
-spec create_mockup_environment() -> pid().
create_mockup_environment() ->

	% Mimics a local instance tracker:
	%
	MockFun = fun() ->

			% Does as the real one:
			basic_utils:register_as( get_registration_name(), local_only ),

			% Fakes a local instance tracker:
			receive

				{ registerResultProducer, _BinName, ProducerPid } ->
					ProducerPid ! { wooper_result, result_producer_registered };

				{ registerAgent, _AgentRef, AgentPid } ->
					AgentPid ! { wooper_result, agent_registered }

			end,

			% Forces this process to linger (will wait until end of time), as
			% some destructors expect to find the instance tracker as a
			% registered process:
			%
			receive

				% Such a message will by design never be received:
				never_sent ->
					ok

			end

	end,

	spawn_link( MockFun ).



% Helper functions section.


% Returns a textual description of the specified actor table.
%
-spec actor_table_to_string( ?hashtable_type:?hashtable_type() ) -> string().
actor_table_to_string( Table ) ->

	% { Pid, InstanceInfo } pairs:
	InstancePairs = ?hashtable_type:enumerate( Table ),

	% We want to describe it according to an increasing AAI order:
	%
	AAIStringPairs = lists:foldl(

				fun( { _K=Pid, _V=Info }, Acc ) ->
						ActorString = actor_info_to_string( Info, Pid ),
						[ { Info#actor_info.aai, ActorString } | Acc ]
				end,

				_InitialAcc=[],

				InstancePairs ),

	%io:format( "AAIStringPairs = ~p~n", [ AAIStringPairs ] ),

	% Sort by AAI, then drop it:
	Strings = [ S || { _AAI, S } <- lists:keysort( _Index=1, AAIStringPairs ) ],

	%io:format( "Strings = ~p~n", [ Strings ] ),

	% No reverse needed:
	text_utils:string_list_to_string( Strings ).



% Returns a textual description of the specified actor information.
%
-spec actor_info_to_string( actor_info(), pid() ) ->
									 string().
actor_info_to_string( InstanceInfo, ActorPid ) ->

	% By design the actor is on the same node as this tracker:
	Node = node(),

	AAI = InstanceInfo#actor_info.aai,

	ActorName = case InstanceInfo#actor_info.name of

		undefined ->

			% Sends a oneway to this actor, which will update this tracker (for
			% later use):
			%
			ActorPid ! { triggerNameNotification, self() },

			% For this time (we stay in an asynchronous way), we fall-back to
			% only a basic information:
			io_lib:format( "actor whose AAI is ~B (PID: ~w)",
						   [ AAI, ActorPid ] );

		 Name ->
			% Already available, ok:
			io_lib:format( "actor named '~s' whose AAI is ~B (PID: ~w)",
								  [ Name, AAI, ActorPid ] )

	end,

	% These information are known to exist:
	ActorName ++ io_lib:format( " of class ~s on node ~s",
						  [ InstanceInfo#actor_info.classname, Node ] ).



% Returns a textual description of the specified agent table.
%
-spec agent_table_to_string( ?hashtable_type:?hashtable_type() ) -> string().
agent_table_to_string( Table ) ->

	% { Pid, AgentRef } pairs:
	InstancePairs = ?hashtable_type:enumerate( Table ),

	% No specific sorting requested:
	AgentStrings = lists:foldl(

				fun( { _K=Pid, _V=AgentRef }, Acc ) ->
						[ io_lib:format( "agent reference ~p associated "
								"to PID ~p", [ AgentRef, Pid ] ) | Acc ]
				end,

				_InitialAcc=[],

				InstancePairs ),

	%io:format( "AgentStrings = ~p~n", [ AgentStrings ] ),

	% No reverse needed:
	text_utils:string_list_to_string( AgentStrings ).



% Returns a textual description of the specified producer table.
%
-spec producer_table_to_string( ?hashtable_type:?hashtable_type() ) -> string().
producer_table_to_string( Table ) ->

	% { Pid, ProducerRef } pairs:
	ProducerPairs = ?hashtable_type:enumerate( Table ),

	% No specific sorting requested:
	ProducerStrings = lists:foldl(

				fun( { _K=Pid, _V=ProducerRef }, Acc ) ->
						[ io_lib:format( "producer reference ~p associated "
								"to PID ~p", [ ProducerRef, Pid ] ) | Acc ]
				end,

				_InitialAcc=[],

				ProducerPairs ),

	%io:format( "ProducerStrings = ~p~n", [ ProducerStrings ] ),

	% No reverse needed:
	text_utils:string_list_to_string( ProducerStrings ).




% Returns information about specified local actor.
%
% (helper)
%
-spec get_local_actor_info( pid(), wooper:state() ) -> actor_info().
get_local_actor_info( ActorPid, State ) ->

	ActorTable = ?getAttr(actor_table),

	case ?hashtable_type:lookupEntry( _Key=ActorPid, ActorTable ) of

		{ value, ActorInfo } ->
			ActorInfo;

		hashtable_key_not_found ->

			% This *can* happen, as the retrieval of actor information is not
			% synchronous, and an actor may have left in-between the simulation,
			% i.e. have been deleted; returning in this case a blank
			% information:
			case ?getAttr(troubleshooting_mode) of

				true ->

					% We should have an up-to-date zombi table:
					ActorEntry = ?hashtable_type:getEntry( _K=ActorPid,
													 ?getAttr(zombi_table) ),

					zombify( ActorEntry );

				false ->

					#actor_info{

					 classname='(unknown class)',

					 name=text_utils:string_to_binary( "(deleted actor)" ),

					 % "Zombi" actor:
					 aai=0

					}

			end

	end.



% Zombifies the specified actor information.
%
zombify( Info=#actor_info{ name=Name } ) ->

	NewName = io_lib:format( "(deleted actor whose name was '~s')",
							 [ text_utils:binary_to_string( Name ) ] ),

	% We leave the class name and (past) AAI as are:
	Info#actor_info{ name=text_utils:string_to_binary( NewName ) }.



% Resolves specified PID, assumed local, hence known from this current tracker.
%
% (helper)
%
resolve_locally( Pid, State ) ->

	% We must be able to discriminate between the various types that can be
	% resolved: actor (AAI, hence integer), agent (class name, hence atom) and
	% result producer (name, hence binary string).

	% Let's suppose first this local PID corresponds to an actor:
	%
	ActorTable = ?getAttr(actor_table),

	case ?hashtable_type:lookupEntry( _Key=Pid, ActorTable ) of

		{ value, ActorInfo } ->
			ActorInfo#actor_info.aai;

		hashtable_key_not_found ->

			% Then maybe then this local PID corresponds to a simulation agent?
			AgentTable = ?getAttr(agent_table),

			case ?hashtable_type:lookupEntry( _Key=Pid, AgentTable ) of

				{ value, AgentRef } ->
					AgentRef;

				hashtable_key_not_found ->

					% Last chance: might then be result producer?
					ProducerTable = ?getAttr(producer_table),

					case ?hashtable_type:lookupEntry( _Key=Pid,
													 ProducerTable ) of

						{ value, ProducerRef } ->
							ProducerRef;

						hashtable_key_not_found ->
							% Not found at all, abnormal:
							throw( { pid_resolution_failed, Pid } )

					end

			end

	end.



% Registers specified agent.
%
% Returns an updated state.
%
% (helper)
%
register_agent_helper( AgentClassName, AgentPid, State ) ->

	% A priori we could even not store the node name and determine it from the
	% PID when needed, however if both the corresponding node and process are
	% dead, this might be safer to store that information when it is known
	% available.
	%
	AgentRef = { AgentClassName, node( AgentPid ) },

	ReverseState = case ?getAttr(pid_resolution_enabled) of

			true ->
				NewReverseTable = ?hashtable_type:addEntry( AgentRef,
						AgentPid, ?getAttr(agent_ref_to_pid) ),
				setAttribute( State, agent_ref_to_pid, NewReverseTable );

			false ->
				State

	end,

	NewAgentTable = ?hashtable_type:addEntry( _K=AgentPid, _V=AgentRef,
											  ?getAttr(agent_table) ),

	setAttribute( ReverseState, agent_table, NewAgentTable ).



% Ensures that specified PID is local, and returns the node it corresponds to.
%
-spec check_pid_and_get_node( pid() ) -> net_utils:atom_node_name().
check_pid_and_get_node( Pid ) ->

	% The specified PID must be on the local node:
	Node = node( Pid ),

	case node() of

		Node ->
			Node;

		OtherNode ->
			throw( { non_local_pid_registration, { Node, Pid }, OtherNode } )

	end.
