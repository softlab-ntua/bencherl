% Copyright (C) 2010-2014 EDF R&D

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




% Result manager.


% This service allows to declare, keep track of, retrieve, make available,
% possibly post-process (rendering) to the user the results of the simulation.
%
% These are the subset of the simulation outputs that the user wants to obtain
% from the simulation.
%
% See also: class_ResultProducer.erl.
%
-module(class_ResultManager).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ResultSpecification, RootTimeManagerPid,
		 SimRunDir, ResultDirName, Metadata ).


% Where the result manager should be registered.
-define( registration_type, global_only ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, delete/1 ).



% Member method declarations.
-define( wooper_method_export, declareProbe/3,

		 setResourceMapping/3,

		 % Result pattern management:

		 isResultProducerWanted/2, isResultProducerWanted/3,
		 isResultProducerWantedWithOptions/3,

		 addTargetedPattern/2, addTargetedPatterns/2,
		 removeTargetedPattern/2, removeTargetedPatterns/2,
		 setTargetedPatterns/2,

		 addBlacklistedPattern/2, addBlacklistedPatterns/2,
		 removeBlacklistedPattern/2, removeBlacklistedPatterns/2,
		 setBlacklistedPatterns/2,

		 updateMetaData/2,

		 notifyResilienceAgentsOfProbes/2,

		 % Simulation listener API:
		 simulation_started/1, simulation_suspended/1, simulation_resumed/1,
		 simulation_succeeded/1, simulation_stopped/1,

		 getResultDirectory/1,

		 browseResultReports/1, addResultListener/2

).


% Static methods:
-define( wooper_static_method_export, get_registration_name/0,
		 get_result_manager/0, get_result_directory/0, browse_reports/0,
		 get_metadata_string/1, create_mockup_environment/0 ).



% Exported helpers:
-export([ to_string/1, display_queues/1 ]).


% Type section.
%
% See class_TimeManager.hrl for a detailed description of their meaning.



% Describes a Regex pattern to select results:
-type base_result_pattern() :: text_utils:regex_string().

-type target_pattern() :: base_result_pattern() |
		{ base_result_pattern(), class_ResultProducer:producer_options() }.


% Allows to whitelist specific results:
-type targeted_elements() :: { 'targeted_patterns', [ target_pattern() ] }.


% Allows to blacklist specific results:
-type blacklisted_elements() :: [ base_result_pattern() ].


-type selection_pattern() :: targeted_elements() | blacklisted_elements().


-type result_specification() :: 'all_outputs' | 'no_output'
		| 'all_basic_probes_only' | 'all_virtual_probes_only'
		| [ selection_pattern() ].



% Additional information to be passed to result producers:
%-type meta_data() :: option_list:option_list( atom(), text_utils:bin_string()).
-type meta_data() :: option_list:option_list().



% Records the result queue corresponding to a computing node, in order to
% control its load when generating the results and not overload/crash it.
%
-record( result_queue, {

		% The identifier of this queue:
		id :: basic_utils:count(),

		% The name of the computing hosts this result queue applies to:
		host_name :: net_utils:atom_host_name(),

		% The name of the corresponding computing node:
		node_name :: net_utils:atom_node_name(),

		% An evaluation of the simultaneous workers (depending notably on the
		% numberof available cores) that can be used to generate results on the
		% corresponding node:
		max_worker_count :: basic_utils:count(),

		% A list of the result producers that are currently working:
		waited_producers :: [ pid() ],

		% A list of the names (as binaries) of the result producers whose
		% generation is still pending:
		%
		% (we list names rather than PIDs as the names are (and must be) the
		% keys of the hashtable)
		%
		pending_results :: [ text_utils:bin_string() ]

} ).


-type result_queue() :: #result_queue{}.




% Describes a basic probe, as seen by the result manager.
%
% Such an entry is the value associated to the (binary) probe name, in the probe
% table.
%
-record( basic_probe_entry, {

	% The PID of the corresponding probe:
	probe_pid :: pid(),

	% The generation options for that probe:
	probe_options :: class_ResultProducer:producer_options(),

	% Tells whether this probe is to be tracked as a result:
	is_tracked :: boolean()

} ).


-type basic_probe_entry() :: #basic_probe_entry{}.


-export_type([ result_specification/0, meta_data/0, result_queue/0,
			  basic_probe_entry/0 ]).


% For result_manager_name:
-include("class_ResultManager.hrl").


% For instance_tracker_name:
-include("class_InstanceTracker.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.ResultManagement").


% Defines the hashtable type:
-define(hashtable_type,hashtable).

% For app_info*:
-include("traces.hrl").



% Implementation notes.


% The process to handle results is the following:
%
% 1. perform basic checks of the result specification given by the user,
% possibly precomputing match specifications (compiled regular expression)
%
% 2. handle declarations of result producers as they arrive, resulting in
% referencing them or not, potentially disabling them (all depending on their
% matching against the result specification)
%
% 3. on simulation successful termination, retrieve all referenced outputs on
% the user's behalf


% Once patterns are changed, resulting lists could be uniquified.


% The name of an output is ambiguous: for example, it can be either a basic
% probe result, or of a virtual probe result.
%
% As a consequence we have to look-up these names in tables maintained for
% different producer types, and ensure no ambiguity remains.

% So there are tables per producer type (ex: basic_probe_table) that converts
% its name into its information (PID, settings, etc.).


% Flow control had be added for the result generation, otherwise simulations
% encompassing a large number of probes might end up with thousands of plots to
% be generated at once, resulting in a large slow-down, the exhaustion of file
% descriptors or even the crash of the host.
%
% So currently each computing host is loaded as much as reasonably possible
% (depending on its number of cores), pending tasks being requested as current
% tasks are over.

% Note: we reason by hosts rather than nodes, as an host may have for example
% both a computing node and a user node. Result producers created from the
% simulation cases should be taken into account in the overall load of their
% host.

% This flow control does apply to the datalogger as a whole, which still
% performs its result generation task in a sequential, non-distributed way.

% Finally, non-tracked producers (ex: the probes of the performance tracker) are
% not driven by the result manager (the load they induce should be negligible,
% and they have been to be generated whether or not the simulation succeeds).

% There is also a pid_to_queue table, to associate the PID of a producer to the
% result queue which is in charge of.



% The class-specific attributes of a result manager instance are:
%
% - result_spec stores the result settings, after a first early
% validation/transformation pass, for later use; it is either an atom or a
% pair made of two lists (targeted/blacklisted); elements of the targeted
% list are { BinaryPatttern, PrecompiledMatchSpec, OptionList } triplets,
% whereas elements of the blacklisted list are
% { BinaryPatttern, PrecompiledMatchSpec } pairs; for both lists we keep the
% original pattern so that we can know easily which to remove (not sure we
% can compare reliably match specs)
%
% - basic_probe_table is an ?hashtable_type whose keys are names of basic probe
% names (as binaries) and whose values are basic_probe_entry() records
%
% - basic_probe_default_options is a list of default options for basic
% probes
%
% - virtual_probe_table is an ?hashtable_type whose keys are names of virtual
% probes (as binaries) and whose values are virtual_probe_entry() records
%
% - datalogger_default_options is a list of default options for virtual probes
%
% - pid_to_queue :: ?hashtable_type( pid(), result_queue#id() ) is an hashtable
% whose keys are the PID of the tracked producers and whose associated values
% are the ID of the result queue in charge of a given producer: allows to
% convert efficiently a producer PID into a queue ID
%
% - result_queues :: [ result_queue() ] stores the queues of pending results
% assigned to computing nodes, to control their load
%
% - user_node_info :: { net_utils:atom_node_name(), net_utils:atom_host_name() }
% allows to record information about the user node and host, notably so that any
% load induced by result producers created directly from the simulation case can
% be assigned to the relevant host
%
% - result_collected :: boolean() tells whether result collection has already
% been performed
%
% - result_found :: boolean() tells whether there was at least one actual
% collected result
%
% - listeners :: [ pid() ] is a list of the PID of the result listeners, to be
% notified of main result-related events
%
% - meta_data :: meta_data() is an ordered list of {Key,Value} pairs describing
% the metadata that will be sent to each created probe (useful to have these
% information stored in their result files)
%
% - root_time_manager_pid :: pid() the PID of the root time manager



% Constructs a new result manager, from following parameters:
%
% - ResultSpecification allows to specify coarsely what are the outputs to be
% promoted to results
%
% - RootTimeManagerPid is the PID of the root time manager; this result manager
% will subscribe to it as a listener, so that it can know when/if the simulation
% ended on success
%
% - SimRunDir is the name (specified as a string) of the directory in which the
% current simulation runs
%
% - ResultDirName is the name (specified as a plain string) of the result
% directory for the current simulation
%
% - Metadata :: option_list( atom(), text_utils:bin_string() ) is an ordered
% list of meta-data key-value pairs, whose keys are atoms, and values are binary
% strings
%
% See class_TimeManager.erl for further details.
%
-spec construct( wooper_state(), result_specification(), pid(),
	file_utils:directory_name(), file_utils:directory_name(),
	%option_list:option_list( atom(), text_utils:bin_string() ) ) ->
	option_list:option_list() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, "Result Manager" ),

	% We need the number of cores per computing host to later balancer the load:

	ProcessedResultSpec = check_and_transform_result_specification(
				   ResultSpecification ),

	RegistrationName = get_registration_name(),

	% Result producers rely on it:
	basic_utils:register_as( RegistrationName, ?registration_type ),

	class_InstanceTracker:register_agent( RegistrationName ),

	% We want to know when/if the simulation terminates on success:
	RootTimeManagerPid ! { addSimulationListener, self() },

	SimResultDirName = file_utils:join( ResultDirName, "simulation-results" ),

	FinalState = setAttributes( TraceState, [

		{ result_spec, ProcessedResultSpec },
		{ result_dir, SimResultDirName },
		{ simulation_run_dir, SimRunDir },
		{ basic_probe_table, ?hashtable_type:new() },
		{ basic_probe_default_options, [ data_and_plot ] },
		{ virtual_probe_table, ?hashtable_type:new() },
		{ datalogger_default_options, [ plot_only ] },
		{ pid_to_queue, ?hashtable_type:new() },
		{ result_queues, undefined },
		{ user_node_info, undefined },
		{ result_collected, false },
		{ result_found, false },
		{ listeners, [] },
		{ meta_data, Metadata },
		{ root_time_manager_pid, RootTimeManagerPid },
		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

								] ),

	% Late sending, to have it rely on the emitter category:
	?send_trace_fmt( FinalState, "Created with following result specification: "
		"~p and following meta-data: ~s",
		[ ResultSpecification, get_metadata_string( Metadata ) ] ),

	FinalState.



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?trace( "Deleting result manager." ),

	?getAttr(root_time_manager_pid) ! { removeSimulationListener, self() },

	class_InstanceTracker:unregister_agent(),

	basic_utils:unregister( get_registration_name(), ?registration_type ),

	% Then allow chaining:
	State.





% Member methods section.


% Notifies this manager of the information about nodes, hosts and their core
% count.
%
% The user node (and host) has to be specified as well, as the result producers
% directly created from the simulation case will end up being processed by the
% same host as a computing node (if any). So any result from the user node will
% have to be assigned to the local computing node instead, not too overload the
% corresponding host.
%
% (oneway)
%
-spec setResourceMapping( wooper_state(), [ { net_utils:atom_host_name(),
					  net_utils:atom_node_name(), basic_utils:count() } ],
			{ net_utils:atom_node_name(), net_utils:atom_host_name() } ) ->
								oneway_return().
setResourceMapping( State, HostCoreList, UserNodeInfos ) ->

	% Initially there is no pending result producer:
	InitialResultQueues = create_initial_queues( HostCoreList ),

	?wooper_return_state_only( setAttributes( State, [

			{ result_queues, InitialResultQueues },
			{ user_node_info, UserNodeInfos }

							  ] ) ).



% Creates the initial (empty) result queues.
%
% One objective is to ensure that the user host has a queue (and only one), as
% result producers may be created directly from the simulation case.
%
create_initial_queues( HostCoreList ) ->
	create_initial_queues( HostCoreList, _Count=1, _Acc=[] ).


create_initial_queues( _HostCoreList=[], _Count, Acc ) ->
	Acc;


create_initial_queues( _HostCoreList=[ { HostName, NodeName, CoreCount } | T ],
			 Count, Acc ) ->

	 NewQueue = #result_queue{

			id=Count,

			host_name=HostName,

			node_name=NodeName,

			% We ensure that each core will be busy enough, yet not too
			% overloaded:
			max_worker_count=2*CoreCount,

			% No generation triggered yet:
			waited_producers=[],

			% No result known yet:
			pending_results=[]

		 },

	create_initial_queues( T, Count + 1, [ NewQueue | Acc ] ).



% Processes the declaration of the specified probe, whose name is a binary here.
%
% IsToBeTracked tells whether this manager is to track this probe as the
% producer of actual simulation results (if true) or just a producer of results
% that are not simulation results, like facility probes (if false).
%
% Returns, depending on the current state of this manager, either
% output_requested or output_not_requested. Note that this may change over time,
% should targeted/blacklisted patterns are used and are changed.
%
% (request)
%
-spec declareProbe( wooper_state(), binary(), boolean() ) ->
		 request_return( 'output_not_requested' | 'output_requested' ).
declareProbe( State, BinProbeName, IsToBeTracked ) ->

	ProbeName = text_utils:binary_to_string( BinProbeName ),

	%?trace_fmt( "Declaration of basic probe '~s'.", [ ProbeName ] ),

	case is_result_wanted( BinProbeName, basic_probe, State ) of

		false ->
			?wooper_return_state_result( State, output_not_requested );

		{ true, ProbeOptions } ->

			% First, check the same probe is not registered twice:
			ProbeTable = ?getAttr(basic_probe_table),

			case ?hashtable_type:hasEntry( _Key=BinProbeName, ProbeTable ) of

				true ->
					throw( { probe_declared_more_than_once, ProbeName } );

				false ->
						ok

			end,

			ActualProbeOptions = case ProbeOptions of

				undefined ->
						?getAttr(basic_probe_default_options);

				Other ->
						Other

			end,

			RegisteredState = register_probe( BinProbeName, ActualProbeOptions,
								IsToBeTracked, _ProbePid=?getSender(), State ),

			?wooper_return_state_result( RegisteredState, output_requested )

	end.



% Registers specified probe, returns an updated state.
%
% (helper)
%
register_probe( BinProbeName, ProbeOptions, IsToBeTracked, ProbePid, State ) ->

	ProbeEntry = #basic_probe_entry{ probe_pid=ProbePid,
				   probe_options=ProbeOptions, is_tracked=IsToBeTracked },

	NewProbeTable = ?hashtable_type:addEntry( _K=BinProbeName, _V=ProbeEntry,
									   ?getAttr(basic_probe_table) ),


	% Now, takes care of the update of the result queues, if this probe is
	% tracked:
	%
	case IsToBeTracked of

		false ->
			setAttribute( State, basic_probe_table, NewProbeTable );

		true ->
			{ UserNodeName, UserHostAtom } = ?getAttr(user_node_info),

			{ ResultQueue, OtherQueues } = case node( ProbePid ) of

			   UserNodeName ->

					% This probe runs on the user node, hence we must register
					% it in the right result queue (based on the appropriate
					% computing host):
					%
					extract_queue_by_host( UserHostAtom,
										  ?getAttr(result_queues) );

				AComputingNode ->

					extract_queue_by_node( AComputingNode,
										  ?getAttr(result_queues) )

			end,

			% Adds this probe to the pending ones:
			NewResultQueue = ResultQueue#result_queue{

				 pending_results=[ BinProbeName
								  | ResultQueue#result_queue.pending_results ]

													  },

			% Updates also the PID-to-queue table:
			NewPidToQueueTable = ?hashtable_type:addEntry(
				_Key=ProbePid, _Value=NewResultQueue#result_queue.id,
				?getAttr(pid_to_queue) ),

			setAttributes( State, [

						{ basic_probe_table, NewProbeTable },
						{ result_queues, [ NewResultQueue | OtherQueues ] },
						{ pid_to_queue, NewPidToQueueTable }

								   ] )

	end.




% Section for targeted patterns.


% Tells whether the results of the specified producer, specified as a binary
% string, are wanted, i.e. whether they match the result specification.
%
% If not, then they will not be retrieved, thus there is no point in producing
% them anyway, and the corresponding producer should preferably not even be
% created.
%
% (const request)
%
-spec isResultProducerWanted( wooper_state(), text_utils:bin_string() ) ->
		   request_return( 'false' | { 'true', meta_data() } ).
isResultProducerWanted( State, ProducerName ) ->

	% When we do not know the nature (ex: basic or virtual probe) of a producer,
	% we consider it is wanted, knowing its results will be correctly managed on
	% simulation success (only drawback: we may allow some producers to
	% unnecessarily exist).

	isResultProducerWanted( State, ProducerName, _Nature=undefined ).



% Tells whether the outputs of the specified producer, whose name is specified
% as a binary string, are wanted, i.e. whether the name matches the result
% specification.
%
% If not, then these outputs will not be retrieved, thus there is no point in
% producing them anyway, and the corresponding producer should preferably not
% even be created.
%
% Nature is either 'basic_probe', 'virtual_probe' or 'undefined'.
%
% (const request)
%
-spec isResultProducerWanted( wooper_state(), text_utils:bin_string(),
	   class_ResultProducer:producer_nature() ) ->
		  request_return( 'false' | { 'true', meta_data() } ).
isResultProducerWanted( State, ProducerName, Nature ) ->

	%io:format( "isResultProducerWanted for producer '~s' of nature ~p.~n",
	%		  [ ProducerName, Nature ] ),

	Res = case is_result_wanted( ProducerName, Nature, State ) of

		false ->
			false;

		{ true, _Opts } ->
			{ true, ?getAttr(meta_data) }

	end,

	?wooper_return_state_result( State, Res ).



% Tells whether the results of the specified producer, specified as a binary
% string, are wanted, i.e. whether they match the result specification.
%
% If not, then they will not be retrieved, thus there is no point in producing
% them anyway, and the corresponding producer should preferably not even be
% created.
%
% Nature is either 'basic_probe', 'virtual_probe' or 'undefined'.
%
% Returns either false or { true, Metadata, Opts }.
%
% (const request)
%
-spec isResultProducerWantedWithOptions( wooper_state(),
		text_utils:bin_string(), class_ResultProducer:producer_nature() ) ->
   request_return( 'false' |
		  { 'true', class_ResultProducer:producer_options() } ).
isResultProducerWantedWithOptions( State, ProducerName, Nature ) ->

	Res = is_result_wanted( ProducerName, Nature, State ),

	%io:format( "isResultProducerWantedWithOptions for producer '~s' "
	%			"of nature ~p: answer is '~p'.~n", [ ProducerName, Nature,
	%           Res ] ),

	?wooper_return_state_result( State, Res ).



% Adds specified targeted result pattern, expressed as a plain string, to the
% current result specification, which must be already using targeted/blacklisted
% patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec addTargetedPattern( wooper_state(), base_result_pattern() ) ->
								oneway_return().
addTargetedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTarget = text_utils:string_to_binary(Pattern),
					setAttribute( State, result_spec,
								 { [ BinTarget | Targets ], BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Pattern, Other ] ),
					throw( { target_cannot_be_added, Pattern, Other } )

			end;

		false ->
			throw( { added_target_not_a_string, Pattern } )

	end,

	?wooper_return_state_only( NewState ).



% Adds specified targeted result patterns, expressed as plain strings, to the
% current result specification, which must be already using targeted/blacklisted
% patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec addTargetedPatterns( wooper_state(), [ base_result_pattern() ] ) ->
								oneway_return().
addTargetedPatterns( State, Patterns ) ->

	NewState = case text_utils:is_list_of_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( Patterns ),
					setAttribute( State, result_spec,
								 { BinTargets ++ Targets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Patterns, Other ] ),
					throw( { targets_cannot_be_added, Patterns, Other } )

			end;

		false ->
			throw( { added_targets_not_all_strings, Patterns } )

	end,

	?wooper_return_state_only( NewState ).



% Removes specified targeted result pattern, expressed as a plain string, from
% the current result specification, which must be already using
% targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec removeTargetedPattern( wooper_state(), base_result_pattern() ) ->
								   oneway_return().
removeTargetedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTarget = text_utils:string_to_binary( Pattern ),
					% Supposed to be there only once:
					NewTargets = lists:delete( BinTarget, Targets ),
					setAttribute( State, result_spec,
								 { NewTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be removed "
								"when relying on an incompatible result "
								"specification (trying to remove "
								"'~p' from '~p').", [ Pattern, Other ] ),
					throw( { target_cannot_be_removed, Pattern, Other } )

			end;

		false ->
			throw( { removed_target_not_a_string, Pattern } )

	end,

	?wooper_return_state_only( NewState ).



% Removes specified targeted result patterns, expressed as plain strings, from
% the current result specification, which must be already using
% targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec removeTargetedPatterns( wooper_state(), [base_result_pattern()] ) ->
									oneway_return().
removeTargetedPatterns( State, Patterns ) ->

	NewState = case text_utils:is_list_of_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( Patterns ),
					NewTargets = lists:subtract( Targets, BinTargets ),
					setAttribute( State, result_spec,
								 { NewTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, no targeted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Patterns, Other ] ),
					throw( { targets_cannot_be_removed, Patterns, Other } )

			end;

		false ->
			throw( { removed_targets_not_all_strings, Patterns } )

	end,

	?wooper_return_state_only( NewState ).



% Replaces the current targeted result patterns by the specified ones, expressed
% as plain strings, in the current result specification, which must be already
% using targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec setTargetedPatterns( wooper_state(), [ base_result_pattern() ] ) ->
								 oneway_return().
setTargetedPatterns( State, NewPatterns ) ->

	NewState = case text_utils:is_list_of_strings( NewPatterns ) of

		true ->

			case ?getAttr(result_spec) of

				{ _Targets, BlackLists } ->
					BinTargets = text_utils:strings_to_binaries( NewPatterns ),
					setAttribute( State, result_spec,
								 { BinTargets, BlackLists } );

				Other ->
					?error_fmt( "Error, targeted patterns cannot be set "
								"when relying on an incompatible result "
								"specification (trying to set '~p' in '~p').",
								[ NewPatterns, Other ] ),
					throw( { targets_cannot_be_set, NewPatterns, Other } )

			end;

		false ->
			throw( { removed_targets_not_all_strings, NewPatterns } )

	end,

	?wooper_return_state_only( NewState ).




% Section for blacklisted patterns.


% Adds specified blacklisted result pattern, expressed as a plain string, to the
% current result specification, which must be already using targeted/blacklisted
% patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec addBlacklistedPattern( wooper_state(), base_result_pattern() ) ->
								   oneway_return().
addBlacklistedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklist = text_utils:string_to_binary( Pattern ),
					setAttribute( State, result_spec,
							 { Targetlists, [ BinBlacklist | BlackLists ] } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Pattern, Other ] ),
					throw( { blacklist_cannot_be_added, Pattern, Other } )

			end;

		false ->
			throw( { added_blacklist_not_a_string, Pattern } )

	end,

	?wooper_return_state_only( NewState ).



% Adds specified blacklisted result patterns, expressed as plain strings, to the
% current result specification, which must be already using targeted/blacklisted
% patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec addBlacklistedPatterns( wooper_state(), [base_result_pattern()] ) ->
									oneway_return().
addBlacklistedPatterns( State, Patterns ) ->

	NewState = case text_utils:is_list_of_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklists = text_utils:strings_to_binaries( Patterns ),
					setAttribute( State, result_spec,
						 { Targetlists, BinBlacklists ++ BlackLists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Patterns, Other ] ),
					throw( { blacklists_cannot_be_added, Patterns, Other } )

			end;

		false ->
			throw( { added_blacklists_not_all_strings, Patterns } )

	end,

	?wooper_return_state_only( NewState ).



% Removes specified blacklisted result pattern, expressed as a plain string,
% from the current result specification, which must be already using
% targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec removeBlacklistedPattern( wooper_state(), base_result_pattern() ) ->
									  oneway_return().
removeBlacklistedPattern( State, Pattern ) ->

	NewState = case text_utils:is_string( Pattern ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklist = text_utils:string_to_binary( Pattern ),
					% Supposed to be there only once:
					NewBlacklists = lists:delete( BinBlacklist, BlackLists ),
					setAttribute( State, result_spec,
								 { Targetlists, NewBlacklists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be removed "
								"when relying on an incompatible result "
								"specification (trying to remove "
								"'~p' from '~p').", [ Pattern, Other ] ),
					throw( { blacklist_cannot_be_removed, Pattern, Other } )

			end;

		false ->
			throw( { removed_blacklist_not_a_string, Pattern } )

	end,

	?wooper_return_state_only( NewState ).



% Removes specified blacklisted result patterns, expressed as plain strings,
% from the current result specification, which must be already using
% targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec removeBlacklistedPatterns( wooper_state(), [base_result_pattern()] ) ->
									   oneway_return().
removeBlacklistedPatterns( State, Patterns ) ->

	NewState = case text_utils:is_list_of_strings( Patterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ Targetlists, BlackLists } ->
					BinBlacklists = text_utils:strings_to_binaries( Patterns ),
					NewBlacklists = lists:subtract( BlackLists, BinBlacklists ),
					setAttribute( State, result_spec,
								 { Targetlists, NewBlacklists } );

				Other ->
					?error_fmt( "Error, no blacklisted pattern can be added "
								"when relying on an incompatible result "
								"specification (trying to add '~p' to '~p').",
								[ Patterns, Other ] ),
					throw( { blacklists_cannot_be_removed, Patterns, Other } )

			end;

		false ->
			throw( { removed_blacklists_not_all_strings, Patterns } )

	end,

	?wooper_return_state_only( NewState ).



% Replaces the current blacklisted result patterns by the specified ones,
% expressed as plain strings, in the current result specification, which must be
% already using targeted/blacklisted patterns, and not shortcut atoms.
%
% Note: one must of course ensure that the patterns are changed *before* a
% producer whose name is intended to match or not match declares itself.
%
% (oneway)
%
-spec setBlacklistedPatterns( wooper_state(), [ base_result_pattern()] ) ->
									oneway_return().
setBlacklistedPatterns( State, NewPatterns ) ->

	NewState = case text_utils:is_list_of_strings( NewPatterns ) of

		true ->
			case ?getAttr(result_spec) of

				{ TargetLists, _BlackLists } ->
					BinBlacklists = text_utils:strings_to_binaries(
																NewPatterns ),
					setAttribute( State, result_spec,
								 { TargetLists, BinBlacklists } );

				Other ->
					?error_fmt( "Error, blacklisted patterns cannot be set "
								"when relying on an incompatible result "
								"specification (trying to set '~p' in '~p').",
								[ NewPatterns, Other ] ),
					throw( { blacklists_cannot_be_set, NewPatterns, Other } )

			end;

		false ->
			throw( { removed_blacklists_not_all_strings, NewPatterns } )

	end,

	?wooper_return_state_only( NewState ).



% Updates (add any non-existing entry, overwrite it otherwise) the current
% meta-data with the specified (possibly, user-specified) one.
%
% (request, for synchronicity)
%
-spec updateMetaData( wooper_state(), meta_data() ) ->
						 request_return( 'meta_data_added' ).
updateMetaData( State, UpdatingMetaData ) ->

	NewMetaData = lists:foldl( fun( Pair, Acc ) ->
									   option_list:set( Pair, Acc ) end,
							  _AccInit=?getAttr(meta_data),
							  _List=option_list:enumerate( UpdatingMetaData )
							 ),

	NewState = setAttribute( State, meta_data, NewMetaData ),

	?wooper_return_state_result( NewState, meta_data_added ).



% Tells each resilience agent what are its (local) probes that it must manage.
%
% Typically called by the resilience manager, when a serialisation must take
% place.
%
% (request, for synchronicity)
%
-spec notifyResilienceAgentsOfProbes( wooper_state(), [ pid() ] ) ->
											request_return( 'probes_notified' ).
notifyResilienceAgentsOfProbes( State, NodeAgents ) ->

	% Retrieving first the PID of all basic probes:
	ProbePids = [ ProbePid || { _NameKey,
			_Value=#basic_probe_entry{ probe_pid=ProbePid } }
				 <- ?hashtable_type:enumerate( ?getAttr(basic_probe_table) ) ],

	% Creating then an empty hashtable where the keys are the node names, and
	% the associated values are a pair made of the PID of the resilience agent
	% corresponding to that node and of the list of the PIDs of the
	% corresponding (local) probes (initially an empty list):
	%
	EmptyNodeTable = lists:foldl( fun( AgentPid, TableAcc ) ->
				?hashtable_type:addEntry( _K=node( AgentPid ),
									  _V={ AgentPid, _Probes=[] }, TableAcc )
								  end,
								  _EmptyInitialAcc=?hashtable_type:new(),
								  _EmptyList=NodeAgents ),

	% Now let's map the probes onto the agents, using nodes as intermediary:
	FilledNodeTable = lists:foldl( fun( ProbePid, TableAcc ) ->
						% Simply adds this probe to the list for the right node:
						NodeKey = node( ProbePid ),
						{ AgentPid, ProbeList } = ?hashtable_type:getEntry(
								NodeKey, TableAcc ),
						?hashtable_type:addEntry( NodeKey,
									{ AgentPid, [ ProbePid | ProbeList ] },
									TableAcc )

								   end,
							 _FilledInitialAcc=EmptyNodeTable,
							 _FilledList=ProbePids ),

	% Now we can notify each resilience agent of its probes:
	[ AgentPid ! { notifyOfLocalProbes, [ AgentProbeList ], self() } ||
		{ AgentPid, AgentProbeList } <-
			?hashtable_type:values( FilledNodeTable ) ],

	wooper:wait_for_request_answers( _RequestedList=NodeAgents,
											_AckAtom=probes_recorded ),

	?wooper_return_state_result( State, probes_notified ).



% Simulation listener section.



% Optimises result ?hashtable_types and lists all known registered results.
%
% (oneway)
%
-spec simulation_started( wooper_state() ) -> oneway_return().
simulation_started( State ) ->

	BasicProbeTable = ?getAttr(basic_probe_table),

	OptimisedBasicProbeTable = ?hashtable_type:optimise( BasicProbeTable ),
	%?hashtable_type:display( "Basic probe table", OptimisedBasicProbeTable ),

	% Note: we do not distinguish here between probes that are tracked or not:
	BasicProbeBinNames = ?hashtable_type:keys( OptimisedBasicProbeTable ),
	BasicProbeString = text_utils:binary_list_to_string( BasicProbeBinNames ),
	BasicProbeCount = length( BasicProbeBinNames ),

	VirtualProbeTable = ?getAttr(virtual_probe_table),
	OptimisedVirtualProbeTable = ?hashtable_type:optimise( VirtualProbeTable ),
	%?hashtable_type:display( "Virtual probe table",
	%     OptimisedVirtualProbeTable ),

	VirtualProbeBinNames = ?hashtable_type:keys( OptimisedVirtualProbeTable ),
	VirtualProbeString = text_utils:binary_list_to_string(
												  VirtualProbeBinNames ),
	VirtualProbeCount = length( VirtualProbeBinNames ),

	ResultCount = BasicProbeCount + VirtualProbeCount,

	case ResultCount of

		0 ->
			% Results *may* also be declared at simulation-time (dynamically):
			?info( "At simulation start, no expected result is identified." );

		_ ->
			case BasicProbeCount of

				0 ->
					?info_fmt( "At simulation start, only virtual probes "
							   "were declared, they were ~B of them:~s",
							   [ VirtualProbeCount, VirtualProbeString ] );

				_ ->
					case VirtualProbeCount of

						0 ->
							?info_fmt( "At simulation start, only basic probes "
									   "were declared, they were ~B of them:~s",
									   [ BasicProbeCount, BasicProbeString ] );

						_ ->
							?info_fmt( "At simulation start, ~B results were "
									   "declared, made of ~B basic probes and "
									   "~B virtual probes.~n"
									   "Basic probes were:~s~n"
									   "Virtual probes were:~s",
									   [ ResultCount, BasicProbeCount,
										VirtualProbeCount, BasicProbeString,
										VirtualProbeString ] )

					end

			end

	end,

	?wooper_return_state_only( setAttributes( State, [

			{ basic_probe_table, OptimisedBasicProbeTable },
			{ virtual_probe_table, OptimisedVirtualProbeTable }

													  ] ) ).



% Notification ignored.
%
-spec simulation_suspended( wooper_state() ) -> oneway_return().
simulation_suspended( State ) ->
	?wooper_return_state_only( State ).


% Notification ignored.
%
-spec simulation_resumed( wooper_state() ) -> oneway_return().
simulation_resumed( State ) ->
	?wooper_return_state_only( State ).



% This corresponds to a message, interpreted as a oneway call, being sent by the
% root time manager whenever the simulation succeeded, knowing this result
% manager subscribed to it as a simulation listener.
%
-spec simulation_succeeded( wooper_state() ) -> oneway_return().
simulation_succeeded( State ) ->

	?trace( "Simulation succeeded, collecting results now." ),

	class_PluginManager:notify( on_result_gathering_start ),

	% Let's create the result directory for that simulation:
	file_utils:set_current_directory( ?getAttr(simulation_run_dir) ),

	ResultDirName = ?getAttr(result_dir),
	file_utils:create_directory( ResultDirName ),

	% Note that this would have side-effects, as the current working directory
	% of the whole VM (the one of the simulation case - i.e. the user one) would
	% then change, which as detailed below is not wanted:
	%
	% ok = file:set_cwd( ResultDirName ),

	% There would be no problem for all producers that were created from models,
	% as they are running on different VMs with different working directories
	% (by default, under '/tmp').
	%
	% But producers created from the simulation case (ex: a probe created from a
	% test) are in the same VM as the result manager, and thus share the same
	% current working directory.
	%
	% If we went to the output directory, then the .dat file could have been
	% produced in the initial directory (if written immediately), and thus would
	% not be found. The solution is to stay in the initial directory, and to
	% write/extract results in the output one.

	% Requesting results to be produced and sent in parallel, knowing that some
	% flow control is enforced, otherse a given host might be overwhelmed by the
	% number of parallel reports requested, and may even crash because of it.

	?trace_fmt( "Simulation succeeded, collecting results now "
				"from producers in result directory '~s', "
				"while current one is '~s'.~n",
				[ ResultDirName, file_utils:get_current_directory() ] ),

	% First, triggers the possibly most loaded producer:
	VirtualState = trigger_virtual_probe_results( State ),

	% Then, the other producers, and wait for them all:
	ProbeState = manage_all_producers( VirtualState ),

	Listeners = ?getAttr(listeners),

	?trace_fmt( "All results successfully gathered, "
				"notifying all listeners (~p).", [ Listeners ] ),

	% Notifies that the result collection is over, but does not imply there were
	% actual results:
	%
	[ Pid ! results_collected || Pid <- Listeners ],

	class_PluginManager:notify( on_result_gathering_stop ),

	?wooper_return_state_only( ProbeState ).




% Requests the data-logger for any result needed.
%
% Returns an updated state, with updated queues.
%
trigger_virtual_probe_results( State ) ->

	case ?getAttr(result_spec) of

		no_output ->
		   State;


		all_basic_probes_only ->
		   State;


		_Other -> % Includes: all_outputs, all_virtual_probes_only:

			% First, triggers the result generation:

			DataloggerPid = class_DataLogger:get_main_datalogger(),

			DataloggerPid ! { sendResults,
							 [ ?getAttr(datalogger_default_options) ], self() },


			% Then registers this generation:

			DataloggerNode = node( DataloggerPid ),

			% We add only now the datalogger to the pending result producers;
			% this is fortunate, as it will thus be the first one to be picked
			% and thus the first to be triggered (a good thing, knowing it might
			% induce a lot of processing).

			{ DataloggerQueue, OtherQueues } = extract_queue_by_node(
					DataloggerNode, ?getAttr(result_queues) ),

			NewPidToQueueTable = ?hashtable_type:addEntry( _K=DataloggerPid,
				_V=DataloggerQueue#result_queue.id, ?getAttr(pid_to_queue) ),

			% Waited list was presumably empty:
			NewWaited = [ DataloggerPid
						 | DataloggerQueue#result_queue.waited_producers ],

			NewDataloggerQueue = DataloggerQueue#result_queue{
								   waited_producers = NewWaited },

			setAttributes( State, [

				{ result_queues, [ NewDataloggerQueue | OtherQueues ] },
				{ pid_to_queue, NewPidToQueueTable }
				% result_collected set to true upon acknowledgement that all
				% result producers finished.

								  ] )

	end.



% Requests by chunks (whose size depends on the core count of the target host)
% all relevant probes to send their result(s).
%
% Returns an updated state.
%
manage_all_producers( State ) ->

	ManagedState = case ?getAttr(result_spec) of

		no_output ->
			State;


		 % Includes: all_outputs, all_virtual_probes_only,
		 % all_basic_probes_only; basically, we wait and trigger producer until
		 % all of them are over :
		_Other ->

			Queues = ?getAttr(result_queues),

			TotalProducerCount = compute_producer_count( Queues, _Sum=0 ),

			ResultFound = ( TotalProducerCount > 0 ),

			ProbeTable = ?getAttr(basic_probe_table),

			% First trigger of production:
			{ TriggeredQueues, UpdatedPidToQueueTable } = load_result_queues(
						Queues, ?getAttr(pid_to_queue), ProbeTable ),

			ProducerTimeout = get_producer_time_out(),

			% Now waits until all producers have been processed:
			DepletedQueues = wait_and_exhaust_queues( TriggeredQueues,
				UpdatedPidToQueueTable, TotalProducerCount, ProbeTable,
				ProducerTimeout, State ),

			setAttributes( State, [

						{ result_queues, DepletedQueues },
						{ result_found, ResultFound }

								] )

	end,

	setAttribute( ManagedState, result_collected, true ).



compute_producer_count( _Queues=[], Sum ) ->
	Sum;

compute_producer_count( _Queues=[ #result_queue{ pending_results=Pending,
							waited_producers=Waited } | T ], Sum ) ->

	% We may have producers already launched and waited:
	compute_producer_count( T, Sum + length( Pending ) + length( Waited ) ).



% Ensures that each result queue has all possible workers working.
%
% Returns a { TriggeredQueues, PidToQueueTable } pair, where TriggeredQueues is
% a list of updated queues and PidToQueueTable is an updated PID to queue ID
% translation table.
%
load_result_queues( Queues, PidToQueueTable, ProbeTable ) ->
	load_result_queues( Queues, PidToQueueTable, ProbeTable, _AccQueues=[] ).


load_result_queues( _Queues=[], PidToQueueTable, _ProbeTable, AccQueues ) ->
	{ AccQueues, PidToQueueTable } ;

load_result_queues( _Queues=[ Q | T ], PidToQueueTable, ProbeTable,
					AccQueues ) ->

	{ UpdatedQueue, ProducerPidList } = load_result_queue( Q, ProbeTable ),

	QueueId = UpdatedQueue#result_queue.id,

	% Adds these key/value pairs:
	NewPidEntries = [ { Pid, QueueId } || Pid <- ProducerPidList ],

	UpdatedPidToQueueTable = ?hashtable_type:addEntries( NewPidEntries,
									PidToQueueTable ),

	load_result_queues( T, UpdatedPidToQueueTable, ProbeTable,
					   [ UpdatedQueue | AccQueues ] ).



% Loads as much as possible specified queue.
%
% Returns { UpdatedQueue, ProducerPidList }.
%
load_result_queue( Queue=#result_queue{ pending_results=[] }, _ProbeTable ) ->

	% Here, no pending result, nothing to do, thus nothing to change:
	{ Queue, _ProducerPidList=[] };


load_result_queue( Queue=#result_queue{ max_worker_count=MaxCount,
		waited_producers=Waited, pending_results=Pending }, ProbeTable ) ->

	% There are results to be generated, how many worker slots are free?
	SpareSlots = MaxCount - length( Waited ),

	% Selects the SpareSlots first producers:
	{ FirstProducers, OtherProducers } = list_utils:split_at( Pending,
															  SpareSlots ),

	ProducerPidList = trigger_producers( FirstProducers, ProbeTable ),

	NewQueue = Queue#result_queue{
				 waited_producers= ProducerPidList ++ Waited,
				 pending_results=OtherProducers },

	{ NewQueue, ProducerPidList }.



% Triggers specified producers.
%
% Returns a list of their PIDs.
%
trigger_producers( Producers, ProbeTable ) ->
	trigger_producers( Producers, ProbeTable, _Acc=[] ).


trigger_producers( _Producers=[], _ProbeTable, Acc ) ->
	Acc;

trigger_producers( _Producers=[ ProducerName | T ], ProbeTable, Acc ) ->

	ProducerPid = trigger_producer( ProducerName, ProbeTable ),

	trigger_producers( T, ProbeTable, [ ProducerPid | Acc ] ).



% Triggers specified result producer, and returns its PID.
%
% Defined for reusability.
%
trigger_producer( ProducerName, ProbeTable ) ->

	ProbeEntry = ?hashtable_type:getEntry( ProducerName, ProbeTable ),

	ProducerPid = ProbeEntry#basic_probe_entry.probe_pid,

	Options = ProbeEntry#basic_probe_entry.probe_options,

	%io:format( "Triggering producer ~s (~w).~n",
	%		   [ ProducerName, ProducerPid ] ),

	ProducerPid ! { sendResults, [ Options ], self() },

	ProducerPid.



% Waits for current triggered producers to finish, and replenish workers for all
% queues, until no result is pending.
%
% Returns the updated queues.
%
wait_and_exhaust_queues( Queues, _PidToQueueTable, _TotalProducerCount=0,
						_ProbeTable, _ProducerTimeout, _State ) ->
	%io:format( "No producer left, finished!" ),
	Queues;


wait_and_exhaust_queues( Queues, PidToQueueTable, TotalProducerCount,
						 ProbeTable, ProducerTimeout, State ) ->

	%io:format( "Still ~B producers waited.", [ TotalProducerCount ] ),

	% Hijacks the WOOPER main loop to better manage time-outs.
	%
	% We consider that, on a previous step, an initial request in order to
	% produce the first results was sent; let's wait for answers and replenish
	% waited lists:
	%
	ActualProducerPid = receive

		{ wooper_result, { ProducerPid, archive, BinArchive } } ->

			Filenames = file_utils:zipped_term_to_unzipped_files( BinArchive,
												  ?getAttr(result_dir) ),

			?trace_fmt( "Received an archive from producer ~w, "
						"following files were extracted from '~s': ~s",
						[ ProducerPid, file_utils:get_current_directory(),
						  text_utils:string_list_to_string( Filenames ) ] ),

			ProducerPid;


		{ wooper_result, { ProducerPid, raw, { BinFilename, BinContent } } } ->

			Filename = text_utils:binary_to_string( BinFilename ),
			TargetFilename = file_utils:join( ?getAttr(result_dir), Filename ),
			file_utils:write_whole( TargetFilename, BinContent ),

			?trace_fmt( "Received a raw file from producer ~w: '~s', "
						"written from '~s'.",
						[ ProducerPid, BinFilename,
						  file_utils:get_current_directory() ] ),

			ProducerPid;

		{ wooper_result, { ProducerPid, no_result } } ->

			% A producer may have nothing to report (ex: the data-logger):
			?trace_fmt( "Producer ~w notified that it had no result "
						"to provide.", [ ProducerPid ] ),

			ProducerPid

	% Time-out renewed at each producer answer:
	after ProducerTimeout ->
			throw( { result_producer_time_out, TotalProducerCount,
					 Queues } )

	end,

	{ NewQueues, NewPidToQueueTable } = update_queues_after_result(
			 ActualProducerPid, PidToQueueTable, ProbeTable, Queues ),

	wait_and_exhaust_queues( NewQueues, NewPidToQueueTable,
			TotalProducerCount - 1, ProbeTable, ProducerTimeout, State ).



% Updates the queues after the reception of the result from specified producer.
%
% Returns { NewQueues, NewPidToQueueTable }.
%
update_queues_after_result( ProducerPid, PidToQueueTable, ProbeTable,
							Queues ) ->

	QueueId = ?hashtable_type:getEntry( ProducerPid, PidToQueueTable ),

	{ Queue, OtherQueues } = extract_queue_by_id( QueueId, Queues ),

	NewPidToQueueTable = ?hashtable_type:removeEntry( ProducerPid,
													 PidToQueueTable ),

	DelWaited = lists:delete( ProducerPid,
							  Queue#result_queue.waited_producers ),

	% One less producer online, hence one more to trigger:
	{ NewPending, NewWaited } = case Queue#result_queue.pending_results of

				[] ->
					{ [], DelWaited };

				[ ProducerName | T ] ->

					%io:format( "Replenishing queue #~B with '~s'.~n",
					%		   [ QueueId, ProducerName ] ),

					NewProducerPid = trigger_producer( ProducerName,
													  ProbeTable ),
					{ T, [ NewProducerPid | DelWaited ] }

	end,

	NewQueue = Queue#result_queue{

					 waited_producers=NewWaited,
					 pending_results=NewPending

	},

	NewQueues = [ NewQueue | OtherQueues ],

	{ NewQueues, NewPidToQueueTable }.



% Notification ignored.
%
-spec simulation_stopped( wooper_state() ) -> oneway_return().
simulation_stopped( State ) ->
	?wooper_return_state_only( State ).




% Returns the current result directory in use.
%
% (const request)
%
-spec getResultDirectory( wooper_state() ) ->
								request_return( file_utils:directory_name() ).
getResultDirectory( State ) ->
	?wooper_return_state_result( State, ?getAttr(result_dir) ).



% Requires the result reports to be browsed.
%
% (const request)
%
-spec browseResultReports( wooper_state() ) ->
							 request_return( 'results_browsed' ).
browseResultReports( State ) ->

	% Checking:
	true = ?getAttr(result_collected),

	case ?getAttr(result_found) of

		true ->
			% Note that we will launch the viewer even if all results were
			% non-graphical:
			executable_utils:browse_images_in( ?getAttr(result_dir) );

		false ->
			io:format( "(no result matched the specification, "
					   "thus nothing to be browsed here)~n" )

	end,

	?wooper_return_state_result( State, results_browsed ).



% Adds specified process as a result listener, that will be notified of any
% event it missed.
%
% (oneway)
%
-spec addResultListener( wooper_state(), pid() ) -> oneway_return().
addResultListener( State, ListenerPid ) ->

	% If results were already collected, send past notification:
	case ?getAttr(result_collected) of

		true ->
			ListenerPid ! results_collected;

		false ->
			ok

	end,

	?wooper_return_state_only(
			   appendToAttribute( State, listeners, ListenerPid ) ).




% Static methods section.


% Returns the atom corresponding to the name the result manager should be
% registered as.
%
% Note: executed on the caller node.
%
-spec get_registration_name() -> net_utils:atom_node_name().
get_registration_name() ->
	sim_diasca_result_manager.



% Returns the PID of the (unique) result manager.
%
% (static method, to be used by clients of the result manager, notably result
% producers).
%
-spec get_result_manager() -> pid().
get_result_manager() ->
	basic_utils:wait_for_global_registration_of( get_registration_name() ).



% Returns the path to the current result directory.
%
% (static)
%
-spec get_result_directory() -> file_utils:directory_name().
get_result_directory() ->

	ManagerPid = get_result_manager(),

	ManagerPid ! { getResultDirectory, [], self() },

	receive

		{ wooper_result, Dir } when is_list( Dir ) ->
			Dir

	end.



% Browse reports.
%
% Static method that allows to request the automatic displaying of graphical
% reports (ex: plots from plots) depending on the batch mode being enabled or
% not.
%
% To be used from simulation cases.
%
% Replaces a less reliable macro.
%
% (static)
%
-spec browse_reports() -> no_return().
browse_reports() ->

	% This function is state-less, which is very convenient as, since the start,
	% most services may have been redeployed in the meantime, due to resilience
	% mechanisms having to kick in.

	% Generating performance monitoring results, if any, and in parallel to
	% simulation results:
	%
	WaitForPerformanceTracker = case class_PerformanceTracker:get_tracker() of

		not_registered ->
			%basic_utils:display( "No performance tracker was enabled" ),
			false;


		Pid ->

			?notify_debug_fmt( "Requesting reports from the "
						"performance tracker ~p.~n", [ Pid ] ),

			Pid ! { generateMonitoringReports, [], self() },

			true

	end,

	ResultManagerPid = class_ResultManager:get_result_manager(),

	% All simulation test cases are expected to register themselves as
	% simulation listeners (ex: at simulation start), thus they should be
	% notified also about the simulation success (if any).

	case executable_utils:is_batch() of

		true ->

			% In batch mode here.

			receive

				% We *must* wait for that, otherwise results will not be copied
				% on time before the VM halts (race condition with case
				% termination, we must wait synchronously):
				simulation_succeeded ->

					% We nevertheless have to wait for them:
					ResultManagerPid ! { addResultListener, self() },

					io:format(
					  "In batch mode, no browsing of results performed. "
					  "Waiting for their processing and retrieval.~n" ),


					receive

						results_collected ->
							io:format( "Results are available now.~n" )

					end

			end;


		false ->

			% In interactive mode here.

			receive

				% All simulation test cases are expected to register themselves
				% as simulation listeners (ex: at simulation start), thus they
				% should be notified also about the simulation success (if any).
				simulation_succeeded ->

					% We need to wait for the results:
					ResultManagerPid ! { addResultListener, self() },

					io:format( "Simulation success, result reports to be "
							   "processed, collected then browsed now.~n" ),

					receive

						results_collected ->
							io:format( "Results are available now.~n" )

					end,

					ResultManagerPid ! { browseResultReports, [], self() },

					receive

						{ wooper_result, results_browsed } ->
							?notify_info( "Result reports have been "
									   "successfully browsed." )

					end

			after 5000 ->

					io:format( "Simulation failed, no result gathered." )

			end

	end,

	case WaitForPerformanceTracker of

		true ->

			%basic_utils:display(
			%			"Waiting finally for the performance tracker" ),

			receive

				{ wooper_result, report_generated } ->

					?notify_info( "Monitoring reports have been "
								  "successfully generated." ),

					io:format( "Monitoring reports have been "
							   "successfully generated.~n" )

			end;

		false ->

			ok

	end.



% Returns a plain string describing the specified result-related meta-data.
%
% (static)
%
-spec get_metadata_string( meta_data() ) -> string().
get_metadata_string( Metadata ) ->

	MetadataAllStrings = [ text_utils:binary_to_string( BinText ) ||
					{ _Key, BinText } <- Metadata ],

	text_utils:string_list_to_string( MetadataAllStrings ) .




% Creates a mock-up environment suitable for the test of result producers in
% isolation (i.e. without creating all the simulation services).
%
% (static)
%
-spec create_mockup_environment() -> pid().
create_mockup_environment() ->

	spawn_link( fun() ->

			basic_utils:register_as( ?instance_tracker_name, local_only ),
			basic_utils:register_as( ?result_manager_name, global_only ),

			create_mockup_environment_loop()

				end ).



% Forces the executing process to linger (will never terminate), otherwise for
% example a probe destructor would not find its expected instance tracker as a
% still registered process.
%
create_mockup_environment_loop() ->

	% Mimics a result manager (and the existence of a local instance tracker):
	%

	% Fakes a local instance tracker and a result manager:
	receive

		{ registerResultProducer, _BinName, ProducerPid } ->
			ProducerPid ! { wooper_result, result_producer_registered };

		{ registerAgent, _AgentRef, AgentPid } ->
			AgentPid ! { wooper_result, agent_registered };

		{ declareProbe, [ _Name, _IsTrackedProducer ], ProbePid } ->
			ProbePid ! { wooper_result, output_requested }

	end,

	create_mockup_environment_loop().



% Helper functions.


% We want to defer as much as possible the fine analysis of the result
% matchings, hownever we ensure first that the users used specifications that
% are obviously incorrect (mostly syntax checkings).
%
% Result is either an atom of a pair of two lists.
check_and_transform_result_specification( all_outputs ) ->
	all_outputs;

check_and_transform_result_specification( no_output) ->
	no_output;

check_and_transform_result_specification( all_basic_probes_only ) ->
	all_basic_probes_only;

check_and_transform_result_specification( all_virtual_probes_only ) ->
	all_virtual_probes_only;

% Returns {Targets,Blacklists}:
check_and_transform_result_specification( Specs ) when is_list(Specs) ->
	manage_patterns( Specs, _Targets=[], _Blacklists=[] );

check_and_transform_result_specification( NonMatching ) ->
	throw( { invalid_result_specification, NonMatching } ).



manage_patterns( _Specs=[], Targets, Blacklists ) ->
	{ Targets, Blacklists };

manage_patterns( [ { targeted_patterns, L } | T ], Targets, Blacklists )
  when is_list(L) ->
   manage_patterns( T, manage_targets(L) ++ Targets, Blacklists );

manage_patterns( [ { targeted_patterns, L } | _T ], _Targets, _Blacklists ) ->
	throw( { invalid_result_target_pattern, L } );

manage_patterns( [ { blacklisted_patterns, L } | T ], Targets, Blacklists )
  when is_list(L) ->
	manage_patterns( T, Targets, manage_blacklists(L) ++ Blacklists );

manage_patterns( [ { blacklisted_patterns, L } | _T ], _Targets,
				_Blacklists ) ->
	throw( { invalid_result_blacklist_pattern, L } );

manage_patterns( [ Any | _T ], _Targets, _Blacklists ) ->
	throw( { invalid_result_pattern, Any } ).



% In the Targets list, elements are { BinaryPatttern, PrecompiledMatchSpec,
% OptionList } triplets (default option list is empty).
%
manage_targets( Targets ) ->
	manage_targets( Targets, _Acc=[] ).


manage_targets( _Targets=[], Acc ) ->
	Acc;

% Wanting a list of options, if it is not the 'undefined' atom:
manage_targets( [ { Target, Option } | T ], Acc ) when is_atom(Option)
			  andalso Option =/= undefined ->
	manage_targets( [ { Target, [ Option ] } | T ], Acc );

% We have a list of options here:
manage_targets( [ { Target, Options } | T ], Acc ) ->

	case text_utils:is_string( Target ) of

		true ->
			check_target_option( Options ),
			NewAcc = [ { text_utils:string_to_binary(Target), compile(Target),
						Options } | Acc ],
			manage_targets( T, NewAcc );

		false ->
			throw( { invalid_result_target, Target } )

	end;

% No option here:
manage_targets( [ Target | T ], Acc ) when is_list(Target) ->
	manage_targets( [ { Target, undefined } | T ], Acc );

manage_targets( Other, _Acc ) ->
	throw( { invalid_result_target_specification, Other } ).



% In the blacklisted list, elements are {BinaryPatttern,PrecompiledMatchSpec}
% pairs.
%
manage_blacklists( Blacklists ) ->
	manage_blacklists( Blacklists, _Acc=[] ).


manage_blacklists( _Blacklists=[], Acc ) ->
	Acc;

manage_blacklists( [ BlackList | T ], Acc ) ->

	case text_utils:is_string( BlackList ) of

		true ->
			NewAcc = [ { text_utils:string_to_binary(BlackList),
						compile(BlackList) } | Acc ],
			manage_blacklists( T, NewAcc );

		false ->
			throw( { invalid_result_blacklist, BlackList } )

	end.


% Checks that the option(s) specified with the targeted patterns are valid.
check_target_option( _Options=undefined ) ->
	ok;

check_target_option( _Options=[] ) ->
	ok;

check_target_option( _Options=[ H | T ] ) ->

	case lists:member( H, [ data_only, plot_only, data_and_plot ] ) of

		true ->
			check_target_option( T );

		false ->
			throw( { invalid_targeted_pattern_option, H } )

	end.



% Returns the name of the result directory that should be used.
%
% get_result_directory_name( SimulationName, StartTimestamp ) ->
%	io_lib:format( "~s-on-~s-by-~s/simulation-results", [
%		 file_utils:convert_to_filename( SimulationName ),
%		 basic_utils:get_textual_timestamp_for_path( StartTimestamp ),
%		 file_utils:convert_to_filename( system_utils:get_user_name() ) ] ).



% Returns a precompiled regular expression.
%
compile( Pattern ) ->

	%io:format( "Compiling pattern '~s'.~n", [ Pattern ] ),

	case re:compile( Pattern ) of

		{ ok, MatchSpec } ->
			MatchSpec;

		{ error, Error } ->
			throw( { result_pattern_precompilation_failed, Pattern, Error } )

	end.



% Returns either 'false' or a { 'true', Options } pair, where Options is a list.
%
% ProducerName must be a binary string.
%
is_result_wanted( ProducerName, Nature, State ) ->

	WantedInfos = case ?getAttr(result_spec) of

		all_outputs ->
			{ true, undefined };

		no_output ->
			false;

		all_basic_probes_only ->

			case Nature of

				undefined ->
					{ true, undefined };

				basic_probe ->
					{ true, undefined };

				virtual_probe ->
					false

			end;

		all_virtual_probes_only ->
			case Nature of

				undefined ->
					{ true, undefined };

				basic_probe ->
					false;

				virtual_probe ->
					{ true, undefined }

			end;

		{ TargetPatterns, BlacklistPatterns } ->
			is_selected_with_options( ProducerName, TargetPatterns,
									 BlacklistPatterns )

	end,

	%io:format( "Is result producer '~s' wanted? '~p'.~n",
	%		  [ ProducerName, WantedInfos ] ),

	%?trace_fmt( "Is result producer '~s' wanted? '~p'.",
	%		  [ ProducerName, WantedInfos ] ),

	WantedInfos.



% Tells whether the specified producer name (a binary string) is targeted and
% non-blacklisted. If yes, returns also its associated options.
%
% Return false or {true,Options}
%
is_selected_with_options( BinProducerName, TargetPatterns,
						 BlacklistPatterns ) ->

	ProducerName = text_utils:binary_to_string( BinProducerName ),

	case check_targeted( ProducerName, TargetPatterns ) of

		false ->
			%io:format( "Producer ~p was not selected, as not targeted.~n",
			%		  [ ProducerName ] ),
			false;

		Res -> % = { true, Options } ->
			case check_blacklisted( ProducerName, BlacklistPatterns ) of

				true ->
					%io:format( "Producer ~p was not selected, as targeted "
					%		  "but blacklisted.~n",
					%  [ ProducerName ] ),
					false;

				false ->
					%io:format( "Producer ~p was selected, as targeted "
					%		  "and not blacklisted.~n",
					%  [ ProducerName ] ),
					Res

			end

	end.



check_targeted( _ProducerName, _TargetPatterns=[] ) ->
	false;

check_targeted( ProducerName, [ { _BinPattern, MatchSpec, Opts } | T ] ) ->

	%io:format( "Checking '~s' against targeted pattern ~p.~n",
	%		  [ ProducerName, BinPattern ] ),

	case re:run( ProducerName, MatchSpec ) of

		nomatch ->
			check_targeted( ProducerName, T );

		{ match, _ } = _Match ->
			%io:format( "Targeted match found: '~p'.~n", [ Match ] ),
			{ true, Opts }

	end.


check_blacklisted( _ProducerName, _BlacklistPatterns=[] ) ->
	false;

check_blacklisted( ProducerName, [ { _BinPattern, MatchSpec } | T ] ) ->

	%io:format( "Checking '~s' against blacklisted pattern ~p.~n",
	%		  [ ProducerName, BinPattern ] ),

	case re:run( ProducerName, MatchSpec ) of

		nomatch ->
			check_blacklisted( ProducerName, T );

		{ match, _ } = _Match ->
			%io:format( "Blacklisted match found: '~p'.~n", [ Match ] ),
			true

	end.



% Extracts from the specified result queues the one whose node name is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_node( AtomNodeName, ResultQueues ) ->
	extract_queue_by_node( AtomNodeName, ResultQueues, _Acc=[] ).


extract_queue_by_node( AtomNodeName,
  _ResultQueues=[ Queue=#result_queue{ node_name=AtomNodeName } | T ], Acc ) ->

	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_node( AtomNodeName, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_node( AtomNodeName, T, [ H | Acc ] ).



% Extracts from the specified result queues the one whose host name is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_host( AtomHostName, ResultQueues ) ->
	extract_queue_by_host( AtomHostName, ResultQueues, _Acc=[] ).


extract_queue_by_host( AtomHostName,
  _ResultQueues=[ Queue=#result_queue{ host_name=AtomHostName } | T ], Acc ) ->

	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_host( AtomHostName, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_host( AtomHostName, T, [ H | Acc ] ).



% Extracts from the specified result queues the one whose identifier is the
% specified one. Returns a pair made of this queue and a list of the other
% queues.
%
% (helper)
%
extract_queue_by_id( Id, ResultQueues ) ->
	extract_queue_by_id( Id, ResultQueues, _Acc=[] ).


extract_queue_by_id( Id,
  _ResultQueues=[ Queue=#result_queue{ id=Id } | T ], Acc ) ->

	% Found!
	{ Queue, T ++ Acc };

extract_queue_by_id( Id, _ResultQueues=[ H | T ], Acc ) ->
	extract_queue_by_id( Id, T, [ H | Acc ] ).



% Returns a textual description of specified queue.
%
to_string( #result_queue{ id=Id, host_name=Hostname, node_name=Nodename,
		max_worker_count=MaxWorkerCount, waited_producers=WaitedProducers,
		pending_results=PendingResults } ) ->

	PendingString = case PendingResults of

			 [] ->
				"no pending result";

			_ ->
				io_lib:format( "~B pending results (~s)",
				   [ length( PendingResults ), PendingResults ] )

	end,

	io_lib:format( "Result queue whose ID is #~B, on host '~s' (node: '~s') "
				   "with up to ~B workers allowed, currently waiting for "
				   "producers ~w, having ~s",
				   [ Id, Hostname, Nodename, MaxWorkerCount,
					 WaitedProducers, PendingString ] ).



% Displays on the console specified queues.
%
display_queues( Queues ) ->

	% Sorts by ID:
	QueueStrings = [ to_string(Q) || Q <- lists:keysort( _IndexId=2, Queues ) ],

	io:format( "Result queues:~n ~s",
			   [ text_utils:string_list_to_string( QueueStrings ) ] ).



-spec get_producer_time_out() -> unit_utils:milliseconds().


-ifdef(exec_target_is_production).


% Returns the producer time-out, depending on the execution target: the number
% of milliseconds waited before a result producer is supposed having failed.

% In production mode:
get_producer_time_out() ->
	% 4 hours here (yes, this is quite a lot):
	4 * 60 * 60 * 1000.


-else.


% In development mode, we can rely on the default value:
get_producer_time_out() ->
	% 5 minutes here:
	5 * 60 * 1000.


-endif.
