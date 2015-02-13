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




% Resilience agent.


% Its role is to manage the resilience operations on a specific computing
% host. It is driven by the resilience manager.
%
% See also class_ResilienceManager.erl.
%
-module(class_ResilienceAgent).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ResilienceManagerPid, SecuringNodes,
		 SecuredByNodes, ResilienceDirBin ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, serialiseNode/3, notifyOfLocalProbes/2,
		 recoverNodes/4, relinkInstances/1, updateResilienceMapping/3,
		 toString/1 ).


% Static methods declarations.
-define( wooper_static_method_export,  ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Resilience.Agent").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For time_manager_name:
-include("class_TimeManager.hrl").


% For instance_tracker_name:
-include("class_InstanceTracker.hrl").


% For resilience_agent_name:
-include("class_ResilienceAgent.hrl").


% For resilience_manager_name, node_name() and al:
-include("class_ResilienceManager.hrl").




% Where a resilience agent should be registered.
-define( registration_type, local_only ).


% Atom used by the resilence mechanisms and that should not be found in models:
-define( resilience_marker, sim_diasca_resilience_marker ).




% Implementation notes:
%
% All sizes are in bytes, and are stored as 32-bit unsigned integers.
%
% The serialisation file format is the following:
%
% 1. format version (32 bits), see serialisation_format_version
%
% 2. ID of the type of next serialised instance (16 bits), in:
%      - model instance (actor), see serialised_model_instance
%      - probe instance, see serialised_probe_instance
%      - agent instance, with regard to a simulation service; see
% serialised_agent_instance
%
% 3. actual instance content, whose format depends on previous value:
%
%  - for a model instance:
%       * first the size (32 bits )of the corresponding binary
%       * then the binary itself
%
% Then back to step 2, etc. until reaching end of file.


% The version of the file format for serialisation:
-define( serialisation_format_version, 1 ).


% List of the ID of the known types of serialised instances:
-define( serialised_model_instance, 1 ).
-define( serialised_probe_instance, 2 ).
-define( serialised_agent_instance, 3 ).



% About deserialisation.


% An effort is made so that the deserialisation corresponding to crashed nodes
% is done as uniformly as possible onto surviving nodes.
%
% So recreating the actor and probe instances on the node that read them is
% probably a good choice, moreover it will tend to respect the placement hints
% that were defined at their creation (i.e. actors declared as coupled will
% still evaluated on the same node). And of course probes shall better be
% created alongside the actors that drive them.
%
% Hence no need to create instances through the (new) load balancer, they can
% just be recreated locally.




% The class-specific attributes of the resilience agent are:
%
% - resilience_manager_pid :: pid() is the PID of the resilience manager
%
% - time_manager_pid :: pid() is the PID of the local time manager, i.e. the one
% that is keeping track of all the actors on the node this agent is in charge
% from
%
% - securing :: [ node_name() ] is a list of the nodes this agent secures
%
% - secured_by :: [ node_name() ] is a list of the nodes that secure this agent
%
% - probes :: [ pid() ] is a list of the PIDs of the local probes that this
% agent shall manage
%
% - node_suffix :: string() is the suffix to be used for this node, typically to
% forge serialisation filenames (we do not want to use the full node name, as it
% is too long and includes '@', which should be avoided in a filename)
%
% - resilience_dir :: text_utils:bin_string() is the (local) directory where
% resilience information should be written (as a binary)
%
% - serialisation_info :: [ { class_TimeManager:tick_offset(),
% class_TimeManager:diasca(), [ { net_utils:atom_node_name(),
% file_utils:filename() } ] } ] is a list (in reversed chronological order) of
% entries describing serialisations, notably, for one occurring at { Tick,
% Diasca }, a list of the serialisation files that correspond to each then
% secured node



% Constructs a new resilience agent, from following parameters:
%
% - ResilienceManagerPid, the PID of the resilience manager
%
% - SecuringNodes, a list of the nodes that this agent should secure
%
% - SecuredByNodes, a list of the nodes that should secure this agent
%
% - ResilienceDirBin, the path to the (local) directory where resilience
% information should be written
%
-spec construct( wooper:state(), pid(), [ node_name() ], [ node_name() ],
				 text_utils:bin_string() ) ->  wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	Node = node(),

	TraceState = class_TraceEmitter:construct( State,
			io_lib:format( "Resilience Agent on node ~s", [ Node ] ) ),

	% Ensures also it is a singleton indeed:
	basic_utils:register_as( ?resilience_agent_name, ?registration_type ),

	class_InstanceTracker:register_agent( ?resilience_agent_name ),

	LocalTimeManagerPid = basic_utils:get_registered_pid_for(
							?time_manager_name, local ),

	ResilienceDir = text_utils:binary_to_string( ResilienceDirBin ),

	file_utils:create_directory( ResilienceDir, create_parents ),

	NodeSuffix = get_node_suffix_from( Node ),

	FinalState = setAttributes( TraceState, [

		{ resilience_manager_pid, ResilienceManagerPid },
		{ time_manager_pid, LocalTimeManagerPid },
		{ securing, SecuringNodes },
		{ secured_by, SecuredByNodes },
		{ probes, [] },
		{ node_suffix, NodeSuffix },
		{ resilience_dir, ResilienceDir },
		{ serialisation_info, [] },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

											   ] ),

	?send_info( FinalState, to_string( FinalState ) ),

	FinalState.





% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting resilience manager." ),

	basic_utils:unregister( ?resilience_agent_name, ?registration_type ),

	class_InstanceTracker:unregister_agent(),

	?debug( "Resilience manager deleted." ),

	% Then allow chaining:
	State.





% Methods section.


% Serialises all the information needed for the node this agent runs on.
%
% Typically called by the resilience manager, on each agent.
%
% (request)
%
-spec serialiseNode( wooper:state(), class_TimeManager:tick_offset(),
	   class_TimeManager:diasca() ) ->
						   request_return( { 'node_serialised', pid() } ).
serialiseNode( State, Tick, Diasca ) ->

	?debug_fmt( "Serialising now, at ~p.", [ { Tick, Diasca } ] ),

	io:format( "Serialising now on ~s (~w) at {~p,~p}.~n",
			   [ node(), self(), Tick, Diasca ] ),

	% Serialising is basically writing the state of all local elements (actors,
	% agents, result producers, etc.) which could not be re-created by a new
	% deployment.
	%
	% These states are to be transformed so that transient, technical
	% information is replaced by perennial, abstract one, and then stored in a
	% serialisation file dedicated to that node, that will be exchanged with the
	% ones of other nodes to secure.

	% First, let's retrieve all local actors:

	LocalTimeManagerPid = ?getAttr(time_manager_pid),

	% Anticipated request, received in the produce_serialisation_file/2 helper:
	LocalTimeManagerPid ! { getAllLocalActors, [], self() },

	% We used to rely on the host rather than on the node, to avoid longer names
	% with duplicated information (like the name of the user and of the
	% simulation case); however, for reloading purposes, using the node name is
	% finally simpler:
	%
	SerialisationFilename = get_serialisation_filename_for( node(), Tick,
															Diasca ),

	% While we wait for the actor list, let's prepare the writing:
	SerialisationPath = file_utils:join( ?getAttr(resilience_dir),
										 SerialisationFilename ),

	% Will receive the actor list:
	ProducedState = produce_serialisation_file( SerialisationPath, State ),

	NodePathList = exchange_serialisation_files( SerialisationPath,
												 ProducedState ),

	% Records for this serialisation which node sent which serialisation file:
	NewSerialisationEntry = { Tick, Diasca, NodePathList },

	RecordedState = appendToAttribute( ProducedState, serialisation_info,
									   NewSerialisationEntry ),

	?debug_fmt( "Serialisation finished for ~p.", [ { Tick, Diasca } ] ),

	?wooper_return_state_result( RecordedState, { node_serialised, self() } ).



% Notifies this agent of the local probes it should manage, serialisation-wise.
%
% (request)
%
-spec notifyOfLocalProbes( wooper:state(), [ class_Probe:probe_pid() ] ) ->
								 request_return( { 'probes_recorded', pid() } ).
notifyOfLocalProbes( State, ProbeList ) ->

	NotifiedState = setAttribute( State, probes, ProbeList ),

	?wooper_return_state_result( NotifiedState, { probes_recorded, self() } ).



% Tells this agent to recover the specified nodes, i.e. to respawn locally their
% serialised instances.
%
% (request)
%
-spec recoverNodes( wooper:state(), [ net_utils:atom_node_name() ],
		   class_TimeManager:tick_offset(), class_TimeManager:diasca() ) ->
						  request_return( { 'nodes_recovered', pid() } ).
recoverNodes( State, NodesToRecover, Tick, Diasca ) ->

	NodeFilenames = [ { Node,
					   get_serialisation_filename_for( Node, Tick, Diasca ) }
						  || Node <- NodesToRecover ],

	NodeStrings = [ io_lib:format( "node ~s (file: ~s)", [ Node, Filename ] )
								  || { Node, Filename } <- NodeFilenames ],

	?debug_fmt( "Node ~s recovering serialisation done at ~p "
				"for following nodes:~s",
				[ node(), { Tick, Diasca },
				  text_utils:strings_to_string( NodeStrings ) ] ),

	AgentPid = self(),

	SerialisationDir = ?getAttr(resilience_dir),

	LocalInstanceTracker = basic_utils:get_registered_pid_for(
							 ?instance_tracker_name ),

	% We choose here exactly one process per serialisation archive:
	%
	% (note that the simulation services have already been restored)
	%
	FileReaderPidList = [ spawn_link( fun() ->

			begin

				SerialisationPath = file_utils:join( SerialisationDir,
													 SerialisationFilename ),

				reader_run( SerialisationPath, AgentPid, LocalInstanceTracker )

			end

									  end )
						 || { _Node, SerialisationFilename } <- NodeFilenames ],


	% As deserialisations may fail (ex: outage in their course), we have to
	% define a time-out (in seconds):
	%
	% (2 hours)
	%
	MaxDurationInSeconds = 2 * 60 * 60,

	basic_utils:wait_for_acks( _WaitedSenders=FileReaderPidList,
		MaxDurationInSeconds, _AckReceiveAtom=serialisation_read,
		_ThrowAtom=serialisation_reading_time_out ),

	io:format( "Node ~s successfully read ~B serialisation files.",
			   [ node(), length( FileReaderPidList ) ] ),

	?debug_fmt( "Node ~s successfully read ~B serialisation files.",
				[ node(), length( FileReaderPidList ) ] ),

	?wooper_return_state_result( State, { nodes_recovered, self() } ).



% Tells this agent to relink the node-local instances.
%
% (request)
%
-spec relinkInstances( wooper:state() ) ->
							 request_return( { 'nodes_relinked', pid() } ).
relinkInstances( State ) ->

	% Local instances are: actors, simulation agents, result producers:

	?debug_fmt( "Node ~s successfully relinked.", [ node() ] ),

	?wooper_return_state_result( State, { nodes_relinked, self() } ).



% Updates the resilence mapping for that agent, i.e. which nodes it secures, ny
% which it is secured.
%
% (oneway)
%
-spec updateResilienceMapping( wooper:state(), [ net_utils:atom_node_name() ],
					  [ net_utils:atom_node_name() ] ) -> oneway_return().
updateResilienceMapping( State, NewSecuringNodes, NewSecuredByNodes ) ->

	% Taking advantage of this call to zero the now obsolete PIDs:
	NewState = setAttributes( State, [

		{ time_manager_pid, undefined },
		{ securing, NewSecuringNodes },
		{ secured_by, NewSecuredByNodes }

									  ] ),

	?wooper_return_state_only( NewState ).



% Returns a textual description of this manager.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).





% Static methods section.



% Section for helper functions (not methods).



% The entry point of the processes in charge of feeding the write process with
% the various serialised states.
%
% (note that, due to the parallel processing, the updating of the user data term
% is per-serialiser, not done uniformly)
%
serialiser_loop( FileWriterPid, EntryTransformer,
				 _LocalInstanceTrackerPid=UserData ) ->

	receive

		{ serialise_actor, ActorPid } ->

			io:format( " - actor ~w to be written~n", [ ActorPid ] ),

			% We delegate the actual serialisation to the actor itself:
			ActorPid ! { serialise, [ EntryTransformer, UserData ], self() },

			receive

				{ wooper_result, { BinContent, NewUserData } } ->

					% Prepares everything possible upfront, to lessen the work
					% of the (single) writer process:

					TypeId = ?serialised_model_instance,

					Size = size( BinContent ),

					Binary = << TypeId:16, Size:32, BinContent/binary >>,

					FileWriterPid ! { write_content, Binary },

					io:format( " - actor ~w written (payload: ~B bytes)~n",
							   [ ActorPid, Size ] ),

					serialiser_loop( FileWriterPid, EntryTransformer,
									 NewUserData )

			end;


		{ serialise_probe, ProbePid } ->

			io:format( " - probe ~w to be written~n", [ ProbePid ] ),

			% Delegated serialisation again:
			ProbePid ! { serialise, [ EntryTransformer, UserData ], self() },

			receive

				{ wooper_result, { BinContent, NewUserData } } ->

					% Prepares everything possible upfront, to lessen the work
					% of the (single) writer process:

					TypeId = ?serialised_probe_instance,

					Size = size( BinContent ),

					Binary = << TypeId:16, Size:32, BinContent/binary >>,

					FileWriterPid ! { write_content, Binary },

					io:format( " - probe ~w written (payload: ~B bytes)~n",
							   [ ProbePid, Size ] ),

					serialiser_loop( FileWriterPid, EntryTransformer,
									 NewUserData )

			end;


		{ serialise_agent, AgentPid } ->

			io:format( " - agent ~w to be written~n", [ AgentPid ] ),

			% Actual serialisation still delegated:
			AgentPid ! { serialise, [ EntryTransformer, UserData ], self() },

			receive

				{ wooper_result, { BinContent, NewUserData } } ->

					% Prepares everything possible upfront, to lessen the work
					% of the (single) writer process:

					TypeId = ?serialised_agent_instance,

					Size = size( BinContent ),

					Binary = << TypeId:16, Size:32, BinContent/binary >>,

					FileWriterPid ! { write_content, Binary },

					io:format( " - agent ~w written (payload: ~B bytes)~n",
							   [ AgentPid, Size ] ),

					serialiser_loop( FileWriterPid, EntryTransformer,
									 NewUserData )

			end;


		terminate ->

			io:format( "Serialiser ~w terminated.~n", [ self() ] ),

			% To avoid serialiser-level race conditions:
			FileWriterPid ! serialisation_sent

	end.



% Dispatches (round-robin) the specified kind of serialisation to the
% serialising worker processes.
%
% (helper)
%
dispatch_serialisations( SerialisationKind, InstancePidList, SerialiserRing ) ->

	io:format( "- dispatching ~B serialisations of type ~p onto a ring "
			   "of ~B serialisers~n",
			   [ length( InstancePidList ), SerialisationKind,
				 list_utils:get_ring_size( SerialiserRing ) ] ),

	dispatch_serialisations( SerialisationKind, InstancePidList,
							 SerialiserRing, _Count=0 ).


dispatch_serialisations( _SerialisationKind, _InstancePidList=[],
							   SerialiserRing, Count ) ->
	{ Count, SerialiserRing };

dispatch_serialisations( SerialisationKind,
			 _InstancePidList=[ InstancePid | T ], SerialiserRing, Count ) ->

	{ SerialiserPid, UpdatedRing } = list_utils:head( SerialiserRing ),

	SerialiserPid ! { SerialisationKind, InstancePid },

	dispatch_serialisations( SerialisationKind, T, UpdatedRing, Count + 1 ).






% Writer process section.


% The entry point of the process in charge of the actual file writing of the
% serialisation information, sent by all serialisers.
%
writer_run( SerialisationPath, SerialiserCount, AgentPid ) ->

	% Heavy work to be done here:
	erlang:process_flag( priority, high ),

	% Let's try not to make it be a too terrible bottleneck:
	File = file_utils:open( SerialisationPath, [ write, exclusive, raw, binary,
			{ delayed_write, _Size=512*1024, _Delay=2000 } ] ),

	% First, a mini-header:
	FormatVersion = << ?serialisation_format_version:32 >>,

	file_utils:write( File, FormatVersion ),

	writer_loop( File, SerialiserCount, AgentPid ),

	% Back to normal (rather useless, as terminating anyway):
	erlang:process_flag( priority, normal ),

	% Terminating now:
	file_utils:close( File ).



% The main loop of the writer process.
%
% The serialisation agent has already notified the serialisers that all
% serialisation requests had been sent, and is waiting for this writer, which is
% itself waiting first for all serialisers to report they finished their task.
%
writer_loop( _File, _SerialiserCount=0, AgentPid ) ->

	AgentPid ! serialisation_written,

	% Stop recursing:
	ok;


writer_loop( File, SerialiserCount, AgentPid ) ->

	% Selective receive is key here; we want to exhaust all write_content
	% messages before terminating:
	%
	receive

		{ write_content, Binary } ->

			% We simply write blindy the received binary, as is:
			file_utils:write( File, Binary ),

			writer_loop( File, SerialiserCount, AgentPid );

		% One less serialiser:
		serialisation_sent ->
			writer_loop( File, SerialiserCount - 1, AgentPid )

	end.




% Reader process section.


% The entry point of the process in charge of the actual reading of a
% serialisation archive.
%
reader_run( SerialisationPath, AgentPid, LocalInstanceTracker ) ->

	% Heavy work to be done here:
	erlang:process_flag( priority, high ),

	% Let's try not to make it be a too terrible bottleneck:
	File = file_utils:open( SerialisationPath, [ read, raw, binary,
			{ read_ahead, _Size=512*1024 } ] ),

	% First, reads and checks the mini-header:
	case file_utils:read( File, _HeaderSize=4 ) of

		{ ok, << ?serialisation_format_version:32 >> } ->
			ok;

		Other ->
			throw( { invalid_serialisation_header, Other, SerialisationPath } )

	end,

	reader_loop( File, AgentPid, LocalInstanceTracker ),

	% Back to normal (rather useless, as terminating anyway):
	erlang:process_flag( priority, normal ),

	% Terminating now:
	file_utils:close( File ).



% Reads all elements for specified file.
%
reader_loop( File, AgentPid, LocalInstanceTracker ) ->
	reader_loop( File, AgentPid, LocalInstanceTracker, _Waited=[] ).



% The main loop of the reader process.
%
% Creations are asynchronous and run in parallel.
%
reader_loop( File, AgentPid, LocalInstanceTracker, Waited ) ->

	case file_utils:read( File, _TypeIdSize=2 ) of


		{ ok, << ?serialised_model_instance:16 >> } ->

			ActorPid = read_actor( File, AgentPid, LocalInstanceTracker ),

			reader_loop( File, AgentPid, LocalInstanceTracker,
						 [ ActorPid | Waited ] );


		{ ok, << ?serialised_probe_instance:16 >> } ->

			ProbePid = read_probe( File, AgentPid, LocalInstanceTracker ),

			reader_loop( File, AgentPid, LocalInstanceTracker,
						 [ ProbePid | Waited ] );


		{ ok, << ?serialised_agent_instance:16 >> } ->

			none_to_wait = read_agent( File, AgentPid, LocalInstanceTracker ),

			reader_loop( File, AgentPid, LocalInstanceTracker, Waited );


		eof ->

			%io:format( "End of file reached, waiting for:~n~p~n",
			%		 [ Waited ] ),

			wait_for_readings( Waited ),

			%io:format( "Serialisation file fully processed.~n" ),

			AgentPid ! { serialisation_read, self() };

			% Stop recursing.


		Other ->
			throw( { invalid_serialisation_read, Other } )

	end.



% Reads a serialised actor from file.
%
% Returns its PID.
%
read_actor( File, _AgentPid, LocalInstanceTracker ) ->

	{ ok, << ContentSize:32 >> } = file_utils:read( File, _SizeSize=4 ),

	{ ok, BinContent } = file_utils:read( File, ContentSize ),

	ReaderPid = self(),

	% As anyway the actor will have its own process, let's delegate the work to
	% this process as soon as possible:
	%
	% (the next link will be replaced by one from the actor to the local time
	% manager)
	%
	ActorPid = spawn_link( fun() ->

		io:format( "Reading actor, serialised in ~B bytes, becoming ~p.~n",
				   [ ContentSize, self() ] ),

		{ Classname, RawEntries } = binary_to_term( BinContent ),

		% Too early to transform serialisation markers, as we need up-to-date
		% instance trackers beforehand:
		%
		% (we just update the local tracker with information about that actor)
		%
		% Will never return:
		%
		apply( Classname, wooper_deserialise, [ RawEntries,
				_EntryTransformer=undefined, _UserData=undefined,
				_ListenerPid=ReaderPid ] )

	end ),

	% Could be made more parallel:

	ActorPid ! { getActorInfo, [], self() },

	receive

		{ wooper_result, ActorInfo } ->
			LocalInstanceTracker ! { registerActor, [ ActorPid, ActorInfo ] }

	end,

	ActorPid.




% Reads a serialised probe from file.
%
read_probe( File, _AgentPid, _LocalInstanceTracker ) ->

	% The data format is fully specified in the 'Probe serialisation' section of
	% class_Probe.erl.

	{ ok, << ContentSize:32 >> } = file_utils:read( File, _SizeSize=4 ),

	{ ok, BinStateContent } = file_utils:read( File, ContentSize ),

	{ ok, << CommandSize:32 >> } = file_utils:read( File, _SizeSize=4 ),

	BinCommand = case CommandSize of

					 0 ->
						 undefined;

					 _ ->
						 { ok, BinCmd } = file_utils:read( File, CommandSize ),
						 BinCmd

	end,

	{ ok, << DataSize:32 >> } = file_utils:read( File, _SizeSize=4 ),

	BinData = case DataSize of

					 0 ->
						 undefined;

					 _ ->
						 { ok, BinDat } = file_utils:read( File, DataSize ),
						 BinDat

	end,

	ReaderPid = self(),

	% As anyway the probe will have its own process, let's delegate the work to
	% this process as soon as possible:
	%
	% (there is a slight link-related semantic change: as the probe does not
	% know its creator - typically an actor or the case itself, or even possibly
	% a simulation service, after a deserialisation a probe is not linked
	% anymore to its creator)
	%
	% Yes, the created fun has no local return (no problem):
	%
	spawn( fun() ->

		TotalSize = ContentSize + CommandSize + DataSize,

		io:format( "Reading probe, serialised in a total of ~B bytes "
				   "(instance size: ~B, command size: ~B, data size: ~B), "
				   "becoming ~p.",
				   [ TotalSize, ContentSize, CommandSize, DataSize, self() ] ),

		% Will never return:
		class_Probe:deserialise( BinStateContent, BinCommand, BinData,
								 ReaderPid )

	end ).




% Reads a serialised simulation agent from specified file.
%
% Returns either the PID of a new agent that is to be waited, or 'none_to_wait'.
%
read_agent( File, _AgentPid, _LocalInstanceTracker ) ->

	io:format( "Reading agent.~n" ),

	{ ok, << ContentSize:32 >> } = file_utils:read( File, _SizeSize=4 ),

	{ ok, BinContent } = file_utils:read( File, ContentSize ),

	% Here, all depends on the kind of this agent; if it is a time manager, we
	% are not to create a new instance thereof, we are to merge that serialised
	% information into the redeployed one:
	%
	{ Classname, RawEntries } = binary_to_term( BinContent ),

	case Classname of

		class_TimeManager ->
			class_TimeManager:merge_local_with( RawEntries ),
			none_to_wait;

		_ ->
			none_to_wait

	end.



% Waits for all readings corresponding to a serialisation archive to be done.
%
% (helper)
%
wait_for_readings( _WaitedList=[] ) ->
	ok;

wait_for_readings( WaitedList ) ->

	receive

		{ onDeserialisation, [ CreatedProcessPid, _FinalUserData ] } ->

			NewWaitedList = list_utils:delete_existing( CreatedProcessPid,
											WaitedList ),

			wait_for_readings( NewWaitedList )

	end.



% Returns the filename corresponding to the specified node, tick and diasca.
%
get_serialisation_filename_for( NodeName, Tick, Diasca ) ->
	io_lib:format( "serialisation-~B-~B-from-~s.bin",
					[ Tick, Diasca, get_node_suffix_from( NodeName ) ] ).


% Returns a node suffix from the specified name, typically a node name.
%
% (helper)
%
get_node_suffix_from( AtomName ) ->

	% 'Sim-Diasca_Soda_Resilience_Test-boudevil@tesla' to be transformed into a
	% shorter 'tesla' (which happens here to be a short host name).

	Name = text_utils:atom_to_string( AtomName ),

	list_utils:get_last_element( string:tokens( Name, "@" ) ).



% Generates the serialisation file that corresponds to the local elements to be
% archived.
%
% Returns an updated state.
%
% (helper)
%
produce_serialisation_file( SerialisationPath, State ) ->

	% We now launch a singleton process in charge of the writing into the
	% (single) serialisation file of the serialisation data that is sent by the
	% parallel serialising processes:

	% Beware, closure:
	AgentPid = self(),

	% We want to perform the serialisation as much as possible in parallel on
	% each computing node (at least one worker per core), to keep them busy:
	%
	%SerialiserCount = system_utils:get_core_count() + 1,

	% Simpler, when debugging:
	SerialiserCount = 1,

	% The single file writer process, fed by all serialiser ones:
	FileWriterPid = spawn_link( fun() ->
			 writer_run( SerialisationPath, SerialiserCount, AgentPid ) end ),


	% Implementing here a safety check, so that no 'serialisation marker' atom
	% can be found in the states *prior* to their transformation:
	%
	% This in a term_transformer() (see basic_utils.erl):
	%
	MarkerTransformer = fun( _AtomTerm=?resilience_marker,
							 _MarkUserData=InstancePid ) ->
								% Never allowed in actual states:
								throw( { resilience_marker_in_attribute,
										?resilience_marker, InstancePid } );

						   ( _AtomTerm=AnyOtherAtom, MarkUserData ) ->
								% Other atoms are fine, no-op here:
								{ AnyOtherAtom, MarkUserData }

	end,


	% We will rewrite all terms and subterms found to be a PID, in order that we
	% serialise only abstract identifiers, not technical (non-reproducible)
	% ones:
	%
	% (converts all PIDs, into either AAI, an agent reference or a reference
	% onto a result producer )
	%
	% This in a term_transformer() (see basic_utils.erl):
	%
	PidTransformer = fun( _PidTerm=Pid, _PidUserData=InstanceTrackerPid ) ->

			% Cannot be a request, as, in some cases, the answer will be sent
			% back by a third party (another tracker):
			%
			InstanceTrackerPid ! { resolvePid, [ Pid, self() ] },

			% Corresponds either to an actor, to a simulation agent or a result
			% producer (synchronous call-back):
			%
			PidTranslation = receive

				{ notifyResolvedPid, Translation } ->
							Translation

			end,

			%io:format( "PID ~w resolved in '~p'.~n", [ Pid, PidTranslation ] ),

			% Replaces that found PID by its translation, wrapped in a
			% resilience-tagged pair:
			%
			RewrittenTerm = { ?resilience_marker, PidTranslation },

			{ RewrittenTerm, InstanceTrackerPid }

	end,


	% In charge of converting WOOPER attribute entries.
	%
	% This in an entry_transformer() (see wooper.hrl), a fun using the two
	% previous funs:
	%
	EntryTransformer = fun( Entry={ AttributeName, AttributeValue },
							_Acc={ PastEntries, EntryUserData } ) ->

			% First check (for safety) that there is no pre-existing resilience
			% marker:
			%
			% (not changing value or user data, just checking)
			%
			basic_utils:traverse_term( AttributeValue,
			  _CheckedTypeDescription=atom, MarkerTransformer,
			  _CheckedUserData=Entry ),

			% Then transform the PIDs:
			{ TransformedValue, NewUserData } = basic_utils:traverse_term(
				  AttributeValue, _TypeDescription=pid, PidTransformer,
				  _TermUserData=EntryUserData ),

			NewAttributeEntry = { AttributeName, TransformedValue },

			% New accumulator:
			{ [ NewAttributeEntry | PastEntries ], NewUserData }

	end,

	% Used to propagate among the serialiser processes the PID of the local
	% instance tracker:
	%
	UserData = basic_utils:get_registered_pid_for( ?instance_tracker_name,
												  local ),

	% The serialisers that will drive the serialisation of each local instance
	% of interest (agents, actors, result producers, etc.):
	%
	SerialiserPids = [ spawn_link( fun() ->

			io:format( "Serialiser ~w spawned.~n", [ self() ] ),

			serialiser_loop( FileWriterPid, EntryTransformer, UserData ) end )

					  || _ <- lists:seq( 1, SerialiserCount ) ],

	% Dispatching now the serialisation effort onto these serialisation workers:
	SerialiserRing = list_utils:list_to_ring( SerialiserPids ),

	% Let's start first by the simulation agents to serialise:
	AgentPidList = get_pid_of_all_local_agents( State ),

	{ AgentCount, AgentSerialiserRing } = dispatch_serialisations(
					serialise_agent, AgentPidList, SerialiserRing ),

	% Answer from getAllLocalActors/1 finally collected:
	ActorPidList = receive

		{ wooper_result, ActorPids } ->
			ActorPids

	end,

	% Continues with the actors to serialise:
	{ ActorCount, ActorSerialiserRing } = dispatch_serialisations(
					serialise_actor, ActorPidList, AgentSerialiserRing ),

	% This list was updated beforehand by the resilience manager, with all the
	% local probes (other result producers not managed yet):
	%
	ProbePidList = ?getAttr(probes),

	{ ProbeCount, _ProbeSerialiserRing } = dispatch_serialisations(
					serialise_probe, ProbePidList, ActorSerialiserRing ),

	io:format( "Writing serialisation data in file '~s', "
			   "~w serialising ~B agents, ~B actors and ~B probes.~n",
			   [ SerialisationPath, self(), AgentCount, ActorCount,
				 ProbeCount ] ),


	% Sent in order to avoid any race condition, as the file writer has no other
	% way of determining the end of the serialisation stream:
	%
	[ Serialiser ! terminate || Serialiser <- SerialiserPids ],

	% Then wait for the file to be fully written:
	receive

		% Sent by the file writer process once terminating:
		serialisation_written ->
			ok

	end,

	% Unchanged:
	State.



% Returns a list of the PIDs of all local agents that shall be serialised.
%
% (helper)
%
get_pid_of_all_local_agents( State ) ->

	% A full list may be of agents of interest could be (the additional ones do
	% not need special care):


	% GlobalAgents = [ sim_diasca_performance_tracker, main_data_logger,
	% sim_diasca_result_manager, sim_diasca_data_exchanger_for_case,
	% sim_diasca_root_data_exchanger, sim_diasca_load_balancer,
	% sim_diasca_deployment_manager ]

	% Currently, for global agents:

	% - not supported: sim_diasca_performance_tracker, main_data_logger
	%
	% - sim_diasca_data_exchanger_for_case and sim_diasca_root_data_exchanger
	% not tested yet, but should not be a problem
	%
	% - sim_diasca_load_balancer and sim_diasca_deployment_manager will be
	% recreated from scratch

	% AdditionalGlobalAgents = [ sim_diasca_simulation_case,
	% ceylan_trace_aggregator, sim_diasca_resilience_manager ]

	% Currently, for additional global agents:

	% - not protected:
	%
	%   - sim_diasca_simulation_case and sim_diasca_resilience_manager, as they
	%   have to survive for the simulation to survive at all
	%
	%   - ceylan_trace_aggregator, as it is expected to survive as well, since
	%   it is running on the user node


	% LocalAgents = [ sim_diasca_data_exchanger, sim_diasca_time_manager ]

	% Currently, for local agents:
	%
	% - sim_diasca_data_exchanger not tested, but should not be a problem
	% - sim_diasca_time_manager is specifically managed

	% AdditionalLocalAgents = [ wooper_class_manager,
	% sim_diasca_instance_tracker, sim_diasca_resilience_agent ]

	% Currently, for additional local agents:
	%
	% - wooper_class_manager is still running on all surviving nodes
	% - sim_diasca_instance_tracker is recreated from scratch and then fed
	% - sim_diasca_resilience_agent is still running on all surviving nodes

	% Finally all the agents to be serialised are:
	[ ?getAttr(time_manager_pid) ].




% Sends specified serialisation file to the nodes securing the current node.
%
% Returns a list of { NodeName, FilePath } describing the serialisation files
% that were received.
%
% (helper)
%
exchange_serialisation_files( SerialisationPath, State ) ->

	% Now that we have locally the targeted serialisation file, we can send it
	% to all k nodes securing this node and, reciprocally, wait for the k
	% secured nodes to send their own serialisation file:
	%
	SecuringNodes = ?getAttr(secured_by),
	SecuredNodes = ?getAttr(securing),

	% A bit of checking should not hurt:
	NodeCount = length( SecuringNodes ),
	NodeCount = length( SecuredNodes ),

	% Needing first the PID of the corresponding agents that are securing this
	% node:
	%
	SecuringAgentPidList = [ basic_utils:get_locally_registered_pid_for(
				?resilience_agent_name, Node ) || Node <- SecuringNodes ],

	% Closure:
	ThisAgent = self(),
	TargetDir = ?getAttr(resilience_dir),

	% Let's first start sending our serialisation file, asynchronously (thanks
	% to dedicated processes):

	% Emitters shall declare themselves first to their securing agents:
	_SenderWorkers = [ spawn_link( fun() ->

			% This self() corresponds to this sending process just spawned:
			SecuringAgentPid ! { declareSerialisationEmitter, self() },

			% However we do not know yet which receiver process at the other end
			% shall be used (as there are also multiple receiver workers), so:
			ReceiverWorkerPid = receive

						{ declareSerialisationRecipient, Pid } ->
										Pid

			end,

			net_utils:send_file( SerialisationPath, ReceiverWorkerPid ),

			ThisAgent ! { serialisation_sent, self(), node( SecuringAgentPid ) }

								  end )

						|| SecuringAgentPid <- SecuringAgentPidList ],


	% Meanwhile, thanks to these non-blocking operations, we now can take care
	% of the counterpart receiving part (with the secured nodes, this time):

	[

	 % We wait the receiving of the emitter PID before spawning its associated
	 % receiver process:
	 %
	 receive

		 { declareSerialisationEmitter, EmitterPid } ->

			 spawn_link( fun() ->

				EmitterPid ! { declareSerialisationRecipient, self() },

				ReceivedPath = net_utils:receive_file( EmitterPid, TargetDir ),

				% As all nodes share the same resilience base path, here we can
				% strip it:
				%
				ReceivedFilename = filename:basename( ReceivedPath ),

				BinReceivedPath = text_utils:string_to_binary(
														ReceivedFilename ),

				ThisAgent ! { serialisation_received, self(),
							 node( EmitterPid ), BinReceivedPath }

						 end )


	end || _ <- SecuredNodes ],


	% As serialisations may fail (ex: outage in their course), we have to define
	% a time-out (in seconds):
	%
	MaxWaiting = 30 * 60,

	InitialTimestamp = basic_utils:get_timestamp(),

	% Now wait for both sets of parallel tasks to complete:
	NodePathList = wait_for_serialisation_acks(
				_WaitedSentCount=NodeCount, _WaitedReceivedCount=NodeCount,
				InitialTimestamp, MaxWaiting, _ReceiveAcc=[] ),

	?debug_fmt( "Node ~s received following ~B serialisation files:~s",
			  [ node(), length( NodePathList ), text_utils:strings_to_string(
			   [ io_lib:format( "for secured node '~s': ~s", [ Node, Path ] )
				|| { Node, Path } <- NodePathList ] ) ] ),

	% A priori no need to keep our own backup:
	file_utils:remove_file( SerialisationPath ),

	io:format( "Serialisation finished by ~w on node '~s'.~n",
			   [ self(), node() ] ),

	NodePathList.



% Waits for the worker processes for the sending and receiving of serialisation
% files to complete, returns a list of { NodeName, FilePath } enumerating the
% secured nodes and the corresponding local full path of their serialisation
% file.
%
% (helper)
%
wait_for_serialisation_acks( _WaitedSentCount=0, _WaitedReceivedCount=0,
				_InitialTimestamp, _MaxWaiting, ReceiveAcc ) ->
	ReceiveAcc;

wait_for_serialisation_acks( WaitedSentCount, WaitedReceivedCount,
				InitialTimestamp, MaxWaiting, ReceiveAcc ) ->

	receive

		{ serialisation_sent, _SenderWorkerPid, _SecuringNode } ->
			wait_for_serialisation_acks( WaitedSentCount - 1,
				WaitedReceivedCount, InitialTimestamp, MaxWaiting, ReceiveAcc );

		{ serialisation_received, _ReceiverWorkerPid, SecuredNode,
		 ReceivedPath } ->
			wait_for_serialisation_acks( WaitedSentCount,
				WaitedReceivedCount - 1, InitialTimestamp, MaxWaiting,
				[ { SecuredNode, ReceivedPath } | ReceiveAcc ] )

	after 1000 ->

			% Ensures no time-out was reached:

			CurrentTimestamp = basic_utils:get_timestamp(),

			case basic_utils:get_duration( InitialTimestamp,
										  CurrentTimestamp ) of

				D when D > MaxWaiting ->
					throw( serialisation_exchange_timed_out );

				_ ->
					% Continues waiting then:
					wait_for_serialisation_acks( WaitedSentCount,
							WaitedReceivedCount, InitialTimestamp, MaxWaiting,
												ReceiveAcc )

			end

	end.



% Returns a textual description of this instance.
%
% (helper)
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	Securing = ?getAttr(securing),
	SecuredBy = ?getAttr(secured_by),

	io_lib:format( "Resilience agent on node ~s, using directory ~s, "
				   "securing following ~B node(s):~s and secured by following "
				   "~B node(s): ~s",
				   [ node(), ?getAttr(resilience_dir), length( Securing ),
					 text_utils:atom_list_to_string( Securing ),
					 length( SecuredBy ),
					 text_utils:atom_list_to_string( SecuredBy )
					] ).
