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




% Class modelling a telecom network.
%
% A telecom network is a mesh of functional communicating devices.
%
% The connectivity of devices is primarily stored in a hashtable,
% (device_connectivity_table) for convenience, and transformed into a graph (the
% inherited Mesh) for pathfinding.
%
% This inherited mesh can be updated according to the state of the communication
% devices.
%
-module(class_Network).



% Determines what are the mother classes of this class (if any).
%
% A Network is an Actor, but not a StochasticActor.
%
-define( wooper_superclasses, [ class_Mesh, class_Actor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, NetworkName,
		 DeviceExpectedCount ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, findCommunicationPath/3,
		 markCommunicationPath/2, generateReport/2 ).


% Static method declarations.
-define( wooper_static_method_export, generate_report_for/1 ).


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.Network").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For notify_info and al:
-include("traces.hrl").





% Implementation notes:

% Knowing that each communication device is identified by its PID, a network is
% stored internally as a hashtable whose keys are the communicating devices of
% this network, and whose associated values are the list of communication
% devices in range from that key, i.e. that can be sent data directly.
%
% Ex: { Key, Value } = { DevicePid, [Device1Pid,Device2Pid,..] } means that
% DevicePid can send data either to Device1Pid, or Device2Pid, etc.  By default
% communications are not symmetrical, i.e. if A can communicate with B, this
% does not imply B can communicate with A.  This table is stored in the
% device_connectivity_table attribute.

% From these local relationships, the full communication graph can be deduced,
% corresponding to the inherited mesh:
%
% - mesh nodes are the PID of communicating devices
%
% - mesh links between communicating devices represent the fact that the devices
% can communicate directly
%
% The digraph for rendering and pathfinding is generated from the
% device_connectivity_table table.

% The communication devices are not owned by the network, they have a separate
% life-cycle.


-type device_count() :: basic_utils:count().



% Constructs a new network:
%
% - whose name is NetworkName (a string)
%
% - whose rendered views will be stored in OutputDirectory
%
% - which is expected to federate roughly DeviceExpectedCount devices
%
% By default a network is allowed to have cycles.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	   string() | { string(), file_utils:directory_name() }, device_count() ) ->
					   wooper:state().
construct( State, ActorSettings, { NetworkName, OutputDirectory },
		  DeviceExpectedCount ) ->

	% First the direct mother classes, then this class-specific actions:
	MeshState = class_Mesh:construct( State, { NetworkName, OutputDirectory },
									  _MeshOptions=[] ),

	% Here the Actor constructor and al will be called:
	init_common( MeshState, ActorSettings, NetworkName, DeviceExpectedCount );


% Constructs a new network:
%
% - whose name is NetworkName
%
% - which is expected to federate roughly DeviceExpectedCount devices
%
construct( State, ActorSettings, NetworkName, DeviceExpectedCount ) ->

	% First the direct mother classes, then this class-specific actions:
	MeshState = class_Mesh:construct( State, NetworkName, _MeshOptions=[] ),

	% Here the Actor constructor and al will be called:
	init_common( MeshState, ActorSettings, NetworkName, DeviceExpectedCount ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting network." ),

	% No device owned.

	?debug( "Network deleted." ),

	% Then allow chaining:
	State.




% Methods section.



% Defined simply to avoid a useless warning to be issued / an exception to be
% thrown.
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	?wooper_return_state_only( State ).



% Finds shortest communication path between an emitter and a receiver.
%
% Returns either no_path_found or PathList where PathList is the list of all
% interlocutors between the two endpoints (included).
%
% For example, if A->B->C, MyNet ! {findCommunicationPath,[A,C],self()} should
% return {wooper_result,[A,B,C]}.
%
% The internal (inherited) Mesh is expected to be up-to-date.
%
% (const request)
%
-spec findCommunicationPath( wooper:state(), class_Mesh:pure_node(),
				class_Mesh:pure_node() ) ->
		request_return( 'no_path_found' | [ class_Mesh:pure_node() ] ).
findCommunicationPath( State, Emitter, Receiver ) ->

	case executeRequest( State, findShortestPath, [ Emitter, Receiver ] ) of

		{ NewState, false } ->
			?wooper_return_state_result( NewState, no_path_found );

		{ NewState, NodeList } ->
			?wooper_return_state_result( NewState, NodeList )

	end.



% Marks all the nodes and links in the specified path, described as a list of
% nodes.
%
% The internal (inherited) Mesh is expected to be up-to-date.
%
% Side-effect: any previously marked nodes are unmarked.
%
% (oneway)
%
-spec markCommunicationPath( wooper:state(), [ class_Mesh:pure_node() ] ) ->
								   oneway_return().
markCommunicationPath( State, NodeList ) ->

	% Calling methods inherited from Mesh:
	NodeState = executeOneway( State, setMarkedNodes, [ NodeList ] ),

	{ LinkState, Links } = executeRequest( NodeState, getLinksInPath,
										   [ NodeList ] ),

	MarkState = executeOneway( LinkState, setMarkedLinks, [ Links ] ),

	?wooper_return_state_only( MarkState ).



% Generates a report for current state of this network.
%
% DisplayWanted: boolean telling whether the generated report will be displayed
% to the user (if true).
%
% The internal (inherited) Mesh is expected to be up-to-date.
%
% (const request)
%
-spec generateReport( wooper:state(), boolean() ) ->
		   request_return( 'network_report_generated' ).
generateReport( State, DisplayWanted ) ->

	?info( "Generation of network report requested." ),

	% Delegates to the inherited mesh:
	{ NewState, topological_view_generated } =
		executeRequest( State, generateTopologicalView, DisplayWanted ),

	?wooper_return_state_result( NewState, network_report_generated ).




% Static methods.


% Allows to define whether the network report should be displayed to the user,
% after generation.
%
% (helper function)
%
-spec generate_report_for( pid() ) -> basic_utils:void().
generate_report_for( NetworkPid ) ->

	% Avoids adding a bound variable:
	case executable_utils:is_batch() of

		true ->
			% Boolean means 'display wanted':
			NetworkPid ! { generateReport, false, self() };

		false ->
			NetworkPid ! { generateReport, true, self() }

	end,

	receive

		{ wooper_result, network_report_generated } ->
			?notify_info( "Network report correctly generated." )

	end.




% Helper functions.


% Factors parts common to all constructors.
%
% Returns an updated state.
%
-spec init_common( wooper:state(), class_Actor:actor_settings(), string(),
				  basic_utils:count() ) -> wooper:state().
init_common( State, ActorSettings, NetworkName, DeviceExpectedCount ) ->

	ActorState = class_Actor:construct( State, ActorSettings, NetworkName ),

	?send_info_fmt( ActorState, "Creating a new network "
					"whose name is ~s, with around ~B expected devices.",
					[ NetworkName, DeviceExpectedCount ] ),

	setAttributes( ActorState, [

		{ device_connectivity_table, hashtable:new( DeviceExpectedCount ) },
		{ trace_categorization, ?TraceEmitterCategorization }

								] ).
