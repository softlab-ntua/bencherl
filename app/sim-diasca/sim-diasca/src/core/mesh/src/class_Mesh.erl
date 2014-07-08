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



% Mesh class, for all kinds of graph-based systems (ex: networks).
%
% A mesh is composed of nodes and links.
%
% Each node and each link can have an associated content.
%
% If generateTopologicalView is used, then each node and each link are expected
% to be the PID of a process that respects the class_Graphable API (see
% class_Graphable.erl), and each associated content will be a cached value of
% that rendering information, i.e. a list of options:
%
% { Vertex, Label } = { NodePid, NodeOptions | undefined }
%
% and
%
% { Edge, Label } = { LinkPid, LinkOptions | undefined }.
%
% However links can also be defined once for all, in this case they are static.
%
% Such a link is not defined by a PID (an atom is usually used instead), and its
% associated content will never be updated.
%
% The up_to_date mesh attribute corresponds to the fact that the content
% associated to all nodes and links, cached in the mesh, corresponds to the
% content that would be given by these processes themselves.
%
% Note that some operations lead to the mesh being tagged as non up-to-date,
% thus they might trigger the mesh to send update requests to any processes
% associated to its nodes and links.
%
% The up-to-date status regards content only, not the fact that for example the
% rendering of the mesh is expected to be different because of a recent
% operation.
%
-module(class_Mesh).



% Implementation notes:
%
% This module is mostly an user-friendly object-oriented encapsulation of the
% digraph module, with some added features.
%
% File writings should be made with write/2, rather than format/2.


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter, class_ResultProducer ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Name, IsCyclic ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, delete/1 ).



% Member method declarations.
-define( wooper_method_export,
		 update/1, blank/1, validate/1, invalidate/1,
		 addNode/2, addNode/3, addNodes/2,
		 getContentForNode/2, updateContentForNode/2, getAllNodes/1,
		 addLink/3, addLink/4, addLink/5, addStaticLink/4,
		 getContentForLink/2, updateContentForLink/2, getAllLinks/1,
		 findLink/3, getLinkInformation/2,
		 findPath/3, findShortestPath/3, getLinksInPath/2,
		 findReachableFrom/2, pruneFrom/2, pruneTo/2,
		 setMarkedNodes/2, setMarkedLinks/2, setMarkedLinksFromEndpoints/2,
		 addRenderingRawElement/2,
		 setRenderingSettings/3,
		 getMeshInformation/1,
		 generateTopologicalView/2, generateTopologicalView/3 ).


% Exported as conveniences to simplify user code or case.
-export([ generate_text_panel/2, generate_topological_view_for/1 ]).



% We do not use parametric types currently.


% node() is already a standalone type:
-type pure_node() :: any().

%-type node_content(X) :: X | 'undefined'.
%-type node_content() :: node_content( any() ).
-type node_content() :: any() | 'undefined'.


%-type node_with_content(X) :: { pure_node(), node_content(X) }.
-type node_with_content() :: { pure_node(), node_content() }.


%-type any_node(X) :: pure_node() | node_with_content(X).
%-type any_node() :: any_node( any() ).
-type any_node() :: pure_node() | node_with_content().



-type pure_link() :: any().

%-type link_content(X) :: X | 'undefined'.
%-type link_content() :: link_content( any() ).
-type link_content() :: any() | 'undefined'.

%-type link_with_content(X) :: {pure_link(),link_content(X)}.
-type link_with_content() :: { pure_link(), link_content() }.

%-type any_link(X) :: pure_link() | link_with_content(X).
%-type any_link() :: any_link( any() ).
-type any_link() :: pure_link() | link_with_content().

-type link_connectivity() :: { pure_node(), pure_node(), link_content() }.


-type edge() :: { pure_node(), pure_node() }.



-type node_style() :: 'filled'.
-type link_style() :: 'solid'.


-type elem_color() :: gui_color:color_by_name().

-type node_color() :: elem_color().
-type link_color() :: elem_color().


% PID of a mesh instance:
-type mesh_pid() :: pid().


-export_type([ pure_node/0, node_content/0, node_with_content/0,
			   pure_link/0, link_content/0, link_with_content/0,
			   mesh_pid/0 ]).


% A PID of a Graphable instance:
-type graphable_pid() :: pid().



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Mesh").


% For Trace emitter trace macros:
-include("class_TraceEmitter.hrl").


% For notify* trace macros:
-include("traces.hrl").





% Unmarked settings.


% The color used by default to represent non-marked nodes:
-define(default_node_color,orange).

% The style used by default to represent non-marked nodes:
-define(default_node_style,filled).


% The color used by default to represent non-marked links:
-define(default_link_color,black).

% The style used by default to represent non-marked links:
-define(default_link_style,solid).



% Marked settings.


% The color used by default to represent marked nodes:
-define(default_marked_node_color,"forestgreen").

% The style used by default to represent marked nodes:
-define(default_marked_node_style,"filled").


% The color used by default to represent marked links:
-define(default_marked_link_color,"forestgreen").

% The style used by default to represent marked links:
%-define(default_marked_link_style,"bold").
-define(default_marked_link_style,"solid").



% Implementation notes:
%
% Implementation based on labeled directed graphs, see the digraph module.
%
% Mesh nodes are graph vertices, and mesh links are graph edges.
%
% Content associated to a mesh node or link corresponds to the dot options
% (including dot label) of a graph vertex or graph edge, according to the
% information returned the last time the mesh element was requested.
%
% Such a graph is mutable and modified by digraph calls, thus there is no need
% to re-set the graph once modified (no new graph returned, existing one
% updated).
%
% Undirected graphs could be managed with the same API by duplicating created
% links (both ways).


% See layout commands in http://graphviz.org/Documentation.php (default is
% 'dot'):
%
-type mesh_layout() :: 'dot' | 'neato' | 'twopi' | 'circo' | 'fdp' | 'sfdp'.


-type mesh_option() :: { 'can_be_cyclic', boolean() }
					  | { 'layout', mesh_layout() }.

-type mesh_options() ::  [ mesh_option() ].



% Constructs a new mesh instance:
%
% - Name: the name of the mesh
%
% - OutputDirectory: the directory path to which rendered views will be stored
% (the latter directory element in this path will be created if necessary)
%
% - MeshOptions:
%
%   - a graph layout may be chosen (see mesh_layout())
%
%   - a mesh is allowed to be cyclic by default, unless { can_be_cyclic, false }
%   is specified
%
% Note: when a directory is specified to the constructor, it will created
% regardless of renderings being requested or not.
%
-spec construct( wooper_state(),
				string() | { string(), file_utils:directory_name() },
				mesh_options() ) -> wooper_state().
construct( State, { Name, OutputDirectory }, MeshOptions ) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, Name ),

	ResultState = class_ResultProducer:construct( TraceState, Name ),

	Filename = file_utils:convert_to_filename( Name ),

	init_common( setAttributes( ResultState, [

			{ graph_filename, filename:join( OutputDirectory, Filename ) },
			{ graph_directory, OutputDirectory },
			{ graph_directory_created, false },
			{ digraph, undefined },
			{ marked_links, [] },
			{ marked_nodes, [] }

		] ), MeshOptions );


construct( State, Name, MeshOptions ) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, Name ),

	ResultState = class_ResultProducer:construct( TraceState, Name ),

	init_common( setAttributes( ResultState, [

			{ graph_filename, file_utils:convert_to_filename(Name) },
			{ graph_directory, undefined },
			{ graph_directory_created, true },
			{ digraph, undefined },
			{ marked_links, [] },
			{ marked_nodes, [] }

		] ), MeshOptions ).



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?info( "Deleting mesh." ),

	digraph:delete( ?getAttr(digraph) ),

	?debug( "Mesh deleted." ),

	% Then allow chaining:
	State.




% Methods section.


% Section common to nodes and links.


% Updates the whole mesh content, i.e. the content of all nodes and links, by
% requesting each PID to send its current graph options.
%
% (oneway)
%
-spec update( wooper_state() ) -> oneway_return().
update( State ) ->

	%?debug( "Updating mesh." ),

	Digraph = ?getAttr(digraph),

	Nodes = digraph:vertices( Digraph ),

	lists:foreach(
		fun( Node ) -> update_content_for_node( Node, State ) end,
		Nodes ),

	Links = digraph:edges( Digraph ),

	lists:foreach(
		fun( Link ) -> update_content_for_link( Link, State ) end,
		Links ),

	% State returned almost as was (but nodes and links might be altered):
	?wooper_return_state_only(
		setAttribute( State, update_status, up_to_date ) ).



% Blanks specified mesh: resets its content to an empty digraph, and unmarks all
% nodes and links.
%
% Keeps the original digraph options.
%
% Note: update_status not changed.
%
% (oneway)
%
-spec blank( wooper_state() ) -> oneway_return().
blank( State ) ->

	digraph:delete( ?getAttr(digraph) ),

	?wooper_return_state_only( setAttributes( State, [

		{ digraph, digraph:new( [ ?getAttr(cyclic_option), private ] ) },
		{ marked_links, [] },
		{ marked_nodes, [] } ] )

							  ).



% Validates current cached content for nodes and links, so that for example the
% next topological rendering does not trigger an update for any Graphable
% element from the mesh.
%
% Note: useful for example if the mesh was blanked and recreated from scratch
% from outside, and then rendered.
%
% (oneway)
%
-spec validate( wooper_state() ) -> oneway_return().
validate( State ) ->
	?wooper_return_state_only(
		setAttribute( State, update_status, up_to_date ) ).



% Invalidates current cached content for nodes and links, so that for example
% the next topological rendering triggers an update from any Graphable element
% from the mesh.
%
% Note: the connectivity will remain, only the contents are invalidated.
%
% (oneway)
%
-spec invalidate( wooper_state() ) -> oneway_return().
invalidate( State ) ->
	?wooper_return_state_only(
		setAttribute( State, update_status, out_of_sync ) ).




% Nodes section.


% Adds specified node to the mesh, with possibly an associated content.
%
% AddedNode can be a simple node N (in this case the associated content will be
% the 'undefined' atom), or a tuple { N, AssociatedNodeContent }.
%
% (oneway)
%
-spec addNode( wooper_state(), any_node() ) -> oneway_return().
addNode( State, { AddedNode, AssociatedNodeContent } ) ->
	?wooper_return_state_only(
		add_node( AddedNode, AssociatedNodeContent, State ) );

addNode( State, AddedNode ) ->
	?wooper_return_state_only(
		add_node( AddedNode, _AssociatedNodeContent=undefined, State ) ).



% Adds specified node to the mesh, with possibly an associated content.
%
% (oneway)
%
-spec addNode( wooper_state(), pure_node(), node_content() ) -> oneway_return().
addNode( State, AddedNode, AssociatedNodeContent ) ->
	?wooper_return_state_only(
		add_node( AddedNode, AssociatedNodeContent, State ) ).



% Adds specified list of nodes to the mesh.
%
% Each added node can be either a simple node N, or a tuple
% { N, AssociatedNodeContent }.
%
% (oneway)
%
-spec addNodes( wooper_state(), [ any_node() ] ) -> oneway_return().
addNodes( State, NodeList ) ->
	?wooper_return_state_only( add_nodes( NodeList, State ) ).




% Returns the content associated to target node, or the atom 'node_not_found'.
%
% (const request)
%
-spec getContentForNode( wooper_state(), pure_node() ) ->
							request_return( node_content() ).
getContentForNode( State, Node ) ->
	?wooper_return_state_result( State,
		get_content_for_node( ?getAttr(digraph), Node ) ).



% Updates the content associated to target node, by asking directly it for an
% update.
%
% (oneway)
%
-spec updateContentForNode( wooper_state(), pure_node() ) -> oneway_return().
updateContentForNode( State, Node ) ->
	?wooper_return_state_only( update_content_for_node( Node, State ) ).



% Returns a list of all the nodes of this mesh.
%
% (request)
%
-spec getAllNodes( wooper_state() ) ->
						request_return( [ node_with_content() ] ).
getAllNodes( State ) ->
	?wooper_return_state_result( State, digraph:vertices( ?getAttr(digraph) ) ).




% Links section.


% Adds an (anonymous, therefore static) directed link between FromNode and
% ToNode, with no associated content (undefined).
%
% Note:
%
% - the nodes must exist in the mesh already, otherwise { bad_mesh_node, N }
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then { bad_mesh_link, Path } will
% be returned if a cycle would be created by this node addition
%
% (oneway)
%
-spec addLink( wooper_state(), pure_node(), pure_node() ) -> oneway_return().
addLink( State, FromNode, ToNode ) ->
	addStaticLink( State, FromNode, ToNode, undefined ).



% Adds a directed link between FromNode and ToNode, or updates it, with no
% specific content initially set.
%
% If the specified link is a PID, then next time the mesh will be updated, the
% PID will be expected to correspond to a Graphable instance, whose graph
% information will be requested and stored in the link content, for later use.
%
% Note:
%
% - no content will be associated to this link initially
%
% - the nodes must exist in the mesh already, otherwise { bad_mesh_node, N }
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then { bad_mesh_link, Path } will
% be returned if a cycle would be created
%
% (oneway)
%
-spec addLink( wooper_state(), pure_link(), pure_node(), pure_node() ) ->
					oneway_return().
addLink( State, AddedLink, FromNode, ToNode ) ->
	addLink( State, AddedLink, FromNode, ToNode, _Content=undefined ).



% Adds a directed link between FromNode and ToNode, or updates it, and
% associates this link to specified content.
%
% Note:
%
% - if AddedLink is a PID, next time the mesh will be updated, the content of
% this link will be updated from that PID
%
% - the nodes must exist in the mesh already, otherwise { bad_mesh_node, N }
% will be returned, if node N does not exist
%
% - if the mesh is acyclic, then { bad_mesh_link, Path } will be returned if a
% cycle would be created
%
% (oneway)
%
-spec addLink( wooper_state(), pure_link(), pure_node(), pure_node(),
			 link_content() ) -> oneway_return().
addLink( State, AddedLink, FromNode, ToNode, AssociatedLinkContent ) ->
	?wooper_return_state_only(
		add_link( FromNode, ToNode, AddedLink, AssociatedLinkContent, State ) ).



% Adds a directed static link between FromNode and ToNode, or updates it, and
% associates AssociatedLinkContent to this link.
%
% The link itself is not specifically set, it is static, but content is
% associated to it nevertheless.
%
% If generateTopologicalView is to be used, AssociatedLinkContent is expected to
% be of the form { Name, OptionList } or just Name.
%
% Note:
%
% - content will be associated to this link
%
% - both nodes must exist in the mesh already, otherwise { bad_mesh_node, N }
% will be returned, if node N does not exist
%
% - if the mesh has been declared as acyclic, then { bad_mesh_link, Path } will
% be returned if a cycle would be created
%
% Note: named 'addStaticLink', as addLink/4 already exists.
%
% (oneway)
%
-spec addStaticLink( wooper_state(), pure_node(), pure_node(), link_content() )
				   -> oneway_return().
addStaticLink( State, FromNode, ToNode, AssociatedLinkContent ) ->

	%io:format(
	%	"addStaticLink: FromNode =~w, ToNode=~w, AssociatedLinkContent=~w.~n",
	%	[ FromNode, ToNode, AssociatedLinkContent ] ),

	case digraph:add_edge( ?getAttr(digraph), FromNode, ToNode,
			AssociatedLinkContent ) of

		{ error, { bad_edge, Path } } ->
			throw( { bad_mesh_link, Path } );

		{ error, { bad_vertex, N } } ->
			throw( { bad_mesh_node, N } );

		_ ->
			% Not more out-of-date than before because of this operation:
			?wooper_return_state_only( State )

	end.



% Returns the content associated to target link, or the atom 'link_not_found'.
%
% Note: this is the value cached in the mesh for this link, no update is
% performed.
%
% (const request)
%
-spec getContentForLink( wooper_state(), pure_link() ) ->
			request_return( link_content() | 'link_not_found' ).
getContentForLink( State, Link ) ->
	?wooper_return_state_result( State,
		get_content_for_link( ?getAttr(digraph), Link ) ).



% Updates the content associated to target node, by asking directly it for an
% update.
%
% Expected the specified link to be a Graphable PID.
%
% (oneway)
%
-spec updateContentForLink( wooper_state(), graphable_pid() ) ->
								  oneway_return().
updateContentForLink( State, Link ) when is_pid(Link) ->
	?wooper_return_state_only( update_content_for_link( Link, State ) ).



% Returns a list of all the links of this mesh.
%
% (const request)
%
-spec getAllLinks( wooper_state() ) -> request_return( [ any_link() ] ).
getAllLinks( State ) ->
	?wooper_return_state_result( State, digraph:edges( ?getAttr(digraph) ) ).



% Searches in this mesh for a link from FromNode to ToNode.
%
% Returns either the 'no_link_found' atom if no link was found, otherwise
% { Link, LinkContent }.
%
% (const request)
%
-spec findLink( wooper_state(), pure_node(), pure_node() ) ->
					  request_return( 'no_link_found' | link_with_content() ).
findLink( State, FromNode, ToNode ) ->
	?wooper_return_state_result( State,
		find_link_between( FromNode, ToNode, ?getAttr(digraph) ) ).



% Returns the connectivity and state information for specified link:
% { FromNode, ToNode, LinkContent }.
%
% (const request)
%
-spec getLinkInformation( wooper_state(), pure_link() ) ->
				request_return( link_connectivity() ).
getLinkInformation( State, Link ) ->
	?wooper_return_state_result( State,
		get_link_graph_informations( ?getAttr(digraph), Link ) ).



% Tries to find a path between the source node and the target one.
%
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
%
% (const request)
%
-spec findPath( wooper_state(), pure_node(), pure_node() ) ->
		request_return( [ pure_node() ] | 'false' ).
findPath( State, SourceNode, TargetNode ) ->
	?wooper_return_state_result( State, digraph:get_path( ?getAttr(digraph),
		SourceNode, TargetNode ) ).



% Tries to find the shortest path between the source node and the target one.
%
% Returns either an ordered list of nodes (the path) or false, if no path was
% found.
%
% (const request)
%
-spec findShortestPath( wooper_state(), pure_node(), pure_node() ) ->
		request_return( [ pure_node() ] | 'false' ).
findShortestPath( State, SourceNode,TargetNode ) ->
	?wooper_return_state_result( State, digraph:get_short_path(
		?getAttr(digraph), SourceNode, TargetNode ) ).



% Returns the list of links corresponding to the specified node path.
%
% (const request)
%
-spec getLinksInPath( wooper_state(), [ pure_node() ] ) ->
		request_return( [ pure_link() ] ).
getLinksInPath( State, NodeList ) ->
	?wooper_return_state_result( State,
		get_links_from( NodeList, ?getAttr(digraph) ) ).



% Returns the list of all node names corresponding to nodes that can be reached
% from the specified one.
%
% (const request)
%
-spec findReachableFrom( wooper_state(), pure_node() ) ->
			request_return( [ pure_node() ] ).
findReachableFrom( State, NodeName ) ->
	?wooper_return_state_result( State,
		digraph_utils:reachable( [ NodeName ], ?getAttr(digraph) ) ).



% Eliminates all nodes and links that are not reachable from the specified node.
%
% (oneway)
%
-spec pruneFrom( wooper_state(), pure_node() ) -> oneway_return().
pruneFrom( State, Node ) ->

	Digraph = ?getAttr(digraph),

	AllNodes = digraph:vertices(Digraph),
	NodesToKeep = digraph_utils:reachable( [ Node ], Digraph ),
	NodesToRemove = lists:subtract( AllNodes, NodesToKeep ),

	%io:format( "Keeping ~B nodes, removing ~B.~n",
	%	[ length(NodesToKeep), length(NodesToRemove) ] ),

	% Appropriate links will be removed as well:
	lists:foreach( fun(N) -> digraph:del_vertex( Digraph, N ) end,
		NodesToRemove ),

	% Not more out-of-date than before because of this operation:
	?wooper_return_state_only( State ).



% Eliminates all nodes and links that cannot reach the specified node.
%
% (oneway)
%
-spec pruneTo( wooper_state(), pure_node() ) -> oneway_return().
pruneTo( State, Node ) ->

	Digraph = ?getAttr(digraph),

	AllNodes = digraph:vertices(Digraph),
	NodesToKeep = digraph_utils:reaching( [ Node ], Digraph ),
	NodesToRemove = lists:subtract( AllNodes, NodesToKeep ),

	%io:format( "Keeping ~B nodes, removing ~B.~n",
	%	[ length(NodesToKeep), length(NodesToRemove) ] ),

	% Appropriate links will be removed as well:
	lists:foreach( fun(N) -> digraph:del_vertex( Digraph, N ) end,
		NodesToRemove ),

	% Not more out-of-date than before because of this operation:
	?wooper_return_state_only( State ).



% Sets the list of marked nodes.
%
% These nodes, once the topological view will be generated, will be visually
% marked.
%

% (oneway)
%
-spec setMarkedNodes( wooper_state(), [ pure_node() ] ) -> oneway_return().
setMarkedNodes( State, NodeList ) ->
	?wooper_return_state_only( setAttribute( State, marked_nodes, NodeList ) ).



% Sets the list of marked links.
%
% These links, once the topological view will be generated, will be visually
% marked.
%
% (oneway)
-spec setMarkedLinks( wooper_state(), [ pure_link() ] ) -> oneway_return().
setMarkedLinks( State, LinkList ) ->
	?wooper_return_state_only( setAttribute( State, marked_links, LinkList ) ).



% Sets the list of marked links from specified list of endpoints pairs.
%
% These links, once the topological view will be generated, will be visually
% marked.
%
% (oneway)
%
-spec setMarkedLinksFromEndpoints( wooper_state(), [ edge() ] ) ->
										oneway_return().
setMarkedLinksFromEndpoints( State, EndpointList ) ->

	LinkList = determine_links_from_endpoints( EndpointList,
		?getAttr(digraph), [] ),

	?wooper_return_state_only( setAttribute( State, marked_links, LinkList ) ).



% Adds specified element expressed in raw dot notation to the rendering of this
% mesh.
%
% See also: generate_text_panel/2.
%
% (oneway)
%
-spec addRenderingRawElement( wooper_state(), string() ) -> oneway_return().
addRenderingRawElement( State, RawRenderingElement ) ->
	?wooper_return_state_only( appendToAttribute( State,
		rendering_raw_elements, RawRenderingElement ) ).



% Sets the settings for node and link rendering.
%
% (oneway)
%
-spec setRenderingSettings( wooper_state(),
		{ {node_style(),node_color()}, {node_style(),node_color()} },
		{ {link_style(),link_color()}, {link_style(),link_color()} } ) ->
					  oneway_return().
setRenderingSettings( State,
		{ { NodeStyle, NodeColor }, { MarkedNodeStyle, MarkedNodeColor } },
		{ { LinkStyle, LinkColor }, { MarkedLinkStyle, MarkedLinkColor } } ) ->

	?wooper_return_state_only( setAttributes( State,  [

		{ node_style, NodeStyle },
		{ node_color, NodeColor },
		{ marked_node_style, MarkedNodeStyle },
		{ marked_node_color, MarkedNodeColor },
		{ link_style, LinkStyle },
		{ link_color, LinkColor },
		{ marked_link_style, MarkedLinkStyle },
		{ marked_link_color, MarkedLinkColor }

													   ] ) ).



% Returns a string describing the state of this mesh.
%
% (const request)
%
-spec getMeshInformation( wooper_state() ) -> request_return( string() ).
getMeshInformation( State ) ->

	Digraph = ?getAttr(digraph),

	?wooper_return_state_result( State, lists:flatten( io_lib:format(
		"Mesh ~w status: ~B nodes and ~B links",
		[ self(), length( digraph:vertices(Digraph) ),
			length( digraph:edges(Digraph) ) ] ) ) ).



% Generates a view of current topology of this mesh.
%
% DisplayWanted is a boolean telling whether the generated view will be
% displayed to the user (if true).
%
% (request)
%
-spec generateTopologicalView( wooper_state(), boolean() ) ->
			   request_return( 'topological_view_generated' ).
generateTopologicalView( State, DisplayWanted ) ->

	NewState = generate_topological_view( DisplayWanted,
		file_utils:convert_to_filename(
				   text_utils:binary_to_string( ?getAttr(name) ) ),
		State ),

	?wooper_return_state_result( NewState, topological_view_generated ).



% Generates a view of current topology of this mesh:
%
% - DisplayWanted: boolean telling whether the generated view will be displayed
% to the user (if true)
%
% - FilenameSuffix: a string to add to the base filename (ex: '-0012'), useful
% to iterate on a set of numbered images (ex: 'xx-1.png', 'xx-2.png', etc.)
%
% (request)
%
-spec generateTopologicalView( wooper_state(), boolean(), string() ) ->
				  request_return( 'topological_view_generated' ).
generateTopologicalView( State, DisplayWanted, FilenameSuffix ) ->

	NewState = generate_topological_view( DisplayWanted,
		?getAttr(graph_filename) ++ FilenameSuffix, State ),

	?wooper_return_state_result( NewState, topological_view_generated ).





% Section for helper functions (not methods).


% Adds a node in mesh.
%
% Returns an updated state.
%
add_node( Node, Content, State ) ->

	% Side-effect:
	digraph:add_vertex( ?getAttr(digraph), Node, Content ),

	setAttribute( State, update_status, out_of_sync ).


add_nodes( _NodeList=[], State ) ->
	State;

add_nodes( _NodeList=[ { Node, Content } | T ], State ) ->
	add_nodes( T, add_node( Node, Content, State ) );

add_nodes(_NodeList= [ Node | T ], State ) ->
	add_nodes( T, add_node( Node, _Content=undefined, State ) ).



% Adds a link in mesh.
%
% Returns an updated state.
%
add_link( FromNode, ToNode, Link, Content, State ) ->

	%io:format( "add_link for ~w from ~w to ~w.~n",
	%		  [ Link, FromNode, ToNode ] ),

	% Side-effect:
	case digraph:add_edge( ?getAttr(digraph), Link, FromNode, ToNode,
			Content ) of

		{ error, { bad_edge, Path } } ->
			throw( { bad_mesh_link, Path } );

		{ error, { bad_vertex, N } } ->
			throw( { bad_mesh_node, N } );

		_ ->
			setAttribute( State, update_status, out_of_sync )

	end.



generate_topological_view( DisplayWanted, BaseFileName, State ) ->

	UpdatedState = case ?getAttr(update_status) of

		up_to_date ->
			State;

		_OtherStatus ->
			% Beware of silent unexpected asynchronous updates...
			%io:format( "class_Mesh: updating.~n" ),

			% Returns an updated state:
			executeOneway( State, update )

	end,


	DirState = case ?getAttr(graph_directory_created) of

		false ->

			OutputDirectory = ?getAttr(graph_directory),

			case file:make_dir( OutputDirectory ) of

				ok ->
					ok ;

				{ error, eexist } ->
					ok;

				OtherError ->
					throw( { mesh_directory_creation_failed, OtherError,
						OutputDirectory } )

			end,
			setAttribute( UpdatedState, graph_directory_created, true );

		true ->
			UpdatedState

	end,

	DigraphFilename = BaseFileName ++ ".graph",
	PNGFilename     = BaseFileName ++ ".png",

	?debug_fmt( "Generating topology for ~s: graph in ~s, view in ~s.~n",
		[ ?getAttr(name), DigraphFilename, PNGFilename ] ),

	DigraphFile = file_utils:open( DigraphFilename, [ write ] ),

	write_graph_header( DigraphFile, DirState ),
	write_graph_nodes(  DigraphFile, DirState ),
	write_graph_links(  DigraphFile, DirState ),
	write_raw_elements( DigraphFile, DirState ),

	write_graph_footer( DigraphFile, DirState ),

	file_utils:close( DigraphFile ),

	% false as Dot might issue non-serious warnings:
	executable_utils:generate_png_from_graph_file( PNGFilename,
		DigraphFilename, false ),

	case DisplayWanted of

		true ->
			executable_utils:display_png_file( PNGFilename );

		false ->
			ok

	end,

	% Removes the intermediate graph file:
	%ok = file:delete( DigraphFilename ),

	DirState.



% Writes the graph header for the topology of this mesh in specified file.
%
write_graph_header( DigraphFile, State ) ->

	io:format( DigraphFile, "digraph Mesh_topological_view~n", [] ),
	io:format( DigraphFile, "{~n~n", [] ),

	io:format( DigraphFile, "    layout = ~s~n~n",
			  [ ?getAttr(layout_option) ] ),

	% size = \"10,10\", fontsize = \"14.0\",
	io:format( DigraphFile, "    graph [ label = \"~s\", "
			  "fontsize = \"20.0\"]~n~n", [ ?getAttr(graph_label) ] ),

	io:format( DigraphFile, "    node [ height = 1, width = 1, "
		"fixedsize = true, fontsize = \"10.0\" ]~n~n", [] ).



% Writes the description of graph nodes of this mesh in specified file.
%
write_graph_nodes( DigraphFile, State ) ->

	io:format( DigraphFile, "~n/* Node definitions */~n~n", [] ),

	Nodes = digraph:vertices( ?getAttr(digraph) ),

	write_graph_nodes( DigraphFile, Nodes, State ).

write_graph_nodes( DigraphFile, _NodeList=[], _State ) ->
	io:format( DigraphFile, "\n", [] );

write_graph_nodes( DigraphFile, _NodeList=[ Node | T ], State ) ->

	case get_content_for_node( ?getAttr(digraph), Node ) of

		undefined ->
			throw( { content_less_node, Node } );

		NodeOptions ->

			%io:format( "Node ~w, options: ~p.~n", [ Node, NodeOptions ] ),

			NodeName = class_Graphable:forge_node_name( Node ),
			Options = case lists:member( Node, ?getAttr(marked_nodes) ) of

				false ->
					format_options( NodeOptions );

				true ->
					% Overrides node informations with marked settings:
					format_marked_options_for_nodes( NodeOptions, State )

			end,
			io:format( DigraphFile, "\"~s\" [~s]~n", [ NodeName, Options ] )

	end,

	write_graph_nodes( DigraphFile, T, State ).



% Formats specified options: [ {a,a_value}, {b,b_value}, {c,c_value} ] must
% become: a = "a_value", b = "b_value", c = "c_value".
format_options( undefined ) ->
	"";

format_options( NodeOptions ) ->
	%io:format( "Formatting ~p.~n", [NodeOptions] ),
	text_utils:join( ", ", lists:map( fun( { Name, Value } ) ->
		io_lib:format( "~s = \"~s\"", [ Name, Value ] ) end, NodeOptions ) ).



% Same as format_options, except some options are overridden for nodes:
format_marked_options_for_nodes( undefined, _State ) ->
	"";

format_marked_options_for_nodes( Options, State ) ->
	FilteredOptions = filter_marked_options_for_nodes( Options, _Acc=[],
													   State ),
	format_options( FilteredOptions ).


filter_marked_options_for_nodes( _Options=[], Acc,_State ) ->
	Acc;

filter_marked_options_for_nodes( _Options=[ {color,_Color} | T ] , Acc,
								 State ) ->
	filter_marked_options_for_nodes( T,
		[ { color, ?getAttr(marked_node_color) } | Acc ], State );

filter_marked_options_for_nodes( _Options=[ {style,_Style} | T ], Acc,
								 State ) ->
	filter_marked_options_for_nodes( T,
		[ { style, ?getAttr(marked_node_style) } | Acc ], State );

filter_marked_options_for_nodes( _Options=[ Elem | T ], Acc, State ) ->
	filter_marked_options_for_nodes( T, [ Elem | Acc ], State ).



% Same as format_options, except some options are overridden for links:
format_marked_options_for_links( undefined, _State ) ->
	"";

format_marked_options_for_links( Options, State ) ->
	FilteredOptions = filter_marked_options_for_links( Options, _Acc=[],
													   State ),
	format_options( FilteredOptions ).


filter_marked_options_for_links( _Options=[], Acc, _State ) ->
	Acc;

filter_marked_options_for_links( _Options=[ {color,_Color} | T ] , Acc,
								 State ) ->
	filter_marked_options_for_links( T,
		[ { color, ?getAttr(marked_link_color) } | Acc ], State );

filter_marked_options_for_links( _Options=[ {style,_Style} | T ], Acc,
								 State ) ->
	filter_marked_options_for_links( T,
		[ { style, ?getAttr(marked_link_style) } | Acc ], State );

filter_marked_options_for_links( _Options=[ Elem | T ], Acc, State ) ->
	filter_marked_options_for_links( T, [ Elem | Acc ], State ).



% Writes the description of graph links of this mesh in specified file.
write_graph_links( DigraphFile, State ) ->

	io:format( DigraphFile, "~n/* Link definitions */~n~n", [] ),

	Links = digraph:edges( ?getAttr(digraph) ),

	% Sorts links as well, for increased safety in rendering stability:
	write_graph_links( DigraphFile, Links, State ).


write_graph_links( DigraphFile, _Links=[], _State ) ->
	io:format( DigraphFile, "\n", [] );

write_graph_links( DigraphFile, _Links=[ Link | T ], State ) ->

	{ SourceNode, TargetNode, LinkOptions } =
		get_link_graph_informations( ?getAttr(digraph), Link ),

	SourceNodeName = class_Graphable:forge_node_name( SourceNode ),
	TargetNodeName = class_Graphable:forge_node_name( TargetNode ),

	Options = case lists:member( Link, ?getAttr(marked_links) ) of

		false ->
			format_options( LinkOptions );

		true ->
			% Overrides link informations with marked settings:
			format_marked_options_for_links( LinkOptions, State )

	end,

	io:format( DigraphFile, "\"~s\" -> \"~s\" [~s]~n",
		[ SourceNodeName, TargetNodeName, Options ] ),

	write_graph_links( DigraphFile, T, State ).



% Writes the graph legend of this mesh in specified file.
%
write_raw_elements( DigraphFile, State ) ->
	lists:foreach( fun(Elem) -> io:format( DigraphFile, Elem, [] ) end,
		?getAttr(rendering_raw_elements) ).



% Writes the graph footer for the topology of this mesh in specified file.
%
write_graph_footer( DigraphFile, _State ) ->
	io:format( DigraphFile, "~n}~n", [] ).



% Returns the graph informations associated to specified link, i.e.:
% { SourceNode, TargetNode, LinkOptions }.
%
get_link_graph_informations( Digraph, Link ) ->

	{ Link, SourceNode, TargetNode, LinkOptions } = digraph:edge( Digraph,
																  Link ),

	{ SourceNode, TargetNode, LinkOptions }.



find_link_between( SourceNode, TargetNode, Digraph ) ->

	% Get all edges emanating from FromNode:
	Links = digraph:out_edges( Digraph, SourceNode ),

	find_link_targeting( TargetNode, Links, Digraph ).



% Returns the first link found in link list 'Links' targeting 'TargetNode', or,
% if none is found, 'no_link_found'.
%
% Returns, if found, the digraph link and its associated content.
% Signature: find_link_targeting( TargetNode, Links, Digraph ).
%
find_link_targeting( _TargetNode, _Links=[], _Digraph ) ->
	no_link_found;

find_link_targeting( TargetNode, _Links=[ L | T ], Digraph ) ->

	case digraph:edge( Digraph, L ) of

		{ L, _SourceNode, TargetNode, LinkContent } ->
			% Found, stop the look-up:
			{ L, LinkContent };

		_ ->
			find_link_targeting( TargetNode, T, Digraph )

	end.



% Returns the list of links between the specified list of nodes.
get_links_from( NodeList, Digraph ) ->
	get_links_from( NodeList, Digraph, _Acc=[] ).



get_links_from( [ N1, N2 | T ], Digraph, Acc ) ->

	OutLinks = digraph:out_edges( Digraph, N1 ),

	{ Link, _LinkContent}  = find_link_targeting( N2, OutLinks, Digraph ),

	get_links_from( [ N2 | T ], Digraph, [ Link | Acc ] );


get_links_from( _LastNode, _Digraph, Acc ) ->
	lists:reverse( Acc ).


determine_links_from_endpoints( _EndpointList=[], _Digraph, Acc ) ->
	Acc;


determine_links_from_endpoints( _EndpointList=[ {Source,Target} | H ], Digraph,
								Acc ) ->
	{ Link, _LinkContent } = find_link_between( Source, Target, Digraph ),
	determine_links_from_endpoints( H, Digraph, [ Link | Acc ] ).


get_content_for_node( Digraph, Node ) ->

	case digraph:vertex( Digraph, Node ) of

		{ Node, Content } ->
			Content;

		false ->
			node_not_found

	end.



get_content_for_link( Digraph, Link ) ->

	 case digraph:edge( Digraph, Link ) of

		{ Link, _SourceNode, _TargetNode, Content } ->
			Content;

		false ->
			link_not_found

	end.



update_content_for_node( Node, State ) ->

	Node ! { getGraphOptions, [], self() },

	NodeContent = receive

		{ wooper_result, Content } ->
			Content

	end,

	% It is actually an update:
	add_node( Node, NodeContent, State ).


update_content_for_link( Link, State ) when is_pid(Link) ->

	Link ! { getGraphOptions, [], self() },

	LinkContent = receive

		{ wooper_result, Content } ->
			Content

	end,

	% Needing to retrieve its endpoints to update it:
	{ Link, SourceNode, TargetNode, _LinkContent } =
		digraph:edge( ?getAttr(digraph), Link ),

	% It is actually an update:
	add_link( SourceNode, TargetNode, Link, LinkContent, State );

update_content_for_link( _Link, State ) ->
	State.



% Factors parts common to all constructors.
%
init_common( State, Options ) ->

	CycleInfo = case lists:keyfind( can_be_cyclic, _Index=1, Options ) of

		{ can_be_cyclic, false } ->
			acyclic;

		% Includes{ can_be_cyclic, true } and false (i.e. not found):
		_ ->
			cyclic

	end,

	LayoutInfo = case lists:keyfind( layout, _Index=1, Options ) of

		{ layout, Layout } ->
			case is_known_layout( Layout ) of

				true ->
					Layout;

				false ->
					throw( { unknown_layout, Layout } )

			end;

		_ ->
			dot

	end,

	Name = ?getAttr(name),

	?info_fmt( "Creating a new mesh whose name is ~s, which will be ~w.",
		[ Name, CycleInfo ] ),

	% As long as the mesh will remain empty, it will be up-to-date:
	setAttributes( State, [

		{ digraph, digraph:new( [ CycleInfo, private ] ) },
		{ cyclic_option, CycleInfo },
		{ layout_option, LayoutInfo },
		{ graph_label, io_lib:format( "Topological view of mesh '~s'",
									 [ Name ] ) },
		{ rendering_raw_elements, [] },
		{ node_style, ?default_node_style },
		{ node_color, ?default_node_color },
		{ marked_node_style, ?default_marked_node_style },
		{ marked_node_color, ?default_marked_node_color },
		{ link_style, ?default_link_style },
		{ link_color, ?default_link_color },
		{ marked_link_style, ?default_marked_link_style },
		{ marked_link_color, ?default_marked_link_color },
		{ marked_nodes, [] },
		{ update_status, up_to_date },
		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

						   ] ).



% Generates a string, suitable to be used with addRenderingRawElement, to
% represent a text panel whose title and internal content are the specified
% ones.
%
% The content must respect the syntax specified in:
% http://www.graphviz.org/doc/info/shapes.html#record
%
% See addRenderingRawElement/2.
%
-spec generate_text_panel( string(), string() ) -> string().
generate_text_panel( Title, Content ) ->

	"subgraph cluster_" ++ text_utils:generate_text_name_from(Title) ++ "\n"
		++ "{\n\n"
		++ "    graph [ rankdir = \"LR\" ];\n"
		++ "    fontsize = 25\n"
		++ "    pencolor = white\n"
		++ "    label = \"" ++ Title ++ "\"\n\n"
		++ "    \"node0\" [\n"
		++ "        fixedsize= true\n"
		++ "        fontsize = 20\n"
		++ "        width = 8\n"
		++ "        height = 6\n"
		++ "        shape = \"Mrecord\"\n"
		++ "        label = \"" ++ Content ++ "\"\n"
		++ "    ];\n\n"
		++ "}\n\n".



% Allows to define whether the topological view should be displayed to the user,
% after generation.
%
% (static method)
%
-spec generate_topological_view_for( pid() ) -> basic_utils:void().
generate_topological_view_for( MeshPid ) ->

	% Avoids adding a bound variable:
	case executable_utils:is_batch() of

		true ->
			% Boolean means 'display wanted':
			MeshPid ! { generateTopologicalView, false, self() };

		false ->
			MeshPid ! { generateTopologicalView, true, self() }

	end,

	receive

		{ wooper_result, topological_view_generated } ->
			?notify_info( "Topological view correctly generated." )

	end.



% Tells whether a layout is known.
%
is_known_layout( Layout ) ->
	% See mesh_layout():
	lists:member( Layout, [ dot , neato , twopi , circo , fdp , sfdp ] ).
