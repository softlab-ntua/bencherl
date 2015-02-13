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


% Unit tests for the Mesh class implementation.
%
% See the class_Mesh module.
%
-module(class_Mesh_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


% For result_manager_name:
-include("class_ResultManager.hrl").


% For instance_tracker_name:
-include("class_InstanceTracker.hrl").



% Runs the tests.
%
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Creating a new test Mesh." ),

	% A mesh is a result producer, which needs to rely on a result manager. We
	% fake it here, this test is a mock-up one:
	basic_utils:register_as( ?result_manager_name, global_only ),

	% Mock version thereof needed as well:
	basic_utils:register_as( ?instance_tracker_name, local_only ),

	% Acyclic:
	MyMesh = class_Mesh:new_link( "My test mesh",
								  [ { can_be_cyclic, false } ] ),

	% Still faking:
	receive

		{ registerResultProducer, _BinName, MyMesh } ->
			MyMesh ! { wooper_result, result_producer_registered }

	end,

	FirstNode  = first,
	SecondNode = second,
	ThirdNode  = third,

	% FirstNode  -> SecondNode
	% FirstNode  -> ThirdNode
	% SecondNode -> ThirdNode

	MyMesh ! { addNode, FirstNode },

	NodeList = [ { SecondNode, "hello" }, ThirdNode ],

	% Note the list in list:
	MyMesh ! { addNodes, [ NodeList ] },

	MyMesh ! { getAllNodes, [], self() },
	Nodes = test_receive(),

	?test_info_fmt( "This mesh has following nodes defined: ~p.", [ Nodes ] ),


	?test_info( "Testing link management." ),

	MyMesh ! { getAllLinks, [], self() },
	[] = test_receive(),

	?test_info( "This mesh has no link defined, as expected." ),

	MyMesh ! { addLink, [ FirstNode, SecondNode  ] },
	MyMesh ! { addStaticLink, [ FirstNode, ThirdNode, "I am a link content" ] },
	MyMesh ! { addLink, [ SecondNode, ThirdNode ] },

	MyMesh ! { getAllLinks, [], self() },

	NewLinks = test_receive(),

	?test_info_fmt( "This mesh has now following links defined "
					"(not really readable): ~p.", [ NewLinks ] ),

	MyMesh ! { getMeshInformation, [], self() },
	FirstInfos = test_receive(),

	?test_info( "Received mesh information: " ++ FirstInfos ),

	% Uncomment this to see what happens when a cycle is made in an acyclic
	% mesh:
	%MyMesh ! { addLink, [ ThirdNode, FirstNode ] },

	% Allows to avoid raising an unwanted error:
	MyMesh ! { setResultProducedStatus, true },
	MyMesh ! { setResultCollectedStatus, true },


	MyMesh ! delete,


	?test_info( "Testing mesh rendering as a graph, "
		"using now Graphable instances as nodes and links." ),

	% Acyclic:
	MyGraphableMesh = class_Mesh:new_link( "My test graphable mesh",
					[ { can_be_cyclic, false } ] ),

	% Still faking:
	receive

		{ registerResultProducer, _GraphableBinName, MyGraphableMesh } ->
			MyGraphableMesh ! { wooper_result, result_producer_registered }

	end,

	% Defines some nodes:
	N1 = class_Graphable:new_link( "My first graphable node" ),

	N2 = class_Graphable:new_link( [

		{ label, "My second graphable node" },
		{ bgcolor, yellow },
		{ fillcolor, green },
		{ color, red },
		{ pencolor, cyan }

									] ),

	N3 = class_Graphable:new_link( [

		{ label, "My third graphable node" },
		{ shape, hexagon },
		{ color, blue }

									] ),

	N4 = class_Graphable:new_link( "My fourth graphable node" ),

	?test_info_fmt( "Adding nodes ~p.", [ [ N1, N2, N3, N4 ] ] ),

	% Double list, otherwise will try to call that method with an arity equal
	% to the length of the list + 1:
	MyGraphableMesh ! { addNodes, [ [ N1, N2, N3, N4 ] ] },

	% Defines some links:
	L1 = class_Graphable:new_link( [
		{ label, "My first graphable link" }, { color, green } ] ),

	L2 = class_Graphable:new_link( [
		{ label, "My second graphable link" }, { color, red } ] ),

	MyGraphableMesh ! { addLink, [ L1, N1, N2 ] },

	MyGraphableMesh ! { addStaticLink, [ N1, N3,
		[ { label, "I am a link from first to third" } ] ] },

	MyGraphableMesh ! { addStaticLink, [ N2, N3,
		[ { label, "I am a link from second to third" } ] ] },

	MyGraphableMesh ! { addLink, [ L2, N2, N4 ] },


	MyGraphableMesh ! { findLink, [ N1, N2 ], self() },
	{ Link, LinkLabel } = test_receive(),

	?test_info_fmt( "Link from first to second graphable is ~p, "
				   "whose label is ~s.", [ Link, LinkLabel ] ),


	MyGraphableMesh ! { findPath, [ N1, N4 ], self() },
	Path = test_receive(),

	?test_info_fmt( "Path from first to fourth graphable: ~p.", [ Path ] ),


	MyGraphableMesh ! { findShortestPath, [ N1, N4 ], self() },
	ShortestPath = test_receive(),

	?test_info_fmt( "Shortest path from first to fourth graphable: ~p.",
				   [ ShortestPath ] ),


	MyGraphableMesh ! { setMarkedNodes, [ Path ] },


	MyGraphableMesh ! { getLinksInPath, [ ShortestPath ], self() },
	PathLinks = test_receive(),

	?test_info_fmt( "Links from first to fourth graphable: ~p.",
				   [ PathLinks ] ),

	MyGraphableMesh ! { setMarkedLinks, [ PathLinks ] },

	MyTextPanel = class_Mesh:generate_text_panel( "This is my text panel",
		"{{I am: a test | { Number of bubbles: 42|Alien ratio: 0.7} } | "
		"{ Acceleration Factor: Gaussian | "
		"{ Status: launched | Installed motors: 40/42 [55%] } } }" ),

	MyGraphableMesh ! { addRenderingRawElement, [ MyTextPanel ] },

	class_Mesh:generate_topological_view_for( MyGraphableMesh ),

	% Avoids the failure of a consistency check, for this specific test:
	MyGraphableMesh ! { setResultCollectedStatus, true },

	% Avoid racing with the halt:
	wooper:delete_synchronously_instance( MyGraphableMesh ),

	?test_stop.
