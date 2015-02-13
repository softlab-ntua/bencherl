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



% Basic scalability test of the Sim-Diasca framework.
%
-module(scheduling_scalability_test).


% To be allowed to disable this very lengthy operation:
-export([ link_actors/2 ]).


% For all facilities common to all tests:
-include("test_constructs.hrl").




% Returns the construction parameters of interest for the specified actor
% number.
%
% (helper)
%
determine_actor_settings( ActorCount, SimulationDuration ) ->

	ActorName = io_lib:format( "Actor ~B", [ ActorCount ] ),


	SchedulingPolicy = case ActorCount rem 2 of

		0 ->
			{ periodic, _Period=class_RandomManager:get_uniform_value( 10 ) } ;

		1 ->
			{ erratic, _MinRange=class_RandomManager:get_uniform_value( 8 ) }

	end,

	% Not all will terminate before the simulation ends:
	TerminationTickOffset = 10 + class_RandomManager:get_uniform_value(
							   SimulationDuration + 200 ),

	{ ActorName, SchedulingPolicy, TerminationTickOffset }.




% Adds the specified count of peers, drawn from list, to specified actor.
%
% Note: this may lead to a given peer be added more than once.
%
% (helper)
%
add_initial_peers( _ActorPid, _PotentialPeers, _PeerCount=0 ) ->
	ok;

add_initial_peers( ActorPid, PotentialPeers, PeerCount ) ->

	case length( PotentialPeers ) of

		0 ->
			% No possible peer, stopping there:
			ok;

		Len ->

			PeerIndex = class_RandomManager:get_uniform_value( Len ),
			Peer = lists:nth( PeerIndex, PotentialPeers ),

			ActorPid ! { addInitialPeer, Peer, self() },
			peer_added = test_receive(),

			add_initial_peers( ActorPid, PotentialPeers, PeerCount-1 )

	end.



% Returns the maximum number of peers for a given actor.
%
% (helper)
%
get_max_peer_count() ->
	8.



% Creates automatically (and synchronously) the specified number of initial
% actors, one after the other.
%
% Returns a list of the PID of the created actors.
%
create_initial_actors_direct( ActorCount, LoadBalancerPid,
							 SimulationDuration ) ->
	create_initial_actors_direct( ActorCount, LoadBalancerPid,
								 SimulationDuration, _Acc=[] ).


create_initial_actors_direct( _ActorCount=0, _LoadBalancerPid,
							 _SimulationDuration, Acc ) ->
	Acc;

create_initial_actors_direct( ActorCount, LoadBalancerPid, SimulationDuration,
							 Acc ) ->


	{ ActorName, SchedulingPolicy, TerminationTickOffset } =
		determine_actor_settings( ActorCount, SimulationDuration ),


	case ActorCount rem 100 of

		0 ->
			?test_info_fmt( "Still ~B actors to create.", [ ActorCount ] );

		_Other ->
			ok

	end,

	ActorPid = class_Actor:create_initial_actor( class_TestActor,
		[ ActorName, SchedulingPolicy, no_creation, TerminationTickOffset ],
		  LoadBalancerPid ),

	MaxPeerCount = get_max_peer_count(),

	add_initial_peers( ActorPid, Acc,
					  class_RandomManager:get_uniform_value( MaxPeerCount ) ),

	create_initial_actors_direct( ActorCount-1, LoadBalancerPid,
								 SimulationDuration, [ ActorPid | Acc ] ).




% Creates automatically (and synchronously) the specified number of initial
% actors, in batch.
%
% Returns a list of the PID of the created actors.
%
create_initial_actors_indirect( ActorCount, LoadBalancerPid,
							   SimulationDuration ) ->

	?test_info_fmt( "Will create a batch of ~B actors.", [ ActorCount ] ),

	io:format( "Will create a batch of ~B actors, at ~s.~n", [ ActorCount,
				   basic_utils:get_textual_timestamp() ] ),

	FullCreationList = define_initial_actors_indirect( ActorCount,
										  SimulationDuration, _Acc=[] ),

	?test_info( "Actors defined, now creating them." ),

	io:format( "Actors defined, now creating them, at ~s.~n",
			  [ basic_utils:get_textual_timestamp() ] ),

	ActorList = class_Actor:create_initial_actors( FullCreationList,
												  LoadBalancerPid ),


	?test_info( "Actors created, now linking them." ),

	io:format( "Actors created, now linking them, at ~s.~n",
			  [ basic_utils:get_textual_timestamp() ] ),

	% This part of the test is not scalable:
	%link_actors( ActorList, get_max_peer_count() ),

	io:format( "Actors linked, at ~s.~n",
			  [ basic_utils:get_textual_timestamp() ] ),

	?test_info( "Actors linked." ),

	ActorList.



define_initial_actors_indirect( _ActorCount=0, _SimulationDuration, Acc ) ->
	Acc;

define_initial_actors_indirect( ActorCount, SimulationDuration, Acc ) ->

	{ ActorName, SchedulingPolicy, TerminationTickOffset } =
		determine_actor_settings( ActorCount, SimulationDuration ),

	NewActorDef = { class_TestActor,
		  [ ActorName, SchedulingPolicy, no_creation, TerminationTickOffset ] },

	define_initial_actors_indirect( ActorCount - 1, SimulationDuration,
								   [ NewActorDef | Acc ] ).




% Links specified actor.


link_actors( _ActorList=[], _MaxPeerCount ) ->
	ok;

link_actors( _ActorList= [ ActorPid | OtherActors ], MaxPeerCount ) ->

	add_initial_peers( ActorPid, OtherActors,
					  class_RandomManager:get_uniform_value( MaxPeerCount ) ),

	link_actors( OtherActors, MaxPeerCount ).





% Runs a distributed simulation (of course if relevant computing hosts are
% specified).
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Scheduling scalability test"

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), but computing
	% hosts are specified (to be updated depending on your environment):
	%
	% (note that localhost is excluded)
	DeploymentSettings = #deployment_settings{

		computing_hosts =
			%% { use_host_file_otherwise_local,"sim-diasca-host-candidates.txt",
			%%  exclude_localhost }
			{ use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" }

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init(	SimulationSettings,
								  DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Deployment manager created, retrieving the load balancer." ),

	DeploymentManagerPid ! { getLoadBalancer, [], self() },
	LoadBalancerPid = test_receive(),

	% Increase class_TimeManager:get_maximum_idle_duration() if being in
	% development mode and wanting many actors:
	%
	%ActorCountPerComputingNode = 50000,
	%ActorCountPerComputingNode = 25000,
	%ActorCountPerComputingNode = 15000,
	ActorCountPerComputingNode = 500,
	%ActorCountPerComputingNode = 50,
	%ActorCountPerComputingNode = 5,
	%ActorCountPerComputingNode = 3,

	LoadBalancerPid ! { getComputingNodes, [], self() },
	NodeList = test_receive(),
	ComputingNodeCount = length( NodeList ),


	SimulationDurationInTicks = 15000,

	TotalActorCount = ActorCountPerComputingNode * ComputingNodeCount,

	?test_info_fmt( "Will now create a total of ~B initial actors, "
					"i.e. ~B actors on each of "
					"the ~B actually available computing nodes.",
					[ TotalActorCount, ActorCountPerComputingNode,
					 ComputingNodeCount ] ),

	?test_info( "Requesting to the load balancer the creation of "
		"a first initial test actor." ),

	FirstTerminationTickOffset = 1 + round( SimulationDurationInTicks / 2 ),


	FirstActorPid = class_Actor:create_initial_actor( class_TestActor,
			 [ "First test actor", { periodic, _FirstPeriod=3 },
			 no_creation, FirstTerminationTickOffset ], LoadBalancerPid ),

	FirstActorPid ! { getAAI, [], self() },
	2 = test_receive(),


	?test_info_fmt( "First actor has for PID ~w and for AAI 2.",
				   [ FirstActorPid ] ),

	?test_info( "First actor has a correct AAI." ),

	SecondTerminationTickOffset = 1
		+ round( 2 * SimulationDurationInTicks / 3 ),

	SecondActorPid = class_Actor:create_initial_actor( class_TestActor,
			 [ "Second test actor", { periodic, _SecondPeriod=3 },
			 no_creation, SecondTerminationTickOffset ],
			 LoadBalancerPid ),

	SecondActorPid ! { getAAI, [], self() },
	3 = test_receive(),


	% Meant to be still living at the end of the simulation:
	ThirdTerminationTickOffset = 2 * SimulationDurationInTicks,
	ThirdActorPid = class_Actor:create_initial_actor( class_TestActor,
			[ "Third test actor", { periodic, _ThirdPeriod=3 },
			no_creation, ThirdTerminationTickOffset ], LoadBalancerPid ),

	ThirdActorPid ! { getAAI, [], self() },
	4 = test_receive(),

	?test_info( "First three actors have correct AAI." ),


	?test_info( "Linking actors." ),

	% Requests, to block and not start the simulation before the initial
	% situation is set up (to avoid a race condition):
	%
	SecondActorPid ! { addInitialPeer, FirstActorPid, self() },
	peer_added = test_receive(),

	ThirdActorPid  ! { addInitialPeer, SecondActorPid, self() },
	peer_added = test_receive(),

	FirstActorPid  ! { addInitialPeer, ThirdActorPid, self() },
	peer_added = test_receive(),


	% Two ways of creating the initial actors can be tested here:
	%
	% - the direct creation: actors are created one after the other, with as
	% many calls to the load balancer
	%
	% - the indirect creation: creation parameters are determined first in a
	% large list, which is then sent as a whole to the load-balancer:

	TestBatchOfCreations = true,

	% Three actors were already created:
	ActorToCreate = TotalActorCount - 3,

	PreCreation = basic_utils:get_timestamp(),

	io:format( "Starting batched creation.~n" ),

	case TestBatchOfCreations of

		false ->
			create_initial_actors_direct( ActorToCreate, LoadBalancerPid,
						  SimulationDurationInTicks );

		true ->
			create_initial_actors_indirect( ActorToCreate, LoadBalancerPid,
						  SimulationDurationInTicks )

	end,

	PostCreation = basic_utils:get_timestamp(),

	io:format( "Creation lasted for ~s.~n",
		  [ basic_utils:get_textual_duration( PreCreation, PostCreation ) ] ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=SimulationDurationInTicks,
								   self() ] },


	?test_info( "Requesting textual timings (first)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	FirstTimingString = test_receive(),

	?test_info_fmt( "Received first time: ~s.", [ FirstTimingString ] ),


	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,


	?test_info( "Requesting textual timings (second)." ),

	RootTimeManagerPid ! { getTextualTimings, [], self() },
	SecondTimingString = test_receive(),

	?test_info_fmt( "Received second time: ~s.", [ SecondTimingString ] ),

	sim_diasca:shutdown(),

	?test_stop.
