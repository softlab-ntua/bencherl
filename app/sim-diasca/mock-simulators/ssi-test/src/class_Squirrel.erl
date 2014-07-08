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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.

% The objective of this module is to show the significant features of a
% Sim-Diasca actor in multiple scheduling modes.
%
% This means that it has a periodic schedule and that it can be also triggered
% by messages.



% Class modelling a squirrel actor. It defines the common squirrel actor
% attributes and spontaneous beaviours.
%
% NB: In SSI-Test, each simulation tick corresponds to a week.
%
-module(class_Squirrel).


% Determines what are the mother classes
-define( wooper_superclasses, [ class_ForestDweller ] ).


-define( wooper_construct_parameters, ActorSettings, SquirrelName, GivenAge, 
		ForestPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
	remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
	construct/5 ).


% Declarations of class-specific methods (besides inherited ones).
-define( wooper_method_export, onFirstDiasca/2, beMoved/2, beAllocated/3,
		 notifyTermination/1, deleteFromPeers/2, tryToRegister/3,
		 forestDestroyed/2 ).


% Static method declarations (to be directly called from module):
%-define( wooper_static_method_export, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization, "SSI-Test.Squirrel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new squirrel actor:
%
% - oak_pid: records the PID of the tree where this squirrel lives
%
% - lifespan: is the nature longevity of a squirrel. In this test case, it is
% defined as a static value: 208 weeks
%
% - state: can be
%
%  - beNursed: if it is a newborn
%  - available
%  - gestation: if the female squirrel is waiting for baby
%  - nursing: if the female is nursing its offsprings
%
construct( State, ?wooper_construct_parameters ) ->

	% Firstly, the mother class
	DwellerState = class_ForestDweller:construct( State, ActorSettings, 
										SquirrelName, GivenAge, ForestPid ),

	% For an initial created squirrel, a default age is given to make difference
	% between the initially created squirrels.
	%
	% For the squirrel created during the simulation, the given age is 0 and age
	% will change over simulation time.
	%
	% And the lifespan of a squirrel is defined as 4 years, i.e. 208 weeks.
	%
	{ InitialSquirrelState, AvailableTick } = case GivenAge of

		0 ->
			{ weak, 10 };

		_Others ->
			{ available, 0 }

		end,

	setAttributes( DwellerState, [

		{gender,undefined},
		{oak_pid,undefined},
		{lifespan,208},
		% all newborn squirrel must be nursed for 10 weeks
		{be_nursed_period,10},
		% at squirrel actor creation, the termination_tick_offset is initiated
		% as its lifespan, anyway, it can be modified during the simulation
		{termination_tick_offset,208},
		{termination_waiting_ticks,3},
		{state, InitialSquirrelState},
		{available_tick, AvailableTick},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

	] ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% Called by a relative oak for notifying this squirrel that it is removed from
% the oak.
%
% When recieiving this message, the squirrel asks its forest for a relocation.
%
% (actor oneway)
%
-spec beMoved( wooper_state(), pid() ) -> class_Actor:actor_oneway_return().
beMoved( State, SenderPid ) ->

	NewState = case ?getAttr(oak_pid) of

		undefined ->
			?info_fmt( "I receive a beMoved from ~w, but it is not my oak.",
					  [ SenderPid ] ),
			State;

		% Note the matching to this already-bound variable:
		SenderPid ->

			?info( "I am moved from my oak, I have no home and I am requiring "
				"to be relocated." ),

			UpdatedTargetPeers = lists:delete( SenderPid,
											  ?getAttr(target_peers) ),

			NState = setAttribute( State, target_peers, UpdatedTargetPeers ),

			class_Actor:send_actor_message( ?getAttr(forest_pid),
				requiredLocation, NState );

		_OtherPid ->
			?info_fmt( "I receive a beMoved from ~w, but it is not my oak.",
					  [ SenderPid ] ),
			State

	end,

	?wooper_return_state_only( NewState ).



% Called by the forest for informing new oak pid
%
% (actor oneway)
%
-spec beAllocated( wooper_state(), pid(), pid() ) ->
						 class_Actor:actor_oneway_return().
beAllocated( State, OakPid, _SenderPid ) ->

	TargetPeers = ?getAttr(target_peers),

	UpdatedState = case OakPid of

		undefined ->

				?info( "I am moved from my tree and I am homeless." ),

				NState = setAttributes( State, [

						{oak_pid,undefined},
						{target_peers,TargetPeers}

					]),
				executeOneway( NState, notifyTermination );

		_ ->
				?info_fmt( "I am relocated to ~w.", [ OakPid ] ),
				setAttributes( State, [

					{oak_pid,OakPid},
					{target_peers,[ OakPid | TargetPeers ]}

									   ] )

	end,

	?wooper_return_state_only( UpdatedState ).



% Is requested to delete a specified squirrel pid from the target peers.
%
% An updated state is returned
%
% (actor oneway)
%
-spec deleteFromPeers( wooper_state(), pid() ) ->
							 class_Actor:actor_oneway_return().
deleteFromPeers( State, SenderPid ) ->

	?info_fmt( "~w is deleted from the target peers of ~w.",
			   [ SenderPid, self() ] ),

	TargetPeers = ?getAttr(target_peers),
	UpdatedList = lists:delete( SenderPid, TargetPeers ),

	?wooper_return_state_only(
	  setAttribute( State, target_peers, UpdatedList ) ).



% Message received from the forest.
%
% (actor oneway)
%
-spec forestDestroyed( wooper_state(), pid() ) ->
							 class_Actor:actor_oneway_return().
forestDestroyed( State, SenderPid )->

	?info_fmt( "~w ~w will terminate because of destroyed forest.",
			[ self(), ?getAttr(name) ] ),

	TargetPeers = ?getAttr(target_peers),
	UpdatedList = lists:delete( SenderPid, TargetPeers ),
	NewState = setAttributes( State, [

									 {forest_pid,undefined},
									 {target_peers,UpdatedList}

									  ] ),

	executeOneway( NewState, notifyTermination ).



% The squirrel actor informs all pid in its target_peers about its termination.
%
% This message is sent by the squirrel itself.
%
% (actor oneway)
%
-spec notifyTermination( wooper_state() ) -> class_Actor:actor_oneway_return().
notifyTermination( State ) ->

	% Source and target peers must be notified here, otherwise, next time they
	% will send a message to this actor, they will hang forever:
	%
	% (this returns a new state)

	CurrentOffset = ?getAttr(current_tick_offset),

	?info_fmt( "I inform my relative actors for my termination at tick #~B.",
			  [ CurrentOffset ] ),

	NewState = case ?getAttr(target_peers) of

		[] ->
			State;

		TargetPeers ->

			SendFun = fun( TargetPid, FunState ) ->

				%Returns an updated state:
				class_Actor:send_actor_message( TargetPid,
						deleteFromPeers, FunState )
			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, TargetPeers )

		end,

	executeOneway( NewState, prepareTermination ).



% Called when is_registered is false.
%
% The actor sends a addInPeers message when the forest PID exists and then an
% updated state is returned; otherwise, the original state is returned
%
% (actor oneway)
%
-spec tryToRegister( wooper_state(), class_name(), pid() ) ->
						   class_Actor:actor_oneway_return().
tryToRegister( State, ClassName, _SenderPid ) ->

	UpdatedState = case ?getAttr(forest_pid) of

		undefined ->
			State;

		ForestPid  ->

			NewState = class_Actor:send_actor_message( ForestPid,
								{ addInPeers, ClassName }, State ),

			TargetPeers = ?getAttr(target_peers),

			UpdatedTargetPeers = [ ForestPid | TargetPeers ],

			PeerState = setAttributes( NewState, [

						   {is_registered,true},
						   {target_peers,UpdatedTargetPeers}

						  ] ),

			executeOneway( PeerState, scheduleNextSpontaneousTick )

	end,

	?wooper_return_state_only( UpdatedState ).
