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



% Class modelling a waste (garbage) truck.
%
-module(class_WasteTruck).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_WasteTransport ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, TruckName,
		 InitialLocation, MaxTransportedMass, MaxTransportedVolume,
		 SupportedWasteStates ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, destruct/1 ).



% Member method declarations:
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 notifyNoLoadedWaste/2, notifyLoadedWaste/5,
		 notifyNoUnloadedWaste/2, notifyUnloadedWaste/5,
		 notifyEntryOutcome/4, notifyPlannedRoadExit/3, notifyRoadExit/3,
		 toString/1 ).



% Static method declarations:
-define( wooper_static_method_export, generate_definitions/2 ).



% For all shared defines and types:
-include("city_example_types.hrl").


% Available for debugging purposes:
-export([ display/2, monitor/1]).



% Design notes:


% About pathfinding:
%
% A path is an ordered list of POIs (with none of the intermediate roads), from
% the first to be reached to the last.
%
% Currently the pathfinding is implemented as a very simplistic search,
% breadth-first, on the road network. Later it may be A* pathfinding, if a
% target location was to be determined externally (for example by a waste
% operating center).



% About truck's intents:
%
% For more genericity we use a queue of intents (even if currently a list could
% have been at least as useful); as long as there are stored intents, we try to
% enforce them. If the queue is empty, then we apply the default built-in
% strategy. When in a container, there is not much ambiguity about the action to
% be performed: only the incinerator is capable of both loading and
% unloading. If the truck is empty, then it can only load bottom ash; if it is
% not empty, then if it already has bottom ash, it will try to load more,
% otherwise it will try to unload its cargo. That's it!
%
% Loading and unloading are not symetric: unloading will always be attempted
% first, i.e. a non-empty truck will always try to unload and, on failure, will
% attempt to load before leaving.
%
% As the truck decides of its local actions based on the type of the POI it is
% in, it must keep a (simple) memory of what it did previously (otherwise we
% would for example attempt to unload waste each time it has to make a decision,
% or it would never leave a POI). This memory is kept thanks to intents as well.
%
% As a result, the truck's intent can be modelled as a finite state machine:
%
% - if having recorded actions, applies the first entered
%
% - otherwise, if idle stays idle
%
% - otherwise, if possible, tries to unload, then (whatever the outcome) tries
% to load
%
% - leaves the POI once having attempted to load (whatever the outcome)
%
% So the general action path is to try to: unload then load then leave.
%
% The design is simplified by:
%
% - clearly separating the decisions from their applications (actions)
%
% - having only short-term, local goals; for example the future is never plan in
% full details, we start simply for relevant destinations without further
% planification, and on the path we see whether opportunistic operations can be
% attempted (no micro-management)



% Type section.


% Intent of a waste truck:
%
-type intent() ::   'attempt_loading'
				  | 'attempt_unloading'
				  | { 'drive', path() }
				  | 'be_opportunistic'
				  | 'idle'.



% Type of the container a waste truck is in, to help determining what it is to
% do (ex: load more, unload, etc.):
%
-type container_type() :: 'undefined' | poi_type().


-export_type([ intent/0, container_type/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.GarbageTruck").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Implementation notes:
%
% For breadth-first pathfinding, we could have used digraph:get_short_path/3,
% except that we never rely on an explicit graph, and need to have more code
% flexibility (ex: to pass opaque user-defined data to the predicate and
% feeder).




% The class-specific attributes of a waste truck are:
%
% - intents :: queue() (actually: queue( intent() )) corresponds to a (FIFO)
% queue of the ordered intents this trucks is to fulfill
%
% - container_type :: container_type() describes what is the type of the
% geo-container this truck is in (if any)




% Constructs a new waste truck, from following parameters:
%
% - Location :: class_GIS:location() is the (initial) location of this waste
% truck, expected here to be a point of interest
%
% - MaxTransportedMass :: unit_utils:tons() is the maximum transported mass
%
% - MaxTransportedVolume :: unit_utils:cubic_meters() is the maximum
% transported volume
%
% - SupportedWasteStates :: [ supported_waste_state() ] is the list of the waste
% states this truck can support
%
% A waste truck is created empty.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:location(),
				 unit_utils:tons(), unit_utils:cubic_meters(),
				 [ class_WasteTransport:supported_waste_state() ] ) ->
					   wooper:state().
construct( State, ActorSettings, TruckName, InitialLocation,
		   MaxTransportedMass, MaxTransportedVolume, SupportedWasteStates )
  when is_pid( InitialLocation ) ->

	TransportState = class_WasteTransport:construct( State, ActorSettings,
						 TruckName, InitialLocation, MaxTransportedVolume,
						 MaxTransportedMass, SupportedWasteStates ),

	% As a container (ex: a landfill) cannot change of type, we can use a mere
	% request here:
	%
	InitialLocation ! { getClassName, [], self() },
	ContainerClassname = receive

		{ wooper_result, Classname } ->
			Classname

	end,

	setAttributes( TransportState, [

			{ intents, queue:new() },
			{ container_type, ContainerClassname },
			{ local_tracker_pid,
			  class_InstanceTracker:get_local_tracker() },
			{ trace_categorization,
			  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

							  ] ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%display( "end", State ),

	State.



display( Prefix, State ) ->

	Tank = ?getAttr(tank),

	AAI = class_Actor:get_abstract_identifier( State ),

	io:format( "Truck ~s: AAI=~p, name=~s, random=~p, "
			   "mass=~p, volume=~p, type=~p~n",
			   [ Prefix, AAI, ?getAttr(name),
				 random_utils:get_random_state(),
				 Tank#waste_tank.current_mass_stored,
				 Tank#waste_tank.current_volume_stored,
				 Tank#waste_tank.current_type ] ).



monitor( State ) ->
	display( io_lib:format( "at #~B",
				[ class_Actor:get_current_tick_offset( State ) ] ),
			 State ).



% Section for member methods.



% First scheduling of a waste truck.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% A cheap way of checking that the simulation is started in a reproducible
	% manner:
	%
	%display( "start", State ),

	?info_fmt( "Waste truck just created: ~s", [ to_string( State ) ] ),

	% Initiates the probe recording (empty before loading):
	ProbeState = executeOneway( State, updateProbe ),

	% A truck starts empty and with no specific intent; it is to start only at
	% the next tick, as roads use the first tick to update their connectivity
	% with POIs:
	%
	ScheduledState = executeOneway( ProbeState, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% The definition of the spontaneous behaviour of this truck.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	%monitor( State ),

	NewState = apply_behaviour( State ),

	?wooper_return_state_only( NewState ).




% Notifications for a loading attempt.



% Notification sent back by the waste loading point this truck sent a loadWaste
% actor message to.
%
% (actor oneway)
%
-spec notifyNoLoadedWaste( wooper:state(), pid() ) ->
								 class_Actor:actor_oneway_return().
notifyNoLoadedWaste( State, WasteLoadingPointPid ) ->

	%monitor( State ),

	?debug_fmt( "Truck notified that no waste could be loaded "
				"from loading point ~w (state: ~s).~n",
				[ WasteLoadingPointPid, to_string( State ) ] ),

	% Loading failed, what to do next?

	% The situation is not symetric: failing to unload leads to trying to load
	% (if it makes sense), while failing to load (like here) does not trigger an
	% unloading attempt (otherwise this would never terminate).

	IntentState = case queue:is_empty( ?getAttr(intents) ) of

		true ->

			Intents = case is_empty( State ) of

				true ->
					% Let's try to find a new loading point then (no choice):
					plan_next_loading( State );

				_NotEmpty ->
					% Let's try to find a new unloading point then for the
					% already stored waste:
					plan_next_unloading( State )

			end,

			apply_intents( Intents, State );

		false ->
			% Going directly to next intent then (no loading took place, hence
			% no waiting here):
			apply_behaviour( State )

	end,

	?wooper_return_state_only( IntentState ).



% Notification sent back by the waste loading point to which this truck sent a
% loadWaste actor message.
%
% (actor oneway)
%
-spec notifyLoadedWaste( wooper:state(), unit_utils:tons(), waste_type(),
			class_TimeManager:tick_offset(), loading_point_pid() ) ->
							   class_Actor:actor_oneway_return().
notifyLoadedWaste( State, TakenMass, LoadedWasteType, LoadingTickCount,
				   WasteLoadingPointPid ) ->

	%monitor( State ),

	?trace_fmt( "Truck notified that ~f tons of waste of type ~p could be "
				"loaded from loading point ~w. The loading is to last "
				"for ~B ticks.~n",
				[ TakenMass, LoadedWasteType, WasteLoadingPointPid,
				  LoadingTickCount ] ),

	% Here we have both to account for that action and prepare for after.

	% First, plan the waiting for that unloading:
	CurrentTypeOffset = ?getAttr(current_tick_offset),

	NextSpontaneousTick = CurrentTypeOffset + LoadingTickCount,

	PlannedState = class_Actor:add_spontaneous_tick( NextSpontaneousTick,
													 State ),

	% And apply by anticipation its consequences:
	UpdatedTank = waste_utils:add_waste_to_tank( ?getAttr(tank),
								TakenMass, LoadedWasteType ),

	TankState = setAttribute( PlannedState, tank, UpdatedTank ),


	% Then prepare for the next action (we record rather than apply it, as we
	% must wait the planned spontaneous tick):

	IntentState = case queue:is_empty( ?getAttr(intents) ) of

		true ->

			% No intent, let's determine next one, then.

			% If the tank is full, then we have to unload it; if not, on average
			% (probability: 2/3) we prefer unloading it as well (with a 1/3
			% probability we will try to load it more):
			NextIntent = case waste_utils:is_tank_full( UpdatedTank ) of

				true ->
					plan_next_unloading( TankState );

				false ->

					case waste_utils:is_tank_empty( UpdatedTank ) of

						true ->
							plan_next_loading( TankState );

						false ->
							% Both options can be taken, so:
							case class_RandomManager:get_uniform_value( 3 ) of

								1 ->
									plan_next_loading( TankState );

								_TwoOrThree ->
									plan_next_unloading( TankState )

							end

					end

			end,

			% We just record the intent, it will be applied at next spontaneous
			% tick, after the loading duration:
			record_intents( NextIntent, TankState );

		false ->
			% The planned spontaneous tick will thus have an intent to manage:
			TankState

	end,

	WasteLoadingPointPid ! { toString, [], self() },
	String = receive

				 { wooper_result, Desc } ->
					 Desc

	end,

	ProbeState= executeOneway( IntentState, updateProbe ),

	?debug_fmt( "Truck after loading completion from ~s (~w): located in ~s.",
				[ to_string( ProbeState ), WasteLoadingPointPid, String ] ),

	?wooper_return_state_only( ProbeState ).





% Notifications for an unloading attempt.



% Notification sent back by the waste unloading point this truck sent a
% unloadWaste actor message to: no waste could be unloaded.
%
% (actor oneway)
%
-spec notifyNoUnloadedWaste( wooper:state(), pid() ) ->
								 class_Actor:actor_oneway_return().
notifyNoUnloadedWaste( State, WasteUnloadingPointPid ) ->

	%monitor( State ),

	?debug_fmt( "Truck notified that no waste could be unloaded "
				"from unloading point ~w (state: ~s).~n",
				[ WasteUnloadingPointPid, to_string( State ) ] ),

	% Normally, having tried to unload and failed implies that the truck is
	% still not empty:
	case is_empty( State ) of

		true ->
			throw( should_not_be_empty );

		_ ->
			ok

	end,

	IntentState = case queue:is_empty( ?getAttr(intents) ) of

		true ->

			% If the POI is loadable, we can try to load the truck more instead:
			PoiType = ?getAttr(container_type),

			NextIntent = case waste_utils:is_poi_type_loadable( PoiType ) of

				true ->
					% Might be possible, so we will try to load instead:
					attempt_loading;

				false ->
					% We leave, preferably in order to *unload* more:
					plan_next_unloading( State )

			end,

			% No unloading took place, hence no waiting here:
			apply_intents( NextIntent, State );

		false ->
			% Going directly to next intent then (no unloading took place, hence
			% no waiting here):
			apply_behaviour( State )

	end,

	?wooper_return_state_only( IntentState ).



% Notification sent back by the waste unloading point to which this truck sent a
% unloadWaste actor message: at least part of the waste could be unloaded.
%
% Note: the waste type is specified only for checking purposes.
%
% (actor oneway)
%
-spec notifyUnloadedWaste( wooper:state(), unit_utils:tons(), waste_type(),
			class_TimeManager:tick_offset(), loading_point_pid() ) ->
							   class_Actor:actor_oneway_return().
notifyUnloadedWaste( State, GivenMass, UnloadedWasteType, UnloadingTickCount,
				   WasteUnloadingPointPid ) ->

	%monitor( State ),

	?info_fmt( "Truck notified that ~f tons of waste could be unloaded "
			   "to unloading point ~w. The unloading is to last "
			   "for ~B ticks.~n",
			   [ GivenMass, WasteUnloadingPointPid, UnloadingTickCount ] ),

	% Here we have both to account for that action and prepare for after.

	% First, plan the waiting for that unloading:
	CurrentTypeOffset = ?getAttr(current_tick_offset),

	NextSpontaneousTick = CurrentTypeOffset + UnloadingTickCount,

	PlannedState = class_Actor:add_spontaneous_tick( NextSpontaneousTick,
													 State ),

	% And apply by anticipation its consequences:
	UpdatedTank = waste_utils:remove_waste_from_tank( ?getAttr(tank),
							GivenMass, UnloadedWasteType ),

	TankState = setAttribute( PlannedState, tank, UpdatedTank ),


	% Then prepare for the next action, if needed:
	IntentState = case queue:is_empty( ?getAttr(intents) ) of

		true ->

			% If the tank is empty, then it might be able to load other kinds of
			% waste from there; otherwise, it will have to unload the remaining
			% waste elsewhere:
			NextIntents = case waste_utils:is_tank_empty( UpdatedTank ) of

							 true ->
								 attempt_loading;

							 false ->
								 plan_next_unloading( TankState )

			end,

			% We just record the intent, it will be applied at next spontaneous
			% tick, after the unloading duration:
			record_intents( NextIntents, TankState );

		false ->
			% Will thus already have an intent at the next spontaneous tick:
			TankState

	end,

	WasteUnloadingPointPid ! { toString, [], self() },
	String = receive

				 { wooper_result, Desc } ->
					 Desc

	end,

	ProbeState = executeOneway( IntentState, updateProbe ),

	?debug_fmt( "Truck after unloading completion from ~s (~w): located in ~s.",
				[ to_string( ProbeState ), WasteUnloadingPointPid, String  ] ),

	?wooper_return_state_only( ProbeState ).



% Notification sent back by the POI this truck sent a requestEntry to.
%
% (actor oneway)
%
-spec notifyEntryOutcome( wooper:state(), entry_outcome(), poi_type(),
						 poi_pid() ) -> class_Actor:actor_oneway_return().
notifyEntryOutcome( State, _OutCome=entered, PoiType, PoiPid ) ->

	?debug_fmt( "Entered into POI ~w.", [ PoiPid ] ),

	%monitor( State ),

	POIState = setAttribute( State, container_type, PoiType ),

	EnterState = executeOnewayWith( POIState, class_GeolocalizedElement,
									setLocation, PoiPid ),

	% We may be in an intermediate POI, or we may have arrived:
	NewState = case queue:out( ?getAttr(intents) ) of

		% Here we just arrived, a bit of intent bookkeeping (no more driving):
		{ { value, { drive, _Path=[] } }, PoppedQueue } ->

			apply_behaviour( setAttribute( EnterState, intents, PoppedQueue ) );


		% Here the truck is still in transit:
		{ { value, { drive, NextPath=[ NextPOI | _OtherPOIs ] } },
		  PoppedQueue } ->

			% We might take advantage of this intermediate POI (ex: loading more
			% while on an unloading path), then progress on the path:
			UpdatedIntents = [ be_opportunistic, { drive, NextPath } ],

			% We want to find a road to NextPOI:
			?debug_fmt( "Entered in intermediate POI ~w (type: ~s), "
						"examining opportunistic actions before taking road "
						"to next POI ~w.", [ PoiPid, PoiType, NextPOI ] ),

			apply_intents( UpdatedIntents,
					setAttribute( EnterState, intents, PoppedQueue ) );

		_ ->
			% No intent left, applying default behaviour then:
			apply_behaviour( EnterState )


	end,

	?wooper_return_state_only( NewState ).




% Notification sent by a road to this truck once its origin POI (the one to
% which the truck sent a takeRoadTo actor oneway) sent to the road a driveIn
% call, in order to forecast the time at which it is to leave that road.
%
% (actor oneway)
%
-spec notifyPlannedRoadExit( wooper:state(), class_TimeManager:tick_offset(),
							road_pid() ) -> class_Actor:actor_oneway_return().
notifyPlannedRoadExit( State, PlannedDepartureOffset, RoadPid ) ->

	%monitor( State ),

	?debug_fmt( "Truck planning to exit road ~p at tick #~B.",
				[ RoadPid, PlannedDepartureOffset ] ),

	?wooper_return_state_only( State ).



% Notification sent by a road to this truck once its origin POI (the one to
% which the truck sent a takeRoadTo actor oneway) sent to the road a driveIn
% call.
%
% (actor oneway)
%
-spec notifyRoadExit( wooper:state(), poi_pid(), road_pid() ) ->
					class_Actor:actor_oneway_return().
notifyRoadExit( State, ReachedPOI, RoadPid ) ->

	%monitor( State ),

	?debug_fmt( "Exiting road ~p, arriving at POI ~p, requesting entry.",
				[ RoadPid, ReachedPOI ] ),

	SentState = class_Actor:send_actor_message( ReachedPOI, requestEntry,
												State ),

	?wooper_return_state_only( SentState ).




% Returns a string describing the state of this instance.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).





% Section for static methods.



% Generates a list of instance definitions for the specified number of
% initial waste trucks.
%
-spec generate_definitions( basic_utils:count(),
							[ pid() ] | [ instance_loading:id_ref() ] ) ->
								  [ class_Actor:instance_creation_spec() ].
generate_definitions( WasteTruckCount, AllPOIS ) ->

	POICount = length( AllPOIS ),

	generate_definitions( WasteTruckCount, AllPOIS, POICount, _Acc=[] ).



generate_definitions( _WasteTruckCount=0, _AllPOIS, _POICount, Acc ) ->
	Acc;

generate_definitions( WasteTruckCount, AllPOIS, POICount, Acc ) ->

	TruckName = text_utils:format( "WasteTruck-~B", [ WasteTruckCount ] ),

	% All trucks start in a (random) POI (that can be chosen multiple times):
	DrawnPOI = list_utils:draw_element( AllPOIS, POICount ),

	% Only one kind of garbage truck:

	MaxTransportedMass = 11.0,
	MaxTransportedVolume = 20.0,

	% Supposing all physical states are supported by this truck:
	SupportedWasteStates = [ solid, liquid, gaseous ],

	NewTruckDef = { class_WasteTruck, [ TruckName,
		 _InitialLocation=DrawnPOI, MaxTransportedMass, MaxTransportedVolume,
		 SupportedWasteStates ] },

	generate_definitions( WasteTruckCount - 1, AllPOIS, POICount,
						  [ NewTruckDef | Acc ] ).






% Helper section.



% Records (not apply) specified additional intent(s), which will the next one(s)
% to be executed (hence: not the last ones), in their specified order.
%
% Returns an updated state.
%
% (helper)
%
-spec record_intents( intent() | [ intent() ], wooper:state() ) ->
							wooper:state().
record_intents( NextIntents, State ) when is_list(NextIntents) ->

	% Probably clearer than using from_list and join:
	NewQueue = lists:foldl( fun( I, Q ) -> queue:in_r( I, Q ) end,
				 _Acc0=?getAttr(intents),
				 _List=lists:reverse( NextIntents ) ),

	setAttribute( State, intents, NewQueue );

record_intents( NextIntent, State ) ->
	record_intents( [ NextIntent ], State ).



% Records and applies specified intent.
%
% Returns an updated state.
%
% (helper)
%
-spec apply_intents( intent() | [ intent() ], wooper:state() ) ->
						   wooper:state().
apply_intents( NextIntents, State ) ->

	RecordedState = record_intents( NextIntents, State ),

	apply_behaviour( RecordedState ).



% The place where the truck acts for good.
%
% Returns an updated state.
%
% (helper)
%
-spec apply_behaviour( wooper:state() ) -> wooper:state().
apply_behaviour( State ) ->

	%io:format( "apply behaviour for intents ~p.~n",
	%		  [ queue:to_list(?getAttr(intents)) ] ),

	case queue:out( ?getAttr(intents) ) of

		{ empty, _Q } ->
			apply_default_behaviour( State );

		{ { value, Intent }, PoppedQueue } ->

			IntentState = setAttribute( State, intents, PoppedQueue ),

			case Intent of

				be_opportunistic ->
					?trace( "Truck looking for transit opportunities." ),

					% The default behaviour is perfectly fine, recurses as
					% wanted:
					apply_opportunistic_behaviour( IntentState );

				idle ->
					% In theory, idle is a sink state, but here we act as if it
					% was not the case ('idle' is consumed):
					?trace( "Truck idle." ),
					IntentState;

				attempt_loading ->
					attempt_loading( IntentState );

				attempt_unloading ->
					attempt_unloading( IntentState );

				{ drive, Path } ->
					drive( Path, IntentState )

			end

	end.




% Requests this truck to start towards the next loading point.
%
% Returns the corresponding next intent.
%
% (helper)
%
-spec plan_next_loading( wooper:state() ) -> intent().
plan_next_loading( State ) ->

	% Checking that cannot be already full:
	case is_full( State ) of

		true ->
			throw( full_truck_cannot_be_loaded );

		_NotFull ->
			ok

	end,

	% Thinking to adding new compatible waste:
	case find_path_to_loading_point( State ) of

		no_path_found ->

			%?warning( "Truck did not find a loading point." ),

			% Cannot load more; maybe then unloading could make sense instead?
			case is_empty( State ) of

				true ->
					% Nothing to unload either, we will then stay idle:
					?info_fmt( "Truck ~s, empty and unable to load, "
							   "is to remain idle from now on.",
							   [ to_string( State ) ] ),
					idle;

				_NotEmpty ->

					% Let's unload then instead:
					?debug_fmt( "Truck ~s could not find a path to load more, "
								"thus will try to unload.",
								[ to_string( State ) ] ),

					case find_path_to_unloading_point( State ) of

						no_path_found ->
							?info_fmt( "Truck ~s, not able to load, "
									   "not empty yet unable to unload, "
									   "is to remain idle from now on.",
									   [ to_string( State ) ] ),

							idle;

						_UnLoadPath=[ _CurrentPOI | NextPOIs ] ->

							?debug_fmt( "Truck going to unload through ~p, "
										"short of being able to load.",
										[ NextPOIs ] ),

							[ { drive, NextPOIs }, attempt_unloading ]

					end

			end;


		_LoadPath=[ _CurrentPOI | NextPOIs ] ->

			?debug_fmt( "Truck intends to load waste, "
						"going through POI path ~w.", [ NextPOIs ] ),
			[ { drive, NextPOIs }, attempt_loading ]

	end.



% Requests this truck to start towards the next unloading point.
%
% Returns the corresponding next intent.
%
% (helper)
%
-spec plan_next_unloading( wooper:state() ) -> intent().
plan_next_unloading( State ) ->

	% Checking that cannot be empty:
	case is_empty( State ) of

		true ->
			throw( empty_truck_cannot_be_unloaded );

		_NotEmpty ->
			ok

	end,

	% Thinking to finding a suitable unloading place:
	case find_path_to_unloading_point( State ) of

		no_path_found ->

			% No unloading possible; maybe then an additional loading could make
			% sense instead?
			case is_full( State ) of

				true ->
					% No room to load more, we will then stay idle:
					?info_fmt( "Truck ~s, full and unable to unload, "
							   "is to remain idle from now on.",
							   [ to_string( State ) ] ),
					idle;

				_NotFull ->

					% Let's try to load then instead:
					?debug_fmt( "Truck ~s could not find a path to unload, "
								"thus will try to load.",
								[ to_string( State ) ] ),

					case find_path_to_loading_point( State ) of

						no_path_found ->
							?info_fmt( "Truck ~s, not able to unload, "
									   "not full yet unable to load, "
									   "is to remain idle from now on.",
									   [ to_string( State ) ] ),

							idle;

						_LoadPath=[ _CurrentPOI | NextPOIs ] ->

							?debug_fmt( "Truck going to load through ~p "
										"short of being able to unload.",
										[ NextPOIs ] ),

							[ { drive, NextPOIs }, attempt_loading ]

					end

			end;

		_LoadPath=[ _CurrentPOI | NextPOIs ] ->

			?debug_fmt( "Truck intends to unload waste, "
						"going through POI path ~w.", [ NextPOIs ] ),

			[ { drive, NextPOIs }, attempt_unloading ]

	end.



% Performs an attempt of local waste loading.
%
% (helper)
%
-spec attempt_loading( wooper:state() ) -> wooper:state().
attempt_loading( State ) ->

	PoiType = ?getAttr(container_type),
	PoiPid = ?getAttr(location),

	case waste_utils:is_poi_type_loadable( PoiType ) of

		true ->

			case is_full( State ) of

				true ->
					?debug_fmt( "Truck's intent is to load, it is "
								"stopped at a loadable point (~w), "
								"however it is already full, skipping "
								"this intent.", [ PoiPid ] ),
					apply_behaviour( State );

				_NotFull ->
					?debug_fmt( "Truck's intent is to load, "
								"it is not full and it is stopped "
								"at a loadable point (~w), trying to "
								"load any waste from it.", [ PoiPid ] ),

					FreeMassMargin = get_remaining_free_mass( State ),

					RequestedWasteType = case is_empty( State ) of

							true ->
								% Any type will do:
								none;

							{ TruckWasteType, _CurrentMass } ->
								TruckWasteType

					end,

					class_Actor:send_actor_message( PoiPid,
						  { loadWaste, [ RequestedWasteType, FreeMassMargin ] },
													State )

			end;

		false ->
			?debug_fmt( "Truck's intent is to load, however it is "
						"stopped at a POI (~w) that is not loadable, "
						"skipping this intent.", [ PoiPid ] ),
			apply_behaviour( State )

	end.




% Performs an attempt of local waste unloading.
%
% (helper)
%
-spec attempt_unloading( wooper:state() ) -> wooper:state().
attempt_unloading( State ) ->

	PoiType = ?getAttr(container_type),
	PoiPid = ?getAttr(location),

	case waste_utils:is_poi_type_unloadable( PoiType ) of

		true ->

			case is_empty( State ) of

				true ->
					?debug_fmt( "Truck's intent is to unload, it is "
								"stopped at a unloadable point (~w), "
								"however it is empty, skipping this "
								"intent.", [ PoiPid ] ),
					apply_behaviour( State );


				% Not empty:
				{ TruckWasteType, CurrentMass } ->
					?debug_fmt( "Truck's intent is to unload, "
								"it is not empty and it is stopped "
								"at a unloadable point (~w), trying to "
								"unload waste from it.", [ PoiPid ] ),

					class_Actor:send_actor_message( PoiPid,
					  { unloadWaste, [ TruckWasteType, CurrentMass ] }, State )

			end;

		false ->
			?debug_fmt( "Truck's intent is to unload, however it is "
						"stopped at a POI (~w) that is not unloadable, "
						"skipping this intent.", [ PoiPid ] ),
			apply_behaviour( State )

	end.




% Drives the truck through specified path.
%
% (helper)
%
-spec drive( path(), wooper:state() ) -> wooper:state().
drive( _Path=[ NextPOI | RestOfPath ], State ) ->

	?debug_fmt( "Taking road to the next POI ~p in path.", [ NextPOI ] ),

	SentState = class_Actor:send_actor_message( ?getAttr(location),
						   { takeRoadTo, NextPOI }, State ),

	% We just record (not apply), as the road will notify "proactively" this
	% truck when it will reach its endpoint:
	%
	record_intents( { drive, RestOfPath }, SentState ).



% Applies the defaut behaviour for this truck, regardless of any intent.
%
% Returns an updated state.
%
% (helper)
%
-spec apply_default_behaviour( wooper:state() ) -> wooper:state().
apply_default_behaviour( State ) ->

	PoiType = ?getAttr(container_type),
	PoiPid = ?getAttr(location),

	?debug_fmt( "Truck applying default behaviour in POI ~w (of type ~s).~n",
			   [ PoiPid, PoiType ] ),

	% What to do next? It depends on the type of the containing POI, and on the
	% current cargo:
	%
	case is_empty( State ) of

		true ->

			% So we will try to load (any kind of waste) there:
			case waste_utils:is_poi_type_loadable( PoiType ) of

				true ->

					?debug_fmt( "Truck empty and stopped at a loadable point "
								"(~w), trying to load any waste from it.",
								[ PoiPid ] ),

					FreeMassMargin = get_remaining_free_mass( State ),
					class_Actor:send_actor_message( PoiPid,
						  { loadWaste, [ _WasteType=none, FreeMassMargin ] },
						  State );

				false ->
					?debug_fmt( "Truck empty yet stopped at a POI (~w) that "
								"is not loadable (~s).",
								[  PoiPid, PoiType ] ),

					% Let's try to find a new loading point then (no choice):
					apply_intents( plan_next_loading( State ), State )

			end;

		% Not empty:
		{ TruckWasteType, CurrentMass } ->

			case waste_utils:is_poi_type_unloadable( PoiType ) of

				true ->
					?debug_fmt( "Truck not empty and stopped at an "
								"unloadable point (~w), trying to unload "
								"waste to it.", [ PoiPid ] ),

					class_Actor:send_actor_message( PoiPid,
						  { unloadWaste, [ TruckWasteType, CurrentMass ] },
						  State );

				false ->

					case waste_utils:is_poi_type_loadable( PoiType ) of

						true ->

							?debug_fmt( "Truck not empty, cannot unload in "
										"current POI (~w) of type ~s, but "
										"will attempt to load more waste.",
									   [ PoiPid, PoiType ] ),

							FreeMassMargin = get_remaining_free_mass( State ),

							class_Actor:send_actor_message( PoiPid,
							  { loadWaste, [ TruckWasteType, FreeMassMargin ] },
							  State );

						false ->
							?debug_fmt( "Truck not empty and stopped at "
										"a POI (~w) that is neither loadable "
										"nor unloadable (~s).",
										[  PoiPid, PoiType ] ),

							apply_intents( plan_next_unloading( State ), State )

					end

			end

	end.




% Applies the opportunistic behaviour for this truck, regardless of any intent.
%
% Returns an updated state.
%
% (helper)
%
-spec apply_opportunistic_behaviour( wooper:state() ) -> wooper:state().
apply_opportunistic_behaviour( State ) ->

	PoiType = ?getAttr(container_type),
	PoiPid = ?getAttr(location),

	?debug_fmt( "Truck applying default behaviour in POI ~w (of type ~s).~n",
			   [ PoiPid, PoiType ] ),

	% What to do next? It depends on the type of the containing POI, and on the
	% current cargo:
	%
	case is_empty( State ) of

		true ->

			% So we will try to load (any kind of waste) there:
			case waste_utils:is_poi_type_loadable( PoiType ) of

				true ->

					?debug_fmt( "Truck empty and stopped at a loadable point "
								"(~w), trying to load any waste from it.",
								[ PoiPid ] ),

					FreeMassMargin = get_remaining_free_mass( State ),
					class_Actor:send_actor_message( PoiPid,
						  { loadWaste, [ _WasteType=none, FreeMassMargin ] },
						  State );

				false ->
					% This is a normal case (ex: if being opportunistic in
					% transit).
					?debug_fmt( "Truck empty yet stopped at a POI (~w) that "
								"is not loadable (~s).",
								[  PoiPid, PoiType ] ),
					apply_behaviour( State )

			end;

		% Not empty:
		{ TruckWasteType, CurrentMass } ->

			case waste_utils:is_poi_type_unloadable( PoiType ) of

				true ->
					?debug_fmt( "Truck not empty and stopped at an "
								"unloadable point (~w), trying to unload "
								"waste to it.", [ PoiPid ] ),

					class_Actor:send_actor_message( PoiPid,
						  { unloadWaste, [ TruckWasteType, CurrentMass ] },
						  State );

				false ->

					case waste_utils:is_poi_type_loadable( PoiType ) of

						true ->

							?debug_fmt( "Truck not empty, cannot unload in "
										"current POI (~w) of type ~s, but "
										"will attempt to load more waste.",
									   [ PoiPid, PoiType ] ),

							FreeMassMargin = get_remaining_free_mass( State ),

							class_Actor:send_actor_message( PoiPid,
							  { loadWaste, [ TruckWasteType, FreeMassMargin ] },
							  State );

						false ->
							% This is a normal case (ex: if being opportunistic
							% in transit).
							?debug_fmt( "Truck not empty and stopped at "
										"a POI (~w) that is neither loadable "
										"nor unloadable (~s).",
										[  PoiPid, PoiType ] ),
							apply_behaviour( State )

					end

			end

	end.



% Pathfinding section.



% Returns the outbound POIs of the specified one.
%
% To be used as an higher-order feeder.
%
% (helper)
%
poi_feeder( POI, _UserData ) ->

	% Direct message licit, as graph is static:
	POI ! { getOutboundPOIs, [], self() },
	receive

		{ wooper_result, Children } ->
			Children

	end.



% Returns a path leading to a proper POI where this truck may load its current
% content, or 'no_path_found'.
%
% Note: the truck must currently already be in a POI.
%
find_path_to_loading_point( State ) ->

	% The goal is to find a loading point compatible with current waste stored
	% (if any):
	CurrentWasteType = get_waste_type( State ),

	StartPOI = ?getAttr(location),

	% Tells whether the POI in parameter is suitable for the loading of
	% specified waste type, knowing we do not want the current POI to be
	% returned; closure:
	%
	LoadablePredicate = fun( POIPid, _UserData ) ->

			case POIPid of

					StartPOI ->
						false;

					_ ->
						Res = is_poi_loading_compatible( POIPid,
														 CurrentWasteType ),

						%TrackerPid = ?getAttr(local_tracker_pid),

						%POIAAI = class_InstanceTracker:get_identifier_for(
						%		   POIPid, TrackerPid ),

						%?trace_fmt( "is poi ~B (~w) loading compatible: ~p",
						%			[ POIAAI, POIPid, Res ] ),

						Res

			end

						end,

	graph_utils:find_breadth_first( _From=StartPOI, LoadablePredicate,
									fun poi_feeder/2 ).






% Returns whether the specified POI is compatible, in terms of loading, with the
% specified waste type.
%
is_poi_loading_compatible( POIPid, WasteType ) ->

	% Another static information that can be readily accessed:
	POIPid ! { getClassName, [], self() },
	receive

		{ wooper_result, POIType } ->
			waste_utils:can_produce( POIType, WasteType )

	end.




% Returns a path leading to a proper POI where this truck may unload its current
% content, or 'no_path_found'.
%
% Note: the truck must currently already be in a POI.
%
find_path_to_unloading_point( State ) ->

	% The goal is to find an unloading point compatible with current waste
	% stored (if any):
	%
	CurrentWasteType = get_waste_type( State ),

	StartPOI = ?getAttr(location),

	% Tells whether the place in parameter is suitable for the unloading of
	% specified waste type, knowing we do not want the current POI to be
	% returned; closure:
	UnloadablePredicate = fun( POIPid, _UserData ) ->
								case POIPid of

									StartPOI ->
										false;

									_ ->
										is_poi_unloading_compatible( POIPid,
														  CurrentWasteType )

								end

						end,


	%io:format( "~w searching for unloading location for ~s, from ~w.~n",
	%		  [ self(), CurrentWasteType, StartPOI ] ),

	graph_utils:find_breadth_first( _From=StartPOI, UnloadablePredicate,
									fun poi_feeder/2 ).






% Returns whether the specified POI is compatible, in terms of unloading, with
% the specified waste type.
%
is_poi_unloading_compatible( POIPid, WasteType ) ->

	% Another static information that can be readily accessed:
	POIPid ! { getClassName, [], self() },
	receive

		{ wooper_result, POIType } ->
			waste_utils:can_consume( POIType, WasteType )

	end.




% Tells whether this truck is currently full, thinking to possibly loading it
% more: returns either true (if full), or { TypeOfCurrentWaste, FreeWasteMass }
% where TypeOfCurrentWaste is the type of the current waste in truck (if any),
% and FreeWasteMass is the margin in terms of mass until this truck will be
% full.
%
% (const helper)
%
-spec is_full( wooper:state() ) -> 'true'
				  | { waste_type(), unit_utils:tons() }.
is_full( State ) ->

	Tank = ?getAttr(tank),

	MaxMass = Tank#waste_tank.max_mass_stored,

	CurrentMass = Tank#waste_tank.current_mass_stored,

	case MaxMass - CurrentMass of

		Margin when Margin > 0 ->
			% Returning the waste type and this margin:
			{ Tank#waste_tank.current_type, Margin };

		_SupposedZero ->
			true

	end.



% Tells whether this truck is currently empty, thinking to possibly unloading it
% as much as possible: returns either true (if empty), or { TypeOfCurrentWaste,
% CurrentWasteMass } where TypeOfCurrentWaste is the type of the current waste
% in truck (if any), and CurrentWasteMass is the mass of waste that is this
% truck.
%
% (const helper)
%
-spec is_empty( wooper:state() ) -> 'true'
				  | { waste_type(), unit_utils:tons() }.
is_empty( State ) ->

	Tank = ?getAttr(tank),

	case Tank#waste_tank.current_mass_stored of

		CurrentMass when CurrentMass > 0.0 ->
			{ Tank#waste_tank.current_type, CurrentMass };

		_SupposedZero ->
			true

	end.



% Returns the current waste type (possibly 'none').
%
% (helper)
%
-spec get_waste_type( wooper:state() ) -> 'none' | waste_type().
get_waste_type( State ) ->

	Tank = ?getAttr(tank),

	Tank#waste_tank.current_type.



% Returns the current margin in terms of stored mass.
%
% (helper)
%
-spec get_remaining_free_mass( wooper:state() ) -> unit_utils:tons().
get_remaining_free_mass( State ) ->

	Tank = ?getAttr(tank),

	Tank#waste_tank.max_mass_stored - Tank#waste_tank.current_mass_stored.



% Returns a string describing the state of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	text_utils:format( "Waste truck '~s' (AAI: ~B) in location ~w, storing ~s; "
					   "current intents: ~p, having ~p for random state",
					   [ ?getAttr(name),
						 class_Actor:get_abstract_identifier( State ),
						 ?getAttr(location),
						 waste_utils:waste_tank_to_string( ?getAttr(tank ) ),
						 queue:to_list( ?getAttr(intents) ),
						 random_utils:get_random_state()
					   ] ).
