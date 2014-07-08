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



% Class modelling a road, a directed thoroughfare (unidirectional, one lane)
% linking its source point of interest to its target one.
%
-module(class_Road).



% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_Actor, class_Graphable ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Name, SourcePOI,
		TargetPOI, MaxCapacity ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, delete/1 ).



% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1, driveIn/3,
		 getSourcePOI/1, getTargetPOI/1, registerInGIS/2 ).


% Static method declarations.
-define( wooper_static_method_export, generate_definitions/1 ).


% Number of vehicle slots that a road can offer.
-type capacity() :: basic_utils:count().


-export_type([ capacity/0 ]).


% For types:
-include("city_example_types.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Traffic.Road").



% Allows to use macros for trace sending (to be included after the WOOPER
% header):
%
-include("class_TraceEmitter.hrl").



% Number of meters per vehicle slot:
-define( vehicle_slot_length, 4.0 ).


% The class-specific attributes of an instance of a road are:
%
% - source_poi :: pid() is the PID of the source POI of this road
%
% - target_poi :: pid() is the PID of the target POI of this road
%
% - length :: unit_utils:meters() id the length of this road
%
% - current_capacity :: capacity() is the current vehicle capacity of this road
%
% - max_capacity :: capacity() is the maximum vehicle capacity of this road
%
% - max_average_speed :: unit_utils:km_per_second() is the maximum (i.e. if the
% road has no previous traffic) average speed that can be reached on this road
%
% - vehicle_queue :: queue:queue() is the queue (FIFO) corresponding to this
% road: the first vehicle to enter the road (inserted at its back) is bound to
% be the first to leave it (no overtaking)





% Implementation notes:
%
% The capacity of a road is established in terms of an integer number of
% slots. A slot corresponds to a small vehicle (car or motorcycle). Some vehicle
% (ex: trucks) requires multiple slots.
%
% A road is not a container as we prefer using a queue rather than a list to
% store the vehicles.
%
% Speeds are expressed in kilometers per hour.
%
% For a road, the average speed at any time depends on its load.
%
% By default, a road "takes control" of the vehicles it conveys, as it decides
% whether they can enter and notifies them when they leave (i.e. when they
% reached its outbound POI) - unless in the meantime they communicate changes
% (ex: a car may "decide" to break down).
%
% There is a fairness issue: vehicles are expected to exit the road in the order
% they entered; however, load-based average speeds may not respect that property
% (example of a road entered successively by two vehicles, a large number of
% other vehicles departing in-between: the second vehicule would have an higher
% speed than the first one. To overcome it, any vehicle that would overtake
% another will actually exit a bit later, based on their own expected travel
% duration.



% Corresponds to the alpha coefficient in 'road-characteristics.plot':
-define( load_factor, 45 ).


% Creates a new road, which starts empty (with no vehicles).
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this road (as a plain string)
%
% - SourcePOI is the PID of the source POI for this road
%
% - TargetPOI  is the PID of the target POI for this road
%
% - MaxCapacity is the maximum vehicle capacity of this road
%
% Note: as direct requests are used in this constructor, roads must be created
% initially (before the simulation starts).
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				 class_Actor:name(), poi_pid(), poi_pid(), capacity() ) ->
					   wooper_state().
construct( State, ActorSettings, Name, SourcePOI, TargetPOI, MaxCapacity ) ->

	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	Label = Name ++ "\\n" ++ text_utils:pid_to_string( self() ),

	GraphableState = class_Graphable:construct( ActorState,
									[ { label, Label }, { color, black } ] ),

	% In meters:
	Length = class_GIS:compute_distance( SourcePOI, TargetPOI ),

	% In km/hour:
	MinAverageSpeed = 30,
	MaxAverageSpeed = 110,

	% Longer roads (more than 10 km) are often highway-like:
	HighwayThreshold = 10,

	% Verified: starts at MinAverageSpeed, grows linearly with Length until
	% reaching MaxAverageSpeed, not increasing anymore then. Still in km/hour:
	AverageSpeed = min( MaxAverageSpeed, MinAverageSpeed
		 + ( MaxAverageSpeed - MinAverageSpeed )
					   * ( Length / 1000 ) / HighwayThreshold ),

	setAttributes( GraphableState, [

		{source_poi,SourcePOI},
		{target_poi,TargetPOI},
		{length,Length},
		{current_capacity,0},
		{max_capacity,MaxCapacity},
		{max_average_speed,AverageSpeed},
		{vehicle_queue,queue:new()},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

								 ] ).



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.



% First scheduling on an incinerator.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% This actor is mostly passive.

	?trace_fmt( "Road just created: ~s", [ to_string( State ) ] ),

	% Creates an initial deadline at the tick, to trigger the burners:
	?wooper_return_state_only( State ).




% The definition of the spontaneous behaviour of this road.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	OutPoi = ?getAttr(target_poi),

	% Awoken, hence should have at least one vehicle planned to exit:
	NewState = case queue:out( ?getAttr(vehicle_queue) ) of

		{ empty, _Q } ->
			?warning( "Road scheduled while having no vehicle." ),
			State ;

		{ { value, { CurrentTickOffset, VehicleList } }, PoppedQueue } ->

			% Notifies all exiting vehicles:
			SentState = lists:foldl( fun( VehiclePid, AccState ) ->
					class_Actor:send_actor_message( VehiclePid,
							   { notifyRoadExit, OutPoi }, AccState )
									 end,
									_Acc0=State,
									_List=VehicleList ),

			NewCapacity = ?getAttr(current_capacity) - length( VehicleList ),

			setAttributes( SentState, [

					{ current_capacity, NewCapacity },
					{ vehicle_queue, PoppedQueue }

									   ] );

		{ { value, { TickOffset, VehicleList } }, Q } ->
			?error_fmt( "Road scheduled while first exit (of ~w) is "
						"to happen at #~p instead.",
						[ VehicleList, TickOffset ] ),
			Q

	end,

	?wooper_return_state_only( NewState ).



% Tells this road that the specified vehicle entered it.
%
% Triggers in return a notifyRoadExit call to the vehicle once it reaches the
% end of that road.
%
-spec driveIn( wooper_state(), vehicle_pid(), class_Actor:actor_pid() ) ->
					 class_Actor:actor_oneway_return().
driveIn( State, VehiclePid, _SenderPid ) ->

	% Trucks and cars induce the same load:
	NewCapacity = ?getAttr(current_capacity) + 1,

	NewLoad = NewCapacity / ?getAttr(max_capacity),

	% In meters per second:
	NewAverageSpeed = get_average_speed( NewLoad, State ),

	% In meters:
	RoadLength = ?getAttr(length),

	% In seconds:
	TraversalDuration = RoadLength / NewAverageSpeed,

	% We relaxed considerably the maximal relative error tolerated for the sake
	% of having a procedurally generated benchmarking case:
	%
	TickDuration = class_Actor:convert_seconds_to_non_null_ticks(
				   TraversalDuration, _MaxRelativeError=0.5, State ),

	?trace_fmt( "On this road of length ~f meters, the current average speed "
				"is ~f km/h, resulting on a traversal duration of ~s "
				"(i.e. ~B ticks).",
				[ RoadLength,
				  unit_utils:meters_per_second_to_km_per_hour(NewAverageSpeed),
				  text_utils:duration_to_string( 1000 * TraversalDuration ),
				  TickDuration ] ),

	% Tick at which this vehicle is to reach the road end:
	DepartureTickOffset = ?getAttr(current_tick_offset) + TickDuration,

	Queue = ?getAttr(vehicle_queue),

	% Let's ensure we do not overtake the last vehicle inserted:
	{ NewQueue, ChosenDepartureOffset } = case queue:out_r( Queue ) of

		{ empty, _Queue } ->
			% Empty queue, no overtaking problem here:
			{ queue:in( _Item={ DepartureTickOffset, [ VehiclePid ] }, Queue ),
					   DepartureTickOffset };

		% There is here a last departing time in the queue that can be fetched:
		{ { value, { LastTickOffset, _VehicleList } }, _Q }
								  when LastTickOffset < DepartureTickOffset ->
			% Another easy case, natural order is fine:
			{ queue:in( _Item={ DepartureTickOffset, [ VehiclePid ] }, Queue ),
					   DepartureTickOffset };

		% Here, same departure tick, hence vehicle list growing:
		{ { value, { DepartureTickOffset, VehicleList } }, TruncQ } ->
			NewList = [ VehiclePid | VehicleList ],
			{ queue:in( _Item={ DepartureTickOffset, NewList }, TruncQ ),
					   DepartureTickOffset };

		% Here DepartureTickOffset < LastTickOffset, i.e. the new vehicle would
		% exit the road before at least one previously entered, violating the
		% FIFO model. The elected tick for this new exit will be set in a strict
		% future instead (strictly positive random delay):
		{ { value, { LastTickOffset, _VehicleList } }, _TruncQ } ->

			ThisDepartureOffset = LastTickOffset + 1
				   + class_RandomManager:get_positive_integer_gaussian_value(
								_Mu=float(TickDuration*NewLoad), _Sigma=2.0 ),
			% By design in a strict future, no previous entry for that tick:
			AddQueue = queue:in( _Item={ DepartureTickOffset, [ VehiclePid ] },
								Queue ),
			{ AddQueue, ThisDepartureOffset }

	end,

	% Might be useful for the vehicle:
	SentState = class_Actor:send_actor_message( VehiclePid,
					{ notifyPlannedRoadExit, ChosenDepartureOffset }, State ),

	PlannedState = class_Actor:add_spontaneous_tick( ChosenDepartureOffset,
													SentState ),

	FinalState = setAttributes( PlannedState, [

					{ current_capacity, NewCapacity },
					{ vehicle_queue, NewQueue }

											   ] ),

	?wooper_return_state_only( FinalState ).



% Returns the source POI of this road.
%
% (const request)
%
getSourcePOI( State ) ->
	?wooper_return_state_result( State, ?getAttr(source_poi) ).



% Returns the target POI of this road.
%
% (const request)
%
getTargetPOI( State ) ->
	?wooper_return_state_result( State, ?getAttr(target_poi) ).



% Registers this road into the specified GIS.
%
% (const oneway)
%
-spec registerInGIS( wooper_state(), pid() ) -> oneway_return().
registerInGIS( State, GISPid ) ->

	GISPid ! { declareRoad, [ self(), ?getAttr(source_poi),
							 ?getAttr(target_poi) ] },

	?wooper_return_state_only( State ).



% Static method section.


% Generates full instance definitions from the specified list of initial base
% definitions, which are simply { SourcePOI, TargetPOI } pairs of PID.
%
generate_definitions( BaseDefs ) ->
	generate_definitions( BaseDefs, _Count=0, _Acc=[] ).


generate_definitions( _BaseDefs=[], _Count, Acc ) ->
	% Mandatory here, we need to preserve the order:
	lists:reverse( Acc );

generate_definitions( _BaseDefs=[ { POI, POI } | _T ], _Count, _Acc ) ->
	throw( { looping_road, POI } );

generate_definitions( _BaseDefs=[ { SourcePOI, TargetPOI } | T ], Count,
					 Acc ) ->

	Name = io_lib:format( "Road-~B", [ Count + 1 ] ),

	% Currently the capacity is based on the road length, divided by the size of
	% a vehicle slot, and some randomness:
	%
	Length = class_GIS:compute_distance( SourcePOI, TargetPOI ),


	MaxCapacity = 1 + class_RandomManager:get_positive_integer_gaussian_value(
					  _Mean=Length/?vehicle_slot_length, _Variance=2 ),

	NewDef = { class_Road, [ Name, SourcePOI, TargetPOI, MaxCapacity ] },

	generate_definitions( T, Count + 1, [ NewDef | Acc ] ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper_state() ) -> string().
to_string( State ) ->

	io_lib:format( "Road '~s' from POI ~w to POI ~w, whose length is ~f meters,"
				   " whose current capacity is ~B/~B, whose current traffic"
				   " is made of ~B vehicles",
				   [ ?getAttr(name),
					 ?getAttr(source_poi),
					 ?getAttr(target_poi),
					 ?getAttr(length),
					 ?getAttr(current_capacity),
					 ?getAttr(max_capacity),
					 queue:len( ?getAttr(vehicle_queue) )
					] ).



% Helper section.


% Returns the average speed for a vehicle on that road, in meters per second.
%
% (helper)
%
-spec get_average_speed( math_utils:percent(), wooper_state() ) ->
							   unit_utils:meters_per_second().
get_average_speed( Load, State ) ->

	% Speeds are in km/h:
	Vmin = 5,
	Vmax = max( Vmin + 1, ?getAttr(max_average_speed) ),

	% See road-characteristics.plot:
	V = Vmin + ( Vmax - Vmin ) * math:exp( - Load / ?load_factor ),

	unit_utils:km_per_hour_to_meters_per_second( V ).
