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



% Class modeling a point of interest, which is basically any named point on a
% thoroughfare network.
%
-module(class_PointOfInterest).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_GeoContainer, class_Graphable,
								class_TraceEmitter ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Name, Location, GISPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, delete/1 ).


% Method declarations.
-define( wooper_method_export, requestEntry/2, takeRoadTo/3,
		 declareInboundRoad/2, declareOutboundRoad/2,
		 getConnectivity/1, getOutboundPOIs/1, registerInGIS/2 ).


% Static method declarations.
-define( wooper_static_method_export, ).


% Helpers:
-export([ to_string/1 ]).



% For type definitions:
-include("city_example_types.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.PointOfInterest").




% The class-specific attributes of a point of interest are:
%
% - inbound_roads :: [ road_pid() ], listing the PID of the roads pointing to
% this location of interest
%
% - outbound_roads :: [ road_pid() ], listing the PID of the roads pointing from
% this location of interest





% Creates a new point of interest (POI), which is supposed located in a fixed,
% static position.
%
% The construction parameters are the name of this POI and its location, which
% is either:
%
% - { CoordinateType :: class_GIS:geolocation_flavour(), Location::
% class_GIS:geolocation_coordinate() }
%
% - Location :: the corresponding (static) location
%
-spec construct( wooper_state(), class_TraceEmitter:name(),
				class_GIS:static_location(), pid() ) -> wooper_state().
construct( State, Name, Location, GISPid ) ->

	GISPid ! { recordPointOfInterest, [ self(), Location ] },

	TraceState = class_TraceEmitter:construct( State, Name ),

	GeoState = class_GeoContainer:construct( TraceState, Location ),

	Label = Name ++ "\\n" ++ text_utils:pid_to_string( self() ),

	GraphableState = class_Graphable:construct( GeoState,
								[ { label, Label }, { color, black } ] ),

	setAttributes( GraphableState, [

			{inbound_roads,[]},
			{outbound_roads,[]},
			{trace_categorization,
			 text_utils:string_to_binary(?TraceEmitterCategorization)}

						  ] ).



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.




% Actor oneway section.



% Requests whether this POI can accept the specified vehicle and, if yes, accept
% it.
%
% Note: not to be mixed up with the class_GeoContainer:requestEntry/1 request.
%
% (actor oneway)
%
-spec requestEntry( wooper_state(), vehicle_pid() ) ->
						  class_Actor:actor_oneway_return().
requestEntry( State, VehiclePid ) ->

	% Each POI is a geo-container:
	{ ReqState, EnterRequestOutcome } = class_GeoContainer:request_entry(
											VehiclePid, State ),

	% We must call a method to resolve the actual class name (probably a child
	% class of PointOfInterest:
	{ _SameState, Classname } = executeRequest( ReqState, getClassName ),

	% Sends back the result:
	SentState = class_Actor:send_actor_message( VehiclePid,
		  { notifyEntryOutcome, [ EnterRequestOutcome, Classname ] },
		  ReqState ),

	?wooper_return_state_only( SentState ).



% Requests this POI to find for the specified vehicle a road going to specified
% POI, and request that road to accept it.
%
% (actor oneway)
%
-spec takeRoadTo( wooper_state(), poi_pid(), vehicle_pid() ) ->
						  class_Actor:actor_oneway_return().
takeRoadTo( State, TargetPOI, VehiclePid ) ->

	RoadPid = find_road_to( TargetPOI, State ),

	% Sends the request on behalf of the vehicle, which will be notified
	% directly by the road thanks to a notifyRoadEntry call:
	SentState = class_Actor:send_actor_message( RoadPid,
					{ driveIn, VehiclePid }, State ),

	?wooper_return_state_only( SentState ).



% Methods section.



% Declares an additional inbound road.
%
% (request, for synchronicity)
%
-spec declareInboundRoad( wooper_state(), road_pid() ) ->
								request_return( 'road_declared' ).
declareInboundRoad( State, RoadPid ) ->
	?wooper_return_state_result(
			appendToAttribute( State, inbound_roads, RoadPid ),
			road_declared ).



% Declares an additional outbound road.
%
% (request, for synchronicity)
%
-spec declareOutboundRoad( wooper_state(), road_pid() ) ->
								request_return( 'road_declared' ).
declareOutboundRoad( State, RoadPid ) ->
	?wooper_return_state_result(
			appendToAttribute( State, outbound_roads, RoadPid ),
			road_declared ).



% Returns the road connectivity of this point of interest.
%
% (const request)
%
-spec getConnectivity( wooper_state() ) ->
						 request_return( { [ road_pid() ], [ road_pid() ] } ).
getConnectivity( State ) ->
	?wooper_return_state_result( State,
					{ ?getAttr(inbound_roads), ?getAttr(outbound_roads) } ).



% Returns a list of the outbound POIs, i.e. the POIs that can be reached with a
% road starting from this POI.
%
% (const request)
%
-spec getOutboundPOIs( wooper_state() ) -> request_return( [ poi_pid() ] ).
getOutboundPOIs( State ) ->

	ReceivedPOI = obtain_results_for_request( _RequestName=getTargetPOI,
							  _Args=[], _Targets=?getAttr(outbound_roads) ),

	% Avoid useless duplicates:
	SelectedPOIs = list_utils:uniquify( ReceivedPOI ),

	?wooper_return_state_result( State, SelectedPOIs ).



% Registers this point of interest into the specified GIS.
%
% (const oneway)
%
% Note: not blocking, beware to synchronicity!
%
-spec registerInGIS( wooper_state(), pid() ) -> oneway_return().
registerInGIS( State, GISPid ) ->

	GISPid ! { declarePOI, self() },

	?wooper_return_state_only( State ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper_state() ) -> string().
to_string( State ) ->

	Inbound = ?getAttr(inbound_roads),
	Outbound = ?getAttr(outbound_roads),

	io_lib:format( "Point of interest '~s' located at ~s, "
				   "having ~B inbound road(s) (i.e. ~w) and "
				   "~B outbound road(s) (i.e. ~w)",
				   [ ?getAttr(name),
					 class_GeolocalizedElement:interpret_location( State ),
					 length(Inbound),
					 Inbound,
					 length(Outbound),
					 Outbound

					] ).



% Static methods section.


% Helper functions.



% Returns one of the roads that lead directly to specified POI, chosen at
% random.
%
% (helper)
%
find_road_to( TargetPOI, State ) ->

	OutboundRoads = ?getAttr(outbound_roads),

	CandidateRoads = lists:foldl( fun( OutBoundRoad, Acc ) ->
						 OutBoundRoad ! { getTargetPOI, [], self() },
						 receive

							 { wooper_result, TargetPOI } ->
								 [ OutBoundRoad | Acc ];

							 { wooper_result, _OtherPOI } ->
								 Acc

						 end
				 end,
				 _Acc0=[],
				_List=OutboundRoads ),

	case CandidateRoads of

		[] ->
			throw( { cannot_reach, { from, self()}, { to, TargetPOI },
					{ using_roads, OutboundRoads } } );

		_ ->
			ok

	end,

	list_utils:draw_element( CandidateRoads ).
