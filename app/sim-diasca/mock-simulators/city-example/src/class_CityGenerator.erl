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



% Class modelling the generation of cities, rather than the loading of their
% description from file.
%
-module(class_CityGenerator).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, CityDescription, GISPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, delete/1 ).


% Member method declarations:
-define( wooper_method_export, to_string/1 ).



% Static method declarations:
-define( wooper_static_method_export, area_to_side_length/1 ).


-export([ report/2, report/3 ]).


% For all shared defines and types:
-include("city_example_types.hrl").


% For #city_description{}:
-include("class_CityGenerator.hrl").



% Design notes:
%
% This city generator (which is not an actor) is a scenario that is responsible
% for the creation of a whole city: a simulation case may just stick with this
% creation.



% Description of a city that is to be procedurally created:
%
-type city_description() :: #city_description{}.



% Type section.


-export_type([ city_description/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.CityGenerator").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").






% Implementation notes:
%



% The class-specific attributes of a City Generator instance are:
%
% - city_name :: string() is the name of the city
%
% - dimensions :: linear3D:point() is the opposite point to the origin
%
% - load_balancer_pid :: pid() is the PID of the load balancer
%
% - location_generator_pid:: pid() is the PID of the internal location generator
%
% - incinerators :: [ pid() ] is a plain list of the created incinerators
%
% - residential_sources :: [ pid() ] is a plain list of the created residential
% sources
%
% - industrial_sources :: [ pid() ] is a plain list of the created industrial
% sources
%
% - road_junctions :: [ pid() ] is a plain list of the created road junctions
%
% - roads :: [ pid() ] is a plain list of the created roads




% Constructs a new city generator, from the specified city description (refer to
% the city_description record definition in class_CityGenerator.hrl), passing
% also the PID of the GIS.
%
-spec construct( wooper_state(), city_description(), pid() ) -> wooper_state().
construct( State, CityDescription=#city_description{
					 name=Name,
					 dimensions=Dimensions }, GISPid ) ->

	EmitterState = class_TraceEmitter:construct( State,
			   io_lib:format( "City Generator for ~s", [ Name ] ) ),


	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	LocationGeneratorPid = class_LocationGenerator:new_link(
			   "City Location Generator", Dimensions ),


	CategState = setAttributes( EmitterState, [

		{ name, Name },
		{ dimensions, Dimensions },
		{ load_balancer_pid, LoadBalancerPid },
		{ location_generator_pid, LocationGeneratorPid },
		{ trace_categorization,
				text_utils:string_to_binary( ?TraceEmitterCategorization ) }

											   ] ),

	% Separate function to rely on a trace-enabled state, returns an updated
	% one:
	%
	generate_city( CityDescription, LoadBalancerPid, LocationGeneratorPid,
				   GISPid, CategState ).




-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	?getAttr(location_generator_pid) ! delete,

	State.




% Section for member methods.


% Returns a string describing the state of this instance.
%
% (const request)
%
-spec to_string( wooper_state() ) -> request_return( string() ).
to_string( State ) ->

	FinalString = io_lib:format( "City generator having for world bounds ~p, "
								 "using location generator ~w",
			[ ?getAttr(dimensions), ?getAttr(location_generator_pid) ] ),

	?wooper_return_state_result( State, FinalString ).




% Section for static methods.


% Returns the length (in meters) of the side of a square whose area is the
% specified one, expressed in square kilometers.
%
-spec area_to_side_length( linear:area() ) -> unit_utils:meters().
area_to_side_length( Area ) ->
	math:sqrt( Area * 1000 * 1000 ).



% Generates the full specified city.
%
generate_city( #city_description{

						name=Name,
						incinerator_count=IncineratorCount,
						landfill_count=LandfillCount,
						residential_waste_source_count=ResidentialSourceCount,
						industrial_waste_source_count=IndustrialSourceCount,
						road_junction_count=RoadJunctionCount,
						waste_truck_count=WasteTruckCount

				  },
			   LoadBalancerPid, LocationGeneratorPid, GISPid, State ) ->

	report( "Creating ~B incinerators, ~B landfills, ~B residential and "
			"~B industrial waste sources, and ~B road junctions.",
			[ IncineratorCount, LandfillCount, ResidentialSourceCount,
			  IndustrialSourceCount, RoadJunctionCount ], State ),

	report( " - generating definitions for ~B incinerators",
			   [ IncineratorCount ], State ),

	IncineratorDefs = class_Incinerator:generate_definitions( IncineratorCount,
										LocationGeneratorPid, GISPid ),

	report( " - creating these ~B incinerators", [ IncineratorCount ],
		  State ),

	Incinerators = class_Actor:create_initial_actors( IncineratorDefs,
											LoadBalancerPid ),

	% Result of the request waited later, for increased parallelism:
	GISPid ! { declarePOIs, [ Incinerators ], self() },

	% Defined to wait as many times as declarePOIs will be called:
	POIWaiter= fun() -> receive

		{ wooper_result, poi_list_declared } ->
			ok

						end
	end,


	report( " - generating definitions for ~B landfills", [ LandfillCount ],
			State ),

	LandfillDefs = class_Landfill:generate_definitions( LandfillCount,
										LocationGeneratorPid, GISPid ),


	report( " - creating these ~B landfills", [ LandfillCount ], State ),

	Landfills = class_Actor:create_initial_actors( LandfillDefs,
											LoadBalancerPid ),


	% Result of the request waited later, for increased parallelism:
	GISPid ! { declarePOIs, [ Landfills ], self() },


	report( " - generating definitions for ~B industrial waste sources",
			   [ IndustrialSourceCount ], State ),

	IndustrialSourceDefs = class_IndustrialWasteSource:generate_definitions(
						 IndustrialSourceCount, LocationGeneratorPid, GISPid ),


	report( " - creating these ~B industrial waste sources",
			[ IndustrialSourceCount ], State ),

	IndustrialSources = class_Actor:create_initial_actors( IndustrialSourceDefs,
											LoadBalancerPid ),

	GISPid ! { declarePOIs, [ IndustrialSources ], self() },


	report( " - generating definitions for ~B residential waste sources",
			   [ ResidentialSourceCount ], State ),

	ResidentialSourceDefs = class_ResidentialWasteSource:generate_definitions(
						 ResidentialSourceCount, LocationGeneratorPid, GISPid ),

	report( " - creating these ~B residential waste sources",
			[ ResidentialSourceCount ], State ),

	ResidentialSources = class_Actor:create_initial_actors(
						   ResidentialSourceDefs, LoadBalancerPid ),

	GISPid ! { declarePOIs, [ ResidentialSources ], self() },


	report( " - generating definitions for ~B base road junctions",
			[ RoadJunctionCount ], State ),

	RoadJunctionDefs = class_RoadJunction:generate_definitions(
						 RoadJunctionCount, LocationGeneratorPid, GISPid ),

	report( " - creating these ~B base road junctions",
			[ RoadJunctionCount ], State ),

	RoadJunctions = class_Actor:create_initial_actors( RoadJunctionDefs,
											LoadBalancerPid ),

	GISPid ! { declarePOIs, [ RoadJunctions ], self() },

	Containers = Incinerators ++ ResidentialSources ++ IndustrialSources,

	report( " - generating definitions for ~B waste trucks",
			[ WasteTruckCount ], State ),

	% Initially all waste trucks start in a (loadable) point of interest (none
	% is on a road):
	%
	WasteTruckDefs = class_WasteTruck:generate_definitions( WasteTruckCount,
							Containers ),

	report( " - creating these ~B waste trucks", [ WasteTruckCount ], State ),

	WasteTrucks = class_Actor:create_initial_actors( WasteTruckDefs,
											LoadBalancerPid ),

	report( " - waiting for all POI creations", State ),

	% Waits for POI creations of all kinds:
	POIWaiter(),
	POIWaiter(),
	POIWaiter(),
	POIWaiter(),
	POIWaiter(),

	report( " - generating road network", State ),

	% Links all points of interest as wanted:
	Roads = generate_road_network( Incinerators, Landfills, ResidentialSources,
		IndustrialSources, RoadJunctions, GISPid, LoadBalancerPid, State ),

	report( " - road network generated", State ),

	[ C ! traceContent || C <- Containers ],

	% Quick checking:
	IncineratorCount =       length( Incinerators ),
	LandfillCount =          length( Landfills ),
	ResidentialSourceCount = length( ResidentialSources ),
	IndustrialSourceCount =  length( IndustrialSources ),
	RoadJunctionCount =      length( RoadJunctions ),
	WasteTruckCount =        length( WasteTrucks ),

	report( "Following city elements were created for city ~s:~n"
			   " - ~B incinerators~n"
			   " - ~B landfills~n"
			   " - ~B residential waste sources~n"
			   " - ~B industrial waste sources~n"
			   " - ~B road junctions~n"
			   " - ~B roads~n"
			   " - ~B waste trucks~n",
			  [ Name, IncineratorCount, LandfillCount, ResidentialSourceCount,
				IndustrialSourceCount, RoadJunctionCount, length( Roads ),
				WasteTruckCount ], State ),

	setAttributes( State, [

		{ incinerators, Incinerators },
		{ landfills, Landfills },
		{ residential_sources, ResidentialSources },
		{ industrial_sources, IndustrialSources },
		{ road_junctions, RoadJunctions },
		{ waste_trucks, WasteTrucks },
		{ roads, Roads }

								  ] ).




% Generates a complete road network, so that notably each point of interest can
% be reached (inbound) and go away from (outbound).
%
% For that, roads are to be created.
%
% Returns a list of the PID of the newly created roads.
%
generate_road_network( Incinerators, Landfills, ResidentialSources,
					   IndustrialSources, RoadJunctions, GISPid,
					   LoadBalancerPid, State ) ->

	report( "    + adding roads for junctions", State ),

	% First, add the most natural links, based on proximity, for all
	% road-junctions:
	JunctionRoadDefs = add_roads_for( RoadJunctions, GISPid ),


	% Here we will have to ensure that all non-junction POI are satisfied,
	% i.e. that they have at least one inbound and one outbound connection
	% (preferably exactly one of each).
	%
	% If not, we will add connections, possibly exceeding initial capacities of
	% junctions:
	%
	% Note that this does not ensure that the overall graph is fully connected
	% (no-connected subgraphs may exist).
	%
	report( "    + forcing POI connectivity", State ),

	CompletionRoadDefs = force_connectivity( Incinerators ++ Landfills
					 ++ ResidentialSources ++ IndustrialSources, GISPid ),

	% We could/should add connection roads to ensure that the road network is
	% fully connected (otherwise for example no landfill could be reached from
	% an incinerator, which is not wanted).

	% These definitions just include a source and a target POI:
	BaseRoadDefs = JunctionRoadDefs ++ CompletionRoadDefs,

	RoadCount = length( BaseRoadDefs ),

	report( "    + generating definitions of the corresponding ~B roads",
			[ RoadCount ], State ),

	FullRoadDefs = class_Road:generate_definitions( BaseRoadDefs ),


	report( "    + creating these ~B corresponding roads",
			[ RoadCount ], State ),

	Roads = class_Actor:create_initial_actors( FullRoadDefs, LoadBalancerPid ),

	report( "    + declaring these ~B corresponding roads",
			[ RoadCount ], State ),

	% To declare the roads, we prepare a list of { RoadPid, Source, Target }
	% triplets:
	%
	RoadTriplets = lists:zipwith( fun( Road, { Source, Target } ) ->
										 { Road, Source, Target } end,
								  Roads,
								  BaseRoadDefs ),


	% Roads shall *also* be declared at the level of the GIS:
	GISPid ! { declareRoads, [ RoadTriplets ], self() },

	declare_roads_to_poi( RoadTriplets ),

	receive

		{ wooper_result, roads_declared } ->
			ok

	end,

	report( "    + roads declared, network generated", State ),

	Roads.





% Completes all road junctions.
%
add_roads_for( RoadJunctions, GISPid ) ->
	add_roads_for( RoadJunctions, GISPid, _AccRoads=[] ).


add_roads_for( _RoadJunctions=[], _GISPid, AccRoads ) ->
	AccRoads;

add_roads_for( _RoadJunctions=[ J | T ], GISPid, AccRoads ) ->

	J ! { getUnsatisfiedConnections, [], self() },

	NewRoads = receive

		{ wooper_result, fully_connected } ->
			[];

		{ wooper_result,
		 { lacking_inbounds, LackInboundCount, InboundPOIs } } ->
			find_and_create_inbound( J, LackInboundCount, InboundPOIs, GISPid );

		{ wooper_result,
		 { lacking_outbounds, LackOutboundCount, OutboundPOIs } } ->
			find_and_create_outbound( J, LackOutboundCount, OutboundPOIs,
									 GISPid );

		{ wooper_result, { lacking_both, LackInboundCount, InboundPOIs,
						  LackOutboundCount, OutboundPOIs } } ->

			InRoads = find_and_create_inbound( J, LackInboundCount,
										InboundPOIs, GISPid ),

			OutRoads = find_and_create_outbound( J, LackOutboundCount,
										OutboundPOIs, GISPid ),

			InRoads ++ OutRoads

	end,

	add_roads_for(  T, GISPid, NewRoads ++ AccRoads ).



% Creates Count outbound roads from specified junction, not duplicating any
% pre-existing road.
%
find_and_create_outbound( Junction, Count, CurrentOutbounds, GISPid ) ->

	GISPid ! { searchNearestPointsOfInterest, [ Junction,
				  _ExcludedPOIs=CurrentOutbounds, Count ], self() },

	receive

		{ wooper_result, SelectedPOIs } ->
			get_road_defs( _From=Junction, _To=SelectedPOIs )

	end.



% Creates Count inbound roads from specified junction, not duplicating any
% pre-existing road.
%
find_and_create_inbound( Junction, Count, CurrentInbounds, GISPid ) ->

	GISPid ! { searchNearestPointsOfInterest, [ Junction,
				  _ExcludedPOIs=CurrentInbounds, Count ], self() },

	receive

		{ wooper_result, SelectedPOIs } ->
			get_road_defs( _From=SelectedPOIs, _To=Junction )

	end.



% Returns the roads that were needed so that all specified POIs have both at
% least one inbound and one outbound connection.
%
force_connectivity( POIList, GISPid ) ->
	force_connectivity( POIList, GISPid, _AccRoads=[] ).


force_connectivity( _POIList=[], _GISPid, AccRoads ) ->
	AccRoads;


force_connectivity( _POIList=[ P | T ], GISPid, AccRoads ) ->

	P ! { getConnectivity, [], self() },

	NewRoads = receive

		{ wooper_result, { _InBounds=[], _Outbounds=[] } } ->
			% POI fully separated, let's create a two-way link with nearest POI:
			GISPid ! { searchNearestPointsOfInterest, [ P, _Count=1 ], self() },
			receive

				{ wooper_result, [ TargetPOI ] } ->
					% Bidirectional road, problem solved:
					[ { P, TargetPOI }, { TargetPOI, P } ]
			end;

		{ wooper_result, { _InBounds=[], _Outbounds=[ POI | _T ] } } ->
			% Let's transform this outbound one-way into a two-way:
			[ { POI, P } ];

		{ wooper_result, { _InBounds=[ POI | _T ], _Outbounds=[] } } ->
			% Let's transform this inbound one-way into a two-way:
			[ { P, POI } ];

		{ wooper_result, { _InBounds, _Outbounds } } ->
			% Already at least one of each, nothing to be done here:
			[]

	end,

	force_connectivity( T, GISPid, NewRoads ++ AccRoads ).



% Creates base road defintions from specified POI(s) to specified POI(s).
%
get_road_defs( From, To ) when is_list( From ) andalso is_pid( To ) ->
	[ { F, To } || F <- From ];

get_road_defs( From, To ) when is_pid( From ) andalso is_list( To ) ->
	[ { From, T } || T <- To ].




% Declares (synchronously) the specified roads to their respective POIs.
%
% Sends back 'roads_declared' and terminates.
%
declare_roads_to_poi( RoadTriplets ) ->

	FinalCount = declare_roads_to_poi_helper( RoadTriplets, _Count=0 ),

	basic_utils:wait_for( _Message={ wooper_result, road_declared },
					_WaitedCount=2*FinalCount ).



declare_roads_to_poi_helper( _RoadTriplets=[], Count ) ->
	Count;

declare_roads_to_poi_helper(
		   _RoadTriplets=[ { RoadPid, SourcePid, TargetPid } | T ], Count ) ->

	SourcePid ! { declareOutboundRoad, RoadPid, self() },
	TargetPid ! { declareInboundRoad, RoadPid, self() },

	declare_roads_to_poi_helper( T, Count+1 ).



% Reports specified message.
%
% Centralised to be easily enabled/disabled.
%
-spec report( string(), wooper_state() ) -> basic_utils:void().
report( Message, State ) ->

	?report( Message ),

	% We may notify the plugins as well:
	class_PluginManager:notify_case_specific( city_generation, Message ).


-spec report( string(), list(), wooper_state() ) -> basic_utils:void().
report( FormatString, Parameters, State ) ->

	Message = io_lib:format( FormatString, Parameters ),

	report( lists:flatten( Message ), State ).
