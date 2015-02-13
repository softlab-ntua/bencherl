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
		 construct/3, destruct/1 ).


% Member method declarations:
-define( wooper_method_export, generateCity/1, writeInitialisation/2,
		 to_string/1 ).



% Static method declarations:
-define( wooper_static_method_export, area_to_side_length/1, get_cell_infos/2 ).


-export([ report/2, report/3 ]).


% For all shared defines and types:
-include("city_example_types.hrl").


% For #city_description{}:
-include("class_CityGenerator.hrl").


% For weather_cell_length:
-include("city_example_settings.hrl").



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
-spec construct( wooper:state(), city_description(), pid() ) -> wooper:state().
construct( State, CityDescription=#city_description{
					 name=Name,
					 dimensions=Dimensions }, GISPid ) ->

	EmitterState = class_TraceEmitter:construct( State,
			   text_utils:format( "City Generator for ~s", [ Name ] ) ),

	LoadBalancerPid = class_LoadBalancer:get_balancer(),

	LocationGeneratorPid = class_LocationGenerator:new_link(
							 "City Location Generator", Dimensions ),


	setAttributes( EmitterState, [

		{ name, Name },
		{ dimensions, Dimensions },
		{ city_description, CityDescription },
		{ gis_pid, GISPid },
		{ load_balancer_pid, LoadBalancerPid },
		{ location_generator_pid, LocationGeneratorPid },
		{ trace_categorization,
				text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								 ] ).




-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?getAttr(location_generator_pid) ! delete,

	State.




% Section for member methods.



% Generates the initial state of a city corresponding to the stored description.
%
% (request)
%
-spec generateCity( wooper:state() ) -> request_return( 'city_generated' ).
generateCity( State ) ->

	NewState = generate_city( ?getAttr(city_description),
		?getAttr(load_balancer_pid), ?getAttr(location_generator_pid),
		?getAttr(gis_pid), State ),

	?wooper_return_state_result( NewState, city_generated ).



% Writes in specified file the initialisation data corresponding to a generated
% city.
%
% (request)
%
-spec writeInitialisation( wooper:state(), file:io_device() ) ->
								 request_return( 'initialisation_written' ).
writeInitialisation( State, File ) ->

	CityDescription = ?getAttr(city_description),

	GISId = "GIS",

	GISPid = ?getAttr(gis_pid),

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	file_utils:write( File, "~n~n% Global section.~n~n", [] ),

	file_utils:write( File, "\"~s\" <- { class_GIS, [ none ] }.", [ GISId ] ),

	GISIdRef = { user_id, GISId },

	LocationGeneratorPid = ?getAttr(location_generator_pid),


	% Regarding all kinds of points of interest, we need to actually create all
	% instances, as they register themselves to this GIS (needed to determine
	% road connectivity and al); but we may deallocate them soon.


	file_utils:write( File, "~n~n~n% Incinerator section.~n~n", [] ),


	IncineratorDefsWithPid = class_Incinerator:generate_definitions(
						CityDescription#city_description.incinerator_count,
						LocationGeneratorPid, GISPid ),

	LoadBalancerPid ! { createInitialActors, [ IncineratorDefsWithPid ],
						self() },

	% We collect the user identifiers (names) of incinerators and other point of
	% interest to initialise later waste trucks thanks to these references.
	%
	% Class is class_Incinerator (File: closure):
	%
	IncRefs = [ begin

					 RefArgs = replace_in_last_arg( GISPid, GISIdRef, Args ),

					% Using ~w rather than ~p to remain mono-line:
					CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
												  [ Name, Class, RefArgs ] ),

					file_utils:write( File, CreationLine ),

					{ user_id, Name }

				end
				|| { Class, Args=[ Name | _T ] } <- IncineratorDefsWithPid ],

	% From createInitialActors:
	IncPids = receive

				  { wooper_result, IPids } when is_list( IPids ) ->
					  IPids

	end,



	file_utils:write( File, "~n~n~n% Landfill section.~n~n", [] ),

	LandfillDefsWithPid = class_Landfill:generate_definitions(
						CityDescription#city_description.landfill_count,
						LocationGeneratorPid, GISPid ),

	LoadBalancerPid ! { createInitialActors, [ LandfillDefsWithPid ], self() },

	% Class is class_Landfill (File: closure):
	LandRefs = [ begin

					 RefArgs = replace_in_last_arg( GISPid, GISIdRef, Args ),

					 % Using ~w rather than ~p to remain mono-line:
					 CreationLine = io_lib:format(
										  "\"~s\" <- { ~s, ~w }.~n~n",
										  [ Name, Class, RefArgs ] ),

					 file_utils:write( File, CreationLine ),

					 { user_id, Name }

				 end
				 || { Class, Args=[ Name | _T ] } <- LandfillDefsWithPid ],

	% From createInitialActors:
	LandPids = receive

				  { wooper_result, LPids } when is_list( LPids ) ->
					  LPids

	end,


	file_utils:write( File, "~n~n~n% Industrial waste source section.~n~n",
					  [] ),

	IndusWasteSourceDefsWithPid =
		class_IndustrialWasteSource:generate_definitions(
			CityDescription#city_description.residential_waste_source_count,
			LocationGeneratorPid, GISPid ),

	LoadBalancerPid ! { createInitialActors, [ IndusWasteSourceDefsWithPid ],
						self() },

	% Class is class_IndustrialWasteSource (File: closure):
	IndusRefs = [ begin

					RefArgs = replace_in_last_arg( GISPid, GISIdRef, Args ),

					% Using ~w rather than ~p to remain mono-line:
					CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
												  [ Name, Class, RefArgs ] ),

					file_utils:write( File, CreationLine ),

					{ user_id, Name }


				  end ||
				  { Class, Args=[ Name | _ ] } <- IndusWasteSourceDefsWithPid ],

	% From createInitialActors:
	IndusPids = receive

				  { wooper_result, InPids } when is_list( InPids ) ->
					  InPids

	end,


	file_utils:write( File, "~n~n~n% Residential waste source section.~n~n",
					  [] ),

	ResidWasteSourceDefsWithPid =
		class_ResidentialWasteSource:generate_definitions(
			CityDescription#city_description.residential_waste_source_count,
			LocationGeneratorPid, GISPid ),

	LoadBalancerPid ! { createInitialActors,
						[ ResidWasteSourceDefsWithPid ], self() },

	% Class is class_ResidentialWasteSource (File: closure):
	ResRefs = [ begin

					RefArgs = replace_in_last_arg( GISPid, GISIdRef, Args ),

					% Using ~w rather than ~p to remain mono-line:
					CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
												  [ Name, Class, RefArgs ] ),

					file_utils:write( File, CreationLine ),

					{ user_id, Name }

				end ||
				  { Class, Args=[ Name | _ ] } <- ResidWasteSourceDefsWithPid ],


	% From createInitialActors:
	ResPids = receive

				  { wooper_result, RPids } when is_list( RPids ) ->
					  RPids

	end,


	% We need to both write down the definitions of road junctions and to create
	% them actually, as they record in their state the outbound and inbound
	% objectives and current values:

	file_utils:write( File, "~n~n~n% Road junction section.~n~n", [] ),

	JunctionDefsWithPid = class_RoadJunction:generate_definitions(
			CityDescription#city_description.road_junction_count,
			LocationGeneratorPid, GISPid ),

	LoadBalancerPid ! { createInitialActors, [ JunctionDefsWithPid ], self() },

	% We take advantage of the waiting to write down these instances:
	%
	% Class is class_RoadJunction (File: closure):
	JuncRefs = [ begin

					 RefArgs = replace_in_last_arg( GISPid, GISIdRef, Args ),

					 % Using ~w rather than ~p to remain mono-line:
					 CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
												   [ Name, Class, RefArgs ] ),

					 file_utils:write( File, CreationLine ),

					 { user_id, Name }


				 end || { Class, Args=[ Name | _ ] } <- JunctionDefsWithPid ],


	% From createInitialActors:
	JuncPids = receive

				  { wooper_result, JPids } when is_list( JPids ) ->
					  JPids

	end,

	file_utils:write( File, "~n~n~n% Waste truck section.~n~n", [] ),


	%io:format( "Junction PIDs: ~p.~n", [ JuncPids ] ),

	% All points of interest where trucks may begin:
	POIRefs = IncRefs ++ LandRefs ++ IndusRefs ++ ResRefs ++ JuncRefs,

	% No need to create actual truck instances.

	TruckDefs = class_WasteTruck:generate_definitions(
			CityDescription#city_description.waste_truck_count, POIRefs ),

	% Class is class_WasteTruck (File: closure):
	[ begin

		  % Using ~w rather than ~p to remain mono-line:
		  CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
										[ Name, Class, Args ] ),

		  file_utils:write( File, CreationLine )

	  end || { Class, Args=[ Name | _H ] } <- TruckDefs ],



	file_utils:write( File, "~n~n~n% Road section.~n~n", [] ),

	% First, add the most natural links, based on proximity, for all road
	% junctions:
	%
	% (returns [ { POISourcePid, POIDestinationPid } ])
	%
	RoadEndpointsForJunctions = add_roads_for( JuncPids, GISPid ),

	RoadEndpointsForConnectivity = force_connectivity(
			   IncPids ++ LandPids ++ ResPids ++ IndusPids, GISPid ),

	% We add connection roads to ensure that the road network is fully connected
	% (otherwise for example no landfill could be reached from an incinerator,
	% which is not wanted).

	% These definitions just include a source and a target POI:
	BaseRoadDefs = RoadEndpointsForJunctions ++ RoadEndpointsForConnectivity,

	FullRoadDefs = class_Road:generate_definitions( BaseRoadDefs ),

	LoadBalancerPid ! { createInitialActors, [ FullRoadDefs ], self() },

	% From createInitialActors, should not be delayed otherwise would mix up
	% with next name receivings:
	%
	_RoadPids = receive

				  { wooper_result, RoPids } when is_list( RoPids ) ->
					  RoPids

	end,

	[ begin

		  % In initialisation data, we cannot specify PIDs, hence we have to
		  % convert the PID of the source and target point of interest into a
		  % proper user identifier reference:

		  SourcePOI ! { getName, [], self() },
		  SourceRef = receive

				   { wooper_result, NSource } ->
					   { user_id, text_utils:binary_to_string( NSource ) }

		  end,

		  TargetPOI ! { getName, [], self() },
		  TargetRef = receive

				   { wooper_result, NTarget } ->
					   { user_id, text_utils:binary_to_string( NTarget ) }

		  end,

		  % Using ~w rather than ~p to remain mono-line:
		  CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
					[ Name, Class, [ Name, SourceRef, TargetRef | H ] ] ),

		  file_utils:write( File, CreationLine )

	  end || { Class, _Args=[ Name, SourcePOI, TargetPOI | H ] }
				 <- FullRoadDefs ],


	file_utils:write( File, "~n~n~n% Weather section.~n~n", [] ),

	{ Length, Width, _Height } = CityDescription#city_description.dimensions,

	{ CellsPerEdge, CellCount } = get_cell_infos( Length, Width ),

	% The system takes care of all cells:
	WeatherDefs = class_WeatherSystem:generate_definitions( CellsPerEdge ),

	report( " - generating definitions for the weather system and the ~B cells",
			[ CellCount ], State ),

	[ begin

		% Using ~w rather than ~p to remain mono-line:
		CreationLine = io_lib:format( "\"~s\" <- { ~s, ~w }.~n~n",
									  [ Name, Class, Args ] ),

		file_utils:write( File, CreationLine )

	  end || { Class, Args=[ Name | _T ] } <- WeatherDefs ],


	file_utils:write( File, "~n~n~n% End of initialisation information.~n~n",
					  [] ),

	?wooper_return_state_result( State, initialisation_written ).



% Returns a string describing the state of this instance.
%
% (const request)
%
-spec to_string( wooper:state() ) -> request_return( string() ).
to_string( State ) ->

	FinalString = text_utils:format( "City generator having for "
				"world bounds ~p, using location generator ~w",
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
						dimensions={ Length, Width, _Height },
						incinerator_count=IncineratorCount,
						landfill_count=LandfillCount,
						residential_waste_source_count=ResidentialSourceCount,
						industrial_waste_source_count=IndustrialSourceCount,
						road_junction_count=RoadJunctionCount,
						waste_truck_count=WasteTruckCount

				  },
			   LoadBalancerPid, LocationGeneratorPid, GISPid, State ) ->

	{ CellsPerEdge, CellCount } = get_cell_infos( Length, Width ),

	report( "Creating ~B incinerators, ~B landfills, ~B residential and "
			"~B industrial waste sources, ~B road junctions, "
			"a weather system comprising ~B (~B^2) weather cells.",
			[ IncineratorCount, LandfillCount, ResidentialSourceCount,
			  IndustrialSourceCount, RoadJunctionCount,
			  CellCount, CellsPerEdge ], State ),

	report( " - generating definitions for ~B incinerators",
			[ IncineratorCount ], State ),

	IncineratorDefs = class_Incinerator:generate_definitions( IncineratorCount,
										LocationGeneratorPid, GISPid ),

	%io:format( "IncineratorDefs = ~p~n", [ IncineratorDefs ] ),

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

	%io:format( "LandfillDefs = ~p~n", [ LandfillDefs ] ),

	report( " - creating these ~B landfills", [ LandfillCount ], State ),

	Landfills = class_Actor:create_initial_actors( LandfillDefs,
												   LoadBalancerPid ),


	% Result of the request waited later, for increased parallelism:
	GISPid ! { declarePOIs, [ Landfills ], self() },


	report( " - generating definitions for ~B industrial waste sources",
			[ IndustrialSourceCount ], State ),

	IndustrialSourceDefs = class_IndustrialWasteSource:generate_definitions(
						 IndustrialSourceCount, LocationGeneratorPid, GISPid ),

	%io:format( "IndustrialSourceDefs = ~p~n", [ IndustrialSourceDefs ] ),

	report( " - creating these ~B industrial waste sources",
			[ IndustrialSourceCount ], State ),

	IndustrialSources = class_Actor:create_initial_actors( IndustrialSourceDefs,
														   LoadBalancerPid ),

	GISPid ! { declarePOIs, [ IndustrialSources ], self() },


	report( " - generating definitions for ~B residential waste sources",
			[ ResidentialSourceCount ], State ),

	ResidentialSourceDefs = class_ResidentialWasteSource:generate_definitions(
						 ResidentialSourceCount, LocationGeneratorPid, GISPid ),

	%io:format( "ResidentialSourceCount = ~p~n", [ ResidentialSourceCount ] ),

	report( " - creating these ~B residential waste sources",
			[ ResidentialSourceCount ], State ),

	ResidentialSources = class_Actor:create_initial_actors(
						   ResidentialSourceDefs, LoadBalancerPid ),

	GISPid ! { declarePOIs, [ ResidentialSources ], self() },


	report( " - generating definitions for ~B base road junctions",
			[ RoadJunctionCount ], State ),

	RoadJunctionDefs = class_RoadJunction:generate_definitions(
						 RoadJunctionCount, LocationGeneratorPid, GISPid ),

	%io:format( "RoadJunctionDefs = ~p~n", [ RoadJunctionDefs ] ),

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

	%io:format( "WasteTruckDefs = ~p~n", [ WasteTruckDefs ] ),

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

	report( "Warning: this creation does not include weather-related models.",
		  State ),

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

	% First, add the most natural links, based on proximity, for all road
	% junctions:
	%
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



% Creates base road definitions from specified POI(s) to specified POI(s).
%
get_road_defs( From, To ) when is_list( From ) andalso is_pid( To ) ->
	[ { F, To } || F <- From ];

get_road_defs( From, To ) when is_pid( From ) andalso is_list( To ) ->
	[ { From, T } || T <- To ].



% Returns { CellsPerEdge, CellCount }.
%
get_cell_infos( Length, Width ) ->

	MinCellsForLength = math_utils:ceiling( Length / ?weather_cell_length ),

	MinCellsForWidth = math_utils:ceiling( Width / ?weather_cell_length ),

	CellsPerEdge = max( MinCellsForLength, MinCellsForWidth ),

	CellCount = CellsPerEdge * CellsPerEdge,

	{ CellsPerEdge, CellCount }.

	% If one wants to silence the weather part of the simulation:
	%{ 0, 0 }.



% Reports specified message.
%
% Centralised to be easily enabled/disabled.
%
-spec report( string(), wooper:state() ) -> basic_utils:void().
report( Message, State ) ->

	?report( Message ),

	% We may notify the plugins as well:
	class_PluginManager:notify_case_specific( city_generation, Message ).



-spec report( string(), list(), wooper:state() ) -> basic_utils:void().
report( FormatString, Parameters, State ) ->

	Message = text_utils:format( FormatString, Parameters ),

	report( Message, State ).



% Replaces, in the last position of specified arguments, the specified PID by
% the specified reference.
%
% (helper)
%
replace_in_last_arg( Pid, Ref, Args ) ->

	[ Pid | T ] = lists:reverse( Args ),

	lists:reverse( [ Ref | T ] ).
