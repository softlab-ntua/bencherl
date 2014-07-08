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



% Class modelling a landfill.
%
-module(class_Landfill).


% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_Actor, class_WasteUnloadingPoint,
								class_PointOfInterest ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Name, Location, GISPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, delete/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 unloadWaste/4 ).


% Static method declarations.
-define( wooper_static_method_export, generate_definitions/3 ).


% Exported helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.Landfill").




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For waste_tank() and al:
-include("city_example_types.hrl").


% Attributes of an instance of a landfill are:
%
% - probe_pid :: class_Probe:probe_pid() is the PID (if any) of the probe
% declared to track waste stocks in this incinerator
%
%
% Inherited attributes of interest:
%
% - waste_capacity :: [ waste_capacity() ] is a plain list storing the state of
% the waste storage tanks (inherited twice, wanted once)



% Implementation notes:
%
% A landfill is the final stage of waste treatment: all kinds of waste are
% stored then, mostly expected to be bottom ash. The capacity of a landfill is
% generally huge.






% Creates a new landfill.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this landfill (as a plain string)
%
% - Location: the (static) location of this landfill
%
% - CapacityInformation describes the waste storage capacity of this landfill
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:static_location(), pid() ) ->
					   wooper_state().
construct( State, ActorSettings, Name, Location, GISPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	{ CapacityInformation, TankCurveNames } = build_capacity(),

	UnloadState = class_WasteUnloadingPoint:construct( ActorState, Location,
												   CapacityInformation ),

	PointState = class_PointOfInterest:construct( UnloadState, Name, Location,
												 GISPid ),


	% For probe labels: duration of one tick; milliseconds needed:
	TickDuration = text_utils:duration_to_string(
			   1000 * class_Actor:convert_ticks_to_seconds( 1, PointState ) ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	WasteStockProbePid = class_Actor:declare_probe(
				_Name=io_lib:format( "~s Waste Stock Probe", [ Name ] ),
				_Curves=TankCurveNames,
				_Zones=[],
				_Title=io_lib:format( "Waste Storage Monitoring "
									  "for Landfill ~s", [ Name ] ),
				_XLabel=io_lib:format(
					  "Simulation tick: one tick corresponds to ~s",
					  [ TickDuration ] ),
				_YLabel="Tons of wastes in each tank of this landfill" ),

	setAttributes( PointState, [

			{waste_capacity,CapacityInformation},
			{probe_pid,WasteStockProbePid},
			{color,orange},
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



% First scheduling on an landfill.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper_state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% A landfill is mostly passive.

	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, ?getAttr(current_tick_offset) }

	end,

	?trace_fmt( "Landfill just created: ~s", [ to_string( State ) ] ),

	% To record initial state in probe:
	PlanState = class_Actor:scheduleNextSpontaneousTick( State ),

	?wooper_return_state_only( PlanState ).




% The definition of the spontaneous behaviour of this landfill.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	% No spontaneous life by itself (mostly triggered).
	send_data_to_probe( State ),

	?wooper_return_state_only( State ).



% Tries to unload to this landfill as much as possible of the specified mass of
% specified waste type (possibly any) into the caller (which is expected to be a
% waste transport requesting to empty its waste).
%
% The answer (the actor message sent back) will be:
%
% - either a notifyUnloadedWaste to acknowledge for good the waste transaction
%
% - or a notifyNoUnloadedWaste to report that no waste unloading will occur this
% time (transaction failed)
%
% (actor oneway)
%
-spec unloadWaste( wooper_state(), waste_type(), unit_utils:tons(), pid() ) ->
					   class_Actor:actor_oneway_return().
unloadWaste( State, WasteType, ProposedMass, WasteUnloaderPid ) ->

	% First call the parent base implementation:
	ParentState = executeOnewayWith( State, class_WasteUnloadingPoint,
			unloadWaste, [ WasteType, ProposedMass, WasteUnloaderPid ] ),

	% Then update the probe:
	send_data_to_probe( ParentState ),

	?wooper_return_state_only( ParentState ).



% Static methods section.



% Generates a list of instance definitions for the specified number of initial
% landfills.
%
% (static)
%
-spec generate_definitions( basic_utils:count(), pid(), pid() ) ->
							 [ class_Actor:instance_creation_spec() ].
generate_definitions( LandfillCount, LocationGeneratorPid, GISPid ) ->

	% Triggers the location generation request in parallel:
	LocationGeneratorPid ! { generateNonAdjacentLocations,
			   [ LandfillCount,
				get_min_distance_between_landfills_and_others(),
				get_min_distance_between_two_landfills() ], self() },

	define_landfills( LandfillCount, GISPid, _Acc=[] ).



define_landfills( _LandfillCount=0, GISPid, Acc ) ->

	% All landfills defined, adding locations as returned by the
	% generateNonAdjacentLocations request:
	receive

		{ wooper_result, Locations } when is_list( Locations ) ->

			% Creates now the full construction parameters:
			merge_parameters( Acc, Locations, GISPid )

	end;

define_landfills( LandfillCount, GISPid, Acc ) ->

	% Defines the build parameters for a new landfill; we want to end up with a
	% list of { class_Landfill, [ Name, Location ] } elements.

	Name = io_lib:format( "Landfill-~B", [ LandfillCount ] ),

	define_landfills( LandfillCount - 1, GISPid, [ Name | Acc ] ).



% Adds the location to the landfill build parameters (a kind of zip
% operation):
%
merge_parameters( Params, Locations, GISPid ) ->
	merge_parameters( Params, Locations, _Acc=[], GISPid ).


merge_parameters( _Params=[], _Locations=[], Acc, _GISPid ) ->
	Acc;

merge_parameters( _Params=[ Name | Tp ], _Locations=[ Loc | Tl ], Acc,
				  GISPid ) ->

	NewLandfillDef = { class_Landfill, [ Name, { wgs84_cartesian, Loc },
										 GISPid ] },

	merge_parameters( Tp, Tl, [ NewLandfillDef | Acc ], GISPid ).




% In meters:
%
get_min_distance_between_landfills_and_others() ->
	40.


% In meters:
%
get_min_distance_between_two_landfills() ->
	300.



% Sends waste data to probe (if any).
%
% (helper)
%
-spec send_data_to_probe( wooper_state() ) -> basic_utils:void().
send_data_to_probe( State ) ->

	% Avoid doing useless operations:
	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->

			% Already correctly ordered by design:
			TankList = ?getAttr(waste_capacity),

			WasteStockSample = list_to_tuple( [
				  Tank#waste_tank.current_mass_stored || Tank <- TankList ]),

			class_Probe:send_data( ProbePid, ?getAttr(current_tick_offset),
								  WasteStockSample )

	end.



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper_state() ) -> string().
to_string( State ) ->

	CapacityInfo = waste_utils:waste_capacity_to_string(
					 ?getAttr(waste_capacity) ),

	io_lib:format( "Landfill '~s' located at ~s making use of ~s",
				   [ ?getAttr(name),
					 class_GeolocalizedElement:interpret_location( State ),
					 CapacityInfo ] ).



% Returns a pair made of the waste capacities for a landfill, and a list of
% corresponding curve descriptions.
%
build_capacity() ->

	% We create one (big) waste tank for each waste type:
	create_waste_tank( waste_utils:get_waste_types(), _AccTank=[], _AccDesc=[],
					   _Count=0 ).


create_waste_tank( _WasteType=[], AccTank, AccDesc, _Count ) ->
	{ AccTank, lists:reverse( AccDesc ) };

create_waste_tank( _WasteType=[ Type | T ], AccTank, AccDesc, Count ) ->

	Id = Count + 1,

	% All tanks start initially empty, and are huge:
	NewTank = #waste_tank{

		id=Id,

		allowed_types=[ Type ],

		current_type=none,

		current_volume_stored=0.0,

		max_volume_stored=5000000000.0,

		% The current mass of waste stored:
		current_mass_stored=0.0,

		% The maximum mass of waste stored:
		max_mass_stored=40000000000.0,

		% Tells whether the tank is being processed (used) or idle:
		busy=false

	},

	NewDesc = io_lib:format( "Quantity of waste stored in waste tank #~B "
							  "used for waste type '~s' (in tons)",
							  [ Id, Type ] ),

	create_waste_tank( T, [ NewTank | AccTank ], [ NewDesc | AccDesc ],
					   Count + 1 ).
