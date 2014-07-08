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



% Class modelling an incinerator.
%
-module(class_Incinerator).


% For waste_tank() and al:
-include("city_example_types.hrl").



% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_Actor, class_WasteLoadingPoint,
						 class_WasteUnloadingPoint, class_PointOfInterest ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, IncineratorName, Location,
		 CapacityInformation, BurnerInformation, GISPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, delete/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 loadWaste/4, unloadWaste/4, toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, generate_definitions/3 ).


% Exported helpers:
-export([ to_string/1, get_supported_waste_types/1,
		  get_current_waste_stored/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.Incinerator").


% Allows to use macros for trace sending (to be included after the WOOPER
% header):
%
-include("class_TraceEmitter.hrl").



% Burner type, part of an incinerator:
%
-record( burner, {

		% The types of wastes that can be burnt in this burner:
		incinerable_types :: [ incinerable_waste() ],

		% Maximum mass that can be burnt during a single phase:
		burning_capacity :: unit_utils:tons(),

		% No 'burning_duration :: unit_utils:minutes()' defined, as it depends
		% on the actual burning mass.

		% Records at what time the current incineration (if any) is to finish:
		end_of_incineration :: 'undefined'
							   | { class_TimeManager:tick_offset(), tank_id() },

		% Records the type and quantity of waste being incinerated, so that the
		% corresponding bottom ash can be generated once incineration is over:
		waste_consumed :: { waste_type(), unit_utils:tons() }

} ).


-type burner() :: #burner{}.

-type burner_capacity() :: [ burner() ].



% Attributes of an instance of an incinerator are:
%
% - ash_tank_id :: tank_id() is the ID of the ash tank (for faster look-ups)
%
% - burners :: [ burner() ] describes the burners this incinerator uses
%
% - deadlines :: [ class_TimeManager:tick_offset() ] allows to check the
% incineration scheduling (fully optional)
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
% An incinerator is a waste unloading point (trucks bring wastes to it), but
% also a waste loading point (as the byproducts of an incineration cannot
% accumulate in an incinerator).
%
% As a result, we must override the methods to load and unload waste, as we do
% not want a truck to pick up input waste from it, or to unload bottom ash in
% it.


% We consider that at any time up to one burner can operate on a given tank.
%
% Tank curves are defined according to their original order. Bottom ash one is
% always the last.




% Creates a new incinerator.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this incinerator (as a plain string)
%
% - Location: the (static) location of this incinerator
%
% - CapacityInformation describes the waste storage capacity of this incinerator
%
% - BurnerInformation describes the burners installed in this incinerator
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				 class_Actor:name(), class_GIS:static_location(),
				 waste_capacity(), burner_capacity(), pid() ) -> wooper_state().
construct( State, ActorSettings, Name, Location, CapacityInformation,
		  BurnerInformation, GISPid ) ->

	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	LoadState = class_WasteLoadingPoint:construct( ActorState, Location,
												  _CapacityInformation=[] ),

	UnloadState = class_WasteUnloadingPoint:construct( LoadState, Location,
												   _CapacityInformation=[] ),

	PointState = class_PointOfInterest:construct( UnloadState, Name, Location,
												 GISPid ),

	{ Tanks, TankCurves } = manage_capacity_information( CapacityInformation ),

	[ waste_utils:check_waste_tank( T ) || T <- Tanks ],

	Burners = manage_burner_information( BurnerInformation ),

	% For probe labels: duration of one tick; milliseconds needed:
	TickDuration = text_utils:duration_to_string(
			   1000 * class_Actor:convert_ticks_to_seconds( 1, PointState ) ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	WasteStockProbePid = class_Actor:declare_probe(
				_Name=io_lib:format( "~s Waste Stock Probe", [ Name ] ),
				_Curves=TankCurves,
				_Zones=[],
				_Title=io_lib:format( "Waste Storage & Treatment Monitoring "
									  "for Incinerator ~s", [ Name ] ),
				_XLabel=io_lib:format(
					  "Simulation tick: one tick corresponds to ~s",
					  [ TickDuration ] ),
				_YLabel="Tons of wastes in each tank of this incinerator" ),

	AshTankId = length( CapacityInformation ) + 1,

	setAttributes( PointState, [

			{ waste_capacity, Tanks },
			{ ash_tank_id, AshTankId },
			{ burners, Burners },
			{ deadlines, [] },
			{ probe_pid, WasteStockProbePid },
			{ color, red },
			{ trace_categorization,
			 text_utils:string_to_binary(?TraceEmitterCategorization) }

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

	% No diasca-direct behaviour needed, real behaviour to start at the next
	% tick:
	%
	PlanState = class_Actor:scheduleNextSpontaneousTick( State ),

	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, ?getAttr(current_tick_offset) }

	end,

	?trace_fmt( "Incinerator just created: ~s", [ to_string( State ) ] ),

	% Creates an initial deadline at the tick, to trigger the burners:
	?wooper_return_state_only( setAttribute( PlanState, deadlines,
							   [ ?getAttr(current_tick_offset) + 1 ] ) ).




% The definition of the spontaneous behaviour of this incinerator.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	Deadlines = ?getAttr(deadlines),

	ProcessedState = case lists:member( CurrentTickOffset, Deadlines ) of

		true ->

			% At least one burner is expected to have finished its work, let's
			% find it and possibly assign it (them) to new incineration(s):
			apply_deadline( CurrentTickOffset, State );

		false ->
			throw( { unplanned_scheduling, CurrentTickOffset, Deadlines } )

	end,

	send_data_to_probe( ProcessedState ),

	?wooper_return_state_only( ProcessedState ).




% Tries to load from this incinerator point as much as possible of the specified
% mass compatible with specified waste type (actually only bottom ash can be
% loaded from an incinerator) into the calling actor, which is expected to be a
% waste transport, located in this point, looking for additional waste.
%
% The answer (the actor message sent back) will be:
%
% - either a notifyLoadedWaste to acknowledge once for good the waste
% transaction
%
% - or a notifyNoLoadedWaste to report that no waste loading will occur this
% time (transaction failed)
%
% (actor oneway)
%
-spec loadWaste( wooper_state(), waste_type(), unit_utils:tons(), pid() ) ->
					   class_Actor:actor_oneway_return().
loadWaste( State, WasteType, MaxWantedMass, WasteLoaderPid ) when
	  WasteType =:= none orelse WasteType =:= bottom_ash ->

	% A truck at an incinerator will attempt to load any waste, but we do not
	% want it to fetch input waste for the incinerator, only output one,
	% i.e. bottom ash, so basically we call the parent method with an additional
	% constraint:
	%
	ParentState = executeOnewayWith( State, class_WasteLoadingPoint, loadWaste,
						  [ bottom_ash, MaxWantedMass, WasteLoaderPid ] ),

	send_data_to_probe( ParentState ),

	?wooper_return_state_only( ParentState );


loadWaste( State, _WasteType, _MaxWantedMass, WasteLoaderPid ) ->

	% Waste type does not match loadable ones, we do not want the truck to fetch
	% input waste:
	NewState = class_Actor:send_actor_message( WasteLoaderPid,
								 notifyNoLoadedWaste, State ),

	?wooper_return_state_only( NewState ).



% Tries to unload to this unloading point as much as possible of the specified
% mass of specified waste type into the caller (which is expected to be a waste
% transport requesting to empty its waste).
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
unloadWaste( State, _WasteType=bottom_ash, _ProposedMass, WasteUnloaderPid ) ->

	% No unloading of bottom ash in an incinerator permitted!
	NewState = class_Actor:send_actor_message( WasteUnloaderPid,
								 notifyNoUnloadedWaste, State ),

	?wooper_return_state_only( NewState );


unloadWaste( State, WasteType, ProposedMass, WasteUnloaderPid ) ->

	% A truck at an incinerator may attempt to unload any waste that is not
	% bottom ash, so basically we call the parent method here:
	%
	ParentState = executeOnewayWith( State, class_WasteUnloadingPoint,
				   unloadWaste, [ WasteType, ProposedMass, WasteUnloaderPid ] ),

	send_data_to_probe( ParentState ),

	?wooper_return_state_only( ParentState ).



% Returns a textual description of this instance.
%
% (const request)
%
-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).



% Applies the deadline: find the burners having finished, updates the
% corresponding tanks.
%
% (helper)
%
apply_deadline( CurrentTickOffset, State ) ->

	{ AddedAsh, Burners, Tanks } = apply_finished_burners( ?getAttr(burners),
		 ?getAttr(waste_capacity), _AccAsh=0.0, _AccBurners=[],
		 CurrentTickOffset, State ),

	[ waste_utils:check_waste_tank( T ) || T <- Tanks ],

	% Extracts and updates ash tank:
	{ InputTanks, UpdatedAshTank } = update_ash_tank( AddedAsh, Tanks, State ),

	% Searches what should be incinerated next:
	{ IdleBurners, ActiveBurners } = split_idle_burners( Burners ),

	% Sorts tanks by decreasing load factors:
	{ SortedTanks, OtherTanks } = get_sorted_tanks( InputTanks ),

	% Now burners in PreviouslyIdleBurners are updated, they may or may not be
	% idle now:
	{ PreviouslyIdleBurners, UpdatedTanks, IncinerationTicks } = assign_burners(
				IdleBurners, SortedTanks, State ),

	[ waste_utils:check_waste_tank( T ) || T <- UpdatedTanks ],

	?trace_fmt( "Applying deadline, having ~B burners (~B active, ~B idle) and "
				"~B tank candidates (~B being empty or busy, last one being "
				"for ashes): ~B incineration(s) started.",
				[ length( Burners ),     length( ActiveBurners ),
				  length( IdleBurners ), length(SortedTanks),
				  length( OtherTanks ),  length( IncinerationTicks ) ] ),

	% Ticks will be automatically uniquified:
	PlanState = class_Actor:add_spontaneous_ticks( IncinerationTicks, State ),

	% Multiple burners can trigger at the same tick:
	NewDeadlines = list_utils:uniquify( IncinerationTicks ++
			lists:delete( CurrentTickOffset, ?getAttr(deadlines) ) ),

	setAttributes( PlanState, [
			{ burners, ActiveBurners ++ PreviouslyIdleBurners },
			{ waste_capacity, [ UpdatedAshTank | OtherTanks ++ UpdatedTanks ] },
			{ deadlines, NewDeadlines }

						   ] ).



% Returns updated burners and tanks (as a pair), once the current incinerations
% are just over.
%
% Iterates on the burners.
%
apply_finished_burners( _Burners=[], Tanks, AccAsh, AccBurners,
					   _CurrentTickOffset, _State ) ->
	{ AccAsh, AccBurners, Tanks };

apply_finished_burners( _Burners=[ Burner | OtherBurners ], Tanks, AccAsh,
					   AccBurners, CurrentTickOffset, State ) ->

	case Burner#burner.end_of_incineration of

		{ CurrentTickOffset, TankID } ->

			%?trace_fmt( "End of incineration for burner ~p.~n", [ Burner ] ),

			{ FoundTank, OtherTanks } = remove_tank_by_id( TankID, Tanks ),

			{ UpdatedTank, AddedAsh } = update_tank_after_burning( FoundTank,
								Burner, State ),

			% This burner just finished operating on the tank:
			NewBurner = Burner#burner{
						end_of_incineration=undefined,
						waste_consumed=undefined },

			% Just iterate now:
			apply_finished_burners( OtherBurners, [ UpdatedTank | OtherTanks ],
				AccAsh + AddedAsh, [ NewBurner | AccBurners ],
				CurrentTickOffset, State );

		_ ->
			% Either idle or still incinerating:
			apply_finished_burners( OtherBurners, Tanks, AccAsh,
					   [ Burner | AccBurners ], CurrentTickOffset, State )

	end.



% Returns { InputTanks, UpdatedAshTank } where UpdatedAshTank is the ash tank
% once updated with the specified quantity of bottom ash being added and
% InputTanks are a list of the other tanks.
%
update_ash_tank( AddedAsh, Tanks, State ) ->

	%io:format( "~f tons of bottom ashes were produced.~n", [ AddedAsh ] ),

	case AddedAsh < 0 of

		true ->
			throw( { negative_ash_produced, AddedAsh } );

		false ->
			ok

	end,

	% Extracts and updates ash tank:
	{ AshTank, OtherTanks } = remove_tank_by_id( ?getAttr(ash_tank_id), Tanks ),

	MaxMass = AshTank#waste_tank.max_mass_stored,

	UpdatedAshTank = case AshTank#waste_tank.current_mass_stored + AddedAsh of

		TooMuch when TooMuch > MaxMass ->
			% We do not want this benchmarking case to fail because of a
			% non-connected enough incinerator:
			%?error_fmt( "Incinerator full, exceeding maximum bottom ash "
			%			"capacity (~f tons over a maximum of ~f).",
			%			[ TooMuch, MaxMass ] ),
			%throw( { incinerator_overflow, ?getAttr(name), TooMuch,
			%   MaxMass } );

			% So we just saturate here:
			AshTank#waste_tank{ current_mass_stored=MaxMass,
								current_type=bottom_ash };

		NewMass when NewMass > 0.0 ->
			AshTank#waste_tank{ current_mass_stored=NewMass,
								current_type=bottom_ash };

		0.0 ->
			AshTank#waste_tank{ current_mass_stored=0.0,
								current_type=none }

	end,

	{ OtherTanks, UpdatedAshTank }.



% Sorts burners according to their operating state.
%
% Returns { IdleBurners, ActiveBurners }.
%
split_idle_burners( Burners ) ->
	split_idle_burners( Burners, _AccIdle=[], _AccActive=[] ).


split_idle_burners( _Burners=[], AccIdle, AccActive ) ->
	{ AccIdle, AccActive };

split_idle_burners( _Burners=[ B=#burner{ end_of_incineration=undefined } | T ],
							   AccIdle, AccActive ) ->
	split_idle_burners( T, [ B | AccIdle ], AccActive );

split_idle_burners( _Burners=[ B | T ], AccIdle, AccActive ) ->
	split_idle_burners( T, AccIdle, [ B | AccActive ] ).





% Returns a pair made of the updated specified tank to take into account
% specified burnt mass, and the resulting mass of bottom ash produced.
%
% (state needed for traces)
%
update_tank_after_burning( Tank,
			#burner{ waste_consumed={ WasteType, BurntMass } }, _State ) ->

	% The tank mass has already been decreased, when the incineration started.

	AshAdded = compute_ash_produced( BurntMass, WasteType ),

	{ Tank#waste_tank{ busy=false }, AshAdded }.




% Static methods section.




% Generates a list of instance definitions for the specified number of initial
% incinerators.
%
% (static)
%
-spec generate_definitions( basic_utils:count(), pid(), pid() ) ->
							 [ class_Actor:instance_creation_spec() ].
generate_definitions( IncineratorCount, LocationGeneratorPid, GISPid ) ->

	% Triggers the location generation request in parallel:
	LocationGeneratorPid ! { generateNonAdjacentLocations,
			   [ IncineratorCount,
				get_min_distance_between_incinerators_and_others(),
				get_min_distance_between_two_incinerators() ], self() },

	define_incinerators( IncineratorCount, GISPid, _Acc=[] ).



define_incinerators( _IncineratorCount=0, GISPid, Acc ) ->

	% All incinerators defined, adding locations as returned by
	% the generateNonAdjacentLocations request:
	receive

		{ wooper_result, Locations } when is_list( Locations ) ->

			% Creates now the full construction parameters:
			merge_parameters( Acc, Locations, GISPid )

	end;

define_incinerators( IncineratorCount, GISPid, Acc ) ->

	% Defines the build parameters for a new incinerator; we want to end up with
	% a list of { class_Incinerator, [ Name, Location, WasteTanks ] } elements.

	Name = io_lib:format( "Incinerator-~B", [ IncineratorCount ] ),

	DrawnTankCount = class_RandomManager:get_positive_integer_gaussian_value(
											   _Mean=3, _Variance=2 ),

	% At least one tank, no more than eight:
	TankCount = math_utils:clamp( 1, 8, DrawnTankCount ),

	CapacityInformation = generate_tanks( TankCount ),

	% 3 burners on average, no more burners than tanks:
	DrawnBurnerCount =
	  class_RandomManager:get_positive_integer_exponential_value( _Lambda=1/3 ),

	BurnerCount = math_utils:clamp( 1, TankCount, DrawnBurnerCount ),

	BurnerInformation = generate_burners( BurnerCount, CapacityInformation ),

	% Location to be added later:
	NewAcc = [ { Name, CapacityInformation, BurnerInformation } | Acc ],

	define_incinerators( IncineratorCount - 1, GISPid, NewAcc ).



% Adds the location to the incinerator build parameters (a kind of zip
% operation):
%
merge_parameters( Params, Locations, GISPid ) ->

	merge_parameters( Params, Locations, _Acc=[], GISPid ).


merge_parameters( _Params=[], _Locations=[], Acc, _GISPid ) ->
	Acc;

merge_parameters( _Params=[ { Name, CapacityInfo, BurnerInfo } | Tp ],
				  _Locations=[ Loc | Tl ], Acc, GISPid ) ->

	NewIncineratorDef = { class_Incinerator, [ Name, { wgs84_cartesian, Loc },
										  CapacityInfo, BurnerInfo, GISPid ] },

	merge_parameters( Tp, Tl, [ NewIncineratorDef | Acc ], GISPid ).



% Generates the specified number of waste tanks, to form a capacity information.
%
generate_tanks( TankCount ) ->
	generate_tanks( TankCount, _Acc=[] ).


generate_tanks( _TankCount=0, Acc ) ->
	Acc;

generate_tanks( TankCount, Acc ) ->

	% Determines the settings for the current tank:

	% In cubic meters:
	{ MinPossibleVolume, MaxPossibleVolume } = { 500, 20000 },
	Volume = MinPossibleVolume + class_RandomManager:get_uniform_value(
							   MaxPossibleVolume - MinPossibleVolume + 1 ),

	% In tons:
	MeanMass = 30,
	VarianceMass = 3,
	Mass = class_RandomManager:get_gaussian_value( MeanMass, VarianceMass ),

	MaxVolume = math_utils:round_after( float( Volume ), 0 ),
	MaxMass = math_utils:round_after( Mass, 0 ),

	AllowedWasteTypes = get_waste_types_for_tank(),

	% All tanks start full and idle:
	%
	% (one can test by making them initially empty as well)
	%
	NewTank = #waste_tank{

		% id are in [1;N] where N corresponds to the bottom ash tank:
		id=TankCount,

		allowed_types = AllowedWasteTypes,

		% Change it too if ever setting non-empty initial incinerators:
		%current_type = none,
		current_type = hd( AllowedWasteTypes ),

		%current_volume_stored = 0.0,
		current_volume_stored = MaxVolume,

		max_volume_stored = MaxVolume,

		%current_mass_stored = 0.0,
		current_mass_stored = MaxMass,

		max_mass_stored = MaxMass,

		busy = false

						  },

	generate_tanks( TankCount - 1 , [ NewTank | Acc ] ).



% Returns the set of waste types that this incinerator can burn.
%
% (helper)
%
-spec get_supported_waste_types( wooper_state() ) -> [ waste_type() ].
get_supported_waste_types( State ) ->

	Tanks = ?getAttr(waste_capacity),

	list_waste_types( Tanks ).


% Helper:
list_waste_types( Tanks ) ->
	list_waste_types( Tanks, _Accs=[] ).


list_waste_types( _Tanks=[], Acc ) ->
	Acc;

list_waste_types( _Tanks=[ #waste_tank{ allowed_types=AllowedTypes } | T ],
				  Acc ) ->
	NewAcc = add_waste_types( AllowedTypes, Acc ),
	list_waste_types( T, NewAcc ).


add_waste_types( _AllowedTypes=[], Acc ) ->
	Acc;

add_waste_types( _AllowedTypes=[ Type | T ], Acc ) ->

	NewAcc = case lists:member( Type, Acc ) of

		true ->
					 Acc;

		false ->
					 [ Type | Acc ]

	end,

	add_waste_types( T, NewAcc ).



% Returns the specified list of tanks, ordered by increasing ID.
%
-spec list_ordered_tanks( [ waste_tank() ] ) -> [ waste_tank() ].
list_ordered_tanks( Tanks ) ->

	% We could abuse the record-tag system instead.

	TankIdList = [ { T, T#waste_tank.id } || T <- Tanks ],

	SortedTankIdList = lists:keysort( _Index=2, TankIdList ),

	% SortedTankList:
	[ T || { T, _Id } <- SortedTankIdList ].



% Returns the type and associated mass of all wastes stored into this
% incinerator, in an aggregated list (i.e. a waste type is listed at most once).
%
% (helper)
%
-spec get_current_waste_stored( wooper_state() ) ->
						   [ { waste_type(), unit_utils:tons() } ].
get_current_waste_stored( State ) ->

	Tanks = ?getAttr(waste_capacity),

	totalize_waste( Tanks, _Acc=[] ).


% Helper:
totalize_waste( _Tanks=[], Acc ) ->
	Acc;

totalize_waste(
	  _Tanks=[ #waste_tank{ current_type=Type, current_mass_stored=Mass } | T ],
	  Acc ) ->

	NewAcc = case lists:keyfind( Key=Type, Index=1, Acc ) of

		{ Type, CurrentMass } ->

			lists:keyreplace( Key, Index, Acc,
							 _NewTuple={ Type, CurrentMass + Mass } );

		false ->
			[ { Type, Mass } | Acc ]

	end,

	totalize_waste( T, NewAcc ).



% Generates the specified number of waste burners, to form a burner information.
%
generate_burners( BurnerCount, CapacityInformation ) when BurnerCount >= 1 ->

	WasteTypesToCover = list_waste_types( CapacityInformation ),

	generate_burners( BurnerCount, WasteTypesToCover, _BurnerAcc=[] ).



% We need to ensure that all waste types can be incinerated, so we have here at
% least one general-purpose burner:
%
generate_burners( _BurnerCount=1, WasteTypesToCover, BurnerAcc ) ->

	NewBurner = #burner{

						% All stored types are covered here:
						incinerable_types = WasteTypesToCover,

						% Lower capacity if general-purpose (in [2,4] tons):
						burning_capacity = float( 1
							+ class_RandomManager:get_uniform_value( 3 ) ),

						% Starts idle:
						end_of_incineration = undefined,

						waste_consumed = undefined

						  },

	[ NewBurner | BurnerAcc ];


generate_burners( BurnerCount, WasteTypesToCover, BurnerAcc ) ->

	% Determines the settings for the current specialized burner:

	% These burners can incinerate a single type of waste:
	UniqueType = list_utils:draw_element( WasteTypesToCover ),

	% They have an higher burning capacity one average, in tons:
	MeanMass = 6,
	VarianceMass = 3,
	Mass = class_RandomManager:get_gaussian_value( MeanMass, VarianceMass ),

	NewBurner = #burner{ incinerable_types=[ UniqueType ],
						 burning_capacity=Mass },

	generate_burners( BurnerCount - 1 , WasteTypesToCover,
					  [ NewBurner | BurnerAcc ] ).



% Helper functions.


% Returns the corresponding tanks and their curve descriptions.
%
-spec manage_capacity_information( waste_capacity() ) ->
										 { [ waste_tank() ], [ string() ] }.
manage_capacity_information( CapacityInformation ) ->

	% We automatically add one additional tank, to output bottom ash:

	% All incinerators have to same kind of ash tank, initially empty:
	AshTank = #waste_tank{

		% First tank for an incinerator is the output (bottom ash) one:
		id=length(CapacityInformation) + 1,

		allowed_types=[ bottom_ash ],

		% Since is empty:
		current_type=none,

		current_volume_stored=0.0,

		max_volume_stored=5000.0,

		% The current mass of waste stored:
		current_mass_stored=0.0,

		% The maximum mass of waste stored:
		max_mass_stored=40.0,

		% Tells whether the task is being processed (used) or idle:
		busy=false

	},

	AshDesc = "Quantity of bottom ash stored in output tank (in tons)",

	manage_capacity_information( CapacityInformation, _AccTank=[ AshTank ],
								_AccDesc=[ AshDesc ] ).


manage_capacity_information( _CapacityInformation=[], AccTank, AccDesc ) ->

	% Tank order does not matter, but description order does:
	{ AccTank, AccDesc };

manage_capacity_information( _CapacityInformation=[
						Tank=#waste_tank{ id=Id, allowed_types=Types } | H ],
						AccTank, AccDesc ) ->

	waste_utils:check_waste_tank( Tank ),

	TankDesc = io_lib:format( "Quantity of waste stored in "
							  "tank #~B (in tons), allowed types being ~w",
							  [ Id, Types ] ),

	manage_capacity_information( H, [ Tank | AccTank ],
								[ TankDesc | AccDesc ] ).



% Returns the corresponding burners.
%
-spec manage_burner_information( [ burner() ] ) -> [ burner() ].
manage_burner_information( BurnerInformation ) ->
	manage_burner_information( BurnerInformation, _Acc=[] ).

manage_burner_information( _BurnerInformation=[], Acc ) ->
	Acc;

manage_burner_information( _BurnerInformation=[
				B=#burner{ incinerable_types=Types } | Burners ], Acc ) ->

	[ waste_utils:check_incinerable( T ) || T <- Types ],

	manage_burner_information( Burners, [ B | Acc ] ).



% Returns the waste types that a new tank may contain.
%
get_waste_types_for_tank() ->

	% Currently only one type of waste can be contain in any tank:

	% All types of incinerable waste have equal probability here:
	[ list_utils:draw_element(
		waste_utils:get_incinerable_waste_types() ) ].



% In meters:
%
get_min_distance_between_incinerators_and_others() ->
	40.


% In meters:
%
get_min_distance_between_two_incinerators() ->
	200.



% Returns { SortedIdleNonEmptyTanks, OtherTanks } where SortedIdleNonEmptyTanks
% are the tanks that are idle and non-empty (having waste to incinerate), sorted
% by decreasing load factor, and OtherTanks are the others.
%
% (helper)
%
get_sorted_tanks( Tanks ) ->

	% Will create a [ { Tank, LoadFactor } ] list in IdleNonEmptyTanksAcc:
	get_sorted_tanks( Tanks, _IdleNonEmptyTanksAcc=[], _OtherTanksAcc=[] ).



get_sorted_tanks( _Tanks=[], IdleNonEmptyTanksAcc, OtherTanksAcc ) ->

	% Here we just finished computing the load-factor and building the two
	% lists:

	% Non-empty tanks must be sorted by decreasing load factor:
	SortedIdleNonEmptyTanks = lists:reverse(
							lists:keysort( _Index=2, IdleNonEmptyTanksAcc ) ),

	% Now that sorted, removing the load factors:
	StrippedNonEmptyTanks = [ Tank
						 || { Tank, _LoadFactor } <- SortedIdleNonEmptyTanks ],

	{ StrippedNonEmptyTanks, OtherTanksAcc };


get_sorted_tanks( _Tanks=[ Tank | OtherTanks ], IdleNonEmptyTanksAcc,
				 OtherTanksAcc ) ->

	CurrentMass = Tank#waste_tank.current_mass_stored,

	% Selects non-empty, non-busy tanks:
	case CurrentMass /= 0.0 andalso not Tank#waste_tank.busy of

		true ->

			% Max mass is strictly positive:
			LoadFactor = CurrentMass / Tank#waste_tank.max_mass_stored,

			get_sorted_tanks( OtherTanks,
							 [ { Tank, LoadFactor } | IdleNonEmptyTanksAcc ],
							 OtherTanksAcc  );

		false ->
			get_sorted_tanks( OtherTanks, IdleNonEmptyTanksAcc,
							  [ Tank | OtherTanksAcc ] )

	end.



% Assigns idle burners to the most loaded tanks, and starts incineration.
%
% Returns { PreviouslyIdleBurners, UpdatedTanks,  IncinerationEndTicks }.
%
% Note: apparently carrying around a state is not necessary.
%
-spec assign_burners( [ burner() ], [ waste_tank() ], wooper_state() ) ->
	{  [ burner() ], [ waste_tank() ], [ class_TimeManager:tick_offset() ] }.
assign_burners( Burners, SortedTanks, State ) ->

	%io:format( "SortedTanks = ~p~n", [ SortedTanks ] ),

	% Iterates on burners:
	assign_burners( Burners, SortedTanks, _AccBurners=[], _AccTanks=[],
				   _AccTicks=[], State ).


assign_burners( _Burners=[], SortedTanks, AccBurners, AccTanks, AccTicks,
			   _State ) ->
	% No burner left:
	{ AccBurners, SortedTanks ++ AccTanks, AccTicks };

assign_burners( _Burners=[ Burner | OtherBurners ], SortedTanks, AccBurners,
				AccTanks, AccTicks, State ) ->

	case find_first_matching_tank( Burner#burner.incinerable_types,
								   SortedTanks ) of

		{ MatchingTank, OtherTanks } ->

			% Checkings:
			undefined = Burner#burner.end_of_incineration,
			undefined = Burner#burner.waste_consumed,

			%io:format( "For burner ~s, found tank ~s.~n",
			%	  [ burner_to_string( Burner ),
			%	   waste_utils:waste_tank_to_string( MatchingTank ) ] ),

			MassToBurn = min( Burner#burner.burning_capacity,
					MatchingTank#waste_tank.current_mass_stored ),

			TankWasteType = MatchingTank#waste_tank.current_type,

			IncinerationEndTick = ?getAttr(current_tick_offset)
				+ compute_incineration_duration( MassToBurn, TankWasteType,
												Burner, State ),

			UpdatedBurner = Burner#burner{

					end_of_incineration={ IncinerationEndTick,
										MatchingTank#waste_tank.id },

					waste_consumed= { TankWasteType, MassToBurn }

										  },

			% Note: the mass that will be burnt by a given incineration is
			% removed as soon as the incineration starts, otherwise it could be
			% loaded by a truck in-between:
			DepletedTank = waste_utils:remove_waste_from_tank( MatchingTank,
							   MassToBurn, TankWasteType ),

			UpdatedTank = DepletedTank#waste_tank{ busy=true },

			waste_utils:check_waste_tank( UpdatedTank ),

			% We must remove the tank from the candidate list, as the waste mass
			% will decrease only once the incineration will be over (otherwise
			% multiple burners could treat the same wastes):
			assign_burners( OtherBurners, OtherTanks,
							[ UpdatedBurner | AccBurners ],
							[ UpdatedTank | AccTanks ],
							[ IncinerationEndTick | AccTicks ], State );


		no_tank_found ->

			%io:format( "No tank found for burner '~s'.~n",
			%		   [ burner_to_string( Burner ) ] ),

			% Relevant tanks must be empty, burner will remain idle then:
			assign_burners( OtherBurners, SortedTanks, [ Burner | AccBurners ],
						   AccTanks, AccTicks, State )

	end.



% Returns { MatchingTank, OtherTanks } where MatchingTank is the first tank in
% the list that has waste that correspond to one of the specified waste types,
% and OtherTanks is the list of other tanks.
%
% If no such compatible tank is found, returns 'no_tank_found'.
%
-spec find_first_matching_tank( [ waste_type() ], [ waste_tank() ] ) ->
				'no_tank_found' | { waste_tank(), [ waste_tank() ] }.
find_first_matching_tank( WasteTypes, Tanks ) ->
	find_first_matching_tank( WasteTypes, Tanks, _Acc=[] ).


find_first_matching_tank( _WasteTypes, _Tanks=[], _Acc ) ->
	no_tank_found;

find_first_matching_tank( WasteTypes, _Tanks=[ Tank | T ], Acc ) ->

	case lists:member( Tank#waste_tank.current_type, WasteTypes ) of

		true ->
			% Tanks found!
			{ Tank, T ++ Acc };

		false ->
			find_first_matching_tank( WasteTypes, T, [ Tank | Acc ] )

	end.



% Returns the tank whose ID is the specified one, and the list without it:
% { FoundTank, OtherTanks }.
%
% (helper)
%
-spec remove_tank_by_id( tank_id(), [ waste_tank() ] ) ->
						 { waste_tank(), [ waste_tank() ] }.
remove_tank_by_id( TankID, Tanks ) ->
	remove_tank_by_id( TankID, Tanks, _Acc=[] ).


remove_tank_by_id( TankID, _Tanks=[], _Acc ) ->
	throw( { tank_id_not_found, TankID } );

remove_tank_by_id( TankID, _Tanks=[ Tank=#waste_tank{ id=TankID } | T ],
				  Acc ) ->
			{ Tank, T ++ Acc };

remove_tank_by_id( TankID, _Tanks=[ Tank | T ], Acc ) ->
			remove_tank_by_id( TankID, T, [ Tank | Acc ] ).




% Computes the duration needed to incinerate specified waste with specified
% burner.
%
-spec compute_incineration_duration( unit_utils:tons(), waste_type(), burner(),
					 wooper_state() ) -> class_TimeManager:tick_offset().
compute_incineration_duration( MassToBurn, _Wastetype=incinerable_waste_type_1,
							   _Burner, State ) ->

	SecondsNeeded = 2.7 * 240 * math:sqrt( 12 * MassToBurn ),

	class_Actor:convert_seconds_to_non_null_ticks( SecondsNeeded,
					  _RelaxedMaxRelativeError=0.03, State );


compute_incineration_duration( MassToBurn, _Wastetype=incinerable_waste_type_2,
							   _Burner, State ) ->

	SecondsNeeded = 5.8 * 240 * math:sqrt( 4 * MassToBurn ),

	class_Actor:convert_seconds_to_non_null_ticks( SecondsNeeded,
							_RelaxedMaxRelativeError=0.03, State ).



% Computes the mass of bottom ash produced due to the incineration of the
% specified mass of specified waste.
%
compute_ash_produced( BurntMass, _WasteType=incinerable_waste_type_1 ) ->
	0.05 * BurntMass;

compute_ash_produced( BurntMass, _WasteType=incinerable_waste_type_2 ) ->
	0.25 * BurntMass;

compute_ash_produced( BurntMass, _OtherWasteType ) ->
	0.11 * BurntMass.



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
			TankList = list_ordered_tanks( ?getAttr(waste_capacity) ),

			WasteStockSample = list_to_tuple( [
				  Tank#waste_tank.current_mass_stored || Tank <- TankList ]),

			class_Probe:send_data( ProbePid, ?getAttr(current_tick_offset),
						  WasteStockSample )

	end.



burners_to_string( Burners ) ->

	BurnerList = [ burner_to_string( B ) || B <- Burners ],

	io_lib:format( "~B burners:~s", [ length( BurnerList ),
						 text_utils:string_list_to_string( BurnerList ) ] ).



burner_to_string( #burner{ incinerable_types=WasteTypes,
						   burning_capacity=BurnCapacity,
						   end_of_incineration=End,
						   waste_consumed=WasteConsumed } ) ->

	Base = io_lib:format( "burner able to incinerate up to ~f tons "
				   "(approximatively) of wastes of type ~p",
				   [ math_utils:round_after( BurnCapacity, 2 ), WasteTypes ] ),

	case End of

		undefined ->
			undefined = WasteConsumed,
			Base ++ "; the burner is currently idle";

		{ EndTickOffset, TankId } ->
			{ WasteType, WasteQuantity } = WasteConsumed,
			% No state, no time conversion possible:
			Base ++ io_lib:format( "; the burner is in operation on "
								   "tank whose ID is ~B, until tick offset ~p,"
								   "incinerating ~f tons of waste of type ~p",
								   [ TankId, EndTickOffset, WasteQuantity,
									WasteType ] )

	end.




% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper_state() ) -> string().
to_string( State ) ->

	CapacityInfo = waste_utils:waste_capacity_to_string(
				list_ordered_tanks( ?getAttr(waste_capacity) ) ),

	BurnerInfo = burners_to_string( ?getAttr(burners) ),

	io_lib:format( "Incinerator '~s' located at ~s making use of a ~s "
				   "and relying on ~s",
				   [ ?getAttr(name),
					 class_GeolocalizedElement:interpret_location( State ),
					 CapacityInfo, BurnerInfo ] ).
