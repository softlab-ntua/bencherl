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



% Class modelling a waste loading point, i.e. a physical location from which
% wastes can be loaded, by garbage trucks.
%
-module(class_WasteLoadingPoint).


% For waste_tank() and al:
-include("city_example_types.hrl").


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_GeoContainer ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Location, CapacityInformation ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, delete/1 ).


% Method declarations.
-define( wooper_method_export, loadWaste/4 ).


% Static method declarations.
-define( wooper_static_method_export, ).



% Design notes:
%
% Even if usually the type of waste wanted to be loaded/unloaded for a loading
% point can be deduced, we specify the type of wastes that is targered
% nevertheless, for more later flexibility.


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.LoadingPoint").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Class-specific attributes of an instance of this class are:
%
% - waste_capacity :: [ waste_capacity() ] is a plain list storing the state of
% the waste storage tanks




% Creates a new waste loading point.
%
% Construction parameters are:
%
% - Location: the location of this loading point
%
% - CapacityInformation describes the waste storage capacity of this point
%
-spec construct( wooper_state(), class_GIS:location(), waste_capacity() ) ->
					   wooper_state().
construct( State, Location, CapacityInformation ) ->

	ContainerState = class_GeoContainer:construct( State, Location ),

	Tanks = manage_capacity_information( CapacityInformation, _Acc=[] ),

	setAttributes( ContainerState, [

					{ waste_capacity, Tanks }

									] ).



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.



% Tries to load from this loading point as much as possible of the specified
% mass compatible with specified waste type into the calling actor, which is
% expected to be a waste transport, located in this point, looking for
% additional waste.
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
loadWaste( State, WasteType, MaxWantedMass, WasteLoaderPid ) ->

	WasteTanks = ?getAttr(waste_capacity),

	[ waste_utils:check_waste_tank( T ) || T <- WasteTanks ],

	{ _State, DescString } = executeRequest( State, toString ),

	?trace_fmt( "Trying to load up to ~f tons of waste of "
				"type compatible with '~s' from the ~B tanks of ~s "
				"to docked waste transport ~w.~n",
				[ MaxWantedMass, WasteType, length( WasteTanks ),
				  DescString, WasteLoaderPid ] ),

	LoadState = case get_waste_from_tanks( WasteType, MaxWantedMass,
										   WasteTanks ) of

		false ->
			?trace_fmt( "No waste could be loaded to transport ~w.",
					   [ WasteLoaderPid ] ),
			class_Actor:send_actor_message( WasteLoaderPid,
								 notifyNoLoadedWaste, State );

		{ NewWasteTanks, RemainingFreeMass, Type } ->
			% Waste loaded:
			LoadedMass = MaxWantedMass - RemainingFreeMass,

			LoadingTickCount = get_loading_duration( Type, LoadedMass, State ),

			?trace_fmt( "Loading, to transport ~w, ~f tons of waste "
						"of type ~s, this will last for ~B ticks.",
						[ WasteLoaderPid, LoadedMass, Type,
						 LoadingTickCount ] ),

			SentState = class_Actor:send_actor_message( WasteLoaderPid,
					{ notifyLoadedWaste,
					  [ LoadedMass, Type, LoadingTickCount ] },
					State ),

			setAttribute( SentState, waste_capacity, NewWasteTanks )

	end,

	[ waste_utils:check_waste_tank( T ) ||
		T <- getAttribute( LoadState, waste_capacity ) ],

	{ _SameState, Desc } = executeRequest( LoadState, toString ),

	?trace_fmt( "After this loading attempt, new state is: ~s.",
				[ Desc ] ),

	?wooper_return_state_only( LoadState ).



% Does its best to retrieve the specified quantity of waste (compatible with the
% specified type) from the specified waste tanks.
%
% Returns either 'false' if no waste at all was taken from tanks, otherwise
% returns a triplet made of updated waste tanks, the remaining requested mass
% that could not be transferred (if any) and the overall type of the waste that
% has been loaded.
%
% (helper)
%
get_waste_from_tanks( WasteType, MaxWantedMass, WasteTanks ) ->
	get_waste_from_tanks( WasteType, MaxWantedMass, WasteTanks, _AccTanks=[],
						_LoadedWasteType=undefined ).


get_waste_from_tanks( _WasteType, _RemainingFreeMass, _WasteTanks=[], _AccTanks,
					  _LoadedWasteType=undefined ) ->
	% No loaded waste type defined, hence nothing loaded:
	false;

get_waste_from_tanks( _WasteType, RemainingFreeMass, _WasteTanks=[], AccTanks,
					  LoadedWasteType ) ->
	{ AccTanks, RemainingFreeMass, LoadedWasteType };


get_waste_from_tanks( WasteType, RemainingFreeMass, _WasteTanks=[
						Tank=#waste_tank{ current_mass_stored=0.0 } | T ],
					  AccTanks, LoadedWasteType ) ->

	% An empty tank is of no use here, continuing the iterating:
	get_waste_from_tanks( WasteType, RemainingFreeMass,  T, [ Tank | AccTanks ],
						  LoadedWasteType );


get_waste_from_tanks( WasteType, RemainingFreeMass, _WasteTanks=[
		  Tank=#waste_tank{ current_type=TankWasteType,
							current_mass_stored=CurrentTankMass } | T ],
					  AccTanks, LoadedWasteType ) ->


	% Non-empty tank here:
	case waste_utils:can_be_mixed( WasteType, TankWasteType ) of

		true ->

			% Yes, so let's empty this tank as much as possible:
			case CurrentTankMass > RemainingFreeMass of

				true ->

					% Here we will saturate the truck with this tank:
					UpdatedTank = waste_utils:remove_waste_from_tank( Tank,
										RemainingFreeMass ),

					% Truck full, hence no need to recurse more:
					NewTanks = [ UpdatedTank | T ] ++ AccTanks,
					% We update the type as well, otherwise we could keep the
					% one of the possibly empty truck which would then be
					% 'none':
					{ NewTanks, _NoMoreMass=0.0, TankWasteType };

				false ->
					% Here we will fully deplete the tank:
					UpdatedTank = waste_utils:remove_waste_from_tank( Tank,
										CurrentTankMass ),

					NewRemainingFreeMass = RemainingFreeMass - CurrentTankMass,

					% Same remark for the update of waste type:
					get_waste_from_tanks( WasteType, NewRemainingFreeMass,  T,
								[ UpdatedTank | AccTanks ], TankWasteType )

			end ;

		false ->
			% Unmatching waste type for this tank, let's continue then:
			get_waste_from_tanks( WasteType, RemainingFreeMass,  T,
								  [ Tank | AccTanks ], LoadedWasteType )

	end.



% Static methods section.




% Helper functions.


% Checks the specified waste capacity.
%
% (helper)
%
manage_capacity_information( _CapacityInformation=[], Acc ) ->
	Acc;

manage_capacity_information( _CapacityInformation=[ Tank | H ], Acc ) ->
	waste_utils:check_waste_tank( Tank ),
	manage_capacity_information( H, [ Tank | Acc ] ).



% Returns the duration needed, in ticks, for the loading of specified mass of
% specified waste type in a waste transport.
%
% (helper)
%
get_loading_duration( _WasteType, LoadedMass, State ) ->

	% A base of 20 minutes, plus 2 minutes per ton (the type does not matter in
	% this model):
	Seconds = ( 20 + 2 * LoadedMass ) * 60,

	class_Actor:convert_seconds_to_ticks( Seconds, State ).
