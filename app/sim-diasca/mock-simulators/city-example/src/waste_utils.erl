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



% Various facilities about waste management.
%
-module(waste_utils).


% Static defines:
%
-export([ get_green_house_gas_types/0, get_gas_types/0,
		  get_recyclable_waste_types/0, get_incinerable_waste_types/0,
		  get_treated_waste_types/0, get_waste_types/0,
		  get_energy_form_types/0, get_emission_types/0 ]).



% For the epsilon constant:
-include("math_utils.hrl").


% Waste-based operations:
%
-export([ can_be_mixed/2, can_produce/2, can_consume/2,
		  get_all_loading_point_types/0, get_all_unloading_point_types/0,
		  is_poi_type_loadable/1, is_poi_type_unloadable/1 ]).



% Checkings:
%
-export([ check_waste_type/1, check_waste_types/1,
		  check_waste_tank/1, check_waste_capacity/1,
		  check_incinerable/1 ]).



% Helpers for tank handling:
%
-export([ add_waste_to_tank/3, remove_waste_from_tank/2,
		  remove_waste_from_tank/3, is_tank_empty/1, is_tank_full/1 ]).



% Textual descriptions:
%
-export([ waste_capacity_to_string/1, waste_tank_to_string/1 ]).


% For waste_tank() and al:
-include("city_example_types.hrl").




% Implementation notes:
%
% Type lists could be determined directly from the type definitions listed in
% city_example_types.hrl.


% Green-house gases (GHG):
-spec get_green_house_gas_types() -> [ green_house_gas() ].
get_green_house_gas_types() ->
	[ 'carbon_dioxide', 'methane' ].


% Gases (all kinds of):
-spec get_gas_types() -> [ gas() ].
get_gas_types() ->
	get_green_house_gas_types() ++ [ 'biogas', 'exhaust_gas' ].


% Wastes that could be valorized:
-spec get_recyclable_waste_types() -> [ waste_type() ].
get_recyclable_waste_types() ->
	[ 'recyclable_waste' ].


% Wastes that can be incinerated:
-spec get_incinerable_waste_types() -> [ waste_type() ].
get_incinerable_waste_types() ->
	[ 'incinerable_waste_type_1', 'incinerable_waste_type_2' ].


% Wastes that cannot be valorized:
-spec get_treated_waste_types() -> [ waste_type() ].
get_treated_waste_types() ->
	[ 'bottom_ash', 'residual_waste' ].


% Wastes (all kinds of solid wastes, in tons):
-spec get_waste_types() -> [ waste_type() ].
get_waste_types() ->
	get_recyclable_waste_types() ++ get_incinerable_waste_types()
		++ get_treated_waste_types().



% Energy used or released, under different possible forms:
-spec get_energy_form_types() -> [ energy_form() ].
get_energy_form_types() ->
	[ 'heat', 'electricity', 'momentum', 'fuel' ].


% Emissions, whether or not they are deemed being pollutants:
-spec get_emission_types() -> [ waste_type() ].
get_emission_types() ->
	get_gas_types() ++ get_waste_types() ++ get_energy_form_types().



% Waste-based operations.


% Tells whether the two specified waste types can be mixed into a single waste
% tank.
%
-spec can_be_mixed( waste_type(), waste_type() ) -> boolean().
can_be_mixed( X, X ) ->
	true;

can_be_mixed( none, _X ) ->
	true;

can_be_mixed( _X, none ) ->
	true;

can_be_mixed( _X, _Y ) ->
	false.





% Tells whether the specified type of POI (ex: an incinerator) may produce the
% specified type of waste (condition that is necessary but not sufficient - some
% instances may not produce all the waste types that their class may list).
%
% The 'none' waste type means any waste.
%
% Used notably by garbage trucks, to determine whether they have a chance to
% load waste from a given POI type.
%
-spec can_produce( poi_type(), waste_type() ) -> boolean().
% First rule: the road junction and the landfill cannot produce anything:
can_produce( _POIType=class_RoadJunction, _WasteType ) ->
	false;

can_produce( _POIType=class_Landfill, _WasteType ) ->
	false;

% all others can produce at least one type of waste:
can_produce( _POIType, _WasteType=none ) ->
	true;

can_produce( _POIType=class_IndustrialWasteSource, WasteType ) ->
	% Each industrial waste source *may* produce one waste type among the
	% incinerable ones:
	lists:member( WasteType, get_incinerable_waste_types() );


can_produce( _POIType=class_ResidentialWasteSource, WasteType ) ->
	% Each residential waste source *may* produce one waste type among the
	% incinerable ones:
	lists:member( WasteType, get_incinerable_waste_types() );

can_produce( _POIType=class_Incinerator, _WasteType=bottom_ash ) ->
	true;

% Implied:
%can_produce( _POIType=class_Incinerator, _WasteType ) ->
%	false;

can_produce( _POIType, _WasteType ) ->
	false.




% Tells whether the specified type of POI may consume the specified type of
% waste (condition that is necessary but not sufficient - some instances may not
% consume all the waste types that their class may list).
%
% Used notably by garbage trucks, to determine whether they have a chance to
% unload waste to a given POI type.
%
-spec can_consume( poi_type(), waste_type() ) -> boolean().
can_consume( _POIType, _WasteType=none ) ->
	throw( unspecified_waste_type );

can_consume( _POIType=class_Incinerator, WasteType ) ->
	% Each incinerator *may* consume any subset of the incinerable types:
	lists:member( WasteType, get_incinerable_waste_types() );

can_consume( _POIType=class_Landfill, _WasteType ) ->
	true;

can_consume( _POIType, _WasteType ) ->
	false.




% Returns all the possible types of loading points.
%
% (helper)
%
-spec get_all_loading_point_types() -> [ poi_type() ].
get_all_loading_point_types() ->
	[ class_IndustrialWasteSource, class_ResidentialWasteSource,
	  class_Incinerator ].



% Returns all the possible types of unloading points.
%
% (helper)
%
-spec get_all_unloading_point_types() -> [ poi_type() ].
get_all_unloading_point_types() ->
	[ class_Incinerator, class_Landfill ].



% Tells whether the specified POI type is a loading point.
%
-spec is_poi_type_loadable( poi_type() ) -> boolean().
is_poi_type_loadable( PoiType ) ->
	lists:member( PoiType, get_all_loading_point_types() ).



% Tells whether the specified POI type is an unloading point.
%
-spec is_poi_type_unloadable( poi_type() ) -> boolean().
is_poi_type_unloadable( PoiType ) ->
	lists:member( PoiType, get_all_unloading_point_types() ).




% Checking section.


% Checks that the specified waste type is valid.
%
-spec check_waste_type( atom() ) -> basic_utils:void().
check_waste_type( Type ) ->

	case lists:member( Type, [ none | get_waste_types() ] ) of

		true ->
			ok;

		false ->
			throw( { unknown_waste_type, Type } )

	end.



% Checks that all the waste types specified in the list are valid.
%
-spec check_waste_types( [ atom() ] ) -> basic_utils:void().
check_waste_types( TypeList ) ->
	[ check_waste_type( T ) || T <- TypeList ].



check_volumes( CurrentVolume, MaxVolume ) when is_float( CurrentVolume )
		andalso is_float( MaxVolume ) andalso CurrentVolume >= 0
		andalso CurrentVolume =< MaxVolume ->
	ok;

check_volumes( CurrentVolume, MaxVolume ) ->
	throw( { incorrect_volumes, CurrentVolume, MaxVolume } ).



check_masses( CurrentMass, MaxMass ) when is_float( CurrentMass )
		andalso is_float( MaxMass ) andalso CurrentMass >= 0
		andalso CurrentMass =< MaxMass ->
	ok;

check_masses( CurrentMass, MaxMass ) ->
	throw( { incorrect_masses, CurrentMass, MaxMass } ).



% Checks that the specified description of a waste tank is valid.
%
-spec check_waste_tank( waste_tank() ) -> basic_utils:void().
check_waste_tank( Tank=#waste_tank{

				allowed_types=WasteTypes,
				current_volume_stored=CurrentVolume,
				max_volume_stored=MaxVolume,
				current_mass_stored=CurrentMass,
				max_mass_stored=MaxMass,
				current_type=CurrentType


						} ) ->

	check_waste_types( WasteTypes ),

	check_volumes( CurrentVolume, MaxVolume ),

	check_masses( CurrentMass, MaxMass ),

	case lists:member( CurrentType, [ none | WasteTypes ] ) of

		true ->
			ok;

		false ->
			throw( { unsupported_waste_type, CurrentType, WasteTypes } )

	end,

	case CurrentMass > 0.0 andalso CurrentType =:= none of

			true ->
				throw( { non_empty_tank_has_no_type, Tank } );

			false ->
				ok

	 end.


% Ensures that specified waste capacity (i.e. list of waste tanks) is valid.
%
-spec check_waste_capacity( waste_capacity() ) -> basic_utils:void().
check_waste_capacity( Tanks ) ->
	[ check_waste_tank( T ) || T <- Tanks ].


-spec check_incinerable( waste_type() ) -> basic_utils:void().
check_incinerable( WasteType ) ->
	true = lists:member( WasteType, get_incinerable_waste_types() ).



% Adds the specified quantity (in terms of mass) of waste (of specified type) to
% specified tank.
%
% Note: useful to perform checkings, and notably to ensure the waste type is
% correctly updated if needed.
%
-spec add_waste_to_tank( waste_tank(), unit_utils:tons(), waste_type() )
					   -> waste_tank().
add_waste_to_tank( _Tank, _MassToAdd, _WasteType=none ) ->
	throw( unspecified_added_waste_type );

add_waste_to_tank( Tank=#waste_tank{ current_mass_stored=CurrentMass,
									 max_mass_stored=MaxMass },
					MassToAdd, _WasteType )
  when CurrentMass + MassToAdd > MaxMass + ?epsilon ->
	throw( { too_large_addition_to_tank, MassToAdd, Tank } );


add_waste_to_tank( Tank=#waste_tank{ current_mass_stored=0.0 },
					MassToAdd, WasteType ) ->
	% No more empty now, type must be updated:
	Tank#waste_tank{ current_mass_stored=MassToAdd, current_type=WasteType };


% Here we fill it more:
add_waste_to_tank( Tank=#waste_tank{ current_mass_stored=CurrentMass,
									 max_mass_stored=MaxMass,
									 current_type=CurrentWasteType },
				   MassToAdd, WasteType ) ->

	% Optional checking:
	case can_be_mixed( WasteType, CurrentWasteType ) of

		true ->
			ok;

		false ->
			throw( { incompatible_waste_type_added, WasteType, Tank } )

	end,

	% Waste stays in its current type:
	NewCandidateMass = CurrentMass + MassToAdd,

	% Beware to rounding errors:
	case math_utils:are_relatively_close( NewCandidateMass, MaxMass ) of

		true ->
			Tank#waste_tank{ current_mass_stored=MaxMass };

		false ->
			Tank#waste_tank{ current_mass_stored=NewCandidateMass }

	end.




% Removes the specified quantity (in terms of mass) of waste (of current type)
% from specified tank.
%
% Note: useful to perform checkings, and notably to ensure the waste type is
% correctly updated if needed.
%
-spec remove_waste_from_tank( waste_tank(), unit_utils:tons(), waste_type() ) ->
									waste_tank().
remove_waste_from_tank( _Tank, _MassToRemove, _WasteType=none ) ->
	throw( unspecified_removed_waste_type );

remove_waste_from_tank( Tank=#waste_tank{ current_type=WasteType },
						MassToRemove, WasteType ) ->
	remove_waste_from_tank( Tank, MassToRemove ).



% Removes the specified quantity (in terms of mass) of waste (of current type)
% from specified tank.
%
-spec remove_waste_from_tank( waste_tank(), unit_utils:tons() ) -> waste_tank().
remove_waste_from_tank( Tank=#waste_tank{ current_mass_stored=CurrentMass },
				MassToRemove ) when MassToRemove > CurrentMass + ?epsilon ->
	throw( { too_large_removal_from_tank, MassToRemove, Tank } );

% Here CurrentMass > MassToRemove, hence will not be empty, type unchanged:
remove_waste_from_tank( Tank=#waste_tank{ current_mass_stored=CurrentMass },
						MassToRemove ) ->
	% Beware to rounding errors:
	case math_utils:are_relatively_close( CurrentMass, MassToRemove ) of

		true ->
			% We consider here that the tank has been emptied:
			Tank#waste_tank{ current_mass_stored=0.0, current_type=none };

		false ->
			Tank#waste_tank{ current_mass_stored=CurrentMass - MassToRemove }

	end.



% Returns whether the specified tank is empty.
%
-spec is_tank_empty( waste_tank() ) -> boolean().
is_tank_empty( Tank ) ->
	Tank#waste_tank.current_mass_stored =:= 0.0.


% Returns whether the specified tank is full.
%
-spec is_tank_full( waste_tank() ) -> boolean().
is_tank_full( Tank ) ->
	Tank#waste_tank.current_mass_stored =:= Tank#waste_tank.max_mass_stored.



% Textual section.


% Returns a textual representation of specified waste capacity.
%
-spec waste_capacity_to_string( waste_capacity() ) -> string().
waste_capacity_to_string( Tanks ) ->

	TankStrings = [ waste_tank_to_string( T ) || T <- Tanks ],

	io_lib:format( "Waste capacity made of ~B waste tanks:~s",
				  [ length( Tanks ),
					text_utils:string_list_to_string(TankStrings) ] ).



% Returns a textual representation of specified waste tank.
%
-spec waste_tank_to_string( waste_tank() ) -> string().
waste_tank_to_string( Tank ) ->

	BusyString = case Tank#waste_tank.busy of

					 true ->
						 "busy";

					 false ->
						 "idle"

	end,

	io_lib:format( "waste tank #~B, currently ~s, "
				   "able to store following waste types: ~p; "
				   "current volume stored: ~f cubic meters (max: ~f); "
				   "current mass stored: ~f tons (max: ~f) of waste of type ~s",
					[
					  Tank#waste_tank.id,
					  BusyString,
					  Tank#waste_tank.allowed_types,
					  Tank#waste_tank.current_volume_stored,
					  Tank#waste_tank.max_volume_stored,
					  Tank#waste_tank.current_mass_stored,
					  Tank#waste_tank.max_mass_stored,
					  Tank#waste_tank.current_type
					 ] ).
