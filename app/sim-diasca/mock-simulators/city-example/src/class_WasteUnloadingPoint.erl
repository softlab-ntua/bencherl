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



% Class modelling a waste unloading point, i.e. a physical location to which
% wastes can be unloaded, by garbage trucks.
%
-module(class_WasteUnloadingPoint).


% For waste_tank() and al:
-include("city_example_types.hrl").


% For city_max_relative_error:
-include("city_example_settings.hrl").


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
		 construct/3, destruct/1 ).


% Method declarations.
-define( wooper_method_export, unloadWaste/4 ).


% Static method declarations.
-define( wooper_static_method_export, ).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.UnloadingPoint").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% The class-specific attributes of an instance of unloading point are:
%
% - waste_capacity :: [ waste_capacity() ] is a plain list storing the state of
% the waste storage tanks




% Creates a new waste unloading point.
%
% Construction parameters are:
%
% - Location: the location of this unloading point
%
% - CapacityInformation describes the waste storage capacity of this point
%
-spec construct( wooper:state(), class_GIS:location(), waste_capacity() ) ->
					   wooper:state().
construct( State, Location, CapacityInformation ) ->

	ContainerState = class_GeoContainer:construct( State, Location ),

	Tanks = manage_capacity_information( CapacityInformation ),

	setAttribute( ContainerState, waste_capacity, Tanks ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.



% Static methods section.


% Helper functions.


% Checkings.
%
% (helper)

manage_capacity_information( CapacityInformation ) ->
	manage_capacity_information( CapacityInformation, _Acc=[] ).


manage_capacity_information( _CapacityInformation=[], Acc ) ->
	Acc;

manage_capacity_information( _CapacityInformation=[ Tank | H ], Acc ) ->
	waste_utils:check_waste_tank( Tank ),
	manage_capacity_information( H, [ Tank | Acc ] ).



% Tries to unload to this unloading point as much as possible of the specified
% mass of specified waste type (possibly any) into the caller (which is expected
% to be a waste transport requesting to empty its waste).
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
-spec unloadWaste( wooper:state(), waste_type(), unit_utils:tons(), pid() ) ->
					   class_Actor:actor_oneway_return().
unloadWaste( _State, _WasteType=none, _ProposedMass, _WasteUnloaderPid ) ->
	throw( cannot_unload_untyped_waste );

unloadWaste( State, WasteType, ProposedMass, WasteUnloaderPid ) ->

	WasteTanks = ?getAttr(waste_capacity),

	[ waste_utils:check_waste_tank( T ) || T <- WasteTanks ],

	{ _State, DescString } = executeRequest( State, toString ),

	?trace_fmt( "Trying to dispatch ~f tons of waste of type ~s "
				"into ~B tanks in ~s.~n",
				[ ProposedMass, WasteType, length( WasteTanks ), DescString ] ),

	UnloadState = case dispatch_waste_into_tanks( WasteType, ProposedMass,
												  WasteTanks ) of

		false ->
			?trace_fmt( "No waste could be unloaded from transport ~w.",
						[ WasteUnloaderPid ] ),
			class_Actor:send_actor_message( WasteUnloaderPid,
											notifyNoUnloadedWaste, State );

		{ NewWasteTanks, RemainingMass } ->

			UnloadedMass = ProposedMass - RemainingMass,

			UnloadingTickCount = get_unloading_duration( WasteType,
												   UnloadedMass, State ),

			?trace_fmt( "Unloading, from transport ~w, ~f tons of waste "
						"of type ~s, this will last for ~B ticks.",
						[ WasteUnloaderPid, UnloadedMass, WasteType,
						  UnloadingTickCount ] ),

			SentState = class_Actor:send_actor_message( WasteUnloaderPid,
				{ notifyUnloadedWaste,
				  [ UnloadedMass, WasteType, UnloadingTickCount ] },
				State ),

			setAttribute( SentState, waste_capacity, NewWasteTanks )

	end,

	[ waste_utils:check_waste_tank( T ) ||
		T <- getAttribute( UnloadState, waste_capacity ) ],

	?wooper_return_state_only( UnloadState ).




% Helper section.


% Does its best to dispatch the specified quantity of waste (of a specified
% type) into the specified waste tanks.
%
% Returns either 'false' if no waste at all was transferred to tanks, otherwise
% returns a pair of updated waste tanks and the remaining mass that could not be
% transferred (if any), and thus is remaining.
%
% (helper)
%
dispatch_waste_into_tanks( WasteType, ProposedMass, WasteTanks ) ->
	dispatch_waste_into_tanks( WasteType, ProposedMass, WasteTanks, _Acc=[] ).


% Two terminating cases:
dispatch_waste_into_tanks( _WasteType, _RemainingMass, _WasteTanks=[],
						   _Acc=[] ) ->
	% No tank changed:
	false;

dispatch_waste_into_tanks( _WasteType, RemainingMass, _WasteTanks=[],
						   AccTank ) ->
	% At least one tank received waste:
	[ waste_utils:check_waste_tank( T ) || T <- AccTank ],
	{ AccTank, RemainingMass };


dispatch_waste_into_tanks( WasteType, RemainingMass, _WasteTanks=[
	  Tank=#waste_tank{
						allowed_types=AllowedTypes,
						current_type=TankWasteType,
						current_mass_stored=CurrentMass,
						max_mass_stored=MaxMass } | T ], AccTank ) ->

	% To be accepted, an incoming waste must be among the allowed ones, and
	% compatible with what is already stored (if any):
	%
	case lists:member( WasteType, AllowedTypes ) andalso
		waste_utils:can_be_mixed( TankWasteType, WasteType ) of

		true ->

			% Eligible tank, waste-type. Has room left?
			case MaxMass - CurrentMass of

				Margin when Margin > 0.0 ->

					% Yes, this tank has room, at least to some extent:
					case RemainingMass > Margin of

						true ->

							% We can fill this tank, but some waste will remain:
							UpdatedTank = waste_utils:add_waste_to_tank( Tank,
											Margin, WasteType ) ,

							waste_utils:check_waste_tank( UpdatedTank ),

							% So we keep on iterating here:
							dispatch_waste_into_tanks( WasteType,
								RemainingMass - Margin, T,
								[ UpdatedTank | AccTank ] );


						false ->

							% We can fully put the remaining waste into that
							% tank:
							LastTank = waste_utils:add_waste_to_tank( Tank,
									RemainingMass, WasteType ),

							waste_utils:check_waste_tank( LastTank ),

							% Returning directly here (no recursion):
							{ [ LastTank | T ] ++ AccTank, _RemainingMass=0.0 }

					end;

				_ZeroMargin ->
					%io:format( "Tank full.~n" ),
					dispatch_waste_into_tanks( WasteType, RemainingMass, T,
											   [ Tank | AccTank ] )
			end;


		false ->

			%io:format( "Non-compatible waste types (tank: ~s, waste: ~s),"
			%	   "continuing iterating.~n", [ TankWasteType, WasteType ] ),

			dispatch_waste_into_tanks( WasteType, RemainingMass, T,
									   [ Tank | AccTank ] )

	end.



% Returns the duration needed, in ticks, for the unloading of specified mass of
% specified waste type in a waste transport.
%
% (helper)
%
get_unloading_duration( _Type, UnloadedMass, State ) ->

	% A base of 2 minutes, plus 1 minute per ton:
	Seconds = ( 2 + 1 * UnloadedMass ) * 60,

	class_Actor:convert_seconds_to_ticks( Seconds, ?city_max_relative_error,
										  State ).
