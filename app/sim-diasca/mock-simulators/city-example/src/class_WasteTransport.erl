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



% Class modelling an abstract waste transport.
%
-module(class_WasteTransport).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor, class_GeolocalizedElement ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, TransportName,
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
		 construct/7, delete/1 ).


% Member method declarations:
-define( wooper_method_export, updateProbe/1, load/2, unload/2, toString/1 ).




% For all shared defines and types:
-include("city_example_types.hrl").


% Design notes:
%
% - currently a waste transport only transports one kind of waste (even if it,
% when empty, can load various different types of wastes)





% Type section.


% Describes the state (ex: liquid, solid, etc.) of the wastes that can be
% transported.
%
-type supported_waste_state() :: physical_state().


-export_type([ supported_waste_state/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Waste.Transport").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").






% Implementation notes:
%
% A waste transport embeds exactly one waste tank.



% The class-specific attributes of a waste transport instance are:
%
% - tank :: waste_tank(): the tank this transport stores waste in



% Constructs a new waste transport, from following parameters:
%
% - InitialLocation is the (initial) location of this waste transport (generally
% a point of interest)
%
% - MaxTransportedMass :: unit_utils:tons() is the maximum transported mass
%
% - MaxTransportedVolume :: unit_utils:cubic_meters() is the maximum
% transported volume
%
% - SupportedWasteStates :: [ supported_waste_state() ] is the list of the waste
% states this transport can support
%
% A waste transport is created empty.
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				class_Actor:name(), class_GIS:location(),
				unit_utils:cubic_meters(), unit_utils:tons(),
				[ supported_waste_state() ] ) -> wooper_state().
construct( State, ActorSettings, TransportName, InitialLocation,
		  MaxTransportedVolume, MaxTransportedMass, SupportedWasteStates ) ->

	ActorState = class_Actor:construct( State, ActorSettings, TransportName ),

	GeoState = class_GeolocalizedElement:construct( ActorState,
												   InitialLocation ),

	Tank = #waste_tank{

		id=1,

		% All types allowed here:
		allowed_types=SupportedWasteStates,

		current_type=none,

		current_volume_stored=0.0,

		max_volume_stored=MaxTransportedVolume,

		current_mass_stored=0.0,

		max_mass_stored=MaxTransportedMass,

		busy=false

	 },

	% For probe labels: duration of one tick; milliseconds needed:
	TickDuration = text_utils:duration_to_string(
			   1000 * class_Actor:convert_ticks_to_seconds( 1, GeoState ) ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	TransportProbePid = class_Actor:declare_probe(
				_Name=io_lib:format( "~s Transported Waste Stock Probe",
									[ TransportName ] ),
				_Curves=[ "Quantity of waste currently stored (in tons)"
						 ],
				_Zones=[],
				_Title=io_lib:format( "Waste Storage Monitoring"
									  "for Transport ~s", [ TransportName ] ),
				_XLabel=io_lib:format(
					  "Simulation tick: one tick corresponds to ~s",
					  [ TickDuration ] ),
				_YLabel="Tons of wastes currently transported" ),

	setAttributes( GeoState, [

		{ tank, Tank },
		{ probe_pid, TransportProbePid },
		{ trace_categorization,
			text_utils:string_to_binary(?TraceEmitterCategorization) }

							  ] ).





-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	State.




% Section for member methods.



% Loads specified waste into this transport, from supposedly direct englobing
% container.
%
% Caller is expected to have ensured beforehand that this transport is able to
% carry the specified waste (both in terms of type and quantity), and must on
% its side take this transaction into account.
%
% (actor oneway)
%
-spec load( wooper_state(), waste_description() ) -> oneway_return().
load( State, WasteDescription=#waste_description{ type=Type, state=WasteState,
		   volume=Volume, mass=Mass } ) ->

	% Can the waste be loaded at all?
	SupportedWasteStates = ?getAttr(supported_waste_states),

	case lists:member( WasteState, SupportedWasteStates ) of

		true ->
			ok;

		false ->
			throw( { unsupported_waste_state, WasteDescription,
					SupportedWasteStates } )

	end,

	CurrentWasteType = ?getAttr(current_waste_type),

	case waste_utils:can_be_mixed( CurrentWasteType, Type ) of

		true ->
			ok;

		false ->
			throw( { unmatching_waste_types, WasteDescription,
					CurrentWasteType } )

	end,

	MaxMass = ?getAttr(max_transported_mass),
	CurrentMass = ?getAttr(current_transported_mass),

	NewMass = CurrentMass + Mass,

	case NewMass < MaxMass of

		true ->
			ok;

		false ->
			throw( { waste_too_heavy, WasteDescription,
					MaxMass - CurrentMass } )

	end,

	MaxVolume = ?getAttr(max_transported_volume),
	CurrentVolume = ?getAttr(current_transported_volume),

	NewVolume = CurrentVolume + Volume,

	case CurrentVolume + Volume < MaxVolume of

		true ->
			ok;

		false ->
			throw( { waste_too_voluminous, WasteDescription,
					MaxVolume - CurrentVolume } )

	end,

	LoadedState = setAttributes( State, [

		  { current_transported_mass, NewMass },
		  { current_transported_volume, NewVolume },

		  % Any mix will become of the new waste type:
		  { current_waste_type, Type }

										] ),

	?wooper_return_state_only( LoadedState ).




% Unloads specified waste into this transport, from supposedly direct englobing
% container.
%
% Caller is expected to have ensured beforehand that this transport is able to
% carry the specified waste (both in terms of type and quantity), and must on
% its side take this transaction into account.
%
% (actor oneway)
%
-spec unload( wooper_state(), waste_description() ) -> oneway_return().
unload( State, WasteDescription=#waste_description{ type=Type, state=WasteState,
		   volume=Volume, mass=Mass } ) ->

	% Can the waste be loaded at all?
	SupportedWasteStates = ?getAttr(supported_waste_states),

	case lists:member( WasteState, SupportedWasteStates ) of

		true ->
			ok;

		false ->
			throw( { unsupported_waste_state, WasteDescription,
					SupportedWasteStates } )

	end,

	CurrentWasteType = ?getAttr(current_waste_type),

	case waste_utils:can_be_mixed( CurrentWasteType, Type ) of

		true ->
			ok;

		false ->
			throw( { unmatching_waste_types, WasteDescription,
					CurrentWasteType } )

	end,

	MaxMass = ?getAttr(max_transported_mass),
	CurrentMass = ?getAttr(current_transported_mass),

	NewMass = CurrentMass + Mass,

	case NewMass < MaxMass of

		true ->
			ok;

		false ->
			throw( { waste_too_heavy, WasteDescription,
					MaxMass - CurrentMass } )

	end,

	MaxVolume = ?getAttr(max_transported_volume),
	CurrentVolume = ?getAttr(current_transported_volume),

	NewVolume = CurrentVolume + Volume,

	case CurrentVolume + Volume < MaxVolume of

		true ->
			ok;

		false ->
			throw( { waste_too_voluminous, WasteDescription,
					MaxVolume - CurrentVolume } )

	end,

	LoadedState = setAttributes( State, [

		  { current_transported_mass, NewMass },
		  { current_transported_volume, NewVolume },

		  % Any mix will become of the new waste type:
		  { current_waste_type, Type }

										] ),

	?wooper_return_state_only( LoadedState ).



% Sends an update to the associated probe.
%
% (const oneway)
%
updateProbe( State ) ->

	CurrentTickOffset = ?getAttr(current_tick_offset),

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	class_Probe:send_data( ?getAttr(probe_pid), CurrentTickOffset,
						  { (?getAttr(tank))#waste_tank.current_mass_stored } ),

	?wooper_return_state_only( State ).



% Section for static methods.

% Returns a string describing the state of this instance.
%
% (const request)
%
-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->

	FinalString = io_lib:format( "Waste transport supporting waste states '~p' "
			"carrying ~f tons (out of a maximum of ~f tons), "
			"~f m^3 (out of a maximum of ~f m^3) of waste of type ~p",
			[ ?getAttr(supported_waste_states),
			  ?getAttr(current_transported_mass),
			  ?getAttr(max_transported_mass),
			  ?getAttr(current_transported_volume),
			  ?getAttr(max_transported_volume),
			  ?getAttr(current_waste_type) ] ),

	?wooper_return_state_result( State, FinalString ).
