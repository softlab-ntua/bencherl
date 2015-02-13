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



% Class modeling any model element that can be geolocalized, in the simulation
% world.
%
% See also: class_GIS.erl
%
-module(class_GeolocalizedElement).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Location ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1 ).


% Method declarations.
-define( wooper_method_export, getLocation/1, getActualLocation/1,
		 setLocation/2 ).


% Static method declarations.
-define( wooper_static_method_export, ).


% Exported helpers.
-export([ enter_in/2, interpret_location/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.GeolocalizedElement").



% Attributes of a geolocalized element are:
%
% - location :: class_GIS:geo_coordinate() is either the current location of
% this element or the PID of its immediate including geocontainer
%
% - local_tracker_pid :: pid() is the PID of the local instance tracker (only
% for debugging purposes)



% Creates a new geolocalized element.
%
% The only parameter is the starting location, which is either:
%
%  - GeoContainerPid :: pid() the PÏD of a geo-container instance in which this
%  element will be located initially
%
%  - { CoordinateType, Location }
%
%  - Location, where Location is implicitly a WGS84 polar coordinate
%
-spec construct( wooper:state(), class_GIS:location() ) -> wooper:state().
construct( State, GeoContainerPid ) when is_pid( GeoContainerPid ) ->
	enter_in( GeoContainerPid, State );


construct( State, { wgs84_polar, PolarCoord } ) ->

	CartesianCoord = class_GIS:wgs84_polar_to_cartesian( PolarCoord ),

	construct( State, { wgs84_cartesian, CartesianCoord } );


construct( State, { wgs84_cartesian, CartesianCoord } ) ->
	setAttributes( State, [
						   { location, CartesianCoord },
						   { local_tracker_pid,
							 class_InstanceTracker:get_local_tracker() }
						  ] );


construct( State, ImplicitlyWGS84PolarCoord ) ->
	construct( State, { wgs84_polar, ImplicitlyWGS84PolarCoord } ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.


% Returns the current location of this element (possibly the PID of a
% container).
%
% (const request)
%
-spec getLocation( wooper:state() ) ->
						 request_return( class_GIS:geo_coordinate() ).
getLocation( State ) ->
	?wooper_return_state_result( State, ?getAttr(location) ).



% Returns the current actual (raw) location of this element.
%
% (const request)
%
-spec getActualLocation( wooper:state() ) ->
						 request_return( class_GIS:raw_location() ).
getActualLocation( State ) ->

	Loc = case ?getAttr(location) of

		ContainerPid when is_pid(ContainerPid) ->

			% Recurses:
			ContainerPid ! { getActualLocation, [], self() },
			receive

				{ wooper_result, ActualLocation } ->
					ActualLocation

			end;

		CartesianCoord  ->
			CartesianCoord

	end,

	?wooper_return_state_result( State, Loc ).



% Sets the current location of this element.
%
% (oneway)
%
-spec setLocation( wooper:state(), class_GIS:geo_coordinate() ) ->
						 oneway_return().
setLocation( State, NewLocation ) ->

	NewState = setAttribute( State, location, NewLocation ),

	?wooper_return_state_only( NewState ).




% Static methods section.




% Helper functions.


% Enters in specified geo-container, expecting this operation to succeed.
%
% Returns an updated state.
%
% Note: not synchronised in simulation (internal use only).
%
% (helper)
%
enter_in( GeoContainerPid, State ) ->

	GeoContainerPid ! { requestEntry, [], self() },

	receive

		{ wooper_result, entered } ->
			setAttributes( State, [
								   { location, GeoContainerPid },
								   { local_tracker_pid,
									 class_InstanceTracker:get_local_tracker() }

								   ] );

		{ wooper_result, entry_refused } ->
			throw( { entry_refused, GeoContainerPid } )

	end.



% Returns a textual description of the location of this element.
%
% (helper)
%
-spec interpret_location( wooper:state() ) -> string().
interpret_location( State ) ->

	case ?getAttr(location) of

		Pid when is_pid( Pid ) ->
			text_utils:format( "inside geocontained ~w", [ Pid ] );

		Loc ->
			class_GIS:wgs84_cartesian_to_string( Loc )

	end.
