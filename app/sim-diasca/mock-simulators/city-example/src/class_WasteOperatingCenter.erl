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



% Class modelling a waste operating center, i.e. a centralised organisation
% which drives a waster chain (waste loading and unloading points, garbabe
% trucks, etc.)
%
-module(class_WasteOperatingCenter).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_GeolocalizedElement ] ).


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
		 construct/2, delete/1 ).


% Method declarations.
-define( wooper_method_export, requestStaticEntry/1 ).


% Static method declarations.
-define( wooper_static_method_export, ).



% Possible answer to an entry request:
-type entry_outcome() :: 'entered' | 'entry_refused'.



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.GeoContainer").


% Attributes of a geolocalized elements are:
%
% - location :: class_GIS:geo_coordinate() is the current location of this
% element




% Creates a new geo-container.
%
% The only parameter is the starting location, which is either:
%
% - ContainerPid :: pid() is the PID of a parent geo-container, supposedly able
% to accept this container
%
% - { CoordinateType :: class_GIS:geolocation_flavour(), Location::
% class_GIS:geolocation_coordinate() }
%
% - Location :: class_GIS:geolocation_coordinate(), where Location is implicitly
% a WGS84 polar coordinate
%
-spec construct( wooper_state(), class_GIS:location() ) -> wooper_state().
construct( State, ContainerPid ) when is_pid(ContainerPid) ->

	class_GeolocalizedElement:construct( State, ContainerPid );


construct( State, { wgs84_polar, PolarCoord } ) ->

	CartesianCoord = class_GIS:wgs84_polar_to_cartesian( PolarCoord ),

	construct( State, { wgs84_cartesian, CartesianCoord } );


construct( State, { wgs84_cartesian, PolarCoord } ) ->
	setAttribute( State, location, PolarCoord );


construct( State, ImplicitlyWGS84PolarCoord ) ->
	construct( State, { wgs84_polar, ImplicitlyWGS84PolarCoord } ).



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.


% Requests this container to enter.
%
% (request)
%
-spec requestStaticEntry( wooper_state() ) -> request_return( entry_outcome() ).
requestStaticEntry( _State ) ->
	throw( is_abstract ).
