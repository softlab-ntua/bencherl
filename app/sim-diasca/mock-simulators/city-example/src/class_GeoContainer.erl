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



% Class modeling a geographical container, which is a geolocalized element
% containing potentially other geolocalized elements.
%
-module(class_GeoContainer).



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
		 construct/2, destruct/1 ).


% Method declarations.
-define( wooper_method_export, requestEntry/1, traceContent/1, toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, to_string/1 ).


% Exported helpers:
-export([ request_entry/2 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.GeoContainer").


% For types:
-include("city_example_types.hrl").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Implementation notes:
%
% Currently all geo-containers have an unbounded capacity.
% As a result, contained elements are simply stored in a list.



% Class-specific attributes of a geo-container element are:
%
% - contained :: [ geolocalized_pid() ] is a list of the geolocalized elements
% currently contained by this geocontainer




% Creates a new geo-container, initially empty.
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
-spec construct( wooper:state(), class_GIS:location() ) -> wooper:state().
construct( State, AnyKindOfLocation ) ->

	LocalizedState = class_GeolocalizedElement:construct( State,
														  AnyKindOfLocation ),

	setAttribute( LocalizedState, contained, [] ).





% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.


% Requests this container to enter.
%
% This default implementation always accepts this incoming geo-element.
%
% (request)
%
-spec requestEntry( wooper:state() ) -> request_return( entry_outcome() ).
requestEntry( State ) ->

	GeoRequester = ?getSender(),

	{ EnterState, Outcome } = request_entry( GeoRequester, State ),

	?wooper_return_state_result( EnterState, Outcome ).



% Traces current state.
%
% (const oneway)
%
-spec traceContent( wooper:state() ) -> oneway_return().
traceContent( State ) ->

	?info( to_string( State ) ),

	?wooper_return_state_only( State ).



% Returns a string describing the state of this instance.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).




% Static methods section.


% Helper functions.


% Returns whether specified geolocalized element has been accepted by that
% container.
%
% (helper)
%
-spec request_entry( geolocalized_pid(), wooper:state() ) ->
						{ wooper:state(), entry_outcome() }.
request_entry( GeoRequester, State ) ->

	?trace_fmt( "Accepting geolocalized element ~w.", [ GeoRequester ] ),

	{ appendToAttribute( State, contained, GeoRequester ), entered }.



% Returns a textual description of that instance.
%
% (helper)
%
to_string( State ) ->

	ContainedString = case ?getAttr(contained) of

		  [] ->
				"not containing any geo-element";

		  List ->
				text_utils:format( "containing following ~B geo-elements: ~w",
								   [ length( List ), List ] )

	end,

	text_utils:format( "Geocontainer named '~s' located at ~w ~s",
				   [ ?getAttr(name), ?getAttr(location), ContainedString ] ).
