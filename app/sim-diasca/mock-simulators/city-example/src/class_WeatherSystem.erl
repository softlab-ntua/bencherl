% Copyright (C) 2014 EDF R&D

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



% Class modelling a weather system over the city, comprising a mesh of weather
% cells.
%
-module(class_WeatherSystem).


% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_Actor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Name, LocationGeneratorPid,
		 GISPid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).


% Method declarations.
-define( wooper_method_export, register/2, onFirstDiasca/2,
		 actSpontaneous/1 ).


% Static method declarations.
-define( wooper_static_method_export, generate_definitions/1 ).


% Exported helpers:
-export([ to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.Weather.System").


-type weather_cell() :: pid().


% In each direction, a cell may be adjacent to either another cell or a border:
-type cell_neighbour() :: weather_cell() | 'border'.


% We could have used a simple [ weather_cell() ] as well:
%
-record( cell_environment, {

		   % The cell on the left (if any):
		   left :: cell_neighbour(),

		   % The cell on the right (if any):
		   right :: cell_neighbour(),

		   % The cell at the top (if any):
		   top :: cell_neighbour(),

		   % The cell at the bottom (if any):
		   bottom :: cell_neighbour()

		  } ).


-type cell_environment() :: #cell_environment{}.


-export_type([ cell_environment/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% User identifier of this overall system:
-define( weather_system_id, "Weather-System" ).



% Implementation notes:

% Cells are numbered from left to right (first coordinate), and from top to
% bottom (second one), both in [1;CellsPerEdge] as all cities are defined in a
% square area.
%
% Their name is typically "Cell-X-Y".



% Implementation notes:
%
% A weather system federates a 2D mesh of weather cells, which spreads over the
% whole simulation world (this forms a plane, parallel to the ground and above
% it).
%
% Each cell is a square and generally has 4 other directly adjacent cells (3 if
% on a border, 2 if in a corner).
%
% Each cell simulates the weather over the ground below it. Its state is
% determined by a 3D vector, that we can imagine holding weather-related
% information (ex: hydrometry, pressure, temperature, wind, etc.).
%
% This state vector is governed by the Lorenz equations, parametrised so that
% each cell is running its own strange attractor.
%
% Each cell is initialised with its own initial conditions, different from all
% others. Moreover they interact over simulation time: the state of a cell is
% mildly impacted by the one of its neighbours.
%
% A weather cell acts upon the ground below it; typically, depending on its
% state, the roads below might experiment rain or even icing, impacting
% adversely the traffic on them.





% The class-specific attributes of a wheather system are:
%
% - cells :: [ weather_cell() ] is a list of all weather cells composing this
% system (which does not need to know how they are interconnected); the system
% owns these cells
%
% - location_generator_pid :: pid() is the PID of the location generator
%
% - gis_pid :: pid() is the PID of the GIS





% Creates a new weather system.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Name is the name of this weather system (as a plain string)
%
% - LocationGeneratorPid is the PID of the location generator
%
% - GISPid is the PID of the GIS
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), pid(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, Name ),

	InitState = setAttributes( ActorState, [

			{ cells, [] },

			{ location_gen_pid, LocationGeneratorPid },

			{ gis_pid, GISPid },

			{ trace_categorization,
			  text_utils:string_to_binary( ?TraceEmitterCategorization ) }

										   ] ),

	?send_info( InitState, "Initialised." ),

	InitState.




% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	Cells = ?getAttr(cells),

	?info_fmt( "Deleting ~B weather cells.", [ length( Cells ) ] ),

	[ C ! delete || C <- Cells ],

	?info( "Deleted." ),

	% Then allow chaining:
	State.





% Methods section.



% Registers the calling cell so that this system supervises it.
%
% This system takes ownership of it.
%
% (acotr oneway)
%
-spec register( wooper:state(), pid() ) -> class_Actor:actor_oneway_return().
register( State, CellPid ) ->

	AddState = appendToAttribute( State, cells, CellPid ),

	?wooper_return_state_only( AddState ).



% First scheduling of the system.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% Purely passive from now.

	?wooper_return_state_only( State ).




% The definition of the spontaneous behaviour of this landfill.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% Purely passive.

	?wooper_return_state_only( State ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->
	text_utils:format( "Weather system made of ~B cells",
					   [ length( ?getAttr(cells) ) ] ).



% Static section.


% Generates a list of instance definitions for the full weather system, cells
% included.
%
% (static)
%
generate_definitions( CellsPerEdge ) ->

	% First the system itself:

	SystemDef = { class_WeatherSystem, [ ?weather_system_id,
					_LocationGeneratorPid=undefined, _GISPid=undefined ] },

	% Then the cells:

	% Anyway all should end up in the attractor sooner or later:
	BaseInitialConditions = { 0.1, 0.0, 0.0 },

	Cells = [ create_cell( X, Y, CellsPerEdge, BaseInitialConditions )
			  || X <- lists:seq( 1, CellsPerEdge ),
				 Y <- lists:seq( 1, CellsPerEdge ) ],

	[ SystemDef | Cells ].






% Helper section.



% Returns a creation definition for the cell located at (X,Y).
%
% (helper)

create_cell( X, Y, CellsPerEdge, _BaseInitialConditions={ Xc, Yc, Zc } ) ->

	Name = get_name_for( X, Y, CellsPerEdge ),

	Xi = ( Xc + 5*X ) / 100,

	Yi = ( Yc + 5*Y ) / 100,

	Zi = Zc + ( Xc + X ) * ( Yc + Y ) / 10000,

	InitialConditions = {  Xi, Yi, Zi },

	Neighbours = [ get_left_neighbour( X, Y, CellsPerEdge ),
				   get_right_neighbour( X, Y, CellsPerEdge ),
				   get_top_neighbour( X, Y, CellsPerEdge ),
				   get_bottom_neighbour( X, Y, CellsPerEdge ) ],

	{ class_WeatherCell, [ Name, InitialConditions, list_to_tuple( Neighbours ),
						   { user_id, ?weather_system_id } ] }.



% Returns the appropriate name for the cells at ( X, Y ) (supposedly within the
% system, not out of bounds)
%
% (helper)
%
get_name_for( X, Y, CellsPerEdge ) when X > 0 andalso X =< CellsPerEdge
							andalso Y > 0 andalso Y =< CellsPerEdge->
	text_utils:format( "Weather-Cell-~B-~B", [ X, Y ] ).



% Returns the name of the left neighbour, or 'border':
%
% (helper)
%
get_left_neighbour( _X=1, _Y, _CellsPerEdge ) ->
	border;

get_left_neighbour( X, Y, CellsPerEdge ) ->
	{ user_id, get_name_for( X - 1, Y, CellsPerEdge ) }.



% Returns the name of the right neighbour, or 'border':
%
% (helper)
%
get_right_neighbour( _X=CellsPerEdge, _Y, CellsPerEdge ) ->
	border;

get_right_neighbour( X, Y, CellsPerEdge ) ->
	{ user_id, get_name_for( X + 1, Y, CellsPerEdge ) }.



% Returns the name of the top neighbour, or 'border':
%
% (helper)
%
get_top_neighbour( _X, _Y=1, _CellsPerEdge ) ->
	border;

get_top_neighbour( X, Y, CellsPerEdge ) ->
	{ user_id, get_name_for( X, Y - 1, CellsPerEdge ) }.



% Returns the name of the bottom neighbour, or 'border':
%
% (helper)
%
get_bottom_neighbour( _X, _Y=CellsPerEdge, CellsPerEdge ) ->
	border;

get_bottom_neighbour( X, Y, CellsPerEdge ) ->
	{ user_id, get_name_for( X, Y + 1, CellsPerEdge ) }.
