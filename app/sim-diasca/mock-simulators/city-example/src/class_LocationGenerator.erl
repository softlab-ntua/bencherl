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



% Class allowing for the generation of adequate locations.
%
% This allows to generate locations:
%
% - simply within the world bounds
%
% - and/or sufficiently far from any set of locations, including all other
% generated ones
%
-module(class_LocationGenerator).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Name, WorldSize ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, delete/1 ).


% Member method declarations:
-define( wooper_method_export, generateLocation/1,
		 generateNonAdjacentLocation/2, generateNonAdjacentLocation/3,
		 generateNonAdjacentLocations/4, toString/1 ).



% Static method declarations:
-define( wooper_static_method_export, ).



% For all shared defines and types:
-include("city_example_types.hrl").



% Design notes:
%
% - the world could be partioned a lot more efficiently (octree, BSP, etc.)




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"City-example.LocationGenerator").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").






% Implementation notes:
%
% - location generators are likely to be bottlenecks when initializing a
% simulation: the location of the potentially many initial actors is determined
% sequentially


% Attributes of a Location Generator instance are:
%
% - world_dimensions :: linear_3D:vector() allows to record the bounds of the
% world of interest
%
% - past_locations :: [ raw_location() ] is the set of all past generated
% locations




% Constructs a new location generator, from following parameters:
%
% - WorldDimensions={ XLen, YLen, ZLen } describes the extent of the world
% (which is a right_cuboid bounding box), from the origin and alongside the
% three canonical axes, specified thanks to a vector pointing from the origin
%
-spec construct( wooper_state(), class_TraceEmitter:name(), linear_3D:vector() )
			   -> wooper_state().
construct( State, Name, WorldDimensions ) ->

	EmitterState = class_TraceEmitter:construct( State, Name ),

	% Bound to be a massive bottleneck:

	?send_info_fmt( EmitterState,
					"Creating location generator with ~s as world bounds.",
					[ linear_3D:to_string( WorldDimensions ) ] ),

	setAttributes( EmitterState, [

		{world_dimensions,WorldDimensions},
		{past_locations,[]}

							  ] ).




-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	State.




% Section for member methods.


% Generates randomly (with an uniform law) a new location within the world
% bounds.
%
% There is possibly already something exactly at the same place.
%
% (request)
%
-spec generateLocation( wooper_state() ) ->
							  request_return( class_GIS:raw_location() ).
generateLocation( State ) ->

	Loc = draw_location( ?getAttr(world_dimensions) ),

	?wooper_return_state_result(
			appendToAttribute( State, past_locations, Loc ),
			Loc ).



% Generates randomly a new location within the world bounds, ensuring it is not
% in the specified radius from any of the past generated locations.
%
% (request)
%
-spec generateNonAdjacentLocation( wooper_state(), class_GIS:length() ) ->
							  request_return( class_GIS:raw_location() ).
generateNonAdjacentLocation( State, Radius ) ->

	Loc = generate_non_adjacent_from( ?getAttr(past_locations), Radius,
									 ?getAttr(world_dimensions) ),

	?wooper_return_state_result(
			appendToAttribute( State, past_locations, Loc ),
			Loc ).



% Generates randomly a new location within the world bounds, ensuring it is not
% in specified radius of any of the specified locations (regardless of the past
% determined ones - but recording these newly returned locations).
%
% (request)
%
-spec generateNonAdjacentLocation( wooper_state(), [ class_GIS:raw_location() ],
		   class_GIS:length() ) -> request_return( class_GIS:raw_location() ).
generateNonAdjacentLocation( State, Locations, Radius ) ->

	Loc = generate_non_adjacent_from( Locations, Radius,
									 ?getAttr(world_dimensions) ),

	?wooper_return_state_result(
			appendToAttribute( State, past_locations, Loc ),
			Loc ).



% Generates randomly the specified number of new locations within the world
% bounds, ensuring they are not in the specified radius GeneralRadius from any
% of the already-generated locations nor in the specified radius PeerRadius from
% the other locations generated by this call.
%
% (request)
%
-spec generateNonAdjacentLocations( wooper_state(), basic_utils:count(),
						class_GIS:length(), class_GIS:length() ) ->
			  request_return( [ class_GIS:raw_location() ] ).
generateNonAdjacentLocations( State, LocationCount, GeneralRadius,
							 PeerRadius ) ->

	PastLocations = ?getAttr(past_locations),

	NewLocations = generate_non_adjacent_from_both( LocationCount,
		 GeneralRadius, PeerRadius, ?getAttr(world_dimensions), PastLocations ),

	NewState = setAttribute( State, past_locations,
							NewLocations ++ PastLocations ),

	?wooper_return_state_result( NewState, NewLocations ).



% Returns a string describing the state of this instance.
%
% (const request)
%
-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->

	FinalString = io_lib:format( "Location generator having ~s "
			"for world bounds, and having generated ~B locations yet",
			[ ?getAttr(world_dimensions),
			   length( ?getAttr(past_locations) ) ] ),

	?wooper_return_state_result( State, FinalString ).




% Helper section.



% Returns a uniformly random location withing specified world bounds, each
% coordinate being in [0,MaxLen-1].
%
draw_location( _WorldDimensions={ XLen, YLen, ZLen } ) ->

	% These are floating-point values, however they correspond to integers:
	{
	  class_RandomManager:get_uniform_floating_point_value( XLen - 1 ),
	  class_RandomManager:get_uniform_floating_point_value( YLen - 1 ),
	  class_RandomManager:get_uniform_floating_point_value( ZLen - 1 )
	}.



% Generates at random a location which is not within Radius of specified
% locations.
%
generate_non_adjacent_from( Locations, Radius, WorldDimensions ) ->
	SquareRadius = math_utils:squarify( Radius ),

	% Up to Count generation attempts will be made:
	AttemptCount=1000,

	generate_non_adjacent_from( Locations, SquareRadius, AttemptCount,
							   WorldDimensions ).



generate_non_adjacent_from( Locations, SquareRadius, _AttemptCount=0,
						   _WorldDimensions ) ->

	% All attempts exhausted:
	throw( { location_generation_failed, SquareRadius, length(Locations) } );


generate_non_adjacent_from( Locations, SquareRadius, AttemptCount, 
							WorldDimensions ) ->

	Loc = draw_location( WorldDimensions ),

	case is_location_close( Loc, Locations, SquareRadius ) of

		true ->
			generate_non_adjacent_from( Locations, SquareRadius, 
										AttemptCount - 1, WorldDimensions );

		false ->
			Loc

	end.



% Generates at random the specified number of locations, each farer than
% GeneralRadius of any previous locations, and farer than PeerRadius of the
% other locations in the returned list.
%
generate_non_adjacent_from_both( LocationCount, GeneralRadius, PeerRadius,
					  WorldDimensions, PastLocations ) ->

	%io:format( "Generating ~B locations in world ~p, each separated "
	%			"from the others by at least ~w meters, "
	%			"each separated from past locations ~w by "
	%           "at least ~w meters.~n",
	%			[ LocationCount, WorldDimensions, GeneralRadius, PastLocations,
	%			 GeneralRadius ] ),

	GeneralSquareRadius = math_utils:squarify( GeneralRadius ),

	PeerSquareRadius = math_utils:squarify( PeerRadius ),

	generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
		PeerSquareRadius, WorldDimensions, PastLocations, _AccGenerated=[],
		_AttemptCount=5000 ).




generate_non_adjacent_from_both( _LocationCount=0, _GeneralSquareRadius,
		 _PeerSquareRadius, _WorldDimensions, _PastLocations, AccGenerated,
		 _AttemptCount ) ->

	%io:format( "Generated locations: ~p.~n", [ AccGenerated ] ),
	AccGenerated;


generate_non_adjacent_from_both( _LocationCount, GeneralSquareRadius,
		 PeerSquareRadius, _WorldDimensions, PastLocations, AccGenerated,
		 _AttemptCount=0 ) ->
	throw( { location_generation_failed, GeneralSquareRadius,
			PeerSquareRadius, length(PastLocations), length(AccGenerated) } );


generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
		 PeerSquareRadius, WorldDimensions, PastLocations, AccGenerated,
		 AttemptCount ) ->

	% We maybe could start with the finest radius.

	% First, let's generate a point far enough from the past locations:
	CandidateLoc = generate_non_adjacent_from( PastLocations,
					 GeneralSquareRadius, WorldDimensions ),

	case is_location_close( CandidateLoc, AccGenerated, PeerSquareRadius ) of

		true ->
			% Too close from counterparts; we need to generate a new candidate:
			% (executing exactly the same call again - except count)
			generate_non_adjacent_from_both( LocationCount, GeneralSquareRadius,
			   PeerSquareRadius, WorldDimensions, PastLocations, AccGenerated,
			   AttemptCount - 1 );

		false ->
			% Perfect, let's acknowledge this candidate and continue:
			generate_non_adjacent_from_both( LocationCount - 1,
			   GeneralSquareRadius,PeerSquareRadius, WorldDimensions,
			   PastLocations, [ CandidateLoc | AccGenerated ], AttemptCount )

	end.



% Tells whether specified location is close (based on the specified square
% radius) of the specified locations.
%
is_location_close( _Location, _Locations=[], _SquareRadius ) ->
	false;

is_location_close( Location, _Locations=[ H | T ], SquareRadius ) ->

	case linear_3D:is_within_square( Location, H, SquareRadius ) of

		true ->
			% Collision!
			true;

		false ->
			is_location_close( Location, T, SquareRadius )

	end.
