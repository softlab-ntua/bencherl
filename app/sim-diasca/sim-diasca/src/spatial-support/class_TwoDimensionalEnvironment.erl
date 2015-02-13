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



% Class modelling a two-dimensional upright rectangular environment, as an actor
% in charge of keeping track of spatialised simulation elements and resolve
% queries on them.
%
% Its lower-left corner is at the origin of the coordinate system ({0,0}).
%
-module(class_TwoDimensionalEnvironment).


% Determines what are the mother classes of this class (if any):
%
-define( wooper_superclasses, [ class_Actor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, Width, Height,
		 BorderSettings ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
%
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).


% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 declareEntity/5, undeclareEntity/2, getEntitiesWithin/4,
		 getTypedEntitiesWithin/5, notifyPosition/3, toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, ).


-define( hashtable_type, lazy_hashtable ).




% Implementation notes:
%
% The environment must be an actor, to exchange actor messages and know the
% simulation time (for timestamps).
%
% Currently an environment is a singleton (this class should be instanciated
% once).
%
% We rely on time-stamped positions and, if available, maximum potential speeds
% in order to be able to determine a disc in which, at a given time, an entity
% surely is. This may avoid to have to request it its location specifically,
% decreasing the number of space-related messages.


% An environment handles entities, which have a given position in this
% environment. Entities may be actors (spatialised ones), or, in some cases (ex:
% if fully passive or constant, like for a mountain), other processes.


% About vicinity determination:
%
% Let's suppose that we are at tick offset T and that we want to know whether a
% given element E (whose maximum speed is defined, and equal to ve_max, in
% meters per tick) is within a radius R (in meters) of a given point P, and that
% the best timestamped position we have is { Pe, Te } where Pe is the position
% and Te is the timestamp (as a tick offset; Te <= T). Let's name D the distance
% between P and Pe.
%
% E may be now anywhere in a disc whose center is Pe and whose radius is Re = (
% T - Te ) * ve_max. If:
%
% - R > D + Re, then E is certainly inside the specified disc
% - R < D - Re then E is certainly outside the specified disc
% - otherwise: we do not know, we have to update E's position

% In the general case, determining vicinity requires positions to be updated,
% hence two diascas. The entities known to be in the vicinity are stored, and
% entities that were requested to update their position are stored too. Knowing
% that multiple vicinity queries can interleave, a table is kept, whose keys are
% the PID of the requesters, and whose values are the corresponding query, for
% each requester.

% Currently a given spatialised actor cannot have multiple vicinity requests
% pending (and sending direct messages of course is not an option, as it would
% break reproducibility).



% Regarding the destruction of spatialised actors.

% Any actor may disappear at any time, but the contract is that, on termination,
% this actor is required to notify all other actors that may try to interact
% with it in the future (i.e. may have kept its PID) about its upcoming
% termination.
%
% For the sake of simplicity, spatialised actors shall rely on
% 'diasca-unlimited' terminations, i.e. they are to linger during all the
% scheduled diascas of the tick at which they decide to terminate.
%
% With such a dynamic environment, where all actors can interact with any (based
% on geographical relationships that cannot be anticipated), in the general case
% there is no simple way for a terminating actor (or for the environment) to
% know which actors shall be notified. The resolution of this issue must thus be
% case-specific.
%
% Possible solutions:
%
% - in a given perception radius around the terminating actor, notify the actors
% nearby of its termination (choosing a proper radius is an issue here)

% By design, during a given diasca, a requesting actor may obtain from the
% environment a vicinity list that includes undeclared actors (depending on how
% requests are reordered).



% Describes how the borders of an environment should be managed:
%
% - 'rectangle': the environment is strictly bounded, the entities are confined
% within it and cannot cross the borders
%
% - 'torus': crossing a border results in the entity appearing on the opposite
% border (ex: going left from the lefmost edge results in arriving back in the
% environment from the rightmost edge)
%
-type border_description() :: 'rectangle' | 'torus'.



% Describes the extent of a border.
%
-type border_extent() :: linear:distance() | 'unlimited'.


% Position of an entity within an environment.
%
-type position() :: linear_2D:point().


% Upper-bound (if any) of a given entity.
%
-type max_speed() :: unit_utils:meters_per_second() | 'undefined'.



% The position of an entity in an environment, at specified simulation
% timestamp (possibly 'undefined' if the simulation is not started yet).
%
-type timed_position() ::
		{ position(), class_TimeManager:tick_offset() | 'undefined' }.




% An entity record corresponds here to the knowledge this environment has about
% a given spatialised entity.
%
% The PID of this entity is not recorded here, as this record is meant to be the
% value associated to a key which is this PID.
%
-record( entity, {

		% Last known position, with its corresponding timestamp:
		last_timed_position = undefined :: timed_position() | 'undefined',

		% An upper-bound (if any) to the speed of this entity:
		max_speed = undefined :: 'undefined' | unit_utils:meters_per_tick(),

		% The entity's class name is cached here to accelerate some queries:
		class_name :: class_name()

	   } ).


-type entity() :: #entity{}.



% Section about spatial queries.



% Describes a query regarding vicinity established based solely on presence in a
% disc.
%
-record( disc_query, {

		   % Center of the vicinity disc:
		   center :: position(),

		   % Radius of the vicinity disc:
		   radius :: linear:radius(),

		   % Entities already known to be in that disc:
		   entities_in :: [ pid() ],

		   % Entities whose position has been requested, to determine whether
		   % they are in that disc (they may be in):
		   %
		   entities_requested :: [ pid() ]

		  } ).


-type disc_query() :: #disc_query{}.



% Describes a spatial query, when having to be stored while being processed.
%

-type spatial_query() :: disc_query().


% To tell whether some entity is in range of another, for example.
-type range_outcome() :: 'in_range' | 'out_range' | 'unknown'.


-export_type([ border_description/0, border_extent/0, position/0, max_speed/0,
			   entity/0, spatial_query/0, range_outcome/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Spatial.2DEnvironment").



% Allows to use macros for trace sending (to be included after the WOOPER
% header):
%
-include("class_TraceEmitter.hrl").





% Attributes of an instance of an environment are:
%
% - width :: border_extent() describes the abscissa extent of the environment,
% from origin
%
% - height :: border_extent() describes the ordinate extent of the environment,
% from origin
%
% - border_settings :: border_description() describes how border crossing shall
% be managed
%
% - entities :: ?hashtable_type:?hashtable_type( pid(), entity() ) is an
% hashtable associating the entity knowledge of this environment (as a record)
% to the PID of this entity
%
% - query_table :: ?hashtable_type:?hashtable_type( pid(), query() ) is the
% working table for tge computation of vicinity queries; keys are the PID of
% the requester of a vicinity determination, whose values are the corresponding
% stored query
%
% - request_table :: ?hashtable_type:?hashtable_type( pid(), [ pid() ] ) is a
% table which allows, when an entity requested for its position answers, to
% determine the pending queries waiting for it, i.e. the requesters of the
% queries for which this entity is in the 'maybe list')



% Creates a new 2D environment.
%
% Construction parameters are:
%
% - ActorSettings is the AAI assigned by the load-balancer to this actor
%
% - Width is the width (in meters) of this environment
%
% - Height is the height (in meters) of this environment
%
% - BorderSettings describes how the borders of this environment shall be
% managed
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		border_extent(), border_extent(), border_description() ) ->
					   wooper:state().
construct( State, ActorSettings, Width, Height, BorderSettings ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										_Name="Environment" ),

	setAttributes( ActorState, [

			{ width, Width },
			{ height, Height },
			{ border_settings, BorderSettings },
			{ entities, ?hashtable_type:new() },
			{ query_table, ?hashtable_type:new() },
			{ request_table, ?hashtable_type:new() },
			{ trace_categorization,
			 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								 ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.





% Methods section.


% Actor oneways.



% First scheduling on an environment.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	% No specific planned scheduling, an environment is mostly passive.

	?trace_fmt( "Environment just created: ~s", [ to_string( State ) ] ),

	% Creates an initial deadline at the tick, to trigger the burners:
	?wooper_return_state_only( State ).




% The definition of the spontaneous behaviour of this environment.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% None specific here (passive behaviour).

	?wooper_return_state_only( State ).



% Declares specified new entity, and triggers back a notifyEnvironmentSettings
% actor message.
%
% (actor oneway)
%
-spec declareEntity( wooper:state(), position(),
		 unit_utils:meters_per_second() | 'undefined', class_name(), pid() ) ->
				class_Actor:actor_oneway_return().
declareEntity( State, CurrentPosition, MaxSpeed, Classname,
			   SpatialisedActorPid ) ->

	?debug_fmt( "~s entity ~p declared at ~p (max speed: ~p).",
				[ Classname, SpatialisedActorPid, CurrentPosition, MaxSpeed ] ),

	TimedPosition = { CurrentPosition, ?getAttr(current_tick_offset) },

	NewEntityRecord = #entity{

		last_timed_position=TimedPosition,

		max_speed = convert_max_speed( MaxSpeed, State ),

		class_name=Classname

	},

	NewEntities = ?hashtable_type:addEntry( _K=SpatialisedActorPid,
							_V=NewEntityRecord, ?getAttr(entities) ),

	NewState = setAttribute( State, entities, NewEntities ),

	SentState = class_Actor:send_actor_message( SpatialisedActorPid,
					{ notifyEnvironmentSettings, [ ?getAttr(width),
							 ?getAttr(height), ?getAttr(border_settings) ] },
					NewState ),

	?wooper_return_state_only( SentState ).



% Undeclares specified previously-declared entity, so that the environment does
% not keep track of it anymore.
%
% Typically called when a spatialised actor is being deleted.
%
% (actor oneway)
%
-spec undeclareEntity( wooper:state(), pid() ) ->
							 class_Actor:actor_oneway_return().
undeclareEntity( State, SpatialisedActorPid ) ->

	?debug_fmt( "Entity ~p undeclared.", [ SpatialisedActorPid ] ),

	NewEntityTable = ?hashtable_type:removeEntry( _K=SpatialisedActorPid,
												  ?getAttr(entities) ),

	% Just a safety check:
	CurrentRequesters = ?hashtable_type:keys( ?getAttr(query_table) ),

	case lists:member( SpatialisedActorPid, CurrentRequesters ) of

		true ->
			throw( { undeclaring_with_pending_request, SpatialisedActorPid } );

		false ->
			ok

	end,

	% This undeclared entity may happen being looked up by pending third-party
	% vicinity requests; it could be removed from their 'maybe' list (based on
	% request_table), however anyway this undeclaring may be executed in this
	% diasca just after a vicinity look-up (that would thus have returned this
	% terminating entity).
	%
	% As a consequence, a vicinity requester shall take into account the fact
	% that among the returned spatialised actors, some of them might be
	% terminating.
	%
	% We do not disrupt the processing of look-ups by removing this actor from
	% pending requests (otherwise expectations could become wrong, like finding
	% this actor is query lists).

	?wooper_return_state_only(
	   setAttribute( State, entities, NewEntityTable ) ).



% Requests a list of the entities around the position of the caller (supposed to
% be a spatialised actor) within specified radius (the caller itself is not
% listed there).
%
% This will trigger back, on a later diasca, on the caller, a
% notifyEntitiesNearby actor message, specifying a list of the PIDs of the
% matching entities when executing this call.
%
% The environment takes advantage of this call to update its knowledge about the
% position of the caller actor.
%
% (actor oneway)
%
-spec getEntitiesWithin( wooper:state(), position(), linear:radius(), pid() ) ->
				class_Actor:actor_oneway_return().
getEntitiesWithin( State, Position, Radius, SpatialisedActorPid ) ->

	?debug_fmt( "Vicinity request from ~p at ~p: radius ~p m.",
				[ SpatialisedActorPid, Position, Radius ] ),

	% First, update the knowledge of this environment about this actor:

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	EntityTable = ?getAttr(entities),

	% Returned table does not have the record for the requesting actor, to avoid
	% finding oneself in one's vicinity:
	%
	{ UpdatedEntityRecord, ShrunkEntityTable } = update_position(
			   SpatialisedActorPid, Position, CurrentTickOffset, EntityTable ),

	% Currently we use the most primitive method spatial-wise: full iteration.

	% Will return { EntitiesIn, EntitiesMaybeIn }:
	SelectFun = fun( { EntityPid, EntityRecord }, Acc={ In, MaybeIn } ) ->

		% Most frequent first:
		case is_in_range( Position, Radius, EntityRecord, CurrentTickOffset ) of

			out_range ->
				Acc;

			unknown ->
				{ In, [ EntityPid | MaybeIn ] };

			in_range ->
				{ [ EntityPid | In ], MaybeIn }

		end

	end,

	FullEntityTable = ?hashtable_type:addEntry( SpatialisedActorPid,
								  UpdatedEntityRecord, ShrunkEntityTable ),

	UpdatedState = setAttribute( State, entities, FullEntityTable ),

	NewState = case lists:foldl(
					  SelectFun,
					  _InitialAcc={ _InitialIns=[], _InitialMaybeIns=[] },
					  _List=?hashtable_type:enumerate( ShrunkEntityTable ) ) of


		{ EntitiesIn, _EntitiesMaybeIn=[] } ->

					?debug_fmt( "Direct answer: all ~p entities in.",
								[ EntitiesIn ] ),

					% Maybe none in vicinity; anyway able to answer directly:
					class_Actor:send_actor_message( SpatialisedActorPid,
							{ notifyEntitiesNearby, [ EntitiesIn ] },
												   UpdatedState );


		{ EntitiesIn, EntitiesMaybeIn } ->

					?debug_fmt( "Entities in: ~p, maybe in: ~p.",
								[ EntitiesIn, EntitiesMaybeIn ] ),

					% Not able to answer directly, requests shall be issued
					% (updates requesters):
					%
					RequestState = request_position_update( EntitiesMaybeIn,
										   SpatialisedActorPid, UpdatedState ),

					% Stores the information about this query for later use:
					Query = #disc_query{ center=Position,
										 radius=Radius,
										 entities_in=EntitiesIn,
										 entities_requested=EntitiesMaybeIn },

					% Environment unsure about at least one entity, requesting
					% them to update their position at next diasca:
					%
					% Currently no more than one pending request per actor:
					QueryTable = ?getAttr(query_table),

					case ?hashtable_type:hasEntry( SpatialisedActorPid,
												   QueryTable ) of

						true ->
							throw( { multiple_pending_requests,
									 SpatialisedActorPid } ) ;

						false ->
							ok

					end,

					NewQTable = ?hashtable_type:addEntry( SpatialisedActorPid,
									  Query, QueryTable ),

					setAttribute( RequestState, query_table, NewQTable )

			   end,

	?wooper_return_state_only( NewState ).



% Requests a list of the entities of the specified type (exact class - hence
% instances of child classes will not be selected) around the position of the
% caller (supposed to be a spatialised actor) within specified radius (the
% caller itself is not listed there).
%
% This will trigger back, on a later diasca, on the caller, a
% notifyEntitiesNearby actor message, specifying a list of the PIDs of the
% matching entities when executing this call.
%
% The environment takes advantage of this call to update its knowledge about the
% position of the caller actor.
%
% (actor oneway)
%
-spec getTypedEntitiesWithin( wooper:state(), class_name(), position(),
							  linear:radius(), pid() ) ->
									class_Actor:actor_oneway_return().
getTypedEntitiesWithin( State, TargetClass, Position, Radius,
						SpatialisedActorPid ) ->

	?debug_fmt( "Vicinity request for class ~s from ~p at ~p: radius ~p m.",
				[ TargetClass, SpatialisedActorPid, Position, Radius ] ),

	% First, update the knowledge of this environment about this actor:

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	EntityTable = ?getAttr(entities),

	% Returned table does not have the record for the requesting actor, to avoid
	% finding oneself in one's vicinity:
	%
	{ UpdatedEntityRecord, ShrunkEntityTable } = update_position(
			   SpatialisedActorPid, Position, CurrentTickOffset, EntityTable ),

	% Currently we use the most primitive method spatial-wise: full iteration.

	% Will return { EntitiesIn, EntitiesMaybeIn }:
	SelectFun = fun( { EntityPid, EntityRecord }, Acc={ In, MaybeIn } ) ->

		% Most frequent first:
		case is_in_range( TargetClass, Position, Radius, EntityRecord,
						  CurrentTickOffset ) of

			out_range ->
				Acc;

			unknown ->
				{ In, [ EntityPid | MaybeIn ] };

			in_range ->
				{ [ EntityPid | In ], MaybeIn }

		end

	end,

	FullEntityTable = ?hashtable_type:addEntry( SpatialisedActorPid,
								  UpdatedEntityRecord, ShrunkEntityTable ),

	UpdatedState = setAttribute( State, entities, FullEntityTable ),

	NewState = case lists:foldl(
					  SelectFun,
					  _InitialAcc={ _InitialIns=[], _InitialMaybeIns=[] },
					  _List=?hashtable_type:enumerate( ShrunkEntityTable ) ) of


		{ EntitiesIn, _EntitiesMaybeIn=[] } ->

					?debug_fmt( "Direct answer: all ~p entities in.",
								[ EntitiesIn ] ),

					% Maybe none in vicinity; anyway able to answer directly:
					class_Actor:send_actor_message( SpatialisedActorPid,
							{ notifyEntitiesNearby, [ EntitiesIn ] },
												   UpdatedState );


		{ EntitiesIn, EntitiesMaybeIn } ->

					?debug_fmt( "Entities in: ~p, maybe in: ~p.",
								[ EntitiesIn, EntitiesMaybeIn ] ),

					% Not able to answer directly, requests shall be issued
					% (updates requesters):
					%
					RequestState = request_position_update( EntitiesMaybeIn,
										   SpatialisedActorPid, UpdatedState ),

					% Stores the information about this query for later use:
					Query = #disc_query{ center=Position,
										 radius=Radius,
										 entities_in=EntitiesIn,
										 entities_requested=EntitiesMaybeIn },

					% Environment unsure about at least one entity, requesting
					% them to update their position at next diasca:
					%
					% Currently no more than one pending request per actor:
					QueryTable = ?getAttr(query_table),

					case ?hashtable_type:hasEntry( SpatialisedActorPid,
												   QueryTable ) of

						true ->
							throw( { multiple_pending_requests,
									 SpatialisedActorPid } ) ;

						false ->
							ok

					end,

					NewQTable = ?hashtable_type:addEntry( SpatialisedActorPid,
									  Query, QueryTable ),

					setAttribute( RequestState, query_table, NewQTable )

			   end,

	?wooper_return_state_only( NewState ).



% Notifies this environment of the current position of the sending entity.
%
% Note: triggered in answer to a getPosition oneway; must only be in the context
% of a getEntitiesWithin call.
%
% (actor oneway)
%
-spec notifyPosition( wooper:state(), position(), pid() ) ->
							class_Actor:actor_oneway_return().
notifyPosition( State, Position, EntityPid ) ->

	?debug_fmt( "Entity ~p notified its position: ~p.",
				[ EntityPid, Position ] ),

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	EntityTable = ?getAttr(entities),

	% First, updates that position:
	{ NewEntityRecord, ShrunkEntityTable } = update_position( EntityPid,
								  Position, CurrentTickOffset, EntityTable ),

	% Second, manages the implicitly pending getEntitiesWithin call; this
	% updated position can unblock multiple requests regarding vicinity:

	RequestTable = ?getAttr(request_table),

	{ RequesterList, ShrunkRequestTable} = ?hashtable_type:extractEntry(
											_Key=EntityPid, RequestTable ),

	% Makes all vicinity queries that depend on the notifier progress (and
	% possibly complete):
	%
	{ FoldState, FoldQueryTable } = lists:foldl(

		   fun( RequesterPid, { AccState, AccQueryTable } ) ->

				{ Query, NewQueryTable } = ?hashtable_type:extractEntry(
									 _K=RequesterPid, AccQueryTable ),

				% Returns an updated accumulator:
				update_query( Query, RequesterPid, EntityPid, Position,
							  NewQueryTable, AccState )

							end,
							_Acc0={ State, ?getAttr(query_table) },
							_List=RequesterList ),

	NewEntityTable = ?hashtable_type:addEntry( EntityPid, NewEntityRecord,
											  ShrunkEntityTable ),

	FinalState = setAttributes( FoldState, [
						{ entities,	NewEntityTable },
						{ query_table, FoldQueryTable },
						{ request_table, ShrunkRequestTable } ] ),

	?wooper_return_state_only( FinalState ).



% Returns a textual description of this instance.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).



% Returns a textual representation of this instance.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	WidthString = case ?getAttr(width) of

			unlimited ->
				"unlimited width";

			W ->
				io_lib:format( "width of ~s",
							   [ text_utils:distance_to_string( 1000 * W ) ] )

	end,

	HeightString = case ?getAttr(height) of

			unlimited ->
				"unlimited height";

			H ->
				io_lib:format( "height of ~s",
							   [ text_utils:distance_to_string( 1000 * H ) ] )

	end,

	io_lib:format( "Environment '~s' with border settings '~p', ~s and ~s, ",
				   [ ?getAttr(name), ?getAttr(border_settings),
					 WidthString, HeightString ] ).



% Converts specified maximum speed, from model-level conventions (meters per
% second) to engine-level ones (meters per tick).
%
% (helper)
%
-spec convert_max_speed( max_speed(), wooper:state() ) ->
							  unit_utils:meters_per_tick() | 'undefined'.
convert_max_speed( _MeterPerSecondSpeed=undefined, _State ) ->
	undefined;

convert_max_speed( MeterPerSecondSpeed, State ) ->

	% In virtual seconds:
	TickDuration = ?getAttr(simulation_tick_duration),

	MeterPerSecondSpeed * TickDuration.



% Returns whether specified tracked entity is exactly of specified class and in
% range (i.e. within specified radius) of specified referenced position.
%
% See implementation notes ('About vicinity determination') for more
% information.
%
% (helper)
%
-spec is_in_range( class_name(), position(), linear:radius(), entity(),
				   class_TimeManager:tick_offset() ) -> range_outcome().
is_in_range( TargetClass,
			 ReferencedPosition,
			 Radius,
			 E=#entity{ class_name=TargetClass },
			 CurrentTickOffset ) ->
	is_in_range( ReferencedPosition, Radius, E, CurrentTickOffset );


% Here the class does not match:
is_in_range( _TargetClass,
			 _ReferencedPosition,
			 _Radius,
			 _EntityRecord,
			 _CurrentTickOffset ) ->
	out_range.



% Returns whether specified tracked entity is exactly of specified class and in
% range (i.e. within specified radius) of specified referenced position.
%
% See implementation notes ('About vicinity determination') for more
% information.
%
% (helper)
%
-spec is_in_range( position(), linear:radius(), entity(),
				   class_TimeManager:tick_offset() ) -> range_outcome().
is_in_range( ReferencedPosition,
			 Radius,
			 #entity{

				last_timed_position={ EntPosition, CurrentTickOffset },
				max_speed=undefined },
			 CurrentTickOffset ) ->

	% Here we do not know the maximum speed of the entity, hence we cannot bound
	% its movement; we strictly need to rely on its current coordinates and
	% fortunately their timestamps are matching (possibly both set to
	% 'undefined'), we thus will be able to decide directly:
	%
	case linear_2D:is_within( EntPosition, ReferencedPosition, Radius ) of

		true ->
			in_range;

		false ->
			out_range

	end;


is_in_range( _ReferencedPosition, _Radius,
			 #entity{
				max_speed=undefined },
			 _CurrentTickOffset ) ->

	% Here we do not know the maximum speed of the entity, hence we cannot bound
	% its movement and strictly need to rely on its current coordinates, however
	% they are currently outdated (timestamps do not match); hence we cannot
	% decide for the moment:
	%
	unknown;


is_in_range( ReferencedPosition, Radius,
			 #entity{
				last_timed_position={ EntPosition, EntTimestamp },
				max_speed=MaxSpeed },
			 CurrentTickOffset ) ->

	% Here, we have a maximum speed specified:
	D = linear_2D:distance( ReferencedPosition, EntPosition ),

	Re = ( CurrentTickOffset - EntTimestamp ) * MaxSpeed,

	% Most likely case first:
	case Radius < D - Re of

		true ->
			% Certainly outside:
			out_range;


		false ->

			case Radius > D + Re of

				true ->
					% Certainly inside:
					in_range;

				false ->
					% Neither case, we do not know yet:
					unknown

			end

	end.



% Requests each listed entity to update its position at next diasca; will
% triggers back a notifyPosition oneway; updates te requester table.
%
% Returns an updated state.
%
% (helper)
%
request_position_update( EntityList, RequestingEntityPid, State ) ->

	RequestTable = ?getAttr(request_table),

	{ FoldState, NewRequestTable } = lists:foldl(

			   fun( EntityPid, { AccState, ReqTable } ) ->

					   GetState = class_Actor:send_actor_message( EntityPid,
											getPosition, AccState ),

					   NewReqTable = declare_requester( RequestingEntityPid,
										 EntityPid, ReqTable ),

					   { GetState, NewReqTable }

				end,

				_Acc0={ State, RequestTable },

				_List=EntityList ),

	setAttribute( FoldState, request_table, NewRequestTable ).




% Updates the record of specified entity, and returns the specified entity
% table with the record corresponding to the specified entity removed.
%
% Returns { entity(), entity_table() }, the corresponding updated entity record
% and the entity table without this record.
%
% (helper)
%
update_position( EntityPid, Position, CurrentTickOffset, EntityTable ) ->

	{ EntRecord, ShrunkEntityTable } = ?hashtable_type:extractEntry( EntityPid,
																 EntityTable ),

	NewEntRecord = EntRecord#entity{
		  last_timed_position={ Position, CurrentTickOffset } },

	{ NewEntRecord, ShrunkEntityTable }.



% Declares a new requester: returns an updated version of the request table.
%
% (helper)
%
declare_requester( RequestingEntityPid, EntityPid, RequestTable ) ->

	% We create the reverse table allowing, when the requested entity answers,
	% to find back the requester(s):
	%
	case ?hashtable_type:lookupEntry( _K=EntityPid, RequestTable ) of

		hashtable_key_not_found ->
			% So add a new entry:
			?hashtable_type:addEntry( EntityPid, [ RequestingEntityPid ],
								RequestTable );

		{ value, RequestingList } ->
			% This requesting entity might be registered multiple times, if for
			% example issuing requests with different radius:
			%
			?hashtable_type:addEntry( EntityPid,
					  [ RequestingEntityPid | RequestingList ], RequestTable )

	end.



% Updates each of the specified requests with this new position information,
% possibly completing some of them.
%
% Returns { wooper:state(), query_table() }.
%
% (helper)
%
update_query( Query=#disc_query{ center=Center, radius=Radius, entities_in=Ins,
						   entities_requested=Requested },
			  RequesterPid, EntityPid, EntityPosition, QueryTable, State ) ->

	NewIns = case linear_2D:is_within( EntityPosition, Center, Radius ) of

		true ->
			[ EntityPid | Ins ];

		false ->
			Ins

	end,

	case list_utils:delete_existing( EntityPid, Requested ) of

				[] ->
					% This query can thus complete now:
					SentState = class_Actor:send_actor_message( RequesterPid,
						{ notifyEntitiesNearby, [ NewIns ] }, State ),

					% This query is expected to be already extracted:
					{ SentState, QueryTable };


				RequesterList ->
					% Still waiting for other requested entities:
					UpdatedQuery = Query#disc_query{ entities_in=NewIns,
									  entities_requested=RequesterList },

					UpdatedQueryTable = ?hashtable_type:addEntry( RequesterPid,
											UpdatedQuery, QueryTable ),

					{ State, UpdatedQueryTable }


	end.
