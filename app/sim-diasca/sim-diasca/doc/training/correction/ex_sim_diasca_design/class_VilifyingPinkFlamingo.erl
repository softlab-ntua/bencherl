% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)

-module(class_VilifyingPinkFlamingo).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_ViviparousBeing, class_Actor ] ).


% Parameters taken by the constructor ('construct'):
-define( wooper_construct_parameters, ActorSettings, Name, Height ).


% If using the default do-nothing destructor:

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		synchronous_new/3, synchronous_new_link/3,
		synchronous_timed_new/3, synchronous_timed_new_link/3,
		remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		construct/4, delete/1 ).



% Member method declarations.
-define( wooper_method_export, actSpontaneous/1,
		beNotifiedOfRival/2, beVilified/3,
		filterPlankton/1, getFeatherColor/1 ).


% Static method declarations.
-define( wooper_static_method_export, get_mean_children_count/0 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Flamingo.Vilifying").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% Expressed in centimetres:
-type height() :: float().

-type color() :: atom().



% Constructs a new VilifyingPinkFlamingo:
%
% - ActorSettings corresponds to the engine settings for this actor
%
% - Name is a string corresponding to the name of the flamingo
%
% - Height is its initial (floating-point) height, in centimetres
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				class_Actor:name(), height() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) when is_list(Name)
		andalso is_float(Height) ->

	% First the direct mother classes:
	ViviparousBeingState = class_ViviparousBeing:construct( State ),

	ActorState = class_Actor:construct( ViviparousBeingState, Name ),

	?send_info_fmt( ActorState, "Creating a vilifying pink flamingo "
				   "named '~s' (AAI: ~B) whose height is ~p centimeters.",
				   [ Name, ActorSettings, Height ] ),

	% Then the class-specific attributes:
	% (name is no more class-specific, as defined in class_Actor)
	%
	% The rival_flamingo attribute holds the Pid of the rival of that flamingo
	% (if any).
	setAttributes( ActorState, [

		{height,Height},
		{feather_color,pink},
		{rival_flamingo,undefined},
		{filter_location,camargue},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

								 ] ).



% Overridden destructor:
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	?info_fmt( "Deletion of vilifying pink flamingo named '~s'.",
			  [ ?getAttr(name) ] ),

	State.



% The spontaneous behaviour of a vilifying pink flamingo.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> oneway_return().
actSpontaneous( State ) ->

	NewState = case ?getAttr(rival_flamingo) of

		undefined ->
			mumble(State);

		RivalPid ->
			% Will vilify the rival if the flamingo logic tells to do so:
			case class_Actor:get_current_tick(State) rem 3 of

				0 ->
					vilify( RivalPid, State );

				_Other ->
					filterPlankton( State )

			end

	end,

	?wooper_return_state_only( NewState ).




% Actor oneway section.


% Notifies this flamingo that it has just been vilified.
%
% If a flamingo is vilified by another one which was not its rival, then the
% vilifier becomes its new rival.
%
% Called by a rival flamingo.
%
% (actor oneway)
%
-spec beVilified( wooper_state(), string(), pid() ) ->
						class_Actor:actor_oneway_return().
beVilified( State, VilificationMessage, SenderPid ) ->

	% Sender has to specify who it is, as it is not a request:
	NewState = case ?getAttr(rival_flamingo) of

		SenderPid ->

			?trace_fmt( "I am flamingo ~s, and I am vilified by "
				"my rival ~w, whose message is: '~s'.",
				[ ?getAttr(name), SenderPid, VilificationMessage ] ),

			State;

		_Other->

			?trace_fmt( "I am flamingo ~s, and I am vilified by ~w, "
				"whose message is: '~s'. It was not my rival but now it is.",
				[ ?getAttr(name), SenderPid, VilificationMessage ] ),

			setAttribute( State, rival_flamingo, SenderPid )

	end,

	?wooper_return_state_only( NewState ).




% Request section.


% Returns the feather color of the flamingo.
%
% (const request)
%
-spec getFeatherColor( wooper_state() ) -> request_return( color() ).
getFeatherColor( State ) ->
	?wooper_return_state_result( State, ?getAttr(feather_color) ).




% Oneway section.


% Makes this flamingo discover a new rival to vilify.
%
% The flamingo forgets any previously identified rival.
%
% (actor oneway)
%
-spec beNotifiedOfRival( wooper_state(), pid() ) ->
							   class_Actor:actor_oneway_return().
beNotifiedOfRival( State, RivalPid ) ->
	?wooper_return_state_only(
		setAttribute( State, rival_flamingo, RivalPid ) ).




% Static section.


% Let's say an average means something here:
%
% (static method, as it does not depend on a state)
%
-spec get_mean_children_count() -> basic_utils:count().
get_mean_children_count() ->
	1.7.




% Helper function section.


% Action to be done when this flamingo decides to mumble.
%
% Returns an updated state.
%
% (helper function)
%
-spec mumble( wooper_state() ) -> wooper_state().
mumble( State ) ->
	?trace( "Mumble, mumble. Life is sweet without a rival." ),
	State.



% Action to be done when this flamingo decides to vilify another flamingo.
%
% Returns an updated state.
%
% (helper function)
%
-spec vilify( pid(), wooper_state() ) -> wooper_state().
vilify( RivalPid, State ) ->

	?trace_fmt( "Vilifying now ~w.", [RivalPid] ),

	Message = io_lib:format(
		"Me, ~s, testify that you are the smallest flamingo I have ever seen.",
		[?getAttr(name)] ) ),

	class_Actor:send_actor_message( RivalPid, {beVilified,[Message]}, State ).



% Requests the flamingo to filter plankton in specified location.
%
% Note: this is a modified version of the previous oneway.
%
% Returns an updated state.
%
% (helper function)
%
-spec filterPlankton( wooper_state() ) -> wooper_state().
filterPlankton( State ) ->

	Location = ?getAttr(filter_location),

	HeightGain = case Location of

		camargue ->
			2.5;

		chile ->
			1.0

	end,

	NewHeight = ?getAttr(height) + HeightGain,

	?trace_fmt( "Filtering plankton in ~w, my new height is ~f, "
		"my rival ~w will not believe its eyes.",
		[ Location, NewHeight, ?getAttr(rival_flamingo) ] ),

	setAttribute( State, height, NewHeight ).
