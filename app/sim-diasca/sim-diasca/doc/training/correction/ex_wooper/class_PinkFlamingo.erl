% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)
%
-module(class_PinkFlamingo).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_ViviparousBeing] ).


% Parameters taken by the constructor ('construct'):
-define( wooper_construct_parameters, Name, Height ).


% If using the default do-nothing destructor:

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
   synchronous_new/2, synchronous_new_link/2,
   synchronous_timed_new/2, synchronous_timed_new_link/2,
   remote_new/3, remote_new_link/3, remote_synchronous_new/3,
   remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
   remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
   construct/3 ).




% Member method declarations:
-define( wooper_method_export, filterPlankton/2, getFeatherColor/1,
		getMeanChildrenCount/1 ).


% Static method declarations:
-define( wooper_static_method_export, get_mean_children_count/0 ).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



-type name() :: string().

-type height() :: float().

-type location() :: 'camargue' | 'chile'.

-type color() :: 'pink' | 'yellow' | 'blue'.



% Constructs a new PinkFlamingo.
%
% Note: this is *not* a Sim-Diasca actor.
%
-spec construct( wooper_state(), name(), height() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) when is_list(Name)
		andalso is_float(Height) ->

	% First the direct mother classes:
	ViviparousBeingState = class_ViviparousBeing:construct( State ),

	% Then the class-specific attributes:
	setAttributes( ViviparousBeingState, [

		{name,Name},
		{height,Height},
		{feather_color,pink}

	] ).



% Requests the flamingo to filter plankton in specified location.
%
% (oneway)
%
-spec filterPlankton( wooper_state(), location() ) -> oneway_return().
filterPlankton( State, camargue ) ->

	NewHeight = ?getAttr(height) + 2.5,

	io:format( "[~s] Glouglou, gouglou, my height is now ~f cm.~n",
		[ ?getAttr(name), NewHeight ] ),

	?wooper_return_state_only( setAttribute( State, height, NewHeight ) );


filterPlankton( State, chile ) ->

	NewHeight = ?getAttr(height) + 1,

	io:format( "[~s] Gobble, gobble, my height is now ~f cm.~n",
		[ ?getAttr(name), NewHeight ] ),

	?wooper_return_state_only( setAttribute( State, height, NewHeight ) ).



% Returns the feather color of the flamingo.
%
% Could be a static method if we knew for sure that all flamingos were pink.
%
% (const request)
%
-spec getFeatherColor( wooper_state() ) -> request_return( color() ).
getFeatherColor( State ) ->
	?wooper_return_state_result( State, ?getAttr(feather_color) ).



% Returns the mean children count for that flamingo (actually does not depend on
% any specific flaming).
%
% (const request)
%
-spec getMeanChildrenCount( wooper_state() ) ->
								  request_return( basic_utils:count() ).
getMeanChildrenCount( State ) ->
	?wooper_return_state_result( State, 1.7 ).



% Static section.


% Let's say an average means something here:

% (this is a static method, as it does not depend on a state)
%
get_mean_children_count() ->
	1.7.
