% Copyright (C) 2008-2014 EDF R&D

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


% Test of Actor class, regarding time management.
%
-module(class_TestCircularActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, Message ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, actSpontaneous/1, addPeer/2, receiveMessage/4 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.CircularTest").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new test actor.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(),	string() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings, ActorName ),

	?send_info( ActorState, "Creating a new test circular actor." ),

	setAttributes( ActorState, [

		{ message, Message },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) } ] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info( "Deleting test circular actor." ),
	% erlang:unlink() not used, as done manager-side.

	?debug( "Test circular actor deleted." ),

	% Then allow chaining:
	State.




% Management section of the actor.


% The core of the test actor behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?info( "Test Actor acting." ),

	NewState = case ?getAttr(initialization_status) of

		completed ->
			say_something( State );

		_OtherStatus ->
			State

	end,

	?wooper_return_state_only( NewState ).



% Adds specified peer to known peers.
%
% (actor oneway)
%
-spec addPeer( wooper:state(), pid() ) -> oneway_return().
addPeer( State, PeerPid ) ->

	?info_fmt( "Chaining to ~w.", [ PeerPid ] ),

	?wooper_return_state_only( setAttribute( State, peer, PeerPid ) ).



% Receives a hello message.
%
% (actor oneway)
%
-spec receiveMessage( wooper:state(), class_Actor:name(), string(), pid() ) ->
							oneway_return().
receiveMessage( State, SenderName, Message , SenderPid ) ->

	?info_fmt( "Received following message from ~s (~w): '~s', "
		"using this message from now on.", [ SenderName, SenderPid, Message ] ),

	?wooper_return_state_only( setAttribute( State, message, Message ) ).




% Section for helper functions (not methods).


% Says hello to all peers.
% Returns an updated state.
%
% (helper function)
%
say_something( State ) ->

	Peer = ?getAttr(peer),

	?info_fmt( "Sending '~s' to ~w.", [ ?getAttr(message), Peer ] ),

	case Peer of

		Peer when is_pid(Peer) ->
			class_Actor:send_actor_message( Peer,
			 { receiveMessage, [ ?getAttr(name), ?getAttr(message) ] }, State );

		undefined ->
			State

	end.
