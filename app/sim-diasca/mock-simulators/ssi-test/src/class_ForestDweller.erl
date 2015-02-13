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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% Class modeling a forest dweller. This module is an abstract one derived from
% class_Actor.erl and defining some common forest dweller attributes and
% spontaneous behaviours.
%
-module(class_ForestDweller).


% Determines what are the mother classes
-define( wooper_superclasses, [ class_Actor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, DwellerName, GivenAge, 
		ForestPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
	remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
	construct/5 ).


% Declarations of common forest dweller spontaneous behaviors
-define( wooper_method_export, beRegistered/2,  beAlert/3,
		prepareTermination/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"SSI-Test.ForestDweller").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


-include("ssi_test_types.hrl").



% Constructs a new forest dweller actor:
%
% - is_registered: shows if the actor is registed to a forest
%
% - givenAge: for an initially created actor,an given age can be other than 0
% for making difference between all initially created ones; it is 0 for an actor
% created during simulation
%
% - termination_tick_off_set: duration after which this actor should terminate
%
% - termination_waiting_ticks: the ticks between the actor informs its
% termination and execute its termination. This attribute is defined for
% avoiding termination_but_trigger error. The value of this attribute should be
% defined according to the specific actor behavior
%
% - termination_initiated: when it is true, means the forest instance is ready
% to be terminated. By default, its value is false
%
% - target_peers: records a list of pids to which the actor sends messages
%
% Reference to class_Actor.erl for other attributes
%
construct( State, ?wooper_construct_parameters ) ->

	% Firstly, the mother class:
	ActorState = class_Actor:construct( State, ActorSettings, DwellerName ),

	% Then the class-specific attributes:
	setAttributes( ActorState, [

		{is_registered,false},
		{name,DwellerName},
		{givenAge,GivenAge},
		{forest_pid,ForestPid},
		{termination_tick_offset,undefined},
		{termination_waiting_ticks,undefined},
		{termination_initiated,false},
		{target_peers,[]},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)}

	] ).




% Method implementation section.



% Message received from the forest with the forest PID in parameter.
%
% (actor oneway)
%
-spec beRegistered( wooper:state(), pid() ) -> oneway_return().
beRegistered( State, ForestPid ) ->

	?info_fmt( "~w has been registed to ~w.", [ self(), ForestPid ] ),

	UpdatedState = setAttributes( State, [
						 {is_registered,true},
						 {forest_pid,ForestPid} 
										  ] ),

	?wooper_return_state_only( UpdatedState ).



% Called whenever an Alert message is received.
%
% This method will be overridden by the specific dweller.
%
% (actor oneway)
%
-spec beAlert( wooper:state(), alert(), pid() ) -> oneway_return().
beAlert( State, _Alert, _SenderPID ) ->
	State.


% Allows to prepare the termination of a dweller.
%
% (helper)
%
-spec prepareTermination( wooper:state() ) -> wooper:state().
prepareTermination( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	setAttributes(State, [

		 {termination_initiated,true},
		 {termination_tick_offset,CurrentOffset}

		 ] ).
