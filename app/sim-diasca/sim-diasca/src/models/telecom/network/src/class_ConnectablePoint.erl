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


% Connectable point, for all places that can be connected with a communication
% device.
%
% When rendered as the node of a graph, if the point is not connected, it will
% be rendered as such (just with its label and a dotted line), otherwise its
% rendering will be the one of its communication device.
%
% See also: class_CommunicatingDevice.erl.
%
-module(class_ConnectablePoint).



% Implementation notes:

% A connectable point can reference a communication device, but it will not own
% it.



% Determines what are the mother classes of this class (if any):
%
% - Actor to preserve ordering of association of devices to points
%
% - Graphable to be able to represent meshes of connected points
%
-define( wooper_superclasses, [ class_Actor, class_Graphable ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, PointName,
		 ConnectedDevicePid ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Method declarations.
-define( wooper_method_export, onFirstDiasca/2, getConnectedDevice/1,
		 connectDevice/2, getGraphInformations/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Constructs a new connectable point:
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - PointName is the name of this point
%
% - ConnectedDevicePid is the PID of the connected device (an instance of the
% class_CommunicatingDevice), if any; otherwise this attribute must be set to
% undefined
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() | 'undefined' ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings, PointName ),

	GraphState = class_Graphable:construct( ActorState, PointName ),

	setAttribute( GraphState, connected_device_pid, ConnectedDevicePid ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	% Class-specific actions:
	% connected_device_pid not owned.
	% Then allow chaining:
	State.




% Methods section.


% Defined simply to avoid a useless warning to be issued / an exception to be
% thrown.
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).



% Returns the communication device connected to this point, if any, otherwise
% the 'undefined' atom.
%
% (const request)
%
-spec getConnectedDevice( wooper:state() ) ->
								request_return( pid() | 'undefined' ).
getConnectedDevice( State ) ->
	?wooper_return_state_result( State, ?getAttr(connected_device_pid) ).



% Connects the specified device to this point.
%
% No device must be already connected.
%
% (oneway)
%
-spec connectDevice( wooper:state(), pid() ) -> oneway_return().
connectDevice( State, DevicePid ) ->

	undefined = ?getAttr(connected_device_pid),

	?wooper_return_state_only(
		setAttribute( State, connected_device_pid, DevicePid ) ).



% Returns {GraphableName,OptionList} where GraphableName is the name of this
% point, and OptionList is the list of all attribute name/value pairs
% corresponding to dot options for the connected communication device (if any),
% otherwise to the ones for the connectable point itself.
%
% (const request)
%
-spec getGraphInformations( wooper:state() ) ->
						request_return( { string(), list() } ).
getGraphInformations( State ) ->

	case ?getAttr(connected_device_pid) of

		undefined ->
			% Call the method inherited from Graphable:
			{ NewState, Result } = executeRequest( State,
												  getGraphInformations ),
			?wooper_return_state_result( NewState, Result );

		DevicePid ->

			DevicePid ! { getGraphInformations, [], self() },

			receive

				{ wooper_result, Result } ->
					?wooper_return_state_result( State, Result )

			end

	end.
