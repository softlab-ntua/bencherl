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


% Communicating device class, base of all devices able to be linked into a
% communication network.
%
% When a communicating device is associated to a network, the network is
% notified on device deletion.
%
-module(class_CommunicatingDevice).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Graphable ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, DeviceName, NetworkPid ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).


% Method declarations.
-define( wooper_method_export, setNetworkListener/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Constructs a new communicating device:
%
% - DeviceName is the name of this communicating device
%
% - NetworkPid is the Pid of the network this device is inserted in
%
-spec construct( wooper:state(), string(), pid() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	% (no option list of abstract devices)
	%
	GraphState = class_Graphable:construct( State, 
											[ { label, DeviceName } ] ),

	setAttribute( GraphState, associated_network_pid, NetworkPid ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	case ?getAttr(associated_network_pid) of

		undefined ->
			ok;

		NetworkPid ->
			NetworkPid ! { notifyDeviceDeletion, self() }

	end,

	% Then allow chaining:
	State.



% Methods section.

% Sets the network listener for this communicationg device.
%
% Any previous listener will be unregistered.
%
% (oneway)
%
-spec setNetworkListener( wooper:state(), pid() ) -> oneway_return().
setNetworkListener( State, NetworkPid ) ->
	?wooper_return_state_only(
		setAttribute( State, associated_network_pid, NetworkPid ) ).
