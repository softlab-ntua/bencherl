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


% Describable class, base of all instances able to output a textual description
% of their state.
-module(class_Describable).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Description ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
	synchronous_new/1, synchronous_new_link/1,
	synchronous_timed_new/1, synchronous_timed_new_link/1,
	remote_new/2, remote_new_link/2, remote_synchronous_new/2,
	remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
	remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
	construct/2, delete/1 ).


% Method declarations.
-define( wooper_method_export, getDescription/1, setDescription/2 ).


% Helper functions.
-export([ get_description/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Implementation Note:
%
% Being a trace emitter is not strictly needed, as it leads to useless
% diamond-shaped multiple inheritance.


% Constructs a new describable instance, based on a record of an in-world
% location.
%
-spec construct( wooper_state(), string() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	setAttribute( State, description, Description ).


% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:

	% Then allow chaining:
	State.




% Methods section.


% Returns the description of this Describable.
%
% (const request)
%
-spec getDescription( wooper_state() ) -> request_return( string() ).
getDescription( State ) ->
	?wooper_return_state_result( State, ?getAttr(description) ).



% Sets the description of this Describable.
%
% (oneway)
%
-spec setDescription( wooper_state(), string() ) -> oneway_return().
setDescription( State, NewDescription ) ->
	?wooper_return_state_only( setAttribute( State, description,
		NewDescription) ).



% Section for helper functions (not methods).


% Returns the description of this Describable.
%
% Note: is never and cannot be overloaded.
%
-spec get_description( wooper_state() ) -> string().
get_description( State ) ->
	?getAttr(description).
