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


% Class modeling the repair behaviour of equipments according to a gaussian
% (normal) law (probability density).
%
% It is the most common repair model, most equipments respect this statistical
% rule.
%
% See also: class_UniformRepairModel.
%
-module(class_GaussianRepairModel).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_RepairModel ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, MTTR, RepairVariance ).


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
-define(wooper_method_export,).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,
	"Actor.StochasticActor.RepairModel.GaussianRepairModel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Constructs a new gaussian repair model actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - MTTR is Mean time to repair, the mean (average) time that a device will take
% to recover from a non-terminal failure; it is measured thanks to a tuple
% { days, hours, minutes, seconds }
%
% - RepairVariance is the variance of the repair gaussian law
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper_state(), class_Actor:actor_settings(),
				class_RepairModel:mttr(), float() ) -> wooper_state().
construct( State, ActorSettings,
		  { MTTRday, MTTRhour, MTTRminute, MTTRsecond }, MTTRvariance ) ->

	% First the direct mother classes:

	% Computing MTTR (expressed in seconds):
	MTTR = MTTRsecond + 60*( MTTRminute + 60*( MTTRhour + 24*MTTRday ) ),

	% Defines a gaussian repair profile, for stochastic class to manage it:
	RepairState = class_RepairModel:construct( State, ActorSettings,
		"Gaussian repair model",
		{ positive_integer_gaussian, MTTR, MTTRvariance } ),

	% Then the class-specific actions:
	StartingState = setAttribute( RepairState, trace_categorization,
		?TraceEmitterCategorization ),

	?send_trace_fmt( StartingState, "Creating a new gaussian repair model "
		"whose MTTR is ~B and whose variance is ~B.", [ MTTR, MTTRvariance ] ),

	StartingState.



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?trace( "Deleting gaussian repair model." ),

	?debug( "Gaussian repair model deleted." ),

	% Then allow chaining:
	State.



% Methods section.


% The actSpontaneous/1 method is inherited from class_RepairModel.
