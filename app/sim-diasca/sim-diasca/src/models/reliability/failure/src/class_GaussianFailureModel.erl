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


% Class modeling the failure behaviour of equipments according to a gaussian
% (normal) law (probability density).
%
% It is a less common failure model than the exponential one.
% See: class_ExponentialFailureModel.
%
-module(class_GaussianFailureModel).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_FailureModel ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, MTTF, FailureVariance ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
%
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).


% Member method declarations.
-define(wooper_method_export,).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,
		"Actor.StochasticActor.FailureModel.GaussianFailureModel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new gaussian failure model actor.
%
% MTTF is Mean Time To Failure, the mean (average) duration before a working
% system fails.
%
% All equipments using a given instance of this failure model will therefore
% have the same MTTF, but the shared random manager (thanks to the stochastic
% inheritance) will ensure each will act independently from the other
% equipments, according to the gaussian distribution.
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		  class_FailureModel:mttf(), math_utils:variance() ) -> wooper:state().
construct( State, ActorSettings,
		  _MTTF={ MTTFday, MTTFhour, MTTFminute, MTTFsecond }, MTTFvariance ) ->

	% First the direct mother classes:

	% Computing MTTF (expressed in seconds):
	MTTF = MTTFsecond + 60 * ( MTTFminute + 60 * ( MTTFhour + 24*MTTFday ) ),

	% Defines an exponential failure profile, for stochastic class to manage it:
	FailureState = class_FailureModel:construct( State, ActorSettings,
		"Gaussian failure model",
		{ positive_integer_gaussian, MTTF, MTTFvariance } ),

	% Then the class-specific actions:
	StartingState = setAttribute( FailureState, trace_categorization,
								  ?TraceEmitterCategorization ),

	?send_trace_fmt( StartingState,
		"Creating a new gaussian failure model whose MTTF is ~B seconds "
		"and whose variance is ~B.", [ MTTF, MTTFvariance ] ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting gaussian failure model." ),

	?debug( "Gaussian failure model deleted." ),

	% Then allow chaining:
	State.



% Methods section.


% Management section of the actor.


% The actSpontaneous/1 method is inherited from class_FailureModel.


% Section for helper functions (not methods).
