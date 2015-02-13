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


% Class modeling the failure behaviour of equipments according to an exponential
% law (probability density).
%
% Note: it is the most common failure model, most electronic equipments respect
% this statistical rule.
%
% See also: class_GaussianFailureModel.
%
-module(class_ExponentialFailureModel).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses, [ class_FailureModel ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, MTTF ).



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
-define(wooper_method_export,).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,
	"Actor.StochasticActor.FailureModel.ExponentialFailureModel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new exponential failure model actor.
%
% MTTF is Mean Time To Failure, the mean (average) time before a working system
% fails.
%
% It is measured thanks to a tuple {days,hours,minutes,seconds}.
%
% All equipments using a given instance of this failure model will therefore
% have the same MTTF, but the shared random manager (thanks to the stochastic
% inheritance) will ensure each will act independently from the other
% equipments, according to the exponential distribution.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_FailureModel:mttf() ) -> wooper:state().
construct( State, ActorSettings,
		  _MTTF={ MTTFday, MTTFhour, MTTFminute, MTTFsecond } ) ->

	% First the direct mother classes:

	% Computing MTTF (expressed in seconds):
	MTTF = MTTFsecond + 60 * ( MTTFminute + 60*( MTTFhour + 24*MTTFday ) ),

	% The mean value of drawn samples is 1/Lambda:
	% (it is a floating-point value)
	Lambda = 1 / MTTF,

	% Defines an exponential failure profile, for stochastic class to manage it:
	FailureState = class_FailureModel:construct( State, ActorSettings,
		"Exponential failure model", { exponential, Lambda } ),

	% Then the class-specific actions:
	StartingState = setAttribute( FailureState, trace_categorization,
								  ?TraceEmitterCategorization ),

	?send_trace_fmt( StartingState,
		"Creating a new exponential failure model whose MTTF is ~B seconds "
		"(lambda is ~f).", [ MTTF, Lambda ] ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting exponential failure model." ),

	?debug( "Exponential failure model deleted." ),

	% Then allow chaining:
	State.



% Methods section.


% The actSpontaneous/1 method is inherited from class_FailureModel.
