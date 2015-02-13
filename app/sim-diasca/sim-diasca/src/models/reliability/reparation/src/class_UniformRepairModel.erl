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


% Class modeling the repair behaviour of equipments according to a uniform law
% (probability density).
%
% It is a less common repair model than the gaussian one.
%
% See: class_GaussianRepairModel.
%
-module(class_UniformRepairModel).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_RepairModel ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings,
		_MTTR={ MaxTTRday, MaxTTRhour, MaxTTRminute, MaxTTRsecond } ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronous_timed_new/3,
		 remote_synchronous_timed_new_link/3, remote_synchronisable_new_link/3,
		 construct/3, destruct/1 ).


% Member method declarations.
-define( wooper_method_export,).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,
		"Actor.StochasticActor.RepairModel.UniformRepairModel").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new repair model actor.
%
% - ActorSettings corresponds to the engine settings for this actor, as
% determined by the load-balancer
%
% - MTTR is Mean Time To Repair, the mean (average) time that a device will take
% to recover from a non-terminal failure
%
% The MaxTTR could be taken here as a workload: with one resource set to repair
% the equipment, it evaluates the time to repair.
%
% Affecting more resources leads to decreased repair durations. So it is a
% measure of the failure gravity in itself, not depending on the repair
% resources.
%
% Note: a random manager must be running beforehand.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_RepairModel:mttr() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:

	% Computing maximum TTR (expressed in seconds):
	MaxTTR = MaxTTRsecond + 60*( MaxTTRminute + 60*(MaxTTRhour+24*MaxTTRday) ),

	% Defines an uniform repair profile, for stochastic class to manage it:
	RepairState = class_RepairModel:construct( State, ActorSettings,
		"Uniform repair model", { uniform, MaxTTR } ),

	% Then the class-specific actions:
	StartingState = setAttribute( RepairState, trace_categorization,
								  ?TraceEmitterCategorization ),

	?send_trace_fmt( StartingState,
		"Creating a new uniform repair model "
		"whose maximum time-to-repair is ~B seconds.", [ MaxTTR ] ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting uniform repair model." ),

	?debug( "Uniform repair model deleted." ),

	% Then allow chaining:
	State.



% Methods section.


% The actSpontaneous/1 method is inherited from class_RepairModel.
