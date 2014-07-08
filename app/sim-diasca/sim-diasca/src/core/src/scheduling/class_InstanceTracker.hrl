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



% Each tracker will be registered locally under this name:
-define( instance_tracker_name, sim_diasca_instance_tracker ).



% Type section.


% This records stores information about a model instance (an actor).
%
% All these information are authoritative but are duplicates, since they are
% already available in the first place at the level of the corresponding actor.
%
% PID is not stored there, as each actual record will be actually a value
% associated to a key, which is the PID of the actor.
%
-record( actor_info, {

		% The actual class name of this instance:
		classname :: wooper:class_name(),

		% The name of this instance, as an (optional) binary:
		name :: text_utils:bin_string() | 'undefined',

		% The AAI of this instance:
		aai :: class_Actor:aai()

} ).


-type actor_info() :: #actor_info{}.



% The name of a (possibly distributed) simulation service.
%
% Used for process registration as well.
%
-type service_name() :: basic_utils:registration_name().


% Abstract (as opposed to a PID) reference to an agent of a simulation service
% on a given node (akin to the actor's AAI).
%
% For example, { 'class_TimeManager', 'my_node@example.org' }.
%
-type agent_ref() :: { wooper:class_name(), net_utils:atom_node_name() }.



% Abstract (as opposed to a PID) reference to a result producer (akin to the
% actor's AAI).
%
% For example, <<"My Foobar Probe">>.
%
-type producer_ref() :: text_utils:bin_string().
