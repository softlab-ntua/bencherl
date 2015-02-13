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



% The name under which the load balancer process is to be registered (globally):
% (this information must be shared with its clients)
-define( load_balancer_name, sim_diasca_load_balancer ).



% Describes how the load balancing of actors should be performed.
%
-record( load_balancing_settings, {

	% How actors should be placed onto computing resources:
	placement_policy = round_robin

} ).


% For convenience:
-type load_balancing_settings() :: #load_balancing_settings{}.



% Describes the actor settings, as determined by the load balancer, at actor
% creation.
%
-record( actor_settings, {

		   % The AAI of this newer actor:
		   aai :: class_Actor:aai(),

		   % The seed of this actor:
		   seed :: random_utils:seed(),

		   % How it is to reorder its incoming messages:
		   message_ordering_mode :: class_Actor:message_ordering_mode()

} ).
