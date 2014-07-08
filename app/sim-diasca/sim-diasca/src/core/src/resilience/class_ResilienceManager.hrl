% Copyright (C) 2012-2014 EDF R&D

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



% Registration name:
-define( resilience_manager_name, sim_diasca_resilience_manager ).


% Exported for testing purposes only:

-define( hashtable_type, lazy_hashtable ).


% Shorthands:
-type node_name() :: net_utils:atom_node_name().
-type node_list() :: [ node_name() ].


% A k-record is the entry for a node; to be stored in the k-map.
%
-record( k_record, {

		% The nodes that this node secures:
		securing = [] :: node_list(),

		% The nodes that secure this node:
		secured_by = [] :: node_list()

							} ).
