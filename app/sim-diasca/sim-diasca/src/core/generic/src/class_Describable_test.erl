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


% Unit tests for the Describable class implementation.
%
% See the class_Describable module.
%
-module(class_Describable_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").



% Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "Creating a new test Describable." ),

	Description = "King Of Brittain",

	MyDescribable = class_Describable:new_link( Description ),


	MyDescribable ! { getDescription, [], self() },
	Description = test_receive(),

	?test_info( "Correct description returned." ),

	NewDescription = "King of the United Kingdom",

	MyDescribable ! { setDescription, [ NewDescription ] },

	MyDescribable ! { getDescription, [], self() },
	NewDescription = test_receive(),

	?test_info( "Correct new description returned." ),

	wooper:delete_synchronously_instance( MyDescribable ),

	?test_stop.
