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



% Unit tests for the GIS (Geographic Information System) basic services.
%
% See the class_GIS.erl tested class.
%
-module(class_GIS_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").




-spec run() -> no_return().
run() ->

	?test_start,

   % Testing offline services first:

	WGS84PolarCoord = { 48.820471, 2.206146, 1500.0 },

	WGS84CartesianCoord = class_GIS:wgs84_polar_to_cartesian( WGS84PolarCoord ),

	test_facilities:display( "Testing the conversion of WGS84 polar "
							 "coordinates into WGS84 cartesian ones: "
							 "'~s' becomes '~s'.",
				 [ class_GIS:wgs84_polar_to_string( WGS84PolarCoord ),
				   class_GIS:wgs84_cartesian_to_string( WGS84CartesianCoord )
						 ] ),

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

	  simulation_name = "Sim-Diasca City-example GIS Test Case",

	  tick_duration = 0.2,

	  result_specification = [ { targeted_patterns, [

					  { "Incinerator-1.*", [ data_and_plot ] }

								] } ]

	},


	DeploymentSettings = #deployment_settings{

		computing_hosts = { use_host_file_otherwise_local,
					   "sim-diasca-host-candidates-for-scale-benchmarks.txt" },

		%node_availability_tolerance = fail_on_unavailable_node,

		% We want to embed additionally this test and its specific
		% prerequisites, defined in the Mock Simulators:
		additional_elements_to_deploy = [{ ".", code },
				{ "gis_location.txt", data } ]

	},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	_DeploymentManagerPid = sim_diasca:init( SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),


	% Testing the GIS service by itself:

	GISPid = class_Actor:create_initial_actor( class_GIS,
											   [ "gis_location.txt" ] ),

	GISPid ! { toString, [], self() },

	GISString = test_receive(),

	test_facilities:display( "GIS current state is: ~s.", [ GISString ] ),

	class_GIS:shutdown( GISPid ),

	?test_stop.
