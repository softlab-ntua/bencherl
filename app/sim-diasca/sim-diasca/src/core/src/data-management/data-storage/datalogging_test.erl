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



% Overall unit test of the Sim-Diasca data-logging facilities.
%
-module(datalogging_test).



% Exported for reuse in the result_management test:
-export([ manage_facility_probe/1 ]).


% For all facilities common to all tests:
-include("test_constructs.hrl").



% Creates a virtual probe directly from the simulation test case as well.
%
-spec manage_facility_probe( string() ) -> basic_utils:void().
manage_facility_probe( ProbeName ) ->

	ProbeRef = case class_DataLogger:create_virtual_probe(

		   ProbeName,

		   _Curves=[ "My first curve", "My second curve" ],

		   _Zones=[],

		   _Title="My manually defined virtual probe",

		   _XLabel="Any notion of time",

		   _YLabel="Any unit" ) of


		non_wanted_virtual_probe ->
			?test_info( "The virtual probe directly created from the test case "
						"did not match the result specification, "
						"thus was not created." ),
			 non_wanted_virtual_probe;

		% Depending on the result specification, this virtual probe ID may or
		% may not be #3:
		Ref = { DataLoggerPid, MyVirtualProbeID } ->

			% We retrieve the probe table soon, otherwise we could receive
			% another message instead:
			DataLoggerPid ! { getProbeTable, MyVirtualProbeID, self() },
			ProbeTable = test_receive(),

			?test_info_fmt( "Retrieved table for virtual probe ~B: ~p.",
							[ MyVirtualProbeID, ProbeTable ] ),

			?test_info( "Feeding the test virtual probe with no regard for "
				"chronological order, and with non-consecutive ticks." ),

			DataLoggerPid ! { setData, [ MyVirtualProbeID, _Tick1=2,
										_Sample1={4,2} ] },

			DataLoggerPid ! { setData, [ MyVirtualProbeID, _Tick2=0,
									  _Sample2={0,0} ] },

			DataLoggerPid ! { setData, [ MyVirtualProbeID, _Tick3=6,
									  _Sample3={2,1} ] },

			?test_info( "Testing the merge of samples, at the same tick." ),
			DataLoggerPid ! { setData, [ MyVirtualProbeID, _Tick4=10,
									  _Sample4={7,undefined} ] },

			% Try defining _Sample5={1,8} to check conflicts are detected:
			DataLoggerPid ! { mergeData, [ MyVirtualProbeID, _Tick4=10,
										_Sample5={undefined,8} ] },

			?test_info( "Adding dynamically a curve, "
						"that will be reordered later." ),

			DataLoggerPid ! { addCurve, [ MyVirtualProbeID,
										 "My dynamic reordered curve" ] },

			DataLoggerPid ! { setData, [ MyVirtualProbeID, _Tick5=11,
									  _Sample6={2,1,4} ] },

			DataLoggerPid ! {setData,[ MyVirtualProbeID, _Tick6=12,
									  _Sample7={1,4,7} ] },


			?test_info( "Performing a synchronous setting." ),
			DataLoggerPid ! { setDataSynchronous, [ MyVirtualProbeID, _Tick7=14,
				   _Sample8={ undefined, undefined, 8 } ], self() },
			datalogging_set_done = test_receive(),

			?test_info( "Performing a synchronous merge "
						"(first values remains undefined)." ),

			DataLoggerPid ! { mergeDataSynchronous, [ MyVirtualProbeID,
				   _Tick7=14, _Sample9={ undefined, 3, undefined } ], self() },
			datalogging_merge_done = test_receive(),


			?test_info( "Now performing local and direct database operations, "
						"based on the already retrieved table identifier." ),

			?test_info( "Performing direct asynchronous sample setting." ),
			class_DataLogger:set_data_synchronous( ProbeTable, _Tick8=16,
								   _Sample10={ -1, 8, undefined } ),


			?test_info( "Performing direct asynchronous sample merging." ),
			class_DataLogger:merge_data_synchronous( ProbeTable, _Tick8=16,
							   _Sample11={ undefined, undefined, -2 } ),

			?test_info( "Performing direct (conditional) asynchronous "
					   "sample setting." ),
			class_DataLogger:send_data( ProbeTable, _Tick9=17,
							   _Sample12={ 4, 4, 4 } ),


			?test_info( "Reordering curves." ),
			DataLoggerPid ! { getCurveRenderOrder, MyVirtualProbeID, self() },

			% Warning: from this test, we could risk otherwise with a mere
			% test_receive to collect instead the probe_report_generated message
			% that is sent in parallel:
			OriginalOrderedCurves = test_receive(),

			[ C1, C2, C3 ] = OriginalOrderedCurves,
			NewOrderedCurves = [ C1, C3, C2 ],

			DataLoggerPid ! { setCurveRenderOrder,
							 [ MyVirtualProbeID, NewOrderedCurves ] },

			?test_info( "Changing the canvas size." ),
			DataLoggerPid ! { setCanvasSize, [ MyVirtualProbeID, 600, 300 ] },

			Ref

	end,

	% We can also plan to send data, with bothering testing explicitly whether
	% the probe creation has been acknowledged (we specify here the virtual
	% probe reference, i.e. a { DataLoggerPid, ProbeID } pair or a
	% non_wanted_virtual_probe atom, not a direct probe table:
	%
	class_DataLogger:send_data( ProbeRef, _Tick10=18, _Sample13={ 0, 0, 0 } ).




% Generates some data to test the data-logging.
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Datalogging test"

		% Allows to test various combinations of result specifications:

		% Default result specification can be left. Otherwise add a comma above
		% and uncomment one of the specifications below:

		% Correct, accepted specifications:

		%result_specification = no_output
		%result_specification = all_outputs
		%result_specification = all_basic_probes_only
		%result_specification = all_virtual_probes_only

		% Will select nothing:
		%result_specification = []

		% Will select all (virtual) probes:
		%result_specification = [ { targeted_patterns, [ ".*" ] } ]

		% Will select only the probe whose name begins with "Curves A for" (not
		% the Curve B counterpart, neither the one created directly from this
		% test):
		%result_specification = [
		%	  { targeted_patterns, [ {"^Curves.*",[plot_only]} ] },
		%	  { blacklisted_patterns, ["Curves B" ] } ]


		% Incorrect, rejected specifications:

		%result_specification = unexpected_option
		%result_specification = [ { targeted_patterns, unexpected_pattern } ]
		%result_specification = [ { targeted_patterns, [ ".*" ] },
		%					unexpected_option ]

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested), except that we request
	% the data-logger:
	%
	DeploymentSettings = #deployment_settings{

			  enable_data_logger = true
			  %node_availability_tolerance = fail_on_unavailable_node

											  },


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation "
		"based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node, as usual:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings,
							   DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Deployment manager created, retrieving the load balancer." ),

	?test_info( "Requesting to the load balancer the creation of "
		"a first initial test actor." ),

	IsBatch = executable_utils:is_batch(),

	?test_info( "Creating an actor that will make use of two virtual probes." ),

	_ActorPid = class_Actor:create_initial_actor( class_DataLoggingActor,
			   [ "First data-logging test actor", _TerminationTickOffset=200,
				_Listener=self() ] ),


	?test_info( "Creating also a virtual probe directly from the test." ),


	% We have to deal here with the fact that this virtual probe may not be
	% wanted, depending on the result specification above:
	manage_facility_probe( "Virtual probe created from test" ),


	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),


	?test_info( "Starting simulation." ),
	RootTimeManagerPid ! { start, [ _StopTick=120, self() ] },

	% Waits until simulation is finished:
	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	?test_info( "Browsing the report results, if not in batch mode." ),
	class_ResultManager:browse_reports(),

	case IsBatch of

		true ->
			% Nothing more, in batch mode.
			ok;

		false ->
			% Display more information in interactive mode:
			mnesia:start(),
			mnesia:info(),
			observer:start(),
			test_facilities:display( "~n(hit CTRL-M on the TV window to view "
					  "the virtual probe tables, by double-clicking "
					  "on their name)~n" )

	end,

	?test_info( "Removing deployment manager, which will in turn take care "
				"of the deletion of all actors and agents on all nodes." ),


	% Here it is safer to swap deployment termination and test stop: waiting for
	% any trace supervisor allows to delay the tear-down of all simulation
	% agents, including database ones.
	%
	sim_diasca:shutdown(),

	?test_stop.
