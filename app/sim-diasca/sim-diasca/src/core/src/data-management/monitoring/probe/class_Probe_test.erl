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



% Unit tests for the Probe class implementation.
%
% Note: unlike the probe_rendering_test, this test uses the full simulation
% framework (ex: the result manager).
%
% See the class_Probe.erl module.
%
-module(class_Probe_test).



% Exported for reuse in the result_management test:
-export([ manage_facility_probe/1 ]).


% For all facilities common to all tests:
-include("test_constructs.hrl").




% Creates a basic facility probe directly from the simulation test case.
%
-spec manage_facility_probe( string() ) -> pid().
manage_facility_probe( ProbeName ) ->

	% Will be overridden anyway:
	TargetDir = class_ResultManager:get_result_directory(),

	% To test different settings:
	%
	ProbeOptions = [

					{ create_command_file_initially, false },
					{ deferred_data_writes, true },
					{ probe_directory, TargetDir }

					],

	%ProbeOptions = [],

	% Corresponds to the first second of Y2K (with default settings):
	InitialTick = 3155695200000,

	% Note: the actual directory the probe files will be created in:
	TargetFacilityDir = file_utils:get_current_directory(),

	% We could as well have used the class_Probe:set_data/3 static method
	% directly (without needing a 'case' here):
	%
	% (this is a facility probe, not a result per se; it should create in the
	% current directory the following files: Test_Facility_Probe.dat,
	% Test_Facility_Probe.p and Test_Facility_Probe.png; and this probe will not
	% be shown in the result browser)
	%
	ProbePid = class_Probe:create_facility_probe(
		{ ProbeName, ProbeOptions },
		_CurveNames=[ "First curve", "Second curve", "Last curve" ],
		_Zones=[],
		_Title="This is a test of the generic probe class for facility probes",
		_XLabel="Simulation tick (20 ms)",
		_YLabel="Number of events",
		TargetFacilityDir ),

	% Tells whether we should display full ticks or tick offsets (note: their
	% origin can be freely defined, there are not necessarily simulation tick
	% offsets):
	UseTickOffsets = true,

	case UseTickOffsets of

		true ->
			ProbePid ! setRotatedTickLabels,
			ProbePid ! { setPointSize, 3 },
			ProbePid ! { setTickOffset, InitialTick };

		false ->
			% Longer, exact, rotated and a probably less useful abscissa
			% labels (useful otherwise they may overlap):
			ProbePid ! setRotatedTickLabels

	end,

	ProbePid ! { setData, [ InitialTick+1, {1,3,7} ] },
	ProbePid ! { setData, [ InitialTick+2, {2,2,3} ] },

	% Here we happen to have no relevant value for the second curve:
	ProbePid ! { setData, [ InitialTick+3, {3,undefined,0} ] },

	% We can jump over time-steps:
	ProbePid ! { setData, [ InitialTick+5, {4,2,-1} ] },

			% Surprise, a new curve is added dynamically:
	ProbePid ! { addCurve, [ "Dynamically-added curve"] },
	ProbePid ! { setData,  [ InitialTick+6, {4,3,1,2} ] },
	ProbePid ! { setData,  [ InitialTick+7, {5,2,3,4} ] },
	ProbePid ! { setData,  [ InitialTick+8, {4,3,7,0} ] },

	% We can jump over time-steps:
	ProbePid ! { setData, [ InitialTick+10, {5,4,8,1} ] },
	ProbePid ! { setData, [ InitialTick+11, {3,4,2,5} ] },

	% Changing the default settings:
	ProbePid ! { setKeyOptions, [ "outside right" ] },

	ProbePid ! { setCanvasSize, [ 800, 300 ] },

	% Let's retrieve the curve names in order to re-order their
	% rendering:
	ProbePid ! { getCurveRenderOrder, [], self() },
	CurveNames = test_receive(),

	?test_info_fmt( "Original curve names: ~s.",
				   [ text_utils:string_list_to_string( CurveNames ) ] ),

	% Let's suppose we want to reorder these curves:
	[ N1, N2, N3, N4 ] = CurveNames,

	NewCurveNames = [ N1, N4, N2, N3 ],

	?test_info_fmt( "Curve names after reordering: ~s.",
				   [ text_utils:string_list_to_string( NewCurveNames ) ] ),

	ProbePid ! { setCurveRenderOrder, [ NewCurveNames ] },

	ProbePid ! { addLabel, [ "This is a label", _FirstLocation={2,1} ] },

	ProbePid ! { addLabel, [ "This is another label",
							_SecondLocation={8,5}, _SecondColor="#FF00FF",
							_Orientation=45, _Position=right ] },

	% Now the dynamic curve is the third, and the so-called "last" is indeed the
	% last.

	class_Probe:send_data( ProbePid, InitialTick+13, {4,3,5,2} ),

	ProbePid.






% Creates a basic facility probe directly from the simulation test case.
%
-spec manage_test_probe( string() ) -> pid().
manage_test_probe( ProbeName ) ->


	% To test different settings:
	%
	% (note however that if the probe directory is overridden and set to the
	% result directory, as the simulation will not have taken place yet, the
	% result manager will not have created the directory yet, thus writings
	% should not be done initially nor immediately)
	ProbeOptions = [

					{ create_command_file_initially, false },
					{ deferred_data_writes, true }

					],

	%ProbeOptions = [],

	InitialTick = 500,

	% We could as well have used the class_Probe:set_data/3 static method
	% directly (without needing a 'case' here):
	%
	% (this is a test probe, i.e. a result per se; if selected by the result
	% manager, it should create in the current directory the following files:
	% Test_probe.dat, Test_probe.p and Test_probe.png; and this probe will not
	% be shown in result browser)
	%
	CreationResult = case class_Probe:declare_test_probe(

		{ ProbeName, ProbeOptions },

		_CurveNames=[ "First curve", "Second curve" ],

		_Zones=[

				{ "Zone A", { abscissa_bottom, "First curve" } },
				{ "Zone B", { "First curve", "Second curve" } },
				{ "Zone C", { abscissa_top, "Second curve" } }

				 ],

		_Title="This is a test of the generic probe class for test probes",

		_XLabel="Simulation tick (20 ms)",

		_YLabel="Number of events" ) of

		non_wanted_probe ->
			?test_info( "The basic probe to be directly created from "
						"the test case did not match the "
						"result specification, thus was not created." ),
			non_wanted_probe;

		ProbePid ->

			?test_info( "Probe selected by the result manager, "
						"sending data to it." ),

			ProbePid ! { setData, [ InitialTick, {0,50} ] },
			ProbePid ! { setData, [ InitialTick+10, {10,60} ] },

			% Here we happen to have no relevant value for the second curve:
			ProbePid ! { setData, [ InitialTick+20, {20,undefined} ] },


			% Surprise, a new curve is added dynamically:
			ProbePid ! { addCurve, [ "Dynamically-added curve"] },
			ProbePid ! { setData,  [ InitialTick+30, {30,70,0} ] },
			ProbePid ! { setData,  [ InitialTick+40, {20,70,5} ] },


			% Let's retrieve the curve names in order to re-order their
			% rendering:
			ProbePid ! { getCurveRenderOrder, [], self() },
			CurveNames = test_receive(),

			?test_info_fmt( "Original curve names: ~s.",
					 [ text_utils:string_list_to_string( CurveNames) ] ),

			% Let's suppose we want to reorder these curves:
			[ N1, N2, N3 ] = CurveNames,

			NewCurveNames = [ N1, N2, N3 ],

			?test_info_fmt( "Curve names after reordering: ~s.",
					 [ text_utils:string_list_to_string( NewCurveNames ) ] ),

			ProbePid ! { setCurveRenderOrder, [ NewCurveNames ] },

			ProbePid ! { addLabel, [ "This is a label",
									_FirstLocation={2,1} ] },

			ProbePid ! { addLabel, [ "This is another label",
						  _SecondLocation={8,5}, _SecondColor="#FF00FF",
						  _Orientation=45, _Position=right ] },

			ProbePid


	end,

	% Just to show that we could use blindly the result of class_Probe:create:
	class_Probe:send_data( CreationResult, InitialTick+50, {10,80,10} ),

	CreationResult.



% Runs the tests for all basic probes not created from an actor:
%
% - facility probes
% - test-specific probes (a.k.a. case-specific probes)
%
-spec run() -> no_return().
run() ->

	?test_start,

	% Default simulation settings (50Hz, batch reproducible) are used, except
	% for the name:
	SimulationSettings = #simulation_settings{

		simulation_name = "Basic probe test"

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

		% Will select all (basic) probes:
		%result_specification = [ {targeted_patterns, [ ".*" ] } ]

		% Will select nothing here, as blacklisted:
		%result_specification = [
		%	  { targeted_patterns, [ {".*",[plot_only]} ] },
		%	  { blacklisted_patterns, ["Test probe" ]} ]


		% Incorrect, rejected specifications:

		%result_specification = unexpected_option
		%result_specification = [ {targeted_patterns, unexpected_pattern} ]
		%result_specification = [ {targeted_patterns, [ ".*" ]},
		%					unexpected_option ]

	},


	% Default deployment settings (unavailable nodes allowed, on-the-fly
	% generation of the deployment package requested, use an host file otherwise
	% fall back to local).
	DeploymentSettings = #deployment_settings{
									enable_performance_tracker = false},


	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},


	?test_info_fmt( "This test will deploy a distributed simulation"
		" based on computing hosts specified as ~p.",
		[ DeploymentSettings#deployment_settings.computing_hosts ] ),


	% Directly created on the user node:
	DeploymentManagerPid = sim_diasca:init(	SimulationSettings,
								 DeploymentSettings, LoadBalancingSettings ),


	?test_info( "Creating a facility probe." ),

	FacilityProbePid = manage_facility_probe(
								_FacilityProbeName="Test Facility Probe" ),


	?test_info( "Creating a test probe." ),

	manage_test_probe( _TestProbeName="Test Probe" ),

	?test_info( "Creating a test probe, checking it is needed indeed." ),

	% No actor to schedule, thus will stop immediately anyway:
	StopTick = 30,

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?test_info( "Waiting for the simulation to end, "
		"since having been declared as a simulation listener." ),


	receive

		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously." )

	end,

	% Allows to manage synchronous aspects and batch mode and al:
	class_Probe:generate_report_for( FacilityProbePid ),

	class_Probe:delete_facility_probe( FacilityProbePid ),

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),


	% Test probe does not need any explicit deallocation.

	sim_diasca:shutdown(),

	?test_stop.
