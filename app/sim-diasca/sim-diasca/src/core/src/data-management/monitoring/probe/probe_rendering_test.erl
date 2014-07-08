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



% Unit tests for the rendering of (basic, generic) probes.
%
% This is not an integration test like class_Probe_test, we just focus on the
% probe own behaviour.
%
% See the class_Probe.erl module.
%
-module(probe_rendering_test).



% For all facilities common to all tests:
-include("test_constructs.hrl").


% For result_manager_name:
-include("class_ResultManager.hrl").




% Runs the tests.
-spec run() -> no_return().
run() ->

	?test_start,

	?test_info( "This test allows to test the rendering of basic probes." ),

	% Here we will emulate from this test the result manager:
	?test_info( "Creating first a mock result manager, "
				"as it is needed by the probe." ),

	_MockResultManager = class_ResultManager:create_mockup_environment(),


	?test_info( "Creating a new Probe." ),

	% Must not be synchronous (otherwise deadlock):
	MyProbe = class_Probe:create_facility_probe( "Test probe",
		_Curves=[ "First curve", "Second curve", "Last curve" ],
		_Zones=[],
		"This is a test of the generic probe class",
		"Simulation tick (20 ms)", "Number of events" ),


	?test_info( "Sending data to the probe." ),

	% Corresponds to the first second of Y2K (with default settings):
	InitialTick = 3155695200000,

	% Tells whether we should display full ticks or tick offsets (note: their
	% origin can be freely defined, there are not necessarily simulation tick
	% offsets):
	UseTickOffsets = true,

	case UseTickOffsets of

		true ->
			MyProbe ! { setTickOffset, InitialTick };

		false ->
			% Longer, exact, rotated and a probably less useful abscissa labels:
			% Useful otherwise they may overlap:
			MyProbe ! setRotatedTickLabels

	end,

	MyProbe ! { setData, [ InitialTick+1, {1,3,7} ] },

	% This works as well, even if the probe was not created, as determined by
	% the result manager:
	class_Probe:send_data( MyProbe, InitialTick+2, {2,2,3} ),

	% Here we happen to have no relevant value for the second curve:
	MyProbe ! { setData, [ InitialTick+3, {3,undefined,0} ] },

	% We can jump over time-steps:
	MyProbe ! { setData, [ InitialTick+5, {4,2,-1} ] },

	% Surprise, a new curve is added dynamically:
	MyProbe ! { addCurve, ["Dynamically-added curve"] },
	MyProbe ! { setData, [ InitialTick+6, {4,3,1,2} ] },
	MyProbe ! { setData, [ InitialTick+7, {5,2,3,4} ] },
	MyProbe ! { setData, [ InitialTick+8, {4,3,7,0} ] },

	% We can jump over time-steps:
	MyProbe ! { setData, [ InitialTick+10, {5,4,8,1} ] },
	MyProbe ! { setData, [ InitialTick+11, {3,4,2,5} ] },

	% Changing the default settings:
	% (commented out, as could fail with ancient gnuplot versions)
	%MyProbe ! { setKeyOptions, [ "outside right" ] },

	MyProbe ! { setCanvasSize, [800,300] },

	% Uncomment to modify ranges:
	%yProbe ! { setAbscissaRange, [ 2, 13 ] },
	%yProbe ! { setOrdinateRange, [ -5, 5 ] },

	% Let's retrieve the curve names in order to re-order their rendering:
	MyProbe ! { getCurveRenderOrder, [], self() },
	CurveNames = test_receive(),

	?test_info_fmt( "Original curve names: ~s.",
					[ text_utils:string_list_to_string( CurveNames ) ] ),

	% Let's suppose we want to swap the third and fourth curves:
	[ N1, N2, N3, N4 ] = CurveNames,

	NewCurveNames = [ N1, N2, N4, N3 ],


	?test_info_fmt( "Curve names after reordering: ~s.",
					 [ text_utils:string_list_to_string( NewCurveNames ) ] ),

	MyProbe ! { setCurveRenderOrder, [ NewCurveNames ] },

	% Now the dynamic curve is the third, and the so-called "last" is indeed the
	% last.

	?test_info( "Requesting the generation of probe report." ),

	% Manages batch mode and al:
	class_Probe:generate_report_for( MyProbe ),

	MyProbe ! delete,

	?test_stop.
