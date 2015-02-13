% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.



% Unit tests for the implementation of trace management.
%
% See the following modules:
% - class_TraceAggregator
% - class_TraceSupervisor
%
% Note: trace services are among the most generic services offered, they are
% used in the vast majority of tests but this one, as the purpose of this test
% is actually to test traces by themselves (cannot use the trace system to test
% the trace system!).
%
-module(traceManagementUnderPressure_test).



% For trace facilities:
-include("traces_for_tests.hrl").



send_traces( _TraceEmitter, _SequenceCount=0 ) ->
	ok;

send_traces( TraceEmitter, SequenceCount ) ->

	test_facilities:display(
						"Pressure test sending set of traces, remaining: ~B.",
						[ SequenceCount ] ),

	% We do not want 'ok' answers on purpose, to speed up the sending:
	TraceEmitter ! sendAsyncTraces,

	send_traces( TraceEmitter, SequenceCount-1 ).



% Run the tests.
%
% Note: this test is among the only ones that do not use the trace
% functionalities for their own behaviours (since it is the subject of these
% tests).
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Starting Trace system, with a trace aggregator "
		"and, if requested, a trace supervisor." ),
	?test_start,


	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "Running in batch mode." );

		false ->
			test_facilities:display( "Running in interactive mode." )

	end,


	test_facilities:display( "Creating a new TestTraceEmitter." ),

	Name = "I am a test emitter of traces",

	% Should not trigger the launch of another global aggregator:
	% (as test_start triggers a *synchronous* aggregator):
	MyTraceEmitter = class_TestTraceEmitter:synchronous_new_link(Name),

	?test_fatal(   "This is a test of the fatal priority for tests." ),
	?test_error(   "This is a test of the error priority for tests." ),
	?test_warning( "This is a test of the warning priority for tests." ),
	?test_info(    "This is a test of the info priority for tests." ),
	?test_trace(   "This is a test of the trace priority for tests." ),
	?test_debug(   "This is a test of the debug priority for tests." ),

	test_facilities:display(
		"Requesting the TestTraceEmitter to send some traces." ),

	% Wait until there is an answer for this trace emitter:
	%
	% (count was set to 500 previously, but synchronized console outputs are too
	% slow for that now)
	%
	send_traces( MyTraceEmitter, _SequenceCount=5 ),

	test_facilities:display( "All traces sent." ),

	MyTraceEmitter ! { getName, [], self() },

	ExpectedName = list_to_binary( Name ),
	ExpectedName = test_receive(),

	?test_info( "Correct name returned." ),

	NewName = "This is my new name",

	MyTraceEmitter ! { setName, [ NewName ] },

	MyTraceEmitter ! { getName, [], self() },

	ExpectedNewName = list_to_binary(NewName),
	ExpectedNewName = test_receive(),

	?test_info( "Correct new name returned." ),

	test_facilities:display( "Deleting this TestTraceEmitter." ),

	MyTraceEmitter ! delete,

	% Test target here:
	?test_stop,

	test_facilities:stop().
