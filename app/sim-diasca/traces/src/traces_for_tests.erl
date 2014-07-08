% Copyright (C) 2007-2014 Olivier Boudeville
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


% This module gathers all code that allows to lighten the trace macros for
% tests.
%
-module(traces_for_tests).


-export([ test_start/2, test_stop/2, test_immediate_stop/2,
		  test_stop_on_shell/2 ]).


% For test_info_fmt and al:
-include("traces_test_header.hrl").


% For TraceType:
-include("traces.hrl").


-include("class_TraceSupervisor.hrl").


-include("traces_test_footer.hrl").



% To be called from the counterpart macro.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our tests) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the test, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the test process).
%
% Returns TraceAggregatorPid.
%
-spec test_start( basic_utils:module_name(), boolean() ) -> pid().
test_start( ModuleName, _InitTraceSupervisor=true ) ->

	% First jump to the other clause:
	TraceAggregatorPid = test_start( ModuleName, false ),

	class_TraceSupervisor:init( traces:get_trace_filename( ModuleName ),
							   ?TraceType, TraceAggregatorPid ),

	TraceAggregatorPid;


test_start( ModuleName, _InitTraceSupervisor=false ) ->

	% See comments above about:
	erlang:process_flag( trap_exit, false ),

	% Create first, synchronously (to avoid race conditions), a trace aggregator
	% (false is to specify a non-private i.e. global aggregator).
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).
	%
	% Goes back to the beginning of line:
	%
	io:format( "~n" ),

	TestIsBatch = executable_utils:is_batch(),

	TraceFilename = traces:get_trace_filename( ModuleName ),

	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		TraceFilename, ?TraceType, ?TraceTitle, _TraceIsPrivate=false,
		TestIsBatch ),

	?test_info_fmt( "Starting test ~s.", [ ModuleName ] ),

	TraceAggregatorPid.




% To be called from the counterpart macro.
-spec test_stop( basic_utils:module_name(), pid() ) -> no_return().
test_stop( ModuleName, TraceAggregatorPid ) ->

	class_TraceSupervisor:wait_for(),

	test_immediate_stop( ModuleName, TraceAggregatorPid ).



% To be called from the counterpart macro.
-spec test_immediate_stop( basic_utils:module_name(), pid() ) -> no_return().
test_immediate_stop( ModuleName, TraceAggregatorPid ) ->

	test_stop_on_shell(  ModuleName, TraceAggregatorPid ),

	test_facilities:finished().



% To be called from the counterpart macro.
-spec test_stop_on_shell( basic_utils:module_name(), pid() ) -> no_return().
test_stop_on_shell( ModuleName, TraceAggregatorPid ) ->

	% Variable shared through macro use:
	TraceAggregatorPid ! { synchronous_delete, self() },

	receive

		{ deleted, TraceAggregatorPid } ->
			ok

	end,

	traces:check_pending_wooper_results(),

	class_TraceAggregator:remove(),

	test_facilities:display( "End of test ~s.", [ ModuleName ] ).
