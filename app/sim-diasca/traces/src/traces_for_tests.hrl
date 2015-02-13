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


% Defines some macros and functions useful for trace-using tests.
% This is thus the main/only header file such tests should include.


% We have kept macros for all the traces (including the ones for tests, and
% start/stop) for the sake of consistency. Moreover doing so allows to
% communicate more easily with agents like the trace aggregator (as we can then
% share discretly variables like TraceAggregatorPid).


% Defines everything regarding application traces:
-include("traces_test_header.hrl").


% For export of run/0:
-include("test_facilities.hrl").


% To avoid warnings if not used:
-export([ test_receive/0, test_failed/1, test_failed/2 ]).


% For notify_* and al:
-include("traces.hrl").







% Start/stop section.


-ifdef(TracingActivated).


% TraceAggregatorPid voluntarily exported from test_start, for test_stop:

-define( test_start,
	TraceAggregatorPid = traces_for_tests:test_start( ?MODULE,
											 _InitTraceSupervisor=true )
).


-define( test_stop,
	traces_for_tests:test_stop( ?MODULE, TraceAggregatorPid )
).


-else. % TracingActivated


% Here, even if the trace sending is deactivated, a trace aggregator is created,
% as some processes nevertheless expect to find one at start-up, or some of them
% may have been recompiled to be trace-enabled.
%
% However no trace supervisor is needed here.
-define( test_start,
	TraceAggregatorPid = traces_for_tests:test_start( ?MODULE,
											 _InitTraceSupervisor=false ) ).


-define( test_stop,
	% No supervisor to wait for, here:
	traces_for_tests:test_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-endif. % TracingActivated



% Valid whether or not tracing is activated:

-define( test_stop_without_waiting_for_trace_supervisor,
	traces_for_tests:test_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-define( test_stop_on_shell,
	traces_for_tests:test_stop_on_shell( ?MODULE, TraceAggregatorPid ) ).







%%%%%%%%%%%%%%%%%%%%%%%%% Between header and footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Defines everything regarding application traces:
-include("traces_test_footer.hrl").



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( test_receive, test_receive() ).



% Helper function to write receive clauses in tests which cannot interfere with
% trace supervision, as a test may also receive trace control message the test
% code should be unware of.
%
% Returns the received value.
%
% Ex: Pid ! { getBaz, [], self() }, MyBaz = test_receive(), ...
%
% to be used instead of:
%
% Pid ! { getBaz, [], self() },
% receive
%
%   { wooper_result, V } ->
%			V
%
% end,
% ...
%
-spec test_receive() -> any().
test_receive() ->
	traces:receive_applicative_message().



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( test_failed, test_failed() ).



% Handles a test failure, using specified string as advertised reason.
%
-spec test_failed( string() ) -> no_return().
test_failed( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	Message = io_lib:format( "Test ~s failed, reason: ~s.~n",
							[ ?MODULE, Reason ] ),

	error_logger:error_msg( Message ),
	?test_fatal( Message ),
	% Needed, otherwise error_logger may not display anything:
	system_utils:await_output_completion(),
	erlang:error( "Test ~s failed.", [ ?MODULE ] ).



% Handles a test failure, using specified first string as an advertised reason
% with format characters (ex: '~w') and specified list as actual values to be
% formatted.
%
-spec test_failed( text_utils:format_string(), [ any() ] ) ->
						 no_return().
test_failed( Reason, FormattedValue ) ->
	test_failed( io_lib:format( Reason, FormattedValue ) ).
