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
% Creation date: Tuesday, January 11, 2011


% Defines some macros and functions useful for trace-using applications.
% This is thus the main/only header file such applications should include.


% We have kept macros for all the traces (including the ones for applications,
% and start/stop) for the sake of consistency. Moreover doing so allows to
% communicate more easily with agents like the trace aggregator (as we can then
% share discreetly variables like TraceAggregatorPid).


% Defines everything regarding application traces:
-include("traces_app_header.hrl").


% For exec/0 export:
-include("app_facilities.hrl").


% To avoid warnings if not used:
-export([ app_receive/0, app_failed/1 ]).


% For notify_* and al:
-include("traces.hrl").





% Start/stop section.


-ifdef(TracingActivated).


% TraceAggregatorPid voluntarily exported from app_start, for app_stop:

-define( app_start,
	TraceAggregatorPid = traces_for_apps:app_start( ?MODULE,
												   _InitTraceSupervisor=true )
).


-define( app_stop,
	traces_for_apps:app_stop( ?MODULE, TraceAggregatorPid )
).


-define( app_stop_without_waiting_for_trace_supervisor,
	traces_for_apps:app_immediate_stop( ?MODULE, TraceAggregatorPid ) ).



-else. % TracingActivated


% Here, even if the trace sending is deactivated, a trace aggregator is created,
% as some processes nevertheless expect to find one at start-up, or some of them
% may have been recompiled to be trace-enabled.
%
% However no trace supervisor is needed here.
-define( app_start,
	TraceAggregatorPid = traces_for_apps:app_start( ?MODULE,
										   _InitTraceSupervisor=false ) ).



-define( app_stop,
	% No supervisor to wait for, here:
	traces_for_apps:app_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-define( app_stop_without_waiting_for_trace_supervisor,
	traces_for_apps:app_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-endif. % TracingActivated







%%%%%%%%%%%%%%%%%%%%%%%%% Between header and footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Defines everything regarding application traces:
-include("traces_app_footer.hrl").



% Helper macro for those who would not know they could have called the
% corresponding function directly:
-define( app_receive, app_receive() ).



% Helper function to write receive clauses in applications which cannot
% interfere with trace supervision.
%
% Returns the received value.
%
% Ex: Pid ! { getBaz, [], self() }, MyBaz = app_receive(), ...
%
-spec app_receive() -> any().
app_receive() ->
	traces:receive_applicative_message().



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( app_failed, app_failed() ).



% Handles an application failure.
-spec app_failed( string() ) -> no_return().
app_failed( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	Message = io_lib:format( "Application ~s failed, reason: ~s.~n",
							[ ?MODULE, Reason ] ),

	error_logger:error_msg( Message ),
	?app_fatal( Message ),
	% Needed, otherwise error_logger may not display anything:
	system_utils:await_output_completion(),
	erlang:error( "Application ~s failed.", [ ?MODULE ] ).
