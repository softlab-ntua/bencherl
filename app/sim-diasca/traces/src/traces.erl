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


% This module gathers all code, common to tests and applications, that allows to
% lighten the trace macros.
%
-module(traces).


-export([ get_trace_filename/1, receive_applicative_message/0,
		  check_pending_wooper_results/0 ]).



-type trace_type() :: 'log_mx_traces' | {'text_traces', 'text_only' | 'pdf' }.

-type emitter_name() :: string().
-type emitter_categorization() :: string().
-type tick() :: integer() | 'none'.
-type time() :: string().
-type location() :: string().
-type message_categorization() :: string().
-type priority() :: 1..6.
-type message() :: string().


% 6 levels of severity, from least important to most: debug, trace, info,
% warning, error and fatal:
%
-type message_type() ::  'debug'| 'trace' | 'info'
					   | 'warning' | 'error' | 'fatal'.


-export_type([ trace_type/0, emitter_name/0, emitter_categorization/0, tick/0,
			   time/0, location/0, message_categorization/0, priority/0,
			   message/0, message_type/0 ]).


% For notify_warning_fmt:
-include("traces.hrl").


% Returns the name of the file in which traces will be written:
-spec get_trace_filename( basic_utils:module_name() ) ->
								file_utils:file_name().
get_trace_filename( ModuleName ) ->
	atom_to_list( ModuleName ) ++ ?TraceExtension.



% Receives an applicative, non-trace message, to protect user messages from the
% trace ones.
%
-spec receive_applicative_message() -> basic_utils:void().
receive_applicative_message() ->

	receive

		{ wooper_result, V } when V /= monitor_ok ->
			V

	end.



% Displays and flushes all remaining WOOPER results.
%
% Defined here, since uses a trace.
%
-spec check_pending_wooper_results() -> basic_utils:void().
check_pending_wooper_results() ->

	receive

		{ wooper_result, AResult }  ->

			?notify_warning_fmt( "Following WOOPER result was unread: ~p.~n",
						   [ AResult ] ),

			check_pending_wooper_results()

	after

		0 ->
			ok

	end.
