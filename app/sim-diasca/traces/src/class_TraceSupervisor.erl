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


% Trace supervisor.
%
% This version just uses LogMX (http://logmx.com) to track the default execution
% trace file, expected to be locally available on disk.
%
-module(class_TraceSupervisor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters,
		{TraceFilename,TraceType,TraceAggregatorPid}, MonitorNow, Blocking ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
	synchronous_new/3, synchronous_new_link/3,
	synchronous_timed_new/3, synchronous_timed_new_link/3,
	remote_new/4, remote_new_link/4, remote_synchronous_new/4,
	remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
	remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
	construct/4, delete/1 ).



% Method declarations.
-define( wooper_method_export, monitor/1, blocking_monitor/1 ).



% Static method declarations (to be directly called from module):
-define( wooper_static_method_export, create/0, create/1, create/2, create/4,
		create/5, init/3, wait_for/0 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For default trace filename:
-include("class_TraceAggregator.hrl").



-define(LogPrefix,"[Trace Supervisor]").

% Use global:registered_names() to check supervisor presence.


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).



% Total width (expressed as a number of characters) of a line of log,
% in text mode (text_traces).
-define(TextWidth,110).


% Constructs a new trace supervisor:
%
% - {TraceFilename,TraceType,TraceAggregatorPid}:
%
%   - TraceFilename is the name of the file where traces should be read from
%
%   - TraceType the type of traces to expect (ex: log_mx_traces, text_traces)
%
%   - TraceAggregatorPid is the PID of the trace aggregator, or undefined
%
% - MonitorNow tells whether the supervision should begin immediately (if true)
% or only when the monitor method is called (if false)
%
% - Blocking tells whether the monitoring should be non-blocking (if equal to
% 'none'); otherwise the monitoring should be blocking, and this Blocking
% parameter should be the PID of the caller to be notified. This parameter has a
% meaning iff MonitorNow is true
%
-spec construct( wooper_state(),
				 { file_utils:file_name(), traces:trace_type(), pid() },
				 boolean(), 'none' | pid() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	%io:format( "~s Creating a trace supervisor, whose PID is ~w.~n",
	%	[ ?LogPrefix, self() ] ),

	% First the direct mother classes (none), then this class-specific actions:
	NewState = setAttributes( State, [

		{trace_filename,TraceFilename},
		{trace_type,TraceType},
		{trace_aggregator_pid,TraceAggregatorPid}

	] ),

	case TraceAggregatorPid of

		AggPid when is_pid(AggPid) ->
			% We have a PID, avoid the race condition that could happen if LogMX
			% was launched before a first trace was written by the aggregator in
			% the trace file:

			%io:format(
			%	   "(trace supervisor waiting for the trace aggregator)~n" ),

			AggPid ! { requestReadyNotification, [], self() },

			receive

				{ wooper_result, trace_file_ready } ->
					%io:format( "Trace aggregator answered.~n" ),
					ok

			end;

		undefined ->

			%io:format(
			%	   "(trace supervisor not waiting any trace aggregator)~n" ),

			% Supposedly no race condition is to be feared here:
			ok

	end,


	EndState = case MonitorNow of

		true ->

			case Blocking of

				Pid when is_pid(Pid) ->
					% Pattern-match the result of in-place invocation:
					% ('monitor_ok' used to be temporarily replaced by '_'
					% due to the LogMX issue with
					% java_security_PrivilegedAction)
					case executeRequest( NewState, blocking_monitor ) of

						{ RequestState, monitor_ok } ->
							% Sends back to the caller:
							Pid ! { wooper_result, monitor_ok },
							self() ! delete,
							RequestState;

						{ AnyState, monitor_failed } ->

							% If needing to ignore a non-significant error from
							% the supervision tool:
							Pid ! { wooper_result, monitor_ok },
							self() ! delete,
							AnyState

							%throw( blocking_monitoring_failed )

					end;

				none ->
					% Non-blocking, handled after the constructor:
					self() ! monitor,
					NewState

			end;

		false ->
			NewState

	end,
	%io:format( "~s Supervisor created.~n", [ ?LogPrefix ] ),
	EndState.



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	%io:format( "~s Deleting supervisor.~n", [ ?LogPrefix ] ),
	% Class-specific actions:
	% Then call the direct mother class counterparts: (none)
	io:format( "~s Supervisor deleted.~n", [ ?LogPrefix ] ),

	% Allow chaining:
	State.




% Methods section.


% Triggers an non-blocking supervision (trace monitoring).
% Will return immediately.
%
% (oneway)
-spec monitor( wooper_state() ) -> oneway_return().
monitor( State ) ->

	NewState = case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			io:format( "~s Supervisor has nothing to monitor, "
				"as the PDF trace report will be generated only on execution "
				"termination.~n", [ ?LogPrefix ] ),
				State;

		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State,
				?getAttr( trace_filename ) ),

			case filelib:is_file( ActualFilename ) of

				true ->
					ok;

				false ->
					error_logger:error_msg( "class_TraceSupervisor:monitor "
						"unable to find trace file '~s'.~n",
						[ ActualFilename ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			io:format( "~s Supervisor will monitor file '~s' now, "
				"with '~s'.~n", [ ?LogPrefix, ActualFilename, Command ] ),

			% Non-blocking (command must be found in the PATH):
			[] = os:cmd( Command ++ " " ++ ActualFilename ++ " 1>/dev/null &" ),
			State

	end,

	?wooper_return_state_only( NewState ).



% Triggers a blocking supervision (trace monitoring).
%
% Will block until the viewer window is closed by the user.
%
% (request)
%
-spec blocking_monitor( wooper_state() ) -> { wooper_state(), 'monitor_ok' }.
blocking_monitor( State ) ->

	case ?getAttr(trace_type) of

		{ text_traces, pdf } ->
			io:format( "~s Supervisor has nothing to monitor, "
				"as the PDF trace report will be generated on execution "
				"termination.~n", [ ?LogPrefix ] ),
			?wooper_return_state_result( State, monitor_ok );

		_Other ->

			{ Command, ActualFilename } = get_viewer_settings( State,
				?getAttr(trace_filename) ),

			case filelib:is_file( ActualFilename ) of

				true ->
					ok;

				false ->
					error_logger:error_msg( "class_TraceSupervisor:monitor "
						"unable to find trace file '~s'.~n",
						[ ActualFilename ] ),
					throw( { trace_file_not_found, ActualFilename } )

			end,

			io:format( "~s Supervisor will monitor file '~s' now with '~s', "
				"blocking until the user closes the viewer window.~n",
				[ ?LogPrefix, ActualFilename, Command ] ),

			% Blocking:
			case os:cmd( Command ++ " " ++ ActualFilename ) of

				[] ->
					io:format( "~s Supervisor ended monitoring of '~s'.~n",
						[ ?LogPrefix, ActualFilename ] ),
					?wooper_return_state_result( State, monitor_ok );

				Other ->
					error_logger:error_msg(
						"The monitoring of trace supervisor failed: ~s.~n",
						[ Other ] ),

					% Must not be a blocking error:
					%?wooper_return_state_result( State, monitor_failed )
					?wooper_return_state_result( State, monitor_ok )
					%throw( trace_supervision_failed )

			end

	end.




% 'Static' methods (module functions):


% Creates the trace supervisor with default settings regarding trace filename,
% start mode (immediate here, not deferred) and trace type (LogMX-based here,
% not text based), with no PID specified for the trace aggregator, and blocks
% until closed.
%
% See create/5 for a more in-depth explanation of the parameters.
%
% (static)
%
-spec create() -> pid().
create() ->
	create( _Blocking=true ).



% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding trace filename, start mode (immediate here, not deferred)
% and trace type (LogMX-based here, not text based), with no PID specified for
% the trace aggregator.
%
% See create/5 for a more in-depth explanation of the parameters.
%
% (static)
-spec create( boolean() ) -> pid().
create( Blocking ) ->
	create( Blocking, ?trace_aggregator_filename ).



% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding start mode (immediate here, not deferred) and trace type
% (LogMX-based here, not text based), with no PID specified for the trace
% aggregator.
%
% See create/5 for a more in-depth explanation of the parameters.
%
% (static)
-spec create( boolean(), file_utils:file_name() ) -> pid().
create( Blocking, TraceFilename ) ->
	create( Blocking, TraceFilename, _TraceType=log_mx_traces,
			_TraceAggregatorPid=undefined ).



% Creates the trace supervisor, then blocks iff Blocking is true, with default
% settings regarding start mode (immediate here, not deferred).
%
% See create/5 for a more in-depth explanation of the parameters.
%
% (static)
-spec create( boolean(), file_utils:file_name(), traces:trace_type(),
			 pid() | 'undefined' ) -> pid().
create( Blocking, TraceFilename, TraceType, TraceAggregatorPid ) ->
	create( Blocking, _MonitorNow=true, TraceFilename, TraceType,
		   TraceAggregatorPid ).



% Creates a trace supervisor:
%
%  - Blocking tells whether the monitoring should be blocking (if true) or not
%  (the supervisor tool is then launched in the background)
%
%  - MonitorNow tells whether the monitoring should start immediately or only
%  when a monitor/blocking_monitor method is called
%
%  - TraceFilename the trace file to monitor
%
%  - TraceType the expected type of the traces (ex: log_mx_traces, text_traces)
%
%  - TraceAggregatorPid is either the PID of the trace aggregator, or the
%  'undefined' atom
%
-spec create( boolean(), boolean(), file_utils:file_name(),
			 traces:trace_type(), pid() | 'undefined' ) -> pid().
create( Blocking, MonitorNow, TraceFilename, TraceType, TraceAggregatorPid ) ->

	BlockingParam = case Blocking of

		true ->
			self() ;

		false ->
			none

	end,

	new_link( { TraceFilename, TraceType, TraceAggregatorPid },
			 MonitorNow, BlockingParam ).



% Returns the path of the tool and the corresponding file that should be used to
% monitor traces.
%
% (const helper)
-spec get_viewer_settings( wooper_state(), file_utils:file_name() ) ->
					{file_utils:path(),file_utils:file_name()}.
get_viewer_settings( State, Filename ) ->

	case ?getAttr(trace_type) of

		log_mx_traces ->
			{ executable_utils:get_default_trace_viewer_path(), Filename };

		{ text_traces, text_only } ->
			{ executable_utils:get_default_wide_text_viewer_path(?TextWidth),
			 Filename };

		{ text_traces, pdf } ->
			PdfTargetFilename = file_utils:replace_extension( Filename,
				?TraceExtension, ".pdf" ),
			{ executable_utils:get_default_pdf_viewer_path(),
			 PdfTargetFilename }

	end.



% Helper for trace macros.
%
% Use the --batch option (ex: erl --batch, or with the make system 'make
% MY_TARGET CMD_LINE_OPT="--batch") to disable the use of the trace supervisor.
%
-spec init( file_utils:file_name(), traces:trace_type(), pid() ) ->
				  'no_trace_supervisor_wanted' | pid().
init( TraceFilename, TraceType, TraceAggregatorPid ) ->

	% By default (with no specific option) a synchronous supervisor is wanted
	% (wait for its launch to complete):

	% One '-' already eaten:
	case executable_utils:is_batch() of

		true ->
			% Option specified to disable the supervisor:
			%io:format( "Supervisor disabled.~n" ),
			no_trace_supervisor_wanted;

		false ->
			% Default: a trace supervisor is used.
			%io:format( "Supervisor enabled.~n" ),
			create( _BlockingSupervisor=true, TraceFilename, TraceType,
				   TraceAggregatorPid )
			%io:format( "Waiting for trace supervisor to be closed.~n" )

	end.



% Waits, usually at the end of a test, for any trace supervisor to be closed by
% the user.
%
-spec wait_for() -> basic_utils:void().
wait_for() ->

	case executable_utils:is_batch() of

		true ->
			% No supervisor was launched.
			% Let live the system for some time instead:
			system_utils:await_output_completion();

		false ->
			% A supervisor must be waited for:
			io:format( "(waiting for the user to stop the trace supervision)~n"
					  ),

			receive

				{ wooper_result, monitor_ok } ->
					%io:format( "Notification received from supervisor.~n" ),
					% Not {test,app}_info, as used in both contexts:
					class_TraceEmitter:send_standalone( info,
						"Traces successfully monitored." )

			end

	end.
