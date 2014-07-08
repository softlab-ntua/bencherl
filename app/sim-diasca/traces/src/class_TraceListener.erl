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


% Trace listener, similar to a remote supervisor.
%
% This version just uses LogMX (http://logmx.com) to track the default execution
% trace file, which will be synchronized automatically: history will be
% retrieved under a zipped form from the aggregator, and next traces will be
% sent directly to this listener as well as to the aggregator.
%
% So the aggregator must have been run with the LogMX trace type beforehand
% (log_mx_traces).
%
-module(class_TraceListener).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, TraceAggregatorPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
	synchronous_new/1, synchronous_new_link/1,
	synchronous_timed_new/1, synchronous_timed_new_link/1,
	remote_new/2, remote_new_link/2, remote_synchronous_new/2,
	remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
	remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
	construct/2, delete/1 ).



% Method declarations.
-define( wooper_method_export, monitor/1, addTrace/2 ).


% Static method declarations (to be directly called from module):
-export([ create/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-define(LogPrefix,"[Trace Listener]").


%-define( LogOutput(Message,Format), io:format(Message,Format) ).
-define( LogOutput(Message,Format), void ).



% Constructs a new trace listener.
%
% TraceAggregatorPid is the PID of the trace aggregator to which this listener
% will be synchronized.
%
-spec construct( wooper_state(), pid() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	io:format( "~s Creating a trace listener whose PID is ~w, "
		"synchronized on trace aggregator ~w.~n",
		[ ?LogPrefix, self(), TraceAggregatorPid ] ),

	% First the direct mother classes (none), then this class-specific actions:

	io:format( "~s Requesting from aggregator a trace synchronization.~n",
		[ ?LogPrefix ] ),

	TraceAggregatorPid ! {addTraceListener,self()},

	receive

		 {trace_zip,Bin,TraceFilename} ->
			% Allows to run for the same directory as aggregator:
			ListenerTraceFilename = "Listener-" ++ TraceFilename,
			io:format( "~s Received from aggregator a trace synchronization "
				"for file '~s', will store it in '~s'.~n",
				[ ?LogPrefix, TraceFilename, ListenerTraceFilename] ),
			file_utils:zipped_term_to_unzipped_file(Bin,ListenerTraceFilename),

			% Will write in it newer traces:
			{ok,File} = file:open( ListenerTraceFilename, [append] ),

			NewState = setAttributes( State, [
				{trace_aggregator_pid,TraceAggregatorPid},
				{trace_filename,ListenerTraceFilename},
				{trace_file,File}
			] ),

			EndState = executeOneway( NewState, monitor ),

			io:format( "~s Trace listener created.~n", [ ?LogPrefix ] ),
			EndState;

		{trace_zip,ErrorReason} ->
			io:format( "~s Trace listener cannot listen to current trace "
				"aggregator, as this aggregator does not use LogMX-based "
				"traces.~n", [ ?LogPrefix ] ),
			throw( {cannot_listen_aggregator,TraceAggregatorPid,ErrorReason} )

	end.



% Overridden destructor.
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	io:format( "~s Deleting trace listener.~n", [ ?LogPrefix ] ),

	% Class-specific actions:
	?getAttr(trace_aggregator_pid) ! {removeTraceListener,self()},
	file:close( ?getAttr(trace_file) ),
	% Then call the direct mother class counterparts: (none)
	io:format( "~s Trace listener deleted.~n", [ ?LogPrefix ] ),

	% Allow chaining:
	State.




% Methods section.


% Triggers an asynchronous supervision (trace monitoring).
% Will return immediately.
% Note: directly inspired from class_TraceSupervisor.erl.
%
% (oneway)
%
-spec monitor( wooper_state() ) -> oneway_return().
monitor( State ) ->

	Filename = ?getAttr( trace_filename ),
	case filelib:is_file( Filename ) of

		true ->
			ok;

		false ->
			error_logger:error_msg( "class_TraceListener:monitor "
				"unable to find trace file '~s'.~n", [ Filename ] ),
			trace_file_not_found

	end,
	io:format( "~s Trace listener will monitor file '~s' with LogMX now.~n",
		[ ?LogPrefix, Filename ] ),

	% Non-blocking (logmx.sh must be found in the PATH):
	[] = os:cmd( executable_utils:get_default_trace_viewer_path() ++ " "
		++ Filename ++ " &" ),

	?wooper_return_state_only( State ).



% Registers a new pre-formatted trace in trace file.
% To be called by the trace aggregator.
%
% (oneway)
%
-spec addTrace( wooper_state(), binary() ) -> oneway_return().
addTrace( State, NewTrace ) ->
	io:format( ?getAttr(trace_file), "~s", [binary_to_list(NewTrace)] ),
	?wooper_return_state_only(State).



% 'Static' methods (module functions):


% Creates the trace listener that will synchronize itself to the specified
% aggregator.
%
% (static)
-spec create( pid() ) -> pid().
create( AggregatorPid ) ->
	new( AggregatorPid ).
