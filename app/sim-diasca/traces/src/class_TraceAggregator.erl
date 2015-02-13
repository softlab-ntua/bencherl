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


% Most basic trace aggregator.
%
% It just collects traces from emitters and store them at once in a file.
%
% Trace listeners can connect to the aggregator. In this case it will stop and
% send first the full current trace file to them. From that moment, incoming
% traces will be both written in file and sent to each trace listener still
% connected.
%
-module(class_TraceAggregator).



% Determines what are the mother classes of this class (if any):
% (the trace aggregator is not a trace emitter)
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, TraceFilename, TraceType, TraceTitle,
		 IsPrivate, IsBatch ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, send/10, addTraceListener/2,
		 removeTraceListener/2, requestReadyNotification/1 ).


% Static method declarations (to be directly called from module):
-export([ create/1, get_aggregator/1, remove/0 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For TraceExtension:
-include("traces.hrl").


% For registration name:
-include("class_TraceAggregator.hrl").


-define(LogPrefix,"[Trace Aggregator]").


% Use global:registered_names() to check aggregator presence.


%-define( LogOutput( Message, Format ), io:format( Message, Format ) ).
-define( LogOutput( Message, Format ), void ).





% Implementation Notes.
%
% The aggregator could store per-emitter constant settings (ex: emitter name and
% categorization) instead of having them sent to it each time.




% Constructs a new trace aggregator:
%
% - TraceFilename is the name of the file where traces should be written to
%
% - TraceType is either 'log_mx_traces', '{ text_traces, text_only }' or
% '{ text_traces, pdf }', depending whether LogMX should be used to browse the
% execution traces, or just a text viewer
%
% - TraceTitle is the title that should be used for traces; mostly used for the
% PDF output
%
% - IsPrivate tells whether this trace aggregator will be privately held (hence
% should not be registered in naming service) or if it is a (registered)
% singleton
%
% - IsBatch tells whether the aggregator is run in a batch context; useful when
% trace type is {text_traces,pdf}, so that this aggregator does not display the
% produced PDF when in batch mode
%
-spec construct( wooper:state(), file_utils:file_name(), traces:trace_type(),
				string(), boolean(), boolean() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes (none here), then this class-specific
	% actions:


	% Creates the trace file as soon as possible:
	File = open_trace_file( TraceFilename ),

	% Increases the chances that the aggregator does not lag too much behind the
	% current simulation state:
	erlang:process_flag( priority, _Level=high ),

	SetState = setAttributes( State, [

		{ trace_filename, TraceFilename },
		{ trace_file, File },
		{ trace_type, TraceType },
		{ trace_title, TraceTitle },
		{ trace_listeners, [] },
		{ is_batch, IsBatch }

											 ] ),

	Message = io_lib:format( "Trace aggregator created, "
							 "trace filename is '~s', trace type is '~w', "
							 "and trace title is '~s'.~n",
							 [ TraceFilename, TraceType, TraceTitle ] ),

	TimestampText = text_utils:string_to_binary(
					  basic_utils:get_textual_timestamp() ),

	% Not State available here:
	EmitterNode = class_TraceEmitter:get_emitter_node_as_binary(),

	MessageCategorization = text_utils:string_to_binary( "Trace Management" ),

	SendState = executeOneway( SetState, send, [

		_TraceEmitterPid=self(),
		_TraceEmitterName=text_utils:string_to_binary( "Trace Aggregator" ),
		_TraceEmitterCategorization="Trace.Emitter",
		_Tick=none,
		_Time=TimestampText,
		_Location=EmitterNode,
		MessageCategorization,
		_Priority=class_TraceEmitter:get_priority_for( info ),
		Message

												] ),

	io:format( "~n~s ~s", [ ?LogPrefix, Message ] ),


	PrivateState = case IsPrivate of

		true ->
			io:format( "~n~s Creating a private trace aggregator, "
					   "whose PID is ~w.~n", [ ?LogPrefix, self() ] ),
			setAttribute( SendState, is_private, true );

		false ->
			%io:format( "~n~s Creating the trace aggregator, "
			%	"whose PID is ~w.~n", [ ?LogPrefix, self() ] ),

			basic_utils:register_as( ?trace_aggregator_name, local_and_global ),

			setAttribute( SendState, is_private, false )

	end,

	% Closure used to avoid exporting the function (beware of self()):
	AggregatorPid = self(),
	OverLoadMonitorPid = spawn_link( fun() ->
					overload_monitor_main_loop( AggregatorPid ) end ),

	OverloadState = setAttribute( PrivateState, overload_monitor_pid,
								  OverLoadMonitorPid ),

	% Returns an updated state:
	manage_trace_header( OverloadState ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%io:format( "~s Deleting trace aggregator.~n", [ ?LogPrefix ] ),

	?getAttr(overload_monitor_pid) ! delete,

	% Class-specific actions:
	case ?getAttr(is_private) of

		true ->
			ok;

		false ->
			basic_utils:unregister( ?trace_aggregator_name, local_and_global )

	end,

	FooterState = manage_trace_footer(State),

	% We were performing immediate writes here (due to the delayed_write option,
	% close may return an old write error and not even try to close the file. In
	% that case we try to close it another time):
	%
	case file:close( ?getAttr(trace_file) ) of

		{ error, _Reason } ->
			file:close( ?getAttr(trace_file) ) ;

		ok  ->
			ok

	end,

	case ?getAttr(trace_type) of

		log_mx_traces ->
			ok;

		{ text_traces, text_only } ->
			ok;

		{ text_traces, pdf } ->

			io:format( "~s Generating PDF trace report.~n", [ ?LogPrefix ] ),

			PdfTargetFilename = file_utils:replace_extension(
				?getAttr(trace_filename), ?TraceExtension, ".pdf" ),

			GenerationCommand = "if make " ++ PdfTargetFilename
				++ " VIEW_PDF=no 1>/dev/null 2>&1; "
				"then echo ok; else echo error; fi",

			%io:format( "PDF generation command is '~s'.~n",
			% [ GenerationCommand ] ),

			case os:cmd( GenerationCommand ) of

				"ok\n" ->

					case ?getAttr(is_batch) of

						true ->
							ok;

						false ->
							io:format( "~s Displaying PDF trace report.~n",
									  [ ?LogPrefix ] ),

							executable_utils:display_pdf_file(
							   PdfTargetFilename )

						end;

				"error\n" ->
					io:format( "~s Generation of PDF from ~s failed.~n",
						[ ?LogPrefix, ?getAttr(trace_filename) ] )

			end

	end,

	% Then call the direct mother class counterparts: (none)

	io:format( "~s Aggregator deleted.~n", [ ?LogPrefix ] ),

	% Allow chaining:
	FooterState.





% Methods section.


% Sends a full trace to this aggregator to have it processed, i.e. stored or
% directly written.
%
% The nine fields correspond to the ones defined in our trace format.
%
% (const oneway)
%
-spec send( wooper:state(), pid(), traces:emitter_name(),
		   traces:emitter_categorization(), traces:tick(), traces:time(),
		   traces:location(), traces:message_categorization(),
		   traces:priority(), traces:message() ) -> oneway_return().
send( State, TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ) ->

	% Useful to check that all fields are of minimal sizes (ex: binaries):
	%io:format( "- TraceEmitterPid: ~w~n- TraceEmitterName: ~w~n"
	%		   "- TraceEmitterCategorization: ~w~n- Tick: ~w~n- Time: ~w~n"
	%		   "- Location: ~w~n- MessageCategorization: ~w~n- Priority: ~w~n"
	%		   "- Message: ~w~n~n",
	%		  [ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
	%		  Tick, Time, Location, MessageCategorization, Priority,
	%         Message ] ),

	Trace = format_trace_for( ?getAttr(trace_type),
		{ TraceEmitterPid, TraceEmitterName,
		TraceEmitterCategorization, Tick, Time, Location,
		MessageCategorization, Priority, Message } ),

	% Was: io:format( ?getAttr(trace_file), "~s", [ Trace ] ),
	% but now we use faster raw writes:
	%ok = file:write( ?getAttr(trace_file), io_lib:format( "~s", [ Trace ] ) ),
	file_utils:write( ?getAttr(trace_file), Trace ),

	Listeners = ?getAttr(trace_listeners),

	case Listeners of

		[] ->
			ok;

		_AtLeastOne ->

			BinTrace = text_utils:string_to_binary( Trace ),

			lists:foreach(
				fun( Listener ) ->

					Listener ! { addTrace, BinTrace }

				end,
				Listeners )

	end,

	?wooper_return_state_only( State ).



% Adds specified trace listener to this aggregator.
%
% (oneway)
%
-spec addTraceListener( wooper:state(), pid() ) -> oneway_return().
addTraceListener( State, ListenerPid ) ->

	% Better log this events directly in the traces:
	NewState = case ?getAttr(trace_type) of

		log_mx_traces ->

			TraceFile = ?getAttr(trace_file),

			% Done ASAP, otherwise the listener could lose traces if buffers of
			% all levels were not flushed:

			% Not sufficient by itself...
			file:sync( TraceFile ),

			% ...so we have also to close and re-open:
			file_utils:close( TraceFile ),

			% Not a trace emitter but still able to send traces (to itself);
			% will be read from mailbox as first live-forwarded message:
			%
			?notify_info_fmt( "Trace aggregator adding trace listener ~w, "
				"and sending it previous traces.~n", [ ListenerPid ] ),

			% Transfers file:
			TraceFilename = ?getAttr(trace_filename),

			% We used to rely on basic ZIP over Erlang carrier:
			%Bin = file_utils:file_to_zipped_term( TraceFilename ),
			%ListenerPid ! { trace_zip, Bin, TraceFilename },

			% Now we prefer using xz over sendFile, which must be a lot more
			% efficient:
			%
			XZFilename = file_utils:compress( TraceFilename,
											  _CompressionFormat=xz ),

			% If compressing 'traceManagement_test.traces' for example, we do
			% not want to send 'traceManagement_test.traces.xz', as if the
			% listener (ex: traceListening_test) happens to decompress the
			% received file on the same directory, it will overwrite the first
			% file.

			% That way we would be producing
			% "Listener-traceManagement_test.traces" for example:
			%
			% The client will have nevertheless to receive this file to a
			% temporary ianother directory, as the same client-side overwriting
			% could happen directly with the compressed file (which have to
			% exist simultaneously on both ends).
			%
			% So finally the server-side renaming is useless, it will be done by
			% the listener:

			io:format( "Sending '~s' to ~w.~n", [ XZFilename, ListenerPid ] ),

			net_utils:send_file( XZFilename, ListenerPid ),

			file_utils:remove_file( XZFilename ),

			NewListeners = [ ListenerPid | ?getAttr(trace_listeners) ],

			NewFile = reopen_trace_file( TraceFilename ),

			setAttributes( State, [
								   { trace_listeners, NewListeners },
								   { trace_file, NewFile } ] );


		OtherTraceType ->

			Message = io_lib:format(
				"Trace aggregator not adding trace listener ~w, "
				"as it requires LogMX traces, whereas the current trace "
				"type is ~w.~n", [ ListenerPid, OtherTraceType ] ),

			io:format( "Warning: " ++ Message ),
			?notify_warning( Message ),
			ListenerPid ! { trace_zip, incompatible_trace_type },
			State

	end,

	?wooper_return_state_only( NewState ).



% Removes specified trace listener from this aggregator.
%
% (oneway)
%
-spec removeTraceListener( wooper:state(), pid() ) -> oneway_return().
removeTraceListener( State, ListenerPid ) ->

	io:format( "~s Removing trace listener ~w.~n",
			   [ ?LogPrefix, ListenerPid ] ),

	?notify_info_fmt( "Trace aggregator removing trace listener ~w.~n",
					  [ ListenerPid ] ),

	UnregisterState = deleteFromAttribute( State, trace_listeners,
										   ListenerPid ),

	?wooper_return_state_only( UnregisterState ).



% Requests this aggregator to send a notification as soon as it is ready,
% i.e. as soon as its trace file was created and its first trace was written.
%
% (const request)
%
-spec requestReadyNotification( wooper:state() ) ->
									  request_return( 'trace_file_ready' ).
requestReadyNotification( State ) ->

	% Being able to answer means ready, as a first synchronised message is sent
	% from the constructor:
	?wooper_return_state_result( State, trace_file_ready ).




% 'Static' methods (module functions):


% Creates the trace aggregator asynchronously, with default settings.
%
% (static)
%
-spec create( boolean() ) -> pid().
create( UseSynchronousNew ) ->
	create( UseSynchronousNew, _TraceType=log_mx_traces ).



% Creates the trace aggregator asynchronously, using specified trace type.
%
% (static)
%
-spec create( boolean(), traces:trace_type() ) -> pid().
create( _UseSynchronousNew=false, TraceType ) ->
	new_link( ?trace_aggregator_filename, TraceType, ?TraceTitle,
			  _IsPrivate=false, _IsBatch=false  );

% Creates the trace aggregator synchronously, using specified trace type.
%
% (static)
%
create( _UseSynchronousNew = true, TraceType ) ->
	% Trace filename, isPrivate:
	synchronous_new_link( ?trace_aggregator_filename, TraceType,
						  ?TraceTitle, _IsPrivate=false, _IsBatch=false ).




% Returns the PID of the current trace aggregator.
%
% The parameter is a boolean telling whether the aggregator should be created if
% not available (if true), or if this method should just return a failure
% notification (if false).
%
% Note: to avoid race conditions between concurrent calls to this static method
% (ex: due to multiple trace emitter instances created in parallel), an
% execution might start with a call to this method with a blocking wait until
% the aggregator pops up in registry services.
%
% Waits a bit before giving up: useful when client and aggregator processes are
% launched almost simultaneously.
%
% (static)
%
-spec get_aggregator( boolean() ) ->
	   'trace_aggregator_launch_failed' | 'trace_aggregator_not_found' | pid().
get_aggregator( CreateIfNotAvailable ) ->

	% Only dealing with registered managers (instead of using directly their
	% PID) allows to be sure only one instance (singleton) is being used, to
	% avoid the case of two managers being launched at the same time (the second
	% will then terminate immediately).

	% If launching multiple trace emitters in a row, first emitter may trigger
	% the launch of trace aggregator, but second emitter might do the same if
	% the aggregator is still being initialized:
	%
	try

		basic_utils:wait_for_global_registration_of( ?trace_aggregator_name )

	catch { global_registration_waiting_timeout, _Name } ->

			case CreateIfNotAvailable of

				true ->

					% Not available, launch it synchronously (with default
					% settings):
					create(true),

					try

						basic_utils:wait_for_global_registration_of(
							?trace_aggregator_name )

					catch { global_registration_waiting_timeout, _Name } ->

							error_logger:error_msg(
								"class_TraceAggregator:get_aggregator "
								"unable to launch successfully "
								"the aggregator.~n" ),
							trace_aggregator_launch_failed

					end;

				false ->
					% Not available and not to be started:
					trace_aggregator_not_found

			end

	end.



% Deletes synchronously the trace aggregator.
%
% (static)
%
-spec remove() -> 'delete' | 'trace_aggregator_not_found'.
remove() ->

	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->
			trace_aggregator_not_found;

		TraceAggregatorPid ->
			% WOOPER convenience:
			wooper:delete_synchronously_instance( TraceAggregatorPid )

	end.



% Code of the process that monitors the aggregator, overloading-wise.
%
overload_monitor_main_loop( AggregatorPid ) ->

	receive

		delete ->
			%io:format( "(overload monitor deleted)~n" ),
			deleted

	% Every 2s:
	after 2000 ->

			{ message_queue_len, QueueLen } = erlang:process_info(
				AggregatorPid, message_queue_len ),

			case QueueLen of

				TooMany when TooMany > 5000 ->
					io:format( "(warning: trace aggregator is overloaded, "
							   "too many traces are being sent, ~B of them "
							   "are still waiting to be processed)~n",
							   [ TooMany ] );

				_Other ->
					ok

			end,
			overload_monitor_main_loop( AggregatorPid )

	end.





% Some defines.


% Columns in text traces have fixed width, in characters (total: 110).
% In this mode, by default, emitter categorization, location and message
% categorization are not written.


% For Pid (ex: locally, <0.33.0>):
-define( PidWidth, 8 ).


% For EmitterName (ex: "First soda machine"):
-define( EmitterNameWidth, 12 ).


% For EmitterCategorization (ex: "TimeManagement"):
%-define( EmitterCategorizationWidth,12 ).
-define( EmitterCategorizationWidth, 0 ).


% For Tick (ex: unknown, 3169899360000):
-define( TickWidth, 14 ).


% For Time (ex: "18/6/2009 16:32:14"):
-define( TimeWidth, 10 ).


% For Location (ex: "soda_deterministic_integration_run@a_example.org"):
%-define( LocationWidth,12 ).
-define( LocationWidth, 0 ).


% For MessageCategorization (ex: "Execution.Uncategorized"):
%-define( MessageCategorizationWidth, 4 ).
-define( MessageCategorizationWidth, 0 ).


% For Priority (ex: warning):
-define( PriorityWidth, 7 ).


% For Message:
-define( MessageWidth, 45 ).




% Helper functions.



% Takes care of any header in the trace header.
%
% Returns an updated state.
%
% (helper function)
%
-spec manage_trace_header( wooper:state() ) -> wooper:state().
manage_trace_header(State) ->

	case ?getAttr(trace_type) of

		log_mx_traces ->
			State;

		{ text_traces, _TargetFormat } ->

			Title = ?getAttr(trace_title) ++ " Execution Trace Report",

			% Builds a proper RST-compatible title layout:
			TitleText = io_lib:format(
				   "~s~n.. _table:~n~n.. contents:: Table of Contents~n~n",
				   [ text_utils:generate_title(Title,1) ] )
				++ io_lib:format( "~s", [ text_utils:generate_title(
												 "Execution Context", 2 ) ] )
				++ io_lib:format( "Report generated on ~s, "
					"from trace file ``~s``, on host ``~s``.~n~n",
					[ basic_utils:get_textual_timestamp(),
					 ?getAttr(trace_filename), net_adm:localhost() ] )
				++ io_lib:format( "~s",
					[ text_utils:generate_title( "Trace Begin", 2 ) ] ),

			PidLines = text_utils:format_text_for_width(
				"Pid of Trace Emitter", ?PidWidth ),

			EmitterNameLines = text_utils:format_text_for_width(
				"Emitter Name", ?EmitterNameWidth ),

			TickLines = text_utils:format_text_for_width(
				"Execution Tick", ?TickWidth ),

			TimeLines = text_utils:format_text_for_width( "User Time",
				?TimeWidth ),

			PriorityLines = text_utils:format_text_for_width(
				"Trace Type", ?PriorityWidth ),

			MessageLines = text_utils:format_text_for_width(
				"Trace Message", ?MessageWidth ),

			HeaderLine = format_linesets( PidLines, EmitterNameLines,
				TickLines, TimeLines, PriorityLines, MessageLines ) ,

			file_utils:write( ?getAttr(trace_file),
							TitleText ++ get_row_separator() ++ HeaderLine
							++ get_row_separator( $= ) ),

			State

	end.



% Takes care of any header in the trace header.
%
% Returns an updated state.
%
% (helper function)
%
-spec manage_trace_footer( wooper:state() ) -> wooper:state().
manage_trace_footer( State ) ->

	case ?getAttr(trace_type) of

		log_mx_traces ->
			State;

		{ text_traces, _TargetFormat } ->
			ok = file:write( ?getAttr(trace_file), io_lib:format(
					   "~s~n~nEnd of execution traces.~n"
					   "~nBack to the table_ of contents "
					   "and to the beginning of traces.",
				[ text_utils:generate_title( "Trace End", 2 ) ] ) ),
			State

	end.



% Returns the typical separator between array rows.
%
get_row_separator() ->
	get_row_separator( $- ).


% Returns the typical separator between array rows, with specified dash element
% to represent horizontal lines.
%
get_row_separator( DashType ) ->
	[ $+ ] ++ string:chars( DashType, ?PidWidth )
		++ [ $+ ] ++ string:chars( DashType, ?EmitterNameWidth )
		++ [ $+ ] ++ string:chars( DashType, ?TickWidth )
		++ [ $+ ] ++ string:chars( DashType, ?TimeWidth )
		++ [ $+ ] ++ string:chars( DashType, ?PriorityWidth )
		++ [ $+ ] ++ string:chars( DashType, ?MessageWidth )
		++ [ $+ ] ++ "\n".



% Formats specified trace according to specified trace type.
%
% Returns a plain string.
%
-spec format_trace_for( traces:trace_type(),
		 { pid(), traces:emitter_name(), traces:emitter_categorization(),
		  traces:tick(), traces:time(), traces:location(),
		  traces:message_categorization(), traces:priority(),
		  traces:message() } ) -> string().
format_trace_for( log_mx_traces, { TraceEmitterPid,
		TraceEmitterName, TraceEmitterCategorization, Tick, Time, Location,
		MessageCategorization, Priority, Message } ) ->
	lists:flatten( io_lib:format( "~w|~s|~s|~w|~s|~s|~s|~w|~s~n",
		[ TraceEmitterPid, TraceEmitterName, TraceEmitterCategorization,
		Tick, Time, Location, MessageCategorization, Priority, Message ] ) );

format_trace_for( { text_traces, _TargetFormat }, { TraceEmitterPid,
		TraceEmitterName, _TraceEmitterCategorization, Tick, Time, _Location,
		_MessageCategorization, Priority, Message } ) ->

	% Not output here:
	% - TraceEmitterCategorization
	% - Location
	% - MessageCategorization

	PidLines = text_utils:format_text_for_width(
		io_lib:format( "~w", [ TraceEmitterPid ] ), ?PidWidth ),

	EmitterNameLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ TraceEmitterName ] ), ?EmitterNameWidth ),

	% Can be a tick or an atom like 'unknown':
	TickLines = text_utils:format_text_for_width(
		io_lib:format( "~p", [ Tick ] ), ?TickWidth ),

	TimeLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ Time ] ), ?TimeWidth ),

	PriorityLines = text_utils:format_text_for_width(
		io_lib:format( "~w", [
			class_TraceEmitter:get_channel_name_for_priority( Priority ) ] ),
		?PriorityWidth ),

	MessageLines = text_utils:format_text_for_width(
		io_lib:format( "~s", [ Message ] ), ?MessageWidth ),

	format_linesets( PidLines, EmitterNameLines, TickLines, TimeLines,
		PriorityLines, MessageLines ) ++ get_row_separator().



% Formats specified list of linesets.
%
format_linesets( PidLines, EmitterNameLines, TickLines, TimeLines,
		PriorityLines, MessageLines ) ->

	Columns = [ PidLines, EmitterNameLines, TickLines, TimeLines, PriorityLines,
		MessageLines ],

	TotalLineCount = lists:max( [ length( L ) || L <- Columns ] ),

	ColumnsPairs = [ { PidLines, ?PidWidth },
					 { EmitterNameLines, ?EmitterNameWidth },
					 { TickLines, ?TickWidth },
					 { TimeLines, ?TimeWidth },
					 { PriorityLines, ?PriorityWidth },
					 { MessageLines, ?MessageWidth } ],

	%io:format( "Column pairs:~n~p~n", [ColumnsPairs] ),

	FullLines = format_full_lines( ColumnsPairs, _Acc=[], TotalLineCount,
								   _Res=[], _CurrentLine="" ),

	string:join( FullLines, "\n" ).



% Returns a list of full lines, made from the lines of each column.
% Here we finished to handle all lines (none remaining):
%
format_full_lines( _Rows, [], 0, Res, CurrentLine ) ->
	lists:reverse( [ CurrentLine | Res ] ) ;

% Here we arrived at the end of a global line, preparing for next one:
%
format_full_lines( [], Acc, RemainingLineCount, Res, CurrentLine ) ->
	format_full_lines( lists:reverse(Acc), [], RemainingLineCount-1,
					   [ CurrentLine ++ "|" | Res ], "" );

% Here the corresponding column has no more content, just filling with spaces:
%
format_full_lines( [ { [], Width } | ColumnPairs ], Acc, RemainingLineCount,
				   Res, CurrentLine ) ->
	format_full_lines( ColumnPairs, [ { [], Width } | Acc ], RemainingLineCount,
		Res, CurrentLine ++ "|" ++ string:chars( $\ ,Width ) );

% Here the corresponding column has content, just adding it:
%
format_full_lines( [ { [ Line | OtherLines ], Width } | ColumnPairs ], Acc,
				   RemainingLineCount, Res, CurrentLine ) ->

	format_full_lines( ColumnPairs, [ { OtherLines, Width } | Acc ],
					   RemainingLineCount, Res, CurrentLine ++ "|" ++ Line ).



% Opens the specified trace file for writing from scratch.
%
% (helper)
%
open_trace_file( TraceFilename ) ->

	file_utils:open( TraceFilename,
					 [ write | get_trace_file_base_options() ] ).


% Reopens the specified trace file for writing from last position.
%
% (helper)
%
reopen_trace_file( TraceFilename ) ->

	file_utils:open( TraceFilename,
					 [ append | get_trace_file_base_options() ] ).


% Returns the base option for trace writing.
get_trace_file_base_options() ->

	% Writes to file, as soon as 32KB or 0.5s is reached:
	%
	[ raw, { delayed_write, _Size=32*1024, _Delay=500 } ].
