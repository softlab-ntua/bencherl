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


% Include guard:
%
-ifndef(traces_hrl_guard).
-define(traces_hrl_guard,).



% Extension to be used for trace file names:
-define( TraceExtension, ".traces" ).


% Per-test trace file (must be defined before the TraceSupervisor include):
-define( TraceFilename, ( atom_to_list(?MODULE) ++ ?TraceExtension ) ).



% Defines the type of requested execution traces.

% The trace type can be either:
%
% - log_mx_traces, for LogMX-compliant traces (the default): then the trace
% aggregator will use a proper encoding so that the Ceylan Java trace parser,
% plugged into LogMX, allows this tool to be used with Ceylan
%
% - {text_traces,TraceTextOutputType} for more basic text-based traces: then the
% trace aggregator will do its best to format the traces as a human-readable
% trace text file; this is mostly useful when LogMX cannot be used for any
% reason, like needing to generate a report; TraceTextOutputType can be:
%
%  - 'text_only', if wanting to have traces be directly written to disk as pure
%  yet human-readable text
%
%  - 'pdf', if wanting to read finally the traces in a generated PDF file
%
% Note:
%
% - if you change (ex: comment/uncomment) the trace type, then you must
% recompile your modules to take it into account
%
% - check in the class_TraceEmitter.hrl file whether TracingActivated is defined
%
-ifndef(TraceType).
	-define( TraceType, log_mx_traces ).
	%-define( TraceType, { text_traces, pdf } ).
	%-define( TraceType, { text_traces, text_only } ).

-endif. % TraceType



% Defines the trace title (ex: for PDF output), if not already specified:
-ifndef(TraceTitle).
	-define( TraceTitle, "Ceylan" ).
-endif.



% For supervisor macros (ex: init_trace_supervisor):
-include("class_TraceSupervisor.hrl").


% For TracingActivated:
-include("class_TraceEmitter.hrl").


% Defines some macros to emit standalone traces, i.e. not from a TraceEmitter,
% and not for test purpose (ex: when writing classical, non-OOP, code).
%
% Note: using 'notify' instead of 'send' to prevent name clashes.

% Usage: '?notify_debug( "Starting!" )'



-ifdef(TracingActivated).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Fatal, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal( Message ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_cat( Message, EmitterCategorization ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message,
											 EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method). Tick corresponds to any measure of time.
%
-define( notify_fatal_full( Message, EmitterCategorization, Tick ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message,
											 EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).




% Subsection for Fatal, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt( Message, FormatValues ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error( Message ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_cat( Message, EmitterCategorization ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message,
											EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_full( Message, EmitterCategorization, Tick ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message,
											EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).




% Subsection for Error, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt( Message, FormatValues ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Warning, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning( Message ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_cat( Message, EmitterCategorization ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message,
											EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_full( Message, EmitterCategorization, Tick ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message,
											EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).





% Subsection for Warning, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt( Message, FormatValues ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		 class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_full( Message, FormatValues, EmitterCategorization,
								  Tick ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Info, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info( Message ),

		class_TraceEmitter:send_standalone( info, Message,
							  atom_to_list( ?MODULE ) )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_cat( Message, EmitterCategorization ),

		class_TraceEmitter:send_standalone( info, Message,
											EmitterCategorization )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),

		 class_TraceEmitter:send_standalone( info, Message, EmitterName,
							 EmitterCategorization, MessageCategorization )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_full( Message, EmitterCategorization, Tick ),

		class_TraceEmitter:send_standalone( info, Message,
											EmitterCategorization, Tick )

).





% Subsection for Info, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt( Message, FormatValues ),

		class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ) )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt_cat( Message, FormatValues, EmitterCategorization ),

		class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_info_fmt_full( Message, FormatValues, EmitterCategorization,
							   Tick ),

		class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick )

).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Trace, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_trace( Message ),

		class_TraceEmitter:send_standalone( trace, Message )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
-define( notify_trace_cat( Message, EmitterCategorization ),
		 class_TraceEmitter:send_standalone( trace, Message,
											 EmitterCategorization )
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_trace_full( Message, EmitterCategorization, Tick ),

		class_TraceEmitter:send_standalone( trace, Message,
											EmitterCategorization, Tick )

).


% Subsection for Trace, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_trace_fmt( Message, FormatValues ),

		class_TraceEmitter:send_standalone( trace,
							  io_lib:format( Message, FormatValues ) )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_trace_fmt_cat( Message, FormatValues, EmitterCategorization ),

		class_TraceEmitter:send_standalone( trace,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization )

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_trace_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		class_TraceEmitter:send_standalone( trace,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick )

).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_debug( Message ),

		class_TraceEmitter:send_standalone( debug, Message )

).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
-define( notify_debug_cat( Message, EmitterCategorization ),

		class_TraceEmitter:send_standalone( debug, Message,
											EmitterCategorization )

).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_debug_full( Message, EmitterCategorization, Tick ),

		class_TraceEmitter:send_standalone( debug, Message,
											EmitterCategorization, Tick )

).



% Subsection for Debug, with formatting.


% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_debug_fmt( Message, FormatValues ),

		class_TraceEmitter:send_standalone( debug,
							  io_lib:format( Message, FormatValues ) )

).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_debug_fmt_cat( Message, FormatValues, EmitterCategorization ),

		class_TraceEmitter:send_standalone( debug,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization )

).



% To send debugs neither from a DebugEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_debug_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		class_TraceEmitter:send_standalone( debug,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick )

).













-else. % TracingActivated


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here (non-critical ones).
% This 'else' branch will be used iff TracingActivated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Fatal, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal( Message ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_cat( Message, EmitterCategorization ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message,
											EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_full( Message, EmitterCategorization, Tick ),

		io:format( "Fatal standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( fatal, Message,
											EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).




% Subsection for Fatal, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt( Message, FormatValues ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		 class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fatal_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		io:format( "Fatal standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_standalone( fatal,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error( Message ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_cat( Message, EmitterCategorization ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message,
											EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_full( Message, EmitterCategorization, Tick ),

		io:format( "Error standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( error, Message,
											EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).




% Subsection for Error, with formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt( Message, FormatValues ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_error_fmt_full( Message, FormatValues, EmitterCategorization,
								Tick ),

		io:format( "Error standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( error,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Warning, without formatting.


% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning( Message ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_cat( Message, EmitterCategorization ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message,
											EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_full( Message, EmitterCategorization, Tick ),

		io:format( "Warning standalone trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_standalone( warning, Message,
											EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).





% Subsection for Warning, with formatting.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt( Message, FormatValues ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_cat( Message, FormatValues, EmitterCategorization ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				   FormatValues ),

		 class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_warning_fmt_full( Message, FormatValues, EmitterCategorization,
								  Tick ),

		io:format( "Warning standalone trace message: " ++ Message ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_standalone( warning,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()

).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_info( Message ),
		 trace_disabled( Message )
).


-define( notify_info_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_info_em( Message, EmitterName, EmitterCategorization,
						 MessageCategorization ),
		 trace_disabled( Message, EmitterName, EmitterCategorization,
						 MessageCategorization )
).


-define( notify_info_full( Message, EmitterCategorization, Tick ),
		 trace_disabled( Message, EmitterCategorization, Tick )
).



-define( notify_info_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_info_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_info_fmt_full( Message, FormatValues, EmitterCategorization,
				 Tick ),
		 trace_disabled( Message, FormatValues, EmitterCategorization, Tick )
).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_trace( Message ),
		 trace_disabled( Message )
).


-define( notify_trace_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_trace_full( Message, EmitterCategorization, Tick ),
		 trace_disabled( Message, EmitterCategorization, Tick )
).



-define( notify_trace_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_trace_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_trace_fmt_full( Message, FormatValues, EmitterCategorization,
				 Tick ),
		 trace_disabled( Message, FormatValues, EmitterCategorization, Tick )
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define( notify_debug( Message ),
		 trace_disabled( Message )
).


-define( notify_debug_cat( Message, EmitterCategorization ),
		 trace_disabled( Message, EmitterCategorization )
).


-define( notify_debug_full( Message, EmitterCategorization, Tick ),
		 trace_disabled( Message, EmitterCategorization, Tick )
).



-define( notify_debug_fmt( Message, FormatValues ),
		 trace_disabled( Message, FormatValues )
).


-define( notify_debug_fmt_cat( Message, FormatValues, EmitterCategorization ),
		 trace_disabled( Message, FormatValues, EmitterCategorization )
).


-define( notify_debug_fmt_full( Message, FormatValues, EmitterCategorization,
				 Tick ),
		 trace_disabled( Message, FormatValues, EmitterCategorization, Tick )
).


-endif.

% End of the TracingActivated branch.




% Section for non-maskable traces.
%
% These tracing primitives are always activated, regardless of the
% TracingActivated setting.
%
% They are sent to the 'info' channel, and are also echoed on the console.
%
% These notify* primitives are the standalone counterparts of the
% class_TraceEmitter-level report* primitives.



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify( Message ),

		 io:format( "[info] " ++ Message ++ "~n" ),

		 class_TraceEmitter:send_standalone( info, Message ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_cat( Message, EmitterCategorization ),

		 io:format( "[info] " ++ Message ++ "~n" ),

		 class_TraceEmitter:send_standalone( info, Message,
							  EmitterCategorization ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_em( Message, EmitterName, EmitterCategorization,
					MessageCategorization ),

		 % EmitterCategorization ignored here:
		 io:format( "[info] [" ++ EmitterName ++ "] " ++ Message ++ "~n" ),

		 class_TraceEmitter:send_standalone( info, Message, EmitterName,
							 EmitterCategorization, MessageCategorization ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).




% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt( Message, FormatValues ),

		 io:format( "[info] " ++ Message ++ "~n", FormatValues ),

		 class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ) ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt_cat( Message, FormatValues, EmitterCategorization ),

		 io:format( "[info] " ++ Message ++ "~n", FormatValues ),

		 class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).



% To send traces neither from a TraceEmitter instance nor from a test (ex: in a
% static method):
%
-define( notify_fmt_full( Message, FormatValues, EmitterCategorization, Tick ),

		 io:format( "[info] " ++ Message ++ "~n", FormatValues ),

		 class_TraceEmitter:send_standalone( info,
							  io_lib:format( Message, FormatValues ),
							  EmitterCategorization, Tick ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 system_utils:await_output_completion()

).




-endif. % traces_hrl_guard
