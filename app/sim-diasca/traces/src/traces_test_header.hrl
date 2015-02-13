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


% Defines some macros and functions useful for trace-using tests.


-ifndef(TraceEmitterCategorization).

-define( TraceEmitterCategorization, "test" ).

-endif. % TraceEmitterCategorization



% Allows to define exports before functions:
-ifndef(TracingActivated).

-export([ test_trace_disabled/1, test_trace_disabled/2 ]).

-endif. % TracingActivated




% Section for trace output macros.


-ifdef(TracingActivated).



-define( test_fatal( Message ),

		io:format( "Fatal test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( fatal, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_fatal_fmt( MessageFormat, FormatValues ),

		io:format( "Fatal trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( fatal,
						 io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_error( Message ),

		io:format( "Error test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( error, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_error_fmt( MessageFormat, FormatValues ),

		io:format( "Error test trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( error,
						io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_warning( Message ),

		io:format( "Warning test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( warning, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_warning_fmt( MessageFormat, FormatValues ),

		io:format( "Warning test trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( warning,
						io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_info( Message ),

		class_TraceEmitter:send_from_test( info, Message )

).


-define( test_info_fmt( MessageFormat, FormatValues ),

		class_TraceEmitter:send_from_test( info,
						io_lib:format( MessageFormat, FormatValues ) )

).



-define( test_trace( Message ),

		class_TraceEmitter:send_from_test( trace, Message )

).


-define( test_trace_fmt( MessageFormat, FormatValues ),

		class_TraceEmitter:send_from_test( trace,
						io_lib:format( MessageFormat, FormatValues ) )

).



-define( test_debug( Message ),

		class_TraceEmitter:send_from_test( debug, Message )

).


-define( test_debug_fmt( MessageFormat, FormatValues ),

		class_TraceEmitter:send_from_test( debug,
						 io_lib:format( MessageFormat, FormatValues ) )

).




-else. % TracingActivated



% Here TracingActivated is not defined: non-critical traces are disabled.



% Message is returned, as otherwise some variables in calling code could be
% determined as unused, and thus would trigger a warning:



-define( test_fatal( Message ),

		io:format( "Fatal test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( fatal, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_error( Message ),

		io:format( "Error test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( error, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).


-define( test_warning( Message ),

		io:format( "Warning test trace message: ~s~n", [ Message ] ),

		class_TraceEmitter:send_from_test( warning, Message ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).


-define( test_info( Message ), test_trace_disabled( Message ) ).


-define( test_trace( Message ), test_trace_disabled( Message ) ).


-define( test_debug( Message ), test_trace_disabled( Message ) ).




-define( test_fatal_fmt( MessageFormat, FormatValues ),

		io:format( "Fatal test trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( fatal,
						io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).



-define( test_error_fmt( MessageFormat, FormatValues ),

		io:format( "Error test trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( error,
						io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).


-define( test_warning_fmt( MessageFormat, FormatValues ),

		io:format( "Warning test trace message: " ++ MessageFormat ++ "~n",
				  FormatValues ),

		class_TraceEmitter:send_from_test( warning,
						io_lib:format( MessageFormat, FormatValues ) ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		system_utils:await_output_completion()
).


-define( test_info_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).


-define( test_trace_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).


-define( test_debug_fmt( Message, FormatValues ),
		test_trace_disabled( Message, FormatValues ) ).


-endif. % TracingActivated
