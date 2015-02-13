% Copyright (C) 2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% This header file is a variation of the class_TraceEmitter.hrl header of the
% Traces layer, made to add spatial information to the traces sent by
% spatialised actors.


% Note: this header file must be included *after* the WOOPER one, as, in
% production mode, this file has to define functions
% (namely trace_disabled/{1,2,3,4,5}).



% This header centralizes notably all macros related to the sending of traces
% from a class_SpatialisedActor.

% These macros must operate on states that are spatialised, i.e. coming directly
% or not from the constructor of class_SpatialisedActor.


% See also class_TraceEmitter.hrl, for plain (non-spatial) actors.


-ifndef(class_SpatialTraceEmitter_hrl_guard).
-define(class_SpatialTraceEmitter_hrl_guard,).


% To avoid that Dialyzer thinks that it will never be called:
%
% (finally commented-out, as this export would have to come in most cases after
% function definitions)
%
% See the explanation about trace_disabled, in class_TraceEmitter.hrl, for more
% details (summary: we have to live with this Dialyzer complaint) .
%
% -export([ get_spatial_message/2 ]).



% Conventions section.



% Technical identifier will be the PID of the trace emitter.

% Name will be the name specified at the creation of the trace emitter.

% EmitterCategorization will the one specified at the creation of the trace
% emitter. It could be deduced from superclasses as well, although it would be
% generally uselessly long and would cause issues in case of multiple
% inheritance.



% See the ENABLE_TRACES make variable to enable/disable tracing:
-ifdef(TracingActivated).


% The type of trace output (ex: LogMX, PDF, etc.) is defined in the traces.hrl
% file.


% The first version of macros uses an explicit state.
%
% The second version of macros uses an implicit state, named 'State', as, except
% in constructors, WOOPER conventions imply such a state exists and, provided
% informations stored in state have not changed (notably emitter name and
% categorization), the initial state declared in a method can be used instead of
% any newer one.



% Selecting a macro implies selecting a level of priority.

% An issue is that the definition of Erlang macros does not take into account
% arities, thus LOG(X) and LOG(X,Y) cannot be defined without a name clash.
%
% This explains why the trace informations have to be specified between brakets:
% using LOG([X,Y]) instead of LOG(X,Y).
%
% Anonymous functions could be used as well.
%
% See http://www.nabble.com/question%3A-macro-definition-tt14840873.html

% For each type of trace (fatal, error, warning, etc.), a few variations of the
% set of specified trace informations are supported.




% Taking 'trace' as an example:
%
% - '?trace( "Hello" )'
%
% - '?trace_cat( "Hello", "My Category" )' ('cat' stands for 'categorized')
%
% - '?trace_full( "Hello", "My Category", 125 )'
%
%
% The use of io_lib:format involved too much typing, so we defined shorter forms
% instead. Taking 'trace' again as an example:
%
% - '?trace_fmt( "Hello ~w.", [V] )' (most frequently used form; 'fmt' stands
% for 'format')
%
% - '?trace_fmt_cat( "Hello ~w.", [V], "My Category" )'
%
% - '?trace_fmt_full( "Hello ~w.", [V], "My Category", 125 )'
%
% (knowing we cannot define macros with same name but different arity)



% Some delay are added when error traces are sent, so that they can be stored
% before the virtual machine is stopped, should it happen (ex: if an exception
% is thrown).
%
% Delays should better be replaced by synchronous operations.


% If traces are enabled, only fatal and error ones will be echoed in the
% terminal, whereas, if the traces are disabled, warning ones will be echoed
% too.


% No variable is to be bound in these macros, otherwise sending more than one
% trace from the same scope would lead to 'badmatch' errors.


% Macros shall not bind variables in their body, otherwise using more than one
% in the same scope would result in the two versions of that variable being
% matched, usually resulting on a badmatch.
%
% So, here, spatial messages are either fed as one-liners to output mechanisms,
% or, when having to be used more than once, they are generated as many times as
% needed (not a real problem, as this extra work happens only in very infrequent
% situations).



% Macros to lighten repetitions induced by macro limitations:
%
% Oddly enough, the macro did not seem correctly expanded (variable 'Message' is
% unbound), so we finally replaced its reference but its actual value.
%
%-define( get_spatial_message_helper, get_spatial_message( Message, State ) ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Fatal, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal( State, Message ),

		class_TraceEmitter:send( fatal, State,
								 get_spatial_message( Message, State ) ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal( Message ),

		class_TraceEmitter:send( fatal, State,
								 get_spatial_message( Message, State ) ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal_cat( State, Message, MessageCategorization ),

		class_TraceEmitter:send( fatal, State,
				get_spatial_message( Message, State ), MessageCategorization ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal_cat( Message, MessageCategorization ),

		class_TraceEmitter:send( fatal, State,
				get_spatial_message( Message, State ), MessageCategorization ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.



% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal_full( State, Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( fatal, State,
				get_spatial_message( Message, State ),
				MessageCategorization, Tick ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		 %
		system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal_full( Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( fatal, State,
				get_spatial_message( Message, State ),
				MessageCategorization, Tick ),

		io:format( "Fatal trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Subsection for Fatal, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal_fmt( State, Message, FormatValues ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ) ),

		io:format( "Fatal trace message: "
					 ++ get_spatial_message( Message, State ) ++ "~n",
				   FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal_fmt( Message, FormatValues ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ) ),

		io:format( "Fatal trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:%
		%
		system_utils:await_output_completion()

).



% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				   get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		io:format( "Fatal trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal_fmt_cat( Message, FormatValues, MessageCategorization ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		io:format( "Fatal trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'fatal' type with specified parameters and an explicit state.
%
-define( send_fatal_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		io:format( "Fatal trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'fatal' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( fatal_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		class_TraceEmitter:send( fatal, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		io:format( "Fatal trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Error, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error( State, Message ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ) ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error( Message ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ) ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error_cat( State, Message, MessageCategorization ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_cat( Message, MessageCategorization ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error_full( State, Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization, Tick ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_full( Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( error, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization, Tick ),

		io:format( "Error trace message: ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).




% Subsection for Error, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error_fmt( State, Message, FormatValues ),

		 class_TraceEmitter:send( error, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) ),

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt( Message, FormatValues ),

		class_TraceEmitter:send( error, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ) ),

		io:format( "Error trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		class_TraceEmitter:send( error, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		io:format( "Error trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),

		class_TraceEmitter:send( error, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		io:format( "Error trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.



% Sends a trace of 'error' type with specified parameters and an explicit state.
%
-define( send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),

		class_TraceEmitter:send( error, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		io:format( "Error trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).



% Sends a trace of 'error' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( error_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		class_TraceEmitter:send( error, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		io:format( "Error trace message: "
				   ++ get_spatial_message( Message, State )
				   ++ "~n", FormatValues ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion()

).











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Warning, without formatting.



% When not in debug mode (like here), one may or may not want warning messages
% to be echoed in the console (like error, fatal, etc. channels).
%
% Swap the display_warning/{1,2} implementations as preferred (and recompile):


-define( display_warning( Message ),

	io:format( "Warning trace message: ~s~n",
			   [ get_spatial_message( Message, State ) ] ),

	% To ensure the asynchronous sending of the trace has a chance to complete,
	% possibly before the interpreter is crashed:
	%
	system_utils:await_output_completion()

).



-define( display_warning_fmt( Message, FormatValues ),

	io:format( "Warning trace message: "
			   ++ get_spatial_message( Message, State ) ++ "~n",
			   FormatValues ),

	% To ensure the asynchronous sending of the trace has a chance to
	% complete, possibly before the interpreter is crashed:
	system_utils:await_output_completion()


).



% Alternate (default) implementation:

%% -define( display_warning( Message ),
%%	ok
%% ).


%% -define( display_warning_fmt( Message, FormatValues ),
%%	ok
%% ).



% Plain, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning( State, Message ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ) ),

		?display_warning( get_spatial_message( Message, State ) )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning( Message ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ) ),

		?display_warning( get_spatial_message( Message, State ) )

).






% Categorized, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_cat( State, Message, MessageCategorization ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization ),

		?display_warning( get_spatial_message( Message, State ) )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_cat( Message, MessageCategorization ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization ),

		?display_warning( get_spatial_message( Message, State ) )

).




% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_full( State, Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization, Tick ),

		?display_warning( get_spatial_message( Message, State ) )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_full( Message, MessageCategorization, Tick ),

		class_TraceEmitter:send( warning, State,
								 get_spatial_message( Message, State ),
								 MessageCategorization, Tick ),

		?display_warning( get_spatial_message( Message, State ) )

).








% Subsection for Warning, with formatting.



% Plain, with 2 variations regarding state: explicit or implicit.



% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt( State, Message, FormatValues ),

		class_TraceEmitter:send( warning, State,
				io_lib:format( get_spatial_message( Message, State ),
							 FormatValues ) ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt( Message, FormatValues ),

		class_TraceEmitter:send( warning, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ) ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_cat( State, Message, FormatValues,
							   MessageCategorization ),

		class_TraceEmitter:send( warning, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),

		class_TraceEmitter:send( warning, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'warning' type with specified parameters and an explicit
% state.
%
-define( send_warning_fmt_full( State, Message, FormatValues,
								MessageCategorization, Tick ),

		class_TraceEmitter:send( warning, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).



% Sends a trace of 'warning' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( warning_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		class_TraceEmitter:send( warning, State, io_lib:format(
				get_spatial_message( Message, State ), FormatValues ),
								 MessageCategorization, Tick ),

		?display_warning_fmt( get_spatial_message( Message, State ),
							  FormatValues )

).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Info, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info( State, Message ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ) )

).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info( Message ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ) )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info_cat( State, Message, MessageCategorization ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info_cat( Message, MessageCategorization ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'info' type with specified parameters and an explicit
% state.
%
-define( send_info_full( State, Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).



% Sends a trace of 'info' type with specified parameters and implicit use of
% a variable named 'State'.
%
-define( info_full( Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).






% Subsection for Info, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt( State, Message, FormatValues ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt( Message, FormatValues ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).


% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).






% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'info' type with specified parameters and an explicit state.
%
-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, Tick ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

).



% Sends a trace of 'info' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( info_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

 ).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Trace, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace( State, Message ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ) )

).



% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace( Message ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ) )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace_cat( State, Message, MessageCategorization ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).



% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace_cat( Message, MessageCategorization ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace_full( State, Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).


% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace_full( Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( trace, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).








% Subsection for Trace, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace_fmt( State, Message, FormatValues ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

)



% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace_fmt( Message, FormatValues ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).



% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace_fmt_cat( Message, FormatValues, MessageCategorization ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a trace of 'trace' type with specified parameters and an explicit state.
%
-define( send_trace_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

).



% Sends a trace of 'trace' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( trace_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		 class_TraceEmitter:send( trace, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

 ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Subsection for Debug, without formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug( State, Message ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ) )

).



% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug( Message ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ) )

).




% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug_cat( State, Message, MessageCategorization ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).



% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_cat( Message, MessageCategorization ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

).






% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug_full( State, Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).



% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_full( Message, MessageCategorization, Tick ),

		 class_TraceEmitter:send( debug, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

).





% Subsection for Debug, with formatting.


% Plain, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug_fmt( State, Message, FormatValues ),

		 class_TraceEmitter:send( debug, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

).



% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt( Message, FormatValues ),

		 class_TraceEmitter:send( debug, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ) )

).





% Categorized, with 2 variations regarding state: explicit or implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		 class_TraceEmitter:send( debug, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).


% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),

		 class_TraceEmitter:send( debug, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

).





% Categorized with tick, with 2 variations regarding state: explicit or
% implicit.


% Sends a debug of 'debug' type with specified parameters and an explicit state.
%
-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),

		 class_TraceEmitter:send( debug, State,	io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

).



% Sends a debug of 'debug' type with specified parameters and implicit use of a
% variable named 'State'.
%
-define( debug_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		 class_TraceEmitter:send( debug, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization, Tick )

 ).














-else. % TracingActivated




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traces are disabled here.
% This 'else' branch will be used iff TracingActivated is not defined above.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% trace_disabled functions are defined to avoid warnings about variables not
% being used.

% Hopefully they will be inlined, and then optimized out as a whole by the
% compiler.

% If not using the trace_disabled functions, deactivating the traces will result
% in variables in classes possibly being declared unused (warning).
%
% Using these functions will cause another problem: if not all macro arities are
% used in that class, the remaining trace_disabled functions will be declared
% themselves as unused.
%
% Exporting them is not a solution, as WOOPER defined already some functions,
% thus no additional exports can be made. And the trace emitter include and the
% WOOPER one cannot be permuted (as this header defines functions as well).
%
% Specifying the parameters 'as are' instead of wrapping them in a
% trace_disabled function (ex: 'State, Message' instead of
% 'trace_disabled(State,Message)' results in the following warning:
% 'Warning: a term is constructed, but never used'



% Used to be exported (otherwise will be themselves determined 'unused'),
% however as explained above could not mix with WOOPER exports, triggering
% "attribute 'export' after function definitions":
%
%-export([ trace_disabled/1, trace_disabled/2, trace_disabled/3,
% trace_disabled/4 ])

% As a result, Dialyzer will complain that 'Function trace_disabled/{1,2,3,4,5}
% will never be called', but it is still the best approach.

% Forced inlining so that trace_disabled functions are optimized out.
%
% It was finally commented out, as it triggered for each trace macro:
% "Warning: a term is constructed, but never used" when traces were deactivated.
% We believe that nonetheless these local do-nothing functions will be optimized
% out by the compiler.
%-compile( {inline,[ trace_disabled/1, trace_disabled/2, trace_disabled/3,
%				   trace_disabled/4, trace_disabled/5 ] } ).


trace_disabled( _ ) ->
	trace_disabled.


trace_disabled( _, _ ) ->
	trace_disabled.


trace_disabled( _, _, _ ) ->
	trace_disabled.


trace_disabled( _, _, _, _ ) ->
	trace_disabled.


trace_disabled( _, _, _, _, _ ) ->
	trace_disabled.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fatal section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Fatal, without formatting.


% Most important trace categories cannot be disabled:

-define( send_fatal( State, Message ),

		io:format( "Fatal trace message (although traces are disabled): ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion(),

		trace_disabled( State )

).


-define( fatal( Message ),

		?send_fatal( State, Message )

).





-define( send_fatal_cat( State, Message, MessageCategorization ),
		?send_fatal( State, Message ++ "(" ++ MessageCategorization ++ ")" )
).


-define( fatal_cat( Message, MessageCategorization ),
		?send_fatal_cat( State, Message, MessageCategorization )
).





-define( send_fatal_full( State, Message, MessageCategorization, Tick ),
		?send_fatal( State, Message ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [ Tick ] ) )
).


-define( fatal_full( Message, MessageCategorization, Tick ),
		?send_fatal_full( State, Message, MessageCategorization, Tick )
).





% Subsection for Fatal, with formatting.


-define( send_fatal_fmt( State, Message, FormatValues ),
		?send_fatal( State, io_lib:format( Message, FormatValues ) )
).


-define( fatal_fmt( Message, FormatValues ),
		?send_fatal_fmt( State, Message, FormatValues )
).





-define( send_fatal_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		?send_fatal( State, io_lib:format( Message, FormatValues )
					 ++ "(" ++ MessageCategorization ++ ")" )
).


-define( fatal_fmt_cat( Message, FormatValues, MessageCategorization ),
		?send_fatal_fmt_cat( State, Message, FormatValues,
							 MessageCategorization )
).





-define( send_fatal_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		?send_fatal( State, io_lib:format( Message, FormatValues )
					 ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [ Tick ] ) )
).


-define( fatal_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		?send_fatal_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick )
).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Error, without formatting.


% Most important trace categories cannot be disabled:

-define( send_error( State, Message ),

		io:format( "Error trace message (although traces are disabled): ~s~n",
				   [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion(),

		trace_disabled( State )

).



-define( error( Message ),

		?send_error( State, Message )

).





-define( send_error_cat( State, Message, MessageCategorization ),

		?send_error( State, Message ++ "(" ++ MessageCategorization ++ ")" )

).


-define( error_cat( Message, MessageCategorization ),

		?send_error_cat( State, Message, MessageCategorization )

).




-define( send_error_full( State, Message, MessageCategorization, Tick ),

		?send_error( State, Message ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [ Tick ] ) )

).


-define( error_full( Message, MessageCategorization, Tick ),

		?send_error_full( State, Message, MessageCategorization, Tick )

).








% Subsection for Error, with formatting.


-define( send_error_fmt( State, Message, FormatValues ),

		?send_error( State, io_lib:format( Message, FormatValues ) )

).


-define( error_fmt( Message, FormatValues ),

		?send_error_fmt( State, Message, FormatValues )

).




-define( send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		?send_error( State, io_lib:format( Message, FormatValues )
					 ++ "(" ++ MessageCategorization ++ ")" )

).


-define( error_fmt_cat( Message, FormatValues, MessageCategorization ),

		?send_error_fmt_cat( State, Message, FormatValues,
							 MessageCategorization )

).








-define( send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),

		?send_error( State, io_lib:format( Message, FormatValues )
					 ++ "(" ++ MessageCategorization
					 ++ io_lib:format( ") at tick ~w", [ Tick ] ) )

).


-define( error_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		?send_error_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick )

).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Warning section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% We may choose either that warning are displayed on the console when traces are
% disabled, or not; one just have to uncomment the relevant parts.


% Subsection for Warning, without formatting.



-define( send_warning( State, Message ),

		io:format( "Warning trace message (although traces are disabled): "
				  "~s~n", [ get_spatial_message( Message, State ) ] ),

		% To ensure the asynchronous output of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		%
		system_utils:await_output_completion(),

		trace_disabled( State, ok )

).


-define( warning( Message ),

		?send_warning( State, Message )
		%trace_disabled( State, Message )

).




-define( send_warning_cat( State, Message, MessageCategorization ),

		?send_warning( State, Message ++ "(" ++ MessageCategorization ++ ")" )
		%trace_disabled( State, Message, MessageCategorization )

).


-define( warning_cat( Message, MessageCategorization ),

		?send_warning_cat( State, Message, MessageCategorization )
		%trace_disabled( State, Message, MessageCategorization )

).




-define( send_warning_full( State, Message, MessageCategorization, Tick ),

		?send_warning( State, Message ++ "(" ++ MessageCategorization
					   ++ io_lib:format( ") at tick ~w", [ Tick ] ) )
		%trace_disabled( State, Message, MessageCategorization, Tick )

).


-define( warning_full( Message, MessageCategorization, Tick ),

		?send_warning_full( State, Message, MessageCategorization, Tick )
		%trace_disabled( State, Message, MessageCategorization, Tick )

).





% Subsection for Warning, with formatting.


-define( send_warning_fmt( State, Message, FormatValues ),

		?send_warning( State, io_lib:format( Message, FormatValues ) )
		%trace_disabled( State, Message, FormatValues )

).


-define( warning_fmt( Message, FormatValues ),

		?send_warning_fmt( State, Message, FormatValues )
		%trace_disabled( State, Message, FormatValues )

).



-define( send_warning_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		?send_warning( State, io_lib:format( Message, FormatValues )
					   ++ "(" ++ MessageCategorization ++ ")" )

		%trace_disabled( State, Message, FormatValues,
		%					 MessageCategorization )

).


-define( warning_fmt_cat( Message, FormatValues, MessageCategorization ),

		?send_warning_fmt_cat( State, Message, FormatValues,
							   MessageCategorization )

		%trace_disabled( State, Message, FormatValues, MessageCategorization )

).





-define( send_warning_fmt_full( State, Message, FormatValues,
								MessageCategorization, Tick ),

		?send_warning( State, io_lib:format( Message, FormatValues )
					   ++ "(" ++ MessageCategorization
					   ++ io_lib:format( ") at tick ~w", [ Tick ] ) )

		%trace_disabled( State, Message, FormatValues,
		%				  MessageCategorization, Tick )

).


-define( warning_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		?send_warning_fmt_full( State, Message, FormatValues,
								MessageCategorization, Tick )

		%trace_disabled( State, Message, FormatValues, MessageCategorization,
		%			   Tick )

).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Info section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Info, without formatting.


% Most important trace categories cannot be disabled:

-define( send_info( State, Message ),
		trace_disabled( State, Message )
).


-define( info( Message ),
		trace_disabled( State, Message )
).






-define( send_info_cat( State, Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).


-define( info_cat( Message, MessageCategorization ),
		trace_disabled( State, Message, MessageCategorization )
).




-define( send_info_full( State, Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( info_full( Message, MessageCategorization, Tick ),
		trace_disabled( State, Message, MessageCategorization, Tick )
).






% Subsection for Info, with formatting.


-define( send_info_fmt( State, Message, FormatValues ),
		trace_disabled( State, Message, FormatValues )
).


-define( info_fmt( Message, FormatValues ),

		trace_disabled( State, Message, FormatValues )

).





-define( send_info_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),

		trace_disabled( State, Message, FormatValues, MessageCategorization )

).


-define( info_fmt_cat( Message, FormatValues, MessageCategorization ),

		trace_disabled( State, Message, FormatValues, MessageCategorization )

).






-define( send_info_fmt_full( State, Message, FormatValues,
							 MessageCategorization, Tick ),

		trace_disabled( State, Message, FormatValues,
						MessageCategorization, Tick )

).


-define( info_fmt_full( Message, FormatValues, MessageCategorization, Tick ),

		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 Tick )

).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trace section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Trace, without formatting.


% Most important trace categories cannot be disabled:

-define( send_trace( State, Message ),
		 trace_disabled( State, Message )
).


-define( trace( Message ),
		 trace_disabled( State, Message )
).





-define( send_trace_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( trace_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_trace_full( State, Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( trace_full( Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).






% Subsection for Trace, with formatting.


-define( send_trace_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( trace_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).



-define( send_trace_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).


-define( trace_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).



-define( send_trace_fmt_full( State, Message, FormatValues,
						  MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, Tick )
).


-define( trace_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 Tick )
).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Debug section.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Subsection for Debug, without formatting.


% Most important debug categories cannot be disabled:

-define( send_debug( State, Message ),
		 trace_disabled( State, Message )
).


-define( debug(Message),
		 trace_disabled( State, Message )
).






-define( send_debug_cat( State, Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).


-define( debug_cat( Message, MessageCategorization ),
		 trace_disabled( State, Message, MessageCategorization )
).





-define( send_debug_full( State, Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).


-define( debug_full( Message, MessageCategorization, Tick ),
		 trace_disabled( State, Message, MessageCategorization, Tick )
).








% Subsection for Debug, with formatting.


-define( send_debug_fmt( State, Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).


-define( debug_fmt( Message, FormatValues ),
		 trace_disabled( State, Message, FormatValues )
).





-define( send_debug_fmt_cat( State, Message, FormatValues,
							 MessageCategorization ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization )
).


-define( debug_fmt_cat( Message, FormatValues, MessageCategorization ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization )
).






-define( send_debug_fmt_full( State, Message, FormatValues,
							  MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues,
						 MessageCategorization, Tick )
).


-define( debug_fmt_full( Message, FormatValues, MessageCategorization, Tick ),
		 trace_disabled( State, Message, FormatValues, MessageCategorization,
						 Tick )
).



-endif. % TracingActivated




% End of the TracingActivated branch.






%
% Section for non-maskable traces.
%



% These tracing primitives are always activated, regardless of the
% TracingActivated setting.
%
% They are sent to the 'info' channel, and are also echoed on the console.

% These report* primitives are the emitter-level counterparts of the standalone
% notify* primitives.



% Plain version, with implicit state.


% Sends a report with specified parameters and implicit use of a variable named
% 'State'.
%
-define( report( Message ),

		 io:format( "[info] " ++ get_spatial_message( Message, State )
					++ "~n" ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ) ),

		% To ensure the asynchronous sending of the trace has a chance to
		% complete, possibly before the interpreter is crashed:
		 %
		system_utils:await_output_completion()

).



% Sends a report with specified parameters and implicit use of a variable named
% 'State'.
%
-define( report_fmt( Message, FormatValues ),

		 % Duplicated calls, as no binding allowed in macros:

		 io:format( "[info] "
					++ io_lib:format( get_spatial_message( Message, State ),
									  FormatValues )
					++ "~n" ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),


		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).




% Categorized version, with implicit state.


% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat( Message, MessageCategorization ),

		 io:format( io_lib:format( "[info][~p]", [ MessageCategorization ] )
					++ get_spatial_message( Message, State ) ++ "~n" ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization )

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).



% Sends a report with specified categorization and implicit use of a variable
% named 'State'.
%
-define( report_cat_fmt( Message, FormatValues, MessageCategorization ),

		 io:format( io_lib:format( "[info][~p]", [ MessageCategorization ] )
			++ io_lib:format( get_spatial_message( Message, State ),
							  FormatValues )
			++ "~n" ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
								  MessageCategorization )

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).




% Categorized version with tick, with implicit state.



% Sends a report with specified categorization and tick, and implicit use of a
% variable named 'State'.
%
-define( report_full( State, Message, MessageCategorization, Tick ),

		 io:format( io_lib:format( "[info][~p][at #~p]",
								   [ MessageCategorization, Tick ] )
					++ get_spatial_message( Message, State ) ++ "~n" ),

		 class_TraceEmitter:send( info, State,
								  get_spatial_message( Message, State ),
								  MessageCategorization, Tick )

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).



% Sends a report with specified categorization and tick, and implicit use of a
% variable named 'State'.
%
-define( report_full_fmt( State, Message, FormatValues, MessageCategorization,
						  Tick ),

		 io:format( io_lib:format( "[info][~p][at #~p]",
								   [ MessageCategorization, Tick ] )
					++ io_lib:format( get_spatial_message( Message, State ),
									  FormatValues )
					++ "~n" ),

		 class_TraceEmitter:send( info, State, io_lib:format(
				 get_spatial_message( Message, State ), FormatValues ),
				MessageCategorization, Tick )

		 % To ensure the asynchronous sending of the trace has a chance to
		 % complete, possibly before the interpreter is crashed:
		 %
		 system_utils:await_output_completion()

).






% Returns an updated message prefixed by any available spatial information.
%
% Defined as a function not to duplicate too much code, and also to avoid
% introducing new bindings in functions using spatial traces.

% (helper)
%
-spec get_spatial_message( string(), wooper:state() ) -> string().
get_spatial_message( Message, State ) ->

	case ?getAttr(position) of

		undefined ->
			Message;

		Pos ->
			ApproxPosString = linear_2D:to_string( Pos, _Precision=1 ),
			Message ++ io_lib:format( "~n[while located around ~s] ",
									  [ ApproxPosString ] )

	end.


-endif. % class_SpatialTraceEmitter_hrl_guard
