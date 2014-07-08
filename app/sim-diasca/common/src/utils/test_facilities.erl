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


% This module defines a few basic facilities for tests, at the level of the
% 'Common' layer.
%
-module(test_facilities).


% To be output before each displayed message:
-define( display_prefix, "  " ).


-export([ start/1, stop/0, display/1, display/2, fail/1, fail/2, finished/0 ] ).



% Starts a test, expected to be the first test statement.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our tests) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the test, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the test process).
%
-spec start( module() | [ module() ] ) -> basic_utils:void().
start( Module ) when is_atom(Module) ->
	erlang:process_flag( trap_exit, false ),
	io:format( "~n~n--> Testing module ~s.~n", [ Module ] );

start( Modules ) when is_list(Modules ) ->
	erlang:process_flag( trap_exit, false ),
	io:format( "~n~n--> Testing modules ~p.~n", [ Modules ] ).



% Stops a test, expected to be the last test statement in the normal case.
-spec stop() -> no_return().
stop() ->
	io:format( "~n--> Successful end of test.~n" ),
	finished().



% Displays a test message.
-spec display( string() ) -> basic_utils:void().
display( Message ) ->
	io:format( Message ++ "\n" ).



% Displays a test message, once formatted.
%
% FormatString is a io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display( string(), list() ) -> basic_utils:void().
display( FormatString, ValueList ) ->
	Message = io_lib:format( FormatString, ValueList ),
	display( Message ).


% Comment out to be able to use the interpreter after the test:
-define(ExitAfterTest,).

-spec finished() -> none().


-ifdef(ExitAfterTest).


finished() ->

	io:format( "(test finished, interpreter halted)~n" ),

	system_utils:await_output_completion(),

	% init:stop/0 could maybe used instead:
	erlang:halt( 0 ).


-else.


finished() ->

	io:format( "(test finished, interpreter still running)~n" ),

	system_utils:await_output_completion(),

	test_success.


-endif.



% To be called whenever a test is to fail (crash on error) immediately.
%
% Ex: test_facilities:fail( "server on strike" )
%
-spec fail( string() ) -> none().
fail( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	io:format( "~n!!!! Test failed, reason: ~s.~n~n", [ Reason ] ),

	erlang:error( "Test failed" ),

	system_utils:await_output_completion(),

	erlang:halt( 1 ).



% To be called whenever a test is to fail (crash on error) immediately.
%
% FormatString is a io:format-style format string, ValueList is the
% corresponding list of field values.
%
% Ex: test_facilities:fail( "server ~s on strike", [ "foobar.org" ] )
%
-spec fail( string(), list() ) -> none().
fail( FormatString, ValueList ) ->

	% For some reason, erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	ErrorMessage = io_lib:format( "~n!!!! Test failed, reason: ~s.~n~n",
								[ io_lib:format( FormatString, ValueList ) ] ),

	io:format( "~n!!!! Test failed, reason: ~s.~n~n", [ ErrorMessage ] ),

	erlang:error( "Test failed" ),

	system_utils:await_output_completion(),

	erlang:halt( 1 ).
