% Copyright (C) 2011-2014 Olivier Boudeville
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


% This module defines a few basic facilities for applications, at the level of
% the 'Common' layer.
%
-module(app_facilities).


-export([ start/1, stop/0, display/1, display/2, fail/1, fail/2, finished/0 ] ).


-spec start( module() | [ module() ] ) -> basic_utils:void().
start( Module ) when is_atom(Module) ->
	io:format( "~n~n--> Starting application ~s.~n", [ Module ] );

start( Modules ) when is_list(Modules ) ->
	io:format( "~n~n--> Starting application ~p.~n", [ Modules ] ).




-spec stop() -> no_return().
stop() ->
	io:format( "~n--> Successful termination of application.~n" ),
	finished().



% Displays an application message.
-spec display( string() ) -> basic_utils:void().
display( Message ) ->
	io:format( Message ++ "\n" ).


% Displays an application message, once formatted.
%
% FormatString is a io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display( string(), list() ) -> basic_utils:void().
display( FormatString, ValueList ) ->
	Message = io_lib:format( FormatString, ValueList ),
	display( Message ).




% Comment out to be able to use the interpreter after the app:
-define(ExitAfterApp,).

-spec finished() -> no_return().


-ifdef(ExitAfterApp).

finished() ->

	io:format( "(execution finished, interpreter halted)~n~n" ),

	system_utils:await_output_completion(),

	% Implies flushing (maybe init:stop/0 should be used):
	erlang:halt( 0 ),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_success.

-else.

finished() ->

	io:format( "(execution finished, interpreter still running)~n~n" ),

	system_utils:await_output_completion(),

	app_success.

-endif.




% To be called whenever an application is to fail (crash on error) immediately.
%
% Ex: app_facilities:fail( "server on strike" )
%
-spec fail( string() ) -> no_return().
fail( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	io:format( "~n!!!! Application failed, reason: ~s.~n~n", [ Reason ] ),

	erlang:error( "Application failed" ),

	system_utils:await_output_completion(),

	erlang:halt( 1 ),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_failed.



% To be called whenever an application is to fail (crash on error) immediately.
%
% FormatString is a io:format-style format string, ValueList is the
% corresponding list of field values.
%
% Ex: app_facilities:fail( "server ~s on strike", [ "foobar.org" ] )
%
-spec fail( string(), list() ) -> no_return().
fail( FormatString, ValueList ) ->

	% For some reason, erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	ErrorMessage = io_lib:format( "~n!!!! Application failed, reason: ~s.~n~n",
								[ io_lib:format( FormatString, ValueList ) ] ),

	io:format( "~n!!!! Application failed, reason: ~s.~n~n", [ ErrorMessage ] ),

	erlang:error( "Application failed" ),

	system_utils:await_output_completion(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_failed.
