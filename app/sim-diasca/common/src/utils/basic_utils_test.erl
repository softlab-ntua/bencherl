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


% Unit tests for the basic utils toolbox.
%
% See the basic_utils.erl tested module.
%
-module(basic_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec check_process_specific_values( integer(), integer() ) ->
										   basic_utils:void().
check_process_specific_values( Min, Max ) ->

	Self = self(),

	F = fun() -> Self ! basic_utils:get_process_specific_value( Min, Max ) end,

	[ spawn( F ) || _X <- lists:seq( 1, 10 ) ],

	G = fun() ->
				receive V ->
						V
				end
		end,

	[ test_facilities:display(
				"Generating a process-specific value in [~B;~B[: ~p.",
				[ Min, Max, G() ] ) || _Y <- lists:seq( 1, 10 ) ].



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the display of a static test message." ),

	test_facilities:display( "Testing the display of a ~s test message.",
							[ dynamic ] ),

	InitialTimestamp = basic_utils:get_timestamp(),
	InitialPreciseTimestamp = basic_utils:get_precise_timestamp(),

	test_facilities:display( "Timestamp is ~s.", [
		basic_utils:get_textual_timestamp( InitialTimestamp ) ] ),

	test_facilities:display( "Timestamp for path is ~s.", [
		basic_utils:get_textual_timestamp_for_path( InitialTimestamp ) ] ),

	TextualTimeStamp = "14/4/2011 18:48:51",
	test_facilities:display( "Parsed timestamp for '~s' is ~p.", [
		TextualTimeStamp,
		basic_utils:string_to_timestamp( TextualTimeStamp ) ] ),

	basic_utils:checkpoint( 1 ),

	basic_utils:checkpoint( 2 ),

	basic_utils:display( "standalone display" ),

	basic_utils:display( "display ~s", [ "with a format string" ] ),

	UnregisteredName = test_non_registered,
	try basic_utils:get_registered_pid_for( UnregisteredName ) of

		_Anything ->
			throw( test_should_have_failed )

	catch

		{ neither_registered_locally_nor_globally, UnregisteredName } ->
			ok

	end,

	not_registered = basic_utils:is_registered( UnregisteredName ),

	RegisteredName = test_registered,
	PidToRegister = self(),
	basic_utils:register_as( PidToRegister, RegisteredName, global_only ),

	try basic_utils:get_registered_pid_for( RegisteredName ) of

		PidToRegister ->
			ok

	catch

		Exception ->
			throw( { test_should_have_succeeded, Exception } )

	end,


	case basic_utils:is_registered( RegisteredName ) of

		not_registered ->
			throw( { neither_registered_locally_nor_globally,
					RegisteredName } );

		Pid when is_pid(Pid) ->
			ok

	end,

	FirstVersion  = { 0, 0, 0 },
	SecondVersion = { 0, 0, 1 },
	ThirdVersion  = { 0, 1, 0 },
	FourthVersion = { 1, 0, 0 },
	FifthVersion  = { 1, 1, 1 },

	first_bigger = basic_utils:compare_versions( SecondVersion, FirstVersion ),
	first_bigger = basic_utils:compare_versions( ThirdVersion, SecondVersion ),
	first_bigger = basic_utils:compare_versions( FifthVersion, FirstVersion ),

	second_bigger = basic_utils:compare_versions( FirstVersion, FourthVersion ),
	second_bigger = basic_utils:compare_versions( ThirdVersion, FourthVersion ),
	second_bigger = basic_utils:compare_versions( SecondVersion, ThirdVersion ),

	equal = basic_utils:compare_versions( FirstVersion, FirstVersion ),
	equal = basic_utils:compare_versions( ThirdVersion, ThirdVersion ),
	equal = basic_utils:compare_versions( FifthVersion, FifthVersion ),

	test_facilities:display( "Comparisons of versions like ~s succeeded.",
		[ text_utils:version_to_string(ThirdVersion) ] ),


	FirstShortVersion  = { 0, 0 },
	SecondShortVersion = { 0, 1 },
	ThirdShortVersion  = { 1, 0 },

	first_bigger = basic_utils:compare_versions( SecondShortVersion,
												FirstShortVersion ),

	first_bigger = basic_utils:compare_versions( ThirdShortVersion,
												SecondShortVersion ),

	first_bigger = basic_utils:compare_versions( ThirdShortVersion,
												FirstShortVersion ),


	second_bigger = basic_utils:compare_versions( FirstShortVersion,
												 SecondShortVersion ),

	second_bigger = basic_utils:compare_versions( SecondShortVersion,
												 ThirdShortVersion ),

	second_bigger = basic_utils:compare_versions( FirstShortVersion,
												 ThirdShortVersion ),


	equal = basic_utils:compare_versions( FirstShortVersion,
										 FirstShortVersion ),

	equal = basic_utils:compare_versions( SecondShortVersion,
										 SecondShortVersion ),

	equal = basic_utils:compare_versions( ThirdShortVersion,
										 ThirdShortVersion ),


	test_facilities:display( "Comparisons of versions like ~s succeeded.",
		[ text_utils:version_to_string(ThirdVersion) ] ),


	{ 4, 22, 11 } = basic_utils:parse_version( "4.22.11" ),

	test_facilities:display( "Generating a new UUID:"
		" '~s'.", [ basic_utils:generate_uuid() ] ),


	test_facilities:display( "Testing typing information." ),

	boolean = basic_utils:get_type_of( true ),

	atom = basic_utils:get_type_of( 'an atom' ),

	binary = basic_utils:get_type_of( list_to_binary( "1" ) ),

	float = basic_utils:get_type_of( 1.0 ),

	function = basic_utils:get_type_of( fun(X) -> X + 1 end ),

	integer = basic_utils:get_type_of( 42 ),

	pid = basic_utils:get_type_of( self() ),

	list = basic_utils:get_type_of( [ 1, 2 ] ),

	%port = basic_utils:get_type_of( APort ),

	tuple = basic_utils:get_type_of( { a, b } ),

	reference = basic_utils:get_type_of( make_ref() ),


	test_facilities:display( "Testing term recursive transformation." ),

	% This term transformer does not change anything in the terms it scans, and
	% just comment the traversal it does:
	IdTermTransformer = fun( Term, UserData ) ->

		NewUserData = [ io_lib:format( "Inspected '~p', ",
									  [ Term ] ) | UserData ],

		{ Term, NewUserData }

						end,

	TermToTraverse = { pseudo_record, [], { a, 1.0},
					   [ { b, 42 }, "hello", [ <<"foo">> ] ], self() },

	{ TraversedTerm, InspectData } = basic_utils:traverse_term( TermToTraverse,
						_Type=atom, IdTermTransformer, _UserData=[] ),

	test_facilities:display( "Traversal of term:~n'~p' with "
							 "id term transformer "
							 "yielded:~n'~p', producing user data '~s'",
							 [ TermToTraverse, TraversedTerm,
							   lists:reverse( InspectData ) ] ),

	% This term transformer changes a term into a textual representation, and
	% does not do anything with user data:
	TextTermTransformer = fun( Term, UserData ) ->

		{ io_lib:format( "~w", [ Term ] ), UserData }

						end,

	% Requested to operate only on PIDs:
	{ NewTraversedTerm, _UselessData } = basic_utils:traverse_term(
				TermToTraverse, _OtherType=pid, TextTermTransformer,
				_OtherUserData=undefined ),

	test_facilities:display( "Traversal of term:~n'~p' with "
							 "text term transformer yielded:~n'~p'.",
							 [ TermToTraverse, NewTraversedTerm ] ),


	test_facilities:display( "Generating a process-specific value: ~w.",
			  [ basic_utils:get_process_specific_value() ] ),

	{ Min, Max } = { 3, 16 },
	check_process_specific_values( Min, Max ),

	basic_utils:display_process_info( self() ),

	test_facilities:display( "This test was compiled with the execution target "
							 "set to '~s', and debug mode is ~s.",
							[ basic_utils:get_execution_target(),
							  basic_utils:is_debug_mode_enabled() ] ),

	FinalPreciseTimestamp = basic_utils:get_precise_timestamp(),

	test_facilities:display( "Precise duration in test is ~p ms.", [
		basic_utils:get_precise_duration( InitialPreciseTimestamp,
										 FinalPreciseTimestamp ) ] ),

	test_facilities:stop().
