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


% Unit tests for the text utils toolbox.
%
% See the text_utils.erl tested module.
%
-module(text_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% For pretty-printing test:
%-record( my_test_record, {

%	first_field,
%	second_field = 1,
%	third_file = "This is a test"

%} ).



print_title( Title, Level ) ->
	test_facilities:display( "Title level ~B:~n~s", [ Level,
		text_utils:generate_title(Title,Level) ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Converting an integer to a string: ~s.",
		[ text_utils:integer_to_string(3245) ] ),

	test_facilities:display( "Converting an atom to a string: ~s.",
		[ text_utils:atom_to_string('hello world') ] ),

	test_facilities:display( "Converting a PID to a string: '~s'.",
		[ text_utils:pid_to_string( self() ) ] ),


	%MyTestRecord = #my_test_record{},

	%test_facilities:display( "Converting a record instance to a string: "
	% "~s.", [ text_utils:record_to_string( MyTestRecord ) ] ),


	test_facilities:display( "Output with term_to_string : ~s, ~s and ~s.",
		[ text_utils:term_to_string( an_atom ),
		  text_utils:term_to_string( [ 1, 2 ] ),
		  text_utils:term_to_string( "A string" ) ] ),


	MaxDepth = 1,
	MaxLen = 5,

	test_facilities:display( "More output with term_to_string "
							 "with max depth ~B and max length ~B: "
							 "~s, ~s and ~s.",
		[ MaxDepth, MaxLen,
		  text_utils:term_to_string( an_atom, MaxDepth, MaxLen ),
		  text_utils:term_to_string( [ 1, 2 ], MaxDepth, MaxLen ),
		  text_utils:term_to_string( "A string", MaxDepth, MaxLen ) ] ),


	ListOfStrings = [ "Hello", "World", "Vampire" ],

	test_facilities:display( "Displaying list ~p as a string:~n~s",
		[ ListOfStrings, text_utils:string_list_to_string(ListOfStrings) ] ),


	LongLine = "This is a long line to test the paragraph formatting.",

	% So that "formatting." has a chance to fit:
	TargetWidth = 10,

	test_facilities:display( "Displaying text '~s' once formatted "
		"for a width of ~B:~n~p",
		[ LongLine, TargetWidth,
			text_utils:format_text_for_width( LongLine, TargetWidth ) ] ),


	JustWideEnoughLine = "<0.33.0>",

	% So that "formatting." has a chance to fit:
	NewTargetWidth = 8,

	test_facilities:display( "Displaying text '~s' once formatted "
		"for a width of ~B:~n~p",
		[ JustWideEnoughLine, NewTargetWidth,
			text_utils:format_text_for_width( JustWideEnoughLine,
				NewTargetWidth) ] ),


	test_facilities:display(
		"Displaying atom list obtained from string list ~p: ~p.",
		[ ListOfStrings, text_utils:string_list_to_atom_list(ListOfStrings) ]),

	FirstTestString = "Hello world!",

	test_facilities:display( "Determining whether '~p' is a string: ~w.",
		[ FirstTestString, text_utils:is_string(FirstTestString) ] ),
	true = text_utils:is_string( FirstTestString ),

	SecondTestString = [ $o, [ $s, $d ], $l ],

	test_facilities:display( "Determining whether '~p' is a string: ~w.",
		[ SecondTestString, text_utils:is_string(SecondTestString) ] ),
	false = text_utils:is_string( SecondTestString ),

	ThirdTestString = [ $e, 1, 2, $r ],

	test_facilities:display( "Determining whether '~p' is a string: ~w.",
		[ ThirdTestString, text_utils:is_string(ThirdTestString) ] ),
	true = text_utils:is_string(ThirdTestString),

	FirstList = [],
	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ FirstList, text_utils:is_list_of_strings(FirstList) ] ),
	true = text_utils:is_list_of_strings(FirstList),

	SecondList = [FirstTestString],
	test_facilities:display( "Determining whether '~p' is "
		"a list of strings: ~w.",
		[ SecondList, text_utils:is_list_of_strings(SecondList) ] ),
	true = text_utils:is_list_of_strings(SecondList),

	ThirdList = [FirstTestString,ThirdTestString],
	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ ThirdList, text_utils:is_list_of_strings(ThirdList) ] ),
	true = text_utils:is_list_of_strings(ThirdList),

	FourthList = [FirstTestString,SecondTestString],
	test_facilities:display(
		"Determining whether '~p' is a list of strings: ~w.",
		[ FourthList, text_utils:is_list_of_strings(FourthList) ] ),
	false = text_utils:is_list_of_strings(FourthList),


	Title = "Alien creatures invaded Ireland!",

	[ print_title( Title, Level ) || Level <- lists:seq( 1, 9 ) ],

	Percent = 0.1234,

	test_facilities:display( " Displaying ~p as a percentage: ~s.",
			  [ Percent, text_utils:percent_to_string( Percent ) ] ),


	test_facilities:display( " Checking string/binary conversions." ),

	"hello" = text_utils:binary_to_string( <<"hello">> ),
	 <<"hello">> = text_utils:string_to_binary( "hello" ),

	StringList = [ "hello", "world" ],
	BinList = [ <<"hello">>, <<"world">> ],

	% Order matters:
	BinList = text_utils:strings_to_binaries( StringList ),
	StringList = text_utils:binaries_to_strings( BinList ),

	10.0 = text_utils:string_to_float( "10" ),
	10.0 = text_utils:string_to_float( "10.0" ),
	try
		text_utils:string_to_float( "Not a float" )

	catch

		throw:_ ->
			ok

	end,

	123 = text_utils:string_to_integer( "123" ),

	% Test also failures:
	%text_utils:string_to_integer( "aa123bb" ),
	%text_utils:string_to_integer( "123.45" ),

	test_facilities:display( " Checking string/atom conversions." ),

	OtherStringList = [ "The", "little red", "wolf" ],
	test_facilities:display(
			  "When strings: ~s are converted into atoms, we have: ~w.",
			  [ text_utils:string_list_to_string(OtherStringList),
			   text_utils:strings_to_atoms(OtherStringList) ] ),

	test_facilities:display( "Testing the textual conversion of distances:" ),

	% In millimeters:
	Distances = [ -1001.5, -1001.0, -1000.5, -1000.0, -999.5, -999.0,
				 -1001, -1000, -999, -1.6, -1.4, -1.0, -0.9, -1, 0,
				 1, 0.9, 2, 999, 1000, 1001, 999999, 1000000, 1000001 ],

	[ test_facilities:display( " - an integer distance of ~w millimeters "
		  "is ~s, and roughly ~s",
		  [ D, text_utils:distance_to_string(D),
		   text_utils:distance_to_short_string(D) ] )
	 || D <- Distances ],


	test_facilities:display( "Testing the textual conversion of durations:" ),

	% In milliseconds:

	Durations = [ -100000, -1000, -1, 0 , 1, 2, 10, 3000, 3599,
				 3600, 3601, 36000, 59000, 60000, 61000, 100000,
				 12345678, 1234567890123 ],

	[ test_facilities:display(
				" - an integer duration of ~w milliseconds is ~s",
				[ D, text_utils:duration_to_string(D) ] )
	 || D <- Durations ],


	test_facilities:display( "Testing the upper-casing of first letter:" ),

	[ test_facilities:display( " - '~s' becomes '~s'",
				[ T, text_utils:uppercase_initial_letter(T) ] )
	 || T <- [ [], "a", "A", "Hello", "hello" ] ],

	WesternText = "I am a lonesome cowboy",

	UUIDText = "93171810-95a0-4382-ad73",

	"I am a lonesome cowboy whose name is 93171810-95a0-4382-ad73" =
	  text_utils:join( _Sep=" ", [ WesternText, "whose name is", UUIDText ] ),


	[ "93171810", "95a0", "4382", "ad73" ] = text_utils:split( UUIDText,
														_OtherSep="-" ),

	TestSplit = "  abcxdefxgh ",

	{ "  abc", "defxgh " } = text_utils:split_at_first( $x, TestSplit ),

	none_found = text_utils:split_at_first( $y, TestSplit ),

	{ "  ", "bcxdefxgh " } = text_utils:split_at_first( $a, TestSplit ),

	{ "", " abcxdefxgh " } = text_utils:split_at_first(  $ , TestSplit ),

	"Helli wirld" = text_utils:substitute( $o, $i, "Hello world" ),

	RemovalCount = 3,

	"I am a lonesome cow" = text_utils:remove_last_characters(
								WesternText, RemovalCount ),

	test_facilities:stop().
