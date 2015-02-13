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


% Gathering of various convenient facilities.
%
% See text_utils_test.erl for the corresponding test.
%
-module(text_utils).



% Note that string:tokens can be used to split strings.



% String management functions.


% Conversions between terms and strings.
%
-export([ term_to_string/1, term_to_string/2, term_to_string/3,
		  integer_to_string/1, atom_to_string/1, pid_to_string/1,
		  record_to_string/1,
		  string_list_to_string/1, string_list_to_string/2,
		  strings_to_string/1, strings_to_string/2,
		  binary_list_to_string/1, binaries_to_string/1,
		  atom_list_to_string/1, atoms_to_string/1,
		  string_list_to_atom_list/1,
		  version_to_string/1,
		  atom_to_binary/1,
		  string_to_binary/1, binary_to_string/1,
		  strings_to_binaries/1, binaries_to_strings/1,
		  string_to_integer/1, string_to_float/1,
		  string_to_atom/1, strings_to_atoms/1, binary_to_atom/1,
		  percent_to_string/1, percent_to_string/2,
		  distance_to_string/1, distance_to_short_string/1,
		  duration_to_string/1,
		  format/2, bin_format/2 ]).



% Other string operations:
%
-export([ uppercase_initial_letter/1,
		  join/2,
		  split/2, split_at_first/2, split_camel_case/1,

		  substitute/3,

		  is_uppercase/1, is_figure/1,
		  remove_ending_carriage_return/1, remove_last_characters/2,

		  trim_whitespaces/1, trim_leading_whitespaces/1,
		  trim_trailing_whitespaces/1,

		  format_text_for_width/2, pad_string/2,
		  is_string/1, is_list_of_strings/1 ]).


% Restructured-Text (RST) related functions.
-export([ generate_title/2 ]).


% HTTP-related operations.
-export([ encode_as_url/1, encode_element_as_url/1, escape/1 ]).



% Miscellaneous functions.
-export([ generate_text_name_from/1 ]).



% Type section.

% These strings are supposed to contain Erlang-fashioned format characters, like
% in "hello ~p!":
%
-type format_string() :: string().



% These strings are supposed to contain Regular Expressions, like in:
% "*-emitter-(first|second)-*".
%
% Patterns are to be expressed according to the “Perl Compatible Regular
% Expressions” conventions, or PCRE for short.
% For more information, see following cheat sheet:
% http://www.bitcetera.com/page_attachments/0000/0030/regex_in_a_nutshell.pdf
%
% See also: http://erlang.org/doc/man/re.html
%
-type regex_string() :: string().


% A string which describes a title:
%
-type title() :: string().


% A string which describes a label:
%
-type label() :: string().


% A binary corresponding to a string:
%
-type bin_string() :: binary().


% A Unicode string.
%
% This is our new default.
%
-type unicode_string() :: unicode:chardata().


% A Unicode character.
%
 % Unicode codepoint for the character.
%
% (unfortunately we cannot define a text_utils:char/0 type, as "type char()
% is a builtin type; it cannot be redefined").
%
-type uchar() :: integer().


% Now our default:
%
% (unfortunately we cannot define a text_utils:string/0 type, as "type string()
% is a builtin type; it cannot be redefined").
%
-type ustring() :: unicode_string().


-export_type([ format_string/0, regex_string/0, title/0, label/0,
			   bin_string/0, unicode_string/0, ustring/0, uchar/0 ]).




% String management functions.


% Returns a human-readable string describing specified term.
%
-spec term_to_string( term() ) -> string().
term_to_string( _Term=[] ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~s", [ Term ] );

		_    ->
			io_lib:format( "~p", [ Term ] )

	end.



% Returns a human-readable string describing specified term, up to the specified
% nesting depth.
%
-spec term_to_string( term(), basic_utils:count() ) -> string().
term_to_string( _Term=[], _MaxDepthCount ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount ) ->

	case io_lib:printable_list( Term ) of

		true ->
			io_lib:format( "~s", [ Term ] );

		_    ->
			io_lib:format( "~P", [ Term, MaxDepthCount ] )

	end.



% Returns a human-readable string describing specified term, up to the specified
% nesting depth, and up to specified string length (at least 3, so that the
% "..." marker can be inserted.
%
-spec term_to_string( term(), basic_utils:count(), basic_utils:count() )
					-> string().
term_to_string( _Term=[], _MaxDepthCount, _MaxLength ) ->
	% Otherwise would be an empty string:
	"[]";

term_to_string( Term, MaxDepthCount, MaxLength ) when MaxLength >= 3 ->

	% First limit the depth (beware of IO-lists!):
	FullString = case io_lib:printable_list( Term ) of

		true ->
			% The '*' character in the format string is not suitable here:
			lists:flatten( io_lib:format( "~s", [ Term ] ) );

		_ ->
			lists:flatten( io_lib:format( "~P", [ Term, MaxDepthCount ] ) )

	end,

	% Then limit the length:
	case length( FullString ) of

		L when L > MaxLength ->
			% We have to truncate here, length( "..." ) = 3
			% MaxLength - 3 = 0 is allowed there:
			string:sub_string( FullString, 1, MaxLength - 3 ) ++ " ..";

		_ ->
			FullString

	end.



% Avoids to have to use lists:flatten when converting an integer to a string.
% Useless when using functions like io:format, that accept iolists as
% parameters.
%
-spec integer_to_string( integer() ) -> string().
integer_to_string( IntegerValue ) ->
	hd( io_lib:format( "~B", [ IntegerValue ] ) ).



% Returns a plain string corresponding to the specified atom.
%
-spec atom_to_string( atom() ) -> string().
atom_to_string( Atom ) ->
	atom_to_list( Atom ).



% Returns a plain string corresponding to the specified PID.
%
-spec pid_to_string( pid() ) -> string().
pid_to_string( Pid ) ->

	% PID are akin to <X.Y.Z>.

	PidAsText = lists:flatten( io_lib:format( "~w", [ Pid ] ) ),

	%io:format( "PID: ~w.~n", [ self() ] ) ,
	% Ex: ["<0","33","0>"]:
	[ $< | Rest ] = PidAsText,

	% Returns "X.Y.Z":
	list_utils:remove_last_element( Rest ).


	%% ActualFirst = case First of

	%%		"0" ->
	%%			[];

	%%		_ ->
	%%			First ++ "."

	%% end,

	%% ActualThird = case Third of

	%%		"0" ->
	%%			[];

	%%		_ ->
	%%			"." ++ Third

	%% end,

	%% ActualFirst ++ Second ++ ActualThird.



% Returns a string describing the specified record.
% Hugely inspired from a Ulf Wiger's snippet. described in
% http://erlang.org/pipermail/erlang-questions/2006-September/023181.html
% Apparently, as records are compile-time structures only, there is no simple
% way of determining the name of their fields at runtime.
%
-spec record_to_string( _ ) -> none().
record_to_string( _Record ) -> % No 'when is_record( Record, Tag ) ->' here.

	throw( { not_implemented, record_to_string } ).

	%RF = fun(R,L) when R == element(1,Record) ->
	%	% Needs apparently a parse transform:
	%   Fields = '#info-'(Record),
	%	true = (L == length(Fields)),
	%	Fields
	%end,
	%
	%io_lib_pretty:print( Record, RF ).



% Returns a string which pretty-prints specified list of strings, with default
% bullets.
%
-spec string_list_to_string( [ ustring() ] ) -> ustring().
string_list_to_string( ListOfStrings ) ->
	io_lib:format( "~n~ts", [ string_list_to_string(
								ListOfStrings, _Acc=[], _Bullet=" + " ) ] ).


% Returns a string which pretty-prints specified list of strings, with
% user-specified bullets.
%
-spec string_list_to_string( [ ustring() ], ustring() ) -> ustring().
string_list_to_string( ListOfStrings, Bullet ) ->
	io_lib:format( "~n~ts", [ string_list_to_string(
								ListOfStrings, _Acc=[], Bullet ) ] ).


string_list_to_string( _ListOfStrings=[], Acc, _Bullet ) ->
	 Acc;

string_list_to_string( _ListOfStrings=[ H | T ], Acc, Bullet )
  when is_list( H ) ->
	string_list_to_string( T, Acc ++ Bullet ++ io_lib:format( "~ts~n", [ H ] ),
						   Bullet ).



% Returns a string which pretty-prints specified list of strings, with default
% bullets.
%
-spec strings_to_string( [ ustring() ] ) -> ustring().
strings_to_string( ListOfStrings ) ->
	string_list_to_string( ListOfStrings ).



% Returns a string which pretty-prints specified list of strings, with
% user-specified bullets.
%
-spec strings_to_string( [ ustring() ], ustring() ) -> ustring().
strings_to_string( ListOfStrings, Bullet ) ->
	string_list_to_string( ListOfStrings, Bullet ).




% Returns a string which pretty-prints specified list of binary strings, with
% default bullets.
%
-spec binary_list_to_string( [ binary() ] ) -> ustring().
binary_list_to_string( ListOfBinaries ) ->
	binaries_to_string( ListOfBinaries ).



% Returns a string which pretty-prints specified list of binary strings, with
% user-specified bullets.
%
-spec binaries_to_string( [ binary() ] ) -> ustring().
binaries_to_string( ListOfBinaries ) ->
	Strings = binaries_to_strings( ListOfBinaries ),
	string_list_to_string( Strings ).



% Returns a string which pretty-prints specified list of atoms, with bullets.
%
-spec atom_list_to_string( [ atom() ] ) -> ustring().
atom_list_to_string( ListOfAtoms ) ->
	io_lib:format( "~n~ts", [ atom_list_to_string( ListOfAtoms, [] ) ] ).


atom_list_to_string( [], Acc ) ->
	 Acc;

atom_list_to_string( [ H | T ], Acc ) when is_atom( H)  ->
	atom_list_to_string( T, Acc ++ io_lib:format( " + ~ts~n", [ H ] ) ).



% Returns a string which pretty-prints specified list of atoms, with bullets.
%
-spec atoms_to_string( [ atom() ] ) -> ustring().
atoms_to_string( ListOfAtoms ) ->
	atom_list_to_string( ListOfAtoms ).



% Returns a list whose elements are atoms corresponding to the strings
% supposedly composing the specified list.
%
% Ex: string_list_to_atom_list( ["abc", "def"] ) should return [abc,def].
%
-spec string_list_to_atom_list( [ ustring() ] ) -> [ atom() ].
string_list_to_atom_list( StringList ) when is_list( StringList ) ->
	[ list_to_atom( X ) || X <- StringList ].




% Returns a string describing the specified three-element version.
%
-spec version_to_string( basic_utils:version() ) -> string().
version_to_string( { V1, V2, V3 } ) ->
	io_lib:format( "~B.~B.~B", [ V1, V2, V3 ] ).



% Returns a binary string corresponding to the specified atom.
%
-spec atom_to_binary( atom() ) -> bin_string().
atom_to_binary( Atom ) ->
	string_to_binary( atom_to_string( Atom ) ).



% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the default number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent() ) -> string().
percent_to_string( Value ) ->
	percent_to_string( Value, _DefaultPrecision=1 ).


% Returns a textual description of the specified percentage, expected to be a
% float in [0,1], with the specified number of digits after the decimal point.
%
-spec percent_to_string( math_utils:percent(), integer() ) -> string().
percent_to_string( Value, Precision ) ->
	% Awful format string to determine:
	io_lib:format( "~.*f%", [ Precision, Value * 100 ] ).



% Returns an exact rounded textual description of the specified distance,
% expected to be expressed as a floating-point number of millimeters, which will
% be first rounded to nearest integer.
%
% Ex: for a distance of 1001.5 millimeters, returns "1m and 2mm".
%
-spec distance_to_string( unit_utils:millimeters()
						 | unit_utils:int_millimeters() ) -> string().
distance_to_string( Millimeters ) when is_float(Millimeters) ->
	distance_to_string( round(Millimeters) );

% Returns an exact textual description of the specified distance, expected to be
% expressed as an integer number of millimeters.
%
% Ex: for an integer distance of 1000001 millimeters, returns "1km and 1mm".
%
distance_to_string( Millimeters ) ->

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	ListWithKm = case Millimeters div Km of

				 0 ->
					 [];

				 KmNonNull->
					 [ io_lib:format( "~Bkm", [ KmNonNull ] ) ]

   end,

	DistAfterKm = Millimeters rem Km,
	%io:format( "DistAfterKm = ~B.~n", [ DistAfterKm ] ),

	ListWithMeters = case DistAfterKm div Meters of

				 0 ->
					 ListWithKm;

				 MetersNonNull->
					 [ io_lib:format( "~Bm", [ MetersNonNull ] ) | ListWithKm ]

   end,

	DistAfterMeters = DistAfterKm rem Meters,
	%io:format( "DistAfterMeters = ~B.~n", [DistAfterMeters] ),

	ListWithCentimeters = case DistAfterMeters div Centimeters of

				 0 ->
					 ListWithMeters;

				 CentNonNull->
					 [ io_lib:format( "~Bcm", [ CentNonNull ] )
					   | ListWithMeters ]

   end,

	DistAfterCentimeters = DistAfterMeters rem Centimeters,
	%io:format( "DistAfterCentimeters = ~B.~n", [ DistAfterCentimeters ] ),

	ListWithMillimeters = case DistAfterCentimeters of

				 0 ->
					 ListWithCentimeters;

				 AtLeastOneMillimeter ->
					 [ io_lib:format( "~Bmm", [ AtLeastOneMillimeter ] )
						   | ListWithCentimeters ]

	end,

	%io:format( "Unit list is: ~w.~n", [ ListWithMillimeters ] ),

	% Preparing for final display:
	case ListWithMillimeters of

		[] ->
			"0mm";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			join( ", ", lists:reverse( Bigger ) ) ++ " and " ++ Smaller

	end.



% Returns an approximate textual description of the specified distance, expected
% to be expressed as a floating-point number of millimeters, which will be first
% rounded to nearest integer.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000.5 millimeters, returns "1.0m".
%
-spec distance_to_short_string( unit_utils:millimeters()
							   | unit_utils:int_millimeters() ) -> string().
distance_to_short_string( Millimeters ) when is_float(Millimeters) ->
	distance_to_short_string( round( Millimeters ) );

% Returns an approximate textual description of the specified distance, expected
% to be expressed as an integer number of millimeters.
%
% Only one unit, the most appropriate one, will be used, with up to 1 figure
% after the comma.
%
% Ex: for a distance of 1000001 millimeters, returns "1.0km".
%
distance_to_short_string( Millimeters ) ->

	% Note: very specific limit distances could be better managed.
	% Ex: 999999 millimeters is 999m, 99cm and 9mm, and "1000.0m" due to
	% rounding, whereas we would have preferred "1km".

	Centimeters = 10,
	Meters = 100 * Centimeters,
	Km = Meters*Meters,

	% First, guess the most suitable unit, then use it:

	case Millimeters div Km of

		0 ->
			% Kilometers are too big:
			case Millimeters div Meters of

				0 ->
					% Meters are too big:
					case Millimeters div Centimeters of

						0 ->
							% Centimeters are too big, stick to mm:
							io_lib:format( "~Bmm", [ Millimeters ] );

						_CmNonNull ->
							io_lib:format( "~.1fcm",
										   [ Millimeters / Centimeters ] )

					end;

				 _MetersNonNull ->
					io_lib:format( "~.1fm", [ Millimeters / Meters ] )

			end;

		_KmNonNull->
			io_lib:format( "~.1fkm", [ Millimeters / Km ] )

	end.



% Returns an approximate textual description of the specified duration, expected
% to be expressed as an integer number of milliseconds.
%
% Ex: for a duration of 150012 ms, returns:
% "2 minutes, 30 seconds and 12 milliseconds".
%
% See also: basic_utils:get_textual_duration/2.
%
-spec duration_to_string( unit_utils:milliseconds() | float() ) -> string().
duration_to_string( Milliseconds ) when is_float( Milliseconds )->
	duration_to_string( erlang:round( Milliseconds ) );

duration_to_string( Milliseconds ) ->

	FullSeconds = Milliseconds div 1000,

	{ Days, { Hours, Minutes, Seconds } } =
		calendar:seconds_to_daystime( FullSeconds ),

	ListWithDays = case Days of

				   0 ->
					   [];

				   1 ->
					   [ "1 day" ];

				   _ ->
					   [ io_lib:format( "~B days", [ Days ] ) ]

	end,

	ListWithHours = case Hours of

					0 ->
						ListWithDays;

					1 ->
						[ "1 hour" | ListWithDays ];

					_ ->
						[ io_lib:format( "~B hours", [ Hours ] )
						  | ListWithDays ]

	end,

	ListWithMinutes = case Minutes of

					  0 ->
						  ListWithHours;

					  1 ->
						  [ "1 minute" | ListWithHours ];

					  _ ->
						  [ io_lib:format("~B minutes", [ Minutes ] )
							   | ListWithHours ]

	end,

	ListWithSeconds = case Seconds of

					  0 ->
						  ListWithMinutes;

					  1 ->
						  [ "1 second" | ListWithMinutes ];

					  _ ->
						  [ io_lib:format( "~B seconds", [ Seconds ] ) |
							   ListWithMinutes ]

	end,

	ActualMilliseconds = Milliseconds rem 1000,

	ListWithMilliseconds = case ActualMilliseconds of

					   0 ->
						   ListWithSeconds;

					   1 ->
						   [ "1 millisecond" |ListWithSeconds];

					   _ ->
						   [ io_lib:format("~B milliseconds",
										   [ ActualMilliseconds ] )
									| ListWithSeconds ]

	end,

	% Preparing for final display:
	case ListWithMilliseconds of

		[] ->
			"0 millisecond";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			join( ", ", lists:reverse(Bigger) ) ++ " and " ++ Smaller

	end.



% Formats specified string as io_lib:format/2 would do, except it returns a
% flattened version of it and cannot fail (so that for example a badly formatted
% log cannot crash anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec format( format_string(), [ term() ] ) -> ustring().
format( FormatString, Values ) ->

	String = try

				 io_lib:format( FormatString, Values )

			 catch

				 _:_ ->

					 io_lib:format( "[error: badly formatted output] "
									"Format: '~p', values: '~p'",
									[ FormatString, Values] )

	end,

	% Using 'flatten' allows for example to have clearer strings output in case
	% of error:
	%
	lists:flatten( String ).



% Formats specified binary string as io_lib:format/2 would do, except it returns
% a flattened version of it and cannot fail (so that for example a badly
% formatted log cannot crash anymore its emitter process).
%
% Note: rely preferably on '~ts' rather than on '~s', to avoid unexpected
% Unicode inputs resulting on crashes afterwards.
%
-spec bin_format( format_string(), [ term() ] ) -> ustring().
bin_format( FormatString, Values ) ->

	String = try

				 io_lib:format( FormatString, Values )

			 catch

				 _:_ ->

					 io_lib:format( "[error: badly formatted output] "
									"Format: '~p', values: '~p'",
									[ FormatString, Values] )

	end,

	% No flattening needed here:
	erlang:list_to_binary( String ).



% Converts a plain (list-based) string into a binary.
%
-spec string_to_binary( ustring() ) -> binary().
string_to_binary( String ) ->
	erlang:list_to_binary( String ).



% Converts a binary into a plain (list-based) string.
%
-spec binary_to_string( binary() ) -> ustring().
binary_to_string( Binary ) ->
	erlang:binary_to_list( Binary ).



% Converts a list of plain (list-based) strings into a list of binaries.
%
% Order of items remains unaffected.
%
-spec strings_to_binaries( [ ustring() ] ) -> [ binary() ].
strings_to_binaries( StringList ) ->
	% Order must be preserved:
	[ erlang:list_to_binary( S ) || S <- StringList ].



% Converts a list of binaries into list of plain (list-based) string.
%
% Order of items remains unaffected.
%
-spec binaries_to_strings( [ binary() ] ) -> [ ustring() ].
binaries_to_strings( BinaryList ) ->
	% Order must be preserved:
	[ erlang:binary_to_list( B ) || B <- BinaryList ].



% Returns an integer which corresponds to the specified text.
%
% Throws an exception if the conversion failed.
%
-spec string_to_integer( ustring() ) -> integer().
string_to_integer( String ) ->

	try list_to_integer( String ) of

		I ->
			I

	catch

		error:badarg ->
			throw( { integer_conversion_failed , String } )

	end.



% Returns a float which corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Throws an exception if the conversion failed.
%
-spec string_to_float( ustring() ) -> float().
string_to_float( String ) ->

	try list_to_float( String ) of

		F ->
			F

	catch

		error:badarg ->

			try list_to_integer( String ) of

				I ->
					float(I)

			catch

				error:badarg ->
					throw( { float_conversion_failed, String } )

			end

	end.




% Converts specified plain string into an atom.
%
% Note that a bounded number of atoms should be created that way, lest the atom
% table gets saturated.
%
-spec string_to_atom( ustring() ) -> atom().
string_to_atom( String ) ->
	erlang:list_to_atom( String ).



% Converts specified list of plain strings into a corresponding list of atoms.
%
% Note that a bounded number of atoms should be created that way, lest the atom
% table gets saturated.
%
-spec strings_to_atoms( [ ustring() ] ) -> [ atom() ].
strings_to_atoms( StringList ) ->
	[ string_to_atom( S ) || S <- StringList ].



% Converts specified list of plain strings into a corresponding list of atoms.
%
% Note that a bounded number of atoms should be created that way, lest the atom
% table gets saturated.
%
-spec binary_to_atom( binary() ) -> atom().
binary_to_atom( Binary ) ->
	String = binary_to_string( Binary ),
	string_to_atom( String ).



% Returns the specified string, ensuring that its first letter is a majuscule,
% uppercasing it if necessary.
%
-spec uppercase_initial_letter( ustring() ) -> ustring().
uppercase_initial_letter( _Letters=[] ) ->
	[];

uppercase_initial_letter( _Letters=[ First | Others ] ) ->
	[ string:to_upper( First ) | Others ].



% join( Separator, ListToJoin ), to be used like in:
%   join( $-, [ "Barbara", "Ann" ] ) = "Barbara-Ann".
%
% Separator can be a character, like $a, or a string, like ", ".
%
% Python-like 'join', combines items in a list into a string using a separator
% between each item representation.
%
% Inspired from http://www.trapexit.org/String_join_with.
%
% For file-related paths, you are expected to use portable standard
% filename:join functions instead.
%
% Note: conversely, use string:tokens to split the string.
%
-spec join( ustring() | uchar(), [ ustring() ] ) -> ustring().
join( _Separator, _ListToJoin=[] ) ->
	"";

join( Separator, ListToJoin ) ->
	lists:flatten( lists:reverse( join( Separator, ListToJoin, _Acc=[] ) ) ).


% Helper:
%
join( _Separator, _ListToJoin=[], Acc) ->
	Acc;

join( _Separator, _ListToJoin=[ H | [] ], Acc ) ->
	[ H | Acc ];

join( Separator, _ListToJoin=[ H | T ], Acc ) ->
	join( Separator, T, [ Separator, H | Acc ] ).




% Splits the specified string into a list of strings, based on the list of
% specified characters to be interpreted as delimiters.
%
% Defined here not to chase anymore after string:tokens/2.
%
-spec split( ustring(), [ uchar() ] ) -> [ ustring() ].
split( String, Delimiters ) ->
	string:tokens( String, Delimiters ).



% Splits the specified string according to the first occurrence of specified
% character: returns a pair of two strings, containing respectively all
% characters strictly before and strictly after the first occurrence of the
% marker (which thus is not kept).
%
% Ex: split_at_first( $x, "  aaaxbbbxccc" ) shall return { "  aaa", "bbbxccc" }.
%
-spec split_at_first( uchar(), ustring() ) ->
							'none_found' | { ustring(), ustring() }.
split_at_first( Marker, String ) ->
	split_at_first( Marker, String, _Acc=[] ).


% Helper:
split_at_first( _Marker, _ToRead=[], _Read ) ->
	none_found;

split_at_first( Marker, _ToRead=[ Marker | T ], Read ) ->
	{ lists:reverse( Read ), T };

split_at_first( Marker, _ToRead=[ Other | T ], Read ) ->
	split_at_first( Marker, T, [ Other | Read ] ).




% Splits the specified string, expected to be containing a word in CamelCase,
% into a list of strings, based on the internal words (delimited by uppercases,
% knowing a series of uppercase letters, except the last one, is considered as
% an acronym, hence as a single word), in their original order.
%
% Ex: split_camel_case( "IndustrialWasteSource" ) shall return [ "Industrial",
% "Waste", "Source" ], while split_camel_case( "TheySaidNYCWasGreat" ) shall
% return [ "They", "Said", "NYC", "Was", "Great" ].
%
-spec split_camel_case( ustring() ) -> [ ustring() ].
split_camel_case( _String )->
	% TO-DO:
	%lists:reverse( split_camel_case( String, _CurrentWord=[], _AccWords=[] ) ).
	%String.

%% split_camel_case( _String=[ C | T ], CurrentWord, AccWords )->

%%	case is_uppercase( C ) of

%%		true ->
	throw( not_implemented_yet ).



% Substitutes in specified string the source character with the target one.
%
% Note: simpler and probably more efficient that a regular expression.
%
-spec substitute( uchar(), uchar(), ustring() ) -> ustring().
substitute( SourceChar, TargetChar, String ) ->
	substitute( SourceChar, TargetChar, String, _Acc=[] ).


substitute( _SourceChar, _TargetChar, _String=[], Acc ) ->
	lists:reverse( Acc );

substitute( SourceChar, TargetChar, _String=[ SourceChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ TargetChar | Acc ] );

substitute( SourceChar, TargetChar, _String=[ OtherChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ OtherChar | Acc ] ).





% Tells whether specified character is an uppercase one.
%
-spec is_uppercase( uchar() ) -> boolean().
is_uppercase( Char ) ->

	% Simplistic but working:

	OneCharacterString = [ Char ],

	case string:to_upper( OneCharacterString ) of

		OneCharacterString ->
			true;

		_ ->
			false

	end.



% Tells whether specified character is a figure (in 0..9).
%
-spec is_figure( char() ) -> boolean().
is_figure( Char ) when is_integer( Char ) andalso Char >= $0
					   andalso Char =< $9 ->
	true;

is_figure( Char ) when is_integer( Char ) ->
	false.



% Removes the ending "\n" character(s) of specified string.
%
-spec remove_ending_carriage_return( ustring() ) -> ustring().
remove_ending_carriage_return( String ) when is_list( String ) ->

	% See also: list_utils:remove_last_element/1.

	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).



% Removes the last Count characters from String.
%
-spec remove_last_characters( ustring(), basic_utils:count() ) -> ustring().
remove_last_characters( String, Count ) ->

	% Not necessarily the most efficient, but at least it is not an illegal
	% pattern:
	%
	case length( String ) of

		C when C >= Count ->
			string:substr( String, 1, C - Count );

		_->
			throw( { removal_failed, String, Count } )

	end.


% Removes all leading and trailing whitespaces.
%
-spec trim_whitespaces( ustring() ) -> ustring().
trim_whitespaces( InputString ) ->

	% Should be done in one pass:
	trim_leading_whitespaces( trim_trailing_whitespaces( InputString ) ).



% Removes all leading whitespaces.
%
-spec trim_leading_whitespaces( ustring() ) -> ustring().
trim_leading_whitespaces( InputString ) ->

	% Largely inspired from http://www.trapexit.org/Trimming_Blanks_from_String:
	re:replace( InputString, "^\\s*", "",
			[ unicode, { return, list } ] ).



% Removes all trailing whitespaces.
%
-spec trim_trailing_whitespaces( ustring() ) -> ustring().
trim_trailing_whitespaces( InputString ) ->

	% The $ confuses some syntax highlighting systems (like the one of emacs):
	re:replace( InputString, "\\s*$", "", [ unicode, { return, list } ] ).





% Formats specified text according to specified width, expressed in characters.
%
% Returns a list of strings, each of which having Width characters.
%
-spec format_text_for_width( ustring(), pos_integer() ) -> [ ustring() ].
format_text_for_width( Text, Width ) ->

	% Whitespaces converted to spaces:
	CleanedTest = re:replace( lists:flatten( Text ), "\\s+", " ",
		[ global, { return, list } ] ),

	WordList = string:tokens( CleanedTest, " " ),

	%io:format( "Formatting ~p.~n", [ WordList ] ),
	join_words( WordList, Width ).



% Joins words from the list, line by line.
%
join_words( WordList, Width ) ->
	join_words( WordList, Width, _Lines=[], _CurrentLine="",
		_CurrentLineLen=0 ).


join_words( [], _Width, AccLines, _CurrentLine, _CurrentLineLen=0 ) ->
	% Ended with a full line:
	lists:reverse( AccLines );

join_words( [], Width, AccLines, CurrentLine, _CurrentLineLen ) ->
	% Ended with a partial line:
	lists:reverse( [ pad_string( CurrentLine, Width ) | AccLines ] );

join_words( [ Word | RemainingWords ], Width, AccLines, CurrentLine,
		CurrentLineLen ) ->

	%io:format( "Managing word '~s' (len=~B), current line is '~s' (len=~B), "
	%	"width = ~B.~n", [ Word, length( Word ), CurrentLine, CurrentLineLen,
	% Width ] ),

	% Length should be incremented, as a space must be inserted before that
	% word, however we want to accept words whose width would be exactly equal
	% to the line width:
	%
	ActualLength = case CurrentLine of

		"" ->
			length( Word );

		_NonEmpty ->
			% Already at least a letter, we therefore must add a space before
			% the new word:
			length( Word ) + 1

	end,

	case ActualLength of

		CompatibleWidth when CompatibleWidth =< Width ->
			% Word width is manageable.
			% Will this word fit on the current line?
			%
			case CurrentLineLen + CompatibleWidth of

				FittingLen when FittingLen =< Width ->
					% Yes, this word fits on the current line.
					% Avoids adding a space at the beginning of a new line:
					{ NewCurrentLine, NewLineLen } = case CurrentLineLen of

						0 ->
							{ Word, CompatibleWidth };

						Len ->
							{ CurrentLine ++ " " ++ Word,
							  Len + CompatibleWidth + 1 }

					end,

					%io:format("Current line is now '~s'.~n", [NewCurrentLine]),
					join_words( RemainingWords, Width, AccLines, NewCurrentLine,
						NewLineLen );

				_ExceedingLen ->
					% No, with this word the current line would be too wide,
					% inserting it on new line instead:
					PaddedCurrentLine = pad_string( CurrentLine, Width ),
					%io:format( "Inserting line '~s'.~n", [PaddedCurrentLine] ),
					join_words( RemainingWords, Width,
						[ PaddedCurrentLine | AccLines ], Word,
							   CompatibleWidth )

			end;


		_TooLargeWidth ->

			% Will break words as many times as needed:
			%io:format( "Word '~s' is too large (len=~B), breaking it.~n",
			%	[ Word, length( Word ) ] ),
			Subwords = break_word( Word, Width ),

			PaddedCurrentLine = pad_string( CurrentLine, Width ),

			join_words( Subwords ++ RemainingWords, Width,
						[ PaddedCurrentLine | AccLines ], "", 0 )

	end.



% Returns the specified string, padded with spaces to specified width,
% left-justified.
%
-spec pad_string( ustring(), integer() ) -> ustring().
pad_string( String, Width ) when length( String ) =< Width ->
	lists:flatten( io_lib:format( "~*.s", [ -Width, String ] ) ).



% Returns true iff the parameter is a (non-nested) string (actually a plain list
% of integers).
%
% Taken from http://lethain.com
% (see distinguishing-strings-from-lists-in-erlang)
%
% Note: something like [ $e, 1, 2, $r ] is deemed to be a string.
%
-spec is_string( term() ) -> boolean().
is_string( [] ) ->
	true;

is_string( [ H | _ ] ) when not is_integer( H ) ->
	false;

is_string( [ _ | T ] ) ->
	is_string( T );

is_string( _Other ) ->
	false.



% Returns true iff the specified parameter is a list whose all elements are
% strings.
%
-spec is_list_of_strings( list() ) -> boolean().
is_list_of_strings( [] ) ->
	true;

is_list_of_strings( [ H | T ] ) ->

	case is_string( H ) of

		true ->
			is_list_of_strings( T );

		false ->
			false

	end.



% Returns a list of words obtained from the breaking of specified word,
% according to specified maximum width.
%
% Parts of that word will use a separating dash.
%
% Ex: break_word( "simulator", 5 ) returns [ "simu-", "lator" ].
%
break_word( Word, Width ) ->

	% We do not want to have underscores in the word, as if the word happens
	% to be broken just after an underscore, RST will interpret it as a link.
	% Therefore we escape underscores:
	%
	% Used to cut into halves, then preferring truncating a first full-length
	% chunk, finally directly cutting the word into appropriate pieces:
	% CutIndex = length(Word) div 2,
	% CutIndex = Width-1,
	cut_into_chunks( Word, Width, [] ).



% Cuts specified string into pieces, each of them having to fit in specified
% width.
%
cut_into_chunks( _String=[], _ChunkSize, Acc ) ->
	%io:format( "cut_into_chunks return ~p.", [lists:reverse(Acc)]),
	lists:reverse( Acc );

% Last word may take the full width (no dash to add):
cut_into_chunks( String, ChunkSize, Acc ) when length( String ) =< ChunkSize ->
	cut_into_chunks( [], ChunkSize, [ String | Acc ] );

% Here we have to cut the string anyway:
cut_into_chunks( String, ChunkSize, Acc ) ->

	% Rule is to add (and convert) characters until the end of line:
	% (ChunkSize decremented as "-" will be added)

	{ FirstPart, Remaining } = aggregate_word( String, ChunkSize-1, [] ),

	% Each underscore will result into another character (\) being added:
	%io:format( "FirstPart = '~s' (~B), Remaining = '~s'.~n",
	%	[ FirstPart, length( FirstPart ), Remaining ] ),
	cut_into_chunks( Remaining, ChunkSize, [ FirstPart ++ "-" | Acc ] ).



aggregate_word( String, 0, Acc ) ->
	{ lists:reverse( Acc ), String };


% An underscore once escaped would not fit, as it would result into two
% characters ('\_'):
%
aggregate_word( String=[ $_ | _T ], 1, Acc ) ->
	aggregate_word( String, 0, Acc );

% An escaped underscore will fit:
%
aggregate_word( [ $_ | T ], Count, Acc ) ->
	% Adding '_\' as it will reversed (into the expected '\_'):
	aggregate_word( T, Count-2, [ $\_, $\\ | Acc ] );

aggregate_word( [ H | T ], Count, Acc ) ->
	aggregate_word( T, Count-1, [ H | Acc ] ).




% Restructured-Text (RST) related functions.


% Generates a RST-compatible standard title, with the proper ASCII art.
% Follows our general conventions regarding title level, from H1 to Hn.
%
-spec generate_title( ustring(), 1..9 ) -> ustring().
generate_title( Title, Level ) ->

	{ Char, Layout } = get_title_rendering_for( Level ),

	TitleLine = get_line_of( Char, length( Title ) ) ++ "\n",

	case Layout of

		only_below ->
			Title ++ "\n" ++ TitleLine ++ "\n";

		below_and_on_top ->
			TitleLine ++ Title ++ "\n" ++ TitleLine ++ "\n"

	end.



% Returns how a title with specified level can be rendered.
% See demo-for-css-testing.rst for the convention.
%
get_title_rendering_for( 1 ) ->
	{ $=, below_and_on_top };

get_title_rendering_for( 2 ) ->
	{ $-, below_and_on_top };

get_title_rendering_for( 3 ) ->
	{ $=, only_below };

get_title_rendering_for( 4 ) ->
	{ $-, only_below };

get_title_rendering_for( 5 ) ->
	{ $., only_below };

get_title_rendering_for( 6 ) ->
	{ $_, only_below };

get_title_rendering_for( 7 ) ->
	{ $*, only_below };

get_title_rendering_for( 8 ) ->
	{ $:, only_below };

get_title_rendering_for( 9 ) ->
	{ $+, only_below }.




% Returns a line made of Length characters "Character".
% Ex: get_line_of( $+, 5 ) = "+++++".
%
get_line_of( Character, Length ) ->
	lists:flatten( [ Character || _X <- lists:seq( 1, Length ) ] ).



% HTTP-related operations.


% About encoding.

% The character "è" (e with a grave accent, hex code: xE8) might be for example
% either translated as "%C3%A8" or as "%E8". It is apparently the difference
% between encodeURI(chr) and escape(chr) in Javascript.
%
% The most adequate encoding in general seems the first (in which case
% encode_as_url/1 and encode_element_as_url/1 shall be used), however some
% webservers seem to insist on having the second (in which case
% escape/1 and escape_element/1 shall be used).
%
% See also: http://www.javascripter.net/faq/accentedcharacters.htm



% Encodes specified list of {Key,Value} pairs so that it can used into an URL.
%
% Full example:
%
% inets:start(),
% httpc:request( post, { "http://localhost:3000/foo", [],
%  "application/x-www-form-urlencoded",
%  encode_as_url( [ {"username", "bob"}, {"password", "123456"} ] ) }, [], [] ).
%
% Directly inspired from:
% http://stackoverflow.com/questions/114196/url-encode-in-erlang
%
-spec encode_as_url( option_list:option_list() ) -> ustring().
encode_as_url( OptionList ) ->
   encode_as_url( OptionList, _Acc=[] ).

encode_as_url( _OptionList=[], Acc ) ->
	Acc;

% First entry:
encode_as_url( [ { Key, Value } | T ], _Acc=[] ) ->
	encode_as_url( T, encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) );

encode_as_url( [ { Key, Value } | T ], Acc ) ->
	encode_as_url( T, Acc ++ "&" ++ encode_element_as_url( Key ) ++ "="
				   ++ encode_element_as_url( Value ) ).


% Encodes specified element so that it can be used in an URL.
%
-spec encode_element_as_url( ustring() ) -> ustring().
encode_element_as_url( E ) ->
	% They seem to produce quite similar results in our few test cases:
	edoc_lib:escape_uri( E ).
	%encode_uri_rfc3986:encode( E ).



% Escapes specified list of {Key,Value} pairs so that it can used into some URL.
%
-spec escape( option_list:option_list() ) -> ustring().
escape( OptionList ) ->
	%io:format( "~n~nEscaping '~p'.~n", [ OptionList ] ),
	escape( OptionList, _Acc=[] ).

escape( _OptionList=[], Acc ) ->
	Acc;

% First entry:
escape( [ { Key, Value } | T ], _Acc=[] ) ->
	escape( T, escape_key( Key ) ++ "=" ++ escape_value( Value ) );

escape( [ { Key, Value } | T ], Acc ) ->
	escape( T, Acc ++ "&" ++ escape_key( Key ) ++ "="
			++ escape_value( Value ) ).



% Escapes specified element so that it can be used in some URL.
%
-spec escape_key( option_list:key() ) -> ustring().
escape_key( Key ) when is_atom( Key ) ->
	text_utils:atom_to_string( Key ).


-spec escape_value( ustring() ) -> ustring().
escape_value( String ) ->
	R = lists:flatten( [ escape_char( C ) || C <- String ] ),
	%io:format( "'~s' became '~s'.~n", [ String, R ] ),
	R.



% Escapes specified character.
%
% Alphanumerical characters left as are:
escape_char( C ) when C >= 48 andalso C =< 57 ->
	% 0..9 kept as is:
	C;

escape_char( C ) when C >= 65 andalso C =< 90 ->
	% A..Z kept as is:
	C;

escape_char( C ) when C >= 97 andalso C =< 122 ->
	% a..z kept as is:
	C;

escape_char( C ) ->
	% Everything else is blindly encoded:
	io_lib:format( "%~s", [ integer_to_list( C, _HexBase=16 ) ] ).





% Miscellaneous functions.


% Tries to return a string adequate to form a simple name (mostly alphanumerical
% with underscores) from specified term.
%
% See also: file_utils:convert_to_filename/1.
%
-spec generate_text_name_from( term() ) -> ustring().
generate_text_name_from( Term ) ->
	String = term_to_string( Term ),
	fix_characters( String ).




% Non-exported helper functions.

fix_characters( String ) ->
	lists:reverse( fix_characters( lists:flatten( String ), [] ) ).


fix_characters( [], Acc ) ->
	Acc;

% 32 corresponds to space ('$ '):
fix_characters( [ 32 | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ $' | T ], Acc ) ->
	fix_characters( T, [ "_" | Acc ] );

fix_characters( [ H | T ], Acc ) ->
	fix_characters( T, [ H | Acc ] ).
