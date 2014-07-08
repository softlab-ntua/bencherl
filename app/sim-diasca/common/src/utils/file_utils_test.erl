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


% Unit tests for the file_utils toolbox.
%
% See the file_utils.erl tested module.
%
-module(file_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	CurrentDir = file_utils:get_current_directory(),

	{ _RegularFiles, _Directories, _OtherFiles, _Devices } = Elements
		= file_utils:list_dir_elements( CurrentDir ),

	BeamExtension = ".beam",

	test_facilities:display(
			  "File elements in the current directory (~s):~n~p",
			  [ CurrentDir, Elements ] ),

	% Too many outputs:
	%test_facilities:display( "Regular BEAM files in the current directory: "
	% ~n~p", [ file_utils:filter_by_extension( RegularFiles, BeamExtension) ] ),

	test_facilities:display( "All files found recursively "
		"from the current directory:~n~p",
		[ file_utils:find_files_from( CurrentDir ) ] ),


	test_facilities:display( "All BEAM files found recursively "
		"from the current directory:~n~p",
		[ file_utils:find_files_with_extension_from( CurrentDir,
													BeamExtension ) ] ),

	ExcludedDirs = [ ".svn", "non-existing-dir" ],

	test_facilities:display( "All files found recursively "
		"from the current directory, with directories ~p excluded:~n~p",
		[ ExcludedDirs, file_utils:find_files_with_excluded_dirs(
						CurrentDir, ExcludedDirs ) ] ),


	ExcludedSuffixes = [ ".erl", ".beam", "non-existing-suffix" ],

	test_facilities:display( "All files found recursively "
		"from the current directory, with suffixes ~p excluded:~n~p",
		[ ExcludedSuffixes, file_utils:find_files_with_excluded_suffixes(
						CurrentDir, ExcludedSuffixes ) ] ),


	test_facilities:display( "All files found recursively "
			  "from the current directory, with directories ~p and suffixes ~p "
			  "excluded:~n~p",
		[ ExcludedDirs, ExcludedSuffixes,
		  file_utils:find_files_with_excluded_dirs_and_suffixes(
						CurrentDir, ExcludedDirs, ExcludedSuffixes ) ] ),


	true  = file_utils:is_absolute_path( "/etc/host.conf" ),
	false = file_utils:is_absolute_path( "my-dir/my-file" ),
	false = file_utils:is_absolute_path( "" ),

	RelativePath = "my-local-dir/a-file",

	test_facilities:display( "Ensuring '~s' is absolute: ~s", [ RelativePath,
					file_utils:ensure_path_is_absolute( RelativePath ) ] ),

	BasePath ="/etc",

	test_facilities:display(
	  "Ensuring '~s' is absolute with base path '~s': '~s'",
	  [ RelativePath, BasePath,
		file_utils:ensure_path_is_absolute( RelativePath, BasePath ) ] ),


	FirstFilename = "media/frame/1-23-2-98.oaf",

	test_facilities:display( "Path '~s', once transformed into a variable name,"
		" results in: ~s",
		[ FirstFilename, file_utils:path_to_variable_name( FirstFilename ) ] ),



	SecondFilename = "./mnt/zadok/44_12.oaf",

	test_facilities:display( "Path '~s', once transformed into a variable name,"
		" results in: ~s", [ SecondFilename,
						file_utils:path_to_variable_name( SecondFilename ) ] ),


	FirstString = "My name is Bond",
	test_facilities:display( "String '~s', "
		"once transformed into a file name, results in: '~s'",
		[ FirstString, file_utils:convert_to_filename( FirstString ) ] ),


	SecondString = "James,  James <Bond> ('Special' \"Agent\"), Sir",
	test_facilities:display( "String '~s', once transformed into a file name,"
		"results in: '~s'",
		[ SecondString, file_utils:convert_to_filename( SecondString ) ] ),


	SourceFilename  = "/home/jack/rosie.ttf",
	SourceExtension = ".ttf",
	TargetExtension = ".wav",

	NewFilename = file_utils:replace_extension( SourceFilename, SourceExtension,
		TargetExtension ),

	test_facilities:display( "Replacing extension '~s' by '~s' in '~s' "
		"results in: '~s'.",
		[ SourceExtension, TargetExtension, SourceFilename, NewFilename ] ),


	% Commented as not wanting to have too many side-effects:

	%file_utils:create_directory( "tmp-tst" ),
	%file_utils:create_directory( "tmp-tst/first/second", create_parents ),

	Bin = file_utils:read_whole( "GNUmakefile" ),
	test_facilities:display( "Read file: ~p.", [ Bin ] ),
	%file_utils:write_whole( "test.dat", Bin ),

	LsPath = "/bin/ls" = executable_utils:find_executable( "ls" ),
	true = file_utils:is_executable( LsPath ),

	NonExistingPath = "ls-non-existing-exec",
	false = executable_utils:lookup_executable( NonExistingPath ),
	false = file_utils:is_executable( NonExistingPath ),

	test_facilities:stop().
