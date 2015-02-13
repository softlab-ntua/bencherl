% Copyright (C) 2003-2015 Olivier Boudeville
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


% Unit tests for the system utils toolbox.
%
% See the system_utils.erl tested module.
%
-module(system_utils_test).



% For run/0 export and al:
-include("test_facilities.hrl").



print_sizes( [] ) ->
	ok;

print_sizes( [ H | T ] ) ->

	Size = system_utils:get_size( H ),

	test_facilities:display( "     - exact size of ~p is ~s",
							 [ H, system_utils:interpret_byte_size( Size ) ] ),

	print_sizes( T ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	InitialCounters = system_utils:get_cpu_usage_counters(),

	% User-related functions.

	test_facilities:display( "Determining what is the name of the current user:"
							 " ~s.", [ system_utils:get_user_name() ] ),

	test_facilities:display( "Determining what is the home directory "
							 "of the current user: ~s.",
							 [ system_utils:get_user_home_directory() ] ),

	TotalRAM = system_utils:get_total_physical_memory(),

	test_facilities:display( "Determining the total physical volatile memory "
							 "on this computer: ~B bytes, which is ~s.",
			  [ TotalRAM, system_utils:interpret_byte_size( TotalRAM ) ] ),


	UsedMemory = system_utils:get_memory_used_by_vm(),

	test_facilities:display( "Determining the total memory used "
							 "by this Erlang VM: ~B bytes, which is ~s.",
			[ UsedMemory, system_utils:interpret_byte_size( UsedMemory ) ] ),


	{ UsedRAM, _TotalRAM } = system_utils:get_total_memory_used(),

	test_facilities:display( "Determining the total memory used "
							 "by all applications: ~B bytes, which is ~s.",
			[ UsedRAM, system_utils:interpret_byte_size( UsedRAM ) ] ),


	{ UsedSwap, TotalSwap } = system_utils:get_swap_status(),

	test_facilities:display( "Determining the total swap memory "
							 "on this computer: ~B bytes, which is ~s.~n"
							 "Swap use amounts to ~B byte(s), which is ~s.",
				[ TotalSwap, system_utils:interpret_byte_size( TotalSwap ),
				  UsedSwap, system_utils:interpret_byte_size( UsedSwap ) ] ),


	% Lower-level services tested as well:
	system_utils:await_output_completion(),

	% System-related functions.

	test_facilities:display(
		"Determining a description of the current operating system: ~s.",
		[ system_utils:get_operating_system_description() ] ),


	test_facilities:display(
		"Determining the current version of the interpreter (VM): ~s.",
		[ system_utils:get_interpreter_version() ] ),

	Application = kernel,

	test_facilities:display( "Determining the current version of "
							 "the '~s' application: ~w.", [ Application,
					system_utils:get_application_version( Application ) ] ),

	test_facilities:display( "Determining the size of a VM word: ~B bytes.",
							 [ system_utils:get_size_of_vm_word() ] ),

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	SizesToInterpret = [ 0, 1, Kilo-1, Kilo, Kilo+1, 2*Kilo - 1, 2*Kilo,
						 2*Kilo + 1, 10000, Mega-1, Mega, Mega+1,
						 10000000, Giga - 1, Giga, Giga + 1,
						 1140328500,
						 Giga + Kilo, Giga + Mega, Giga + Mega + Kilo,
						 2* Giga,
						 1234567890123],

	test_facilities:display( "Testing size-describing facilities:" ),

	[ test_facilities:display( "    + '~B bytes' translates to: '~s', or "
							   "'~s', in terms of units",
	  [ X, system_utils:interpret_byte_size( X ),
	   system_utils:interpret_byte_size_with_unit( X ) ] )
	 || X <- SizesToInterpret ],

	test_facilities:display(
	  "Evaluating the size in memory of a few terms:" ),

	AFullSentence = "All human beings are born free and equal in dignity "
		"and rights. They are endowed with reason and conscience and "
		"should act towards one another in a spirit of brotherhood.",

	BinaryVersion = text_utils:string_to_binary( AFullSentence ),

	TermsForSize = [ an_atom, 5, "aaa", "aaaa", [], [1], [1,2], {}, {1}, {1,2},
					 self(), dict:new(), orddict:new(), table:new(),
					 AFullSentence, BinaryVersion ],

	print_sizes( TermsForSize ),

	test_facilities:display(
			  "Plain string-binary size ratio for sentence '~s': factor x~f.",
			  [ AFullSentence, system_utils:get_size( AFullSentence )
				  / system_utils:get_size( BinaryVersion ) ] ),


	test_facilities:display( "Getting memory summary:" ),
	system_utils:display_memory_summary(),

	test_facilities:display( "The number of detected cores is ~B.",
							 [ system_utils:get_core_count() ] ),

	test_facilities:display( "The number of Erlang processes "
							 "currently existing on the local node is ~B.",
							 [ system_utils:get_process_count() ] ),

	FinalCounters = system_utils:get_cpu_usage_counters(),

	test_facilities:display( "The aggregated CPU usage during this test: ~f%.",
							 [ system_utils:compute_cpu_usage_between(
								 InitialCounters, FinalCounters ) ] ),

	 { UserPercent, NicePercent, SystemPercent, IdlePercent, OtherPercent } =
	  system_utils:compute_detailed_cpu_usage( InitialCounters, FinalCounters ),

	test_facilities:display( "The detailed CPU usage: user = ~f%, "
							 "nice = ~f%, system = ~f%, idle = ~f%, "
							 "other = ~f%.",
							 [ UserPercent, NicePercent, SystemPercent,
							   IdlePercent, OtherPercent ] ),

	MountPoints = system_utils:get_mount_points(),
	test_facilities:display( "Displaying information about the local, "
							 "actual (non-pseudo) mount points ~p:",
							 [ MountPoints ] ),

	[ test_facilities:display( " - information for filesystem '~s': ~s~n",
							   [ M, system_utils:filesystem_info_to_string(
								  system_utils:get_filesystem_info( M ) ) ] )
								 || M <- MountPoints ],

	test_facilities:display( "Full system information: ~ts",
							 [ system_utils:get_system_description() ] ),

	test_facilities:stop().
