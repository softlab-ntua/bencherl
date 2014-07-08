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
% Creation date: Thursday, February 11, 2010.


% Gathering of various system convenient facilities.
%
% See system_utils_test.erl for the corresponding test.
%
-module(system_utils).




% User-related functions.
-export([ get_user_name/0, get_user_home_directory/0 ]).


% Lower-level services.
-export([ await_output_completion/0, await_output_completion/1 ]).


% System-related functions.
-export([ get_interpreter_version/0, get_application_version/1,
		  get_size_of_vm_word/0, get_size/1,
		  interpret_byte_size/1, interpret_byte_size_with_unit/1,
		  convert_byte_size_with_unit/1, display_memory_summary/0,
		  get_total_physical_memory/0, get_total_physical_memory_on/1,
		  get_memory_used_by_vm/0, get_total_memory_used/0,
		  get_swap_status/0, get_core_count/0, get_process_count/0,
		  compute_cpu_usage_between/2, compute_cpu_usage_for/1,
		  compute_detailed_cpu_usage/2, get_cpu_usage_counters/0,
		  get_disk_usage/0,
		  get_operating_system_description/0,
		  get_system_description/0 ]).


-type byte_size() :: integer().

-opaque cpu_usage_info() :: { integer(), integer(), integer(), integer(),
							 integer() }.


-type cpu_usage_percentages() :: { math_utils:percent(), math_utils:percent(),
								   math_utils:percent(), math_utils:percent(),
								   math_utils:percent() }.


% For record declarations:
-include("system_utils.hrl").


% Describes the static information about a computing host:
-type host_static_info() :: #host_static_info{}.


% Describes the dynamic information about a computing host:
-type host_dynamic_info() :: #host_dynamic_info{}.


-export_type([ byte_size/0, cpu_usage_info/0, cpu_usage_percentages/0,
			   host_static_info/0, host_dynamic_info/0 ]).



% User-related functions.


% Returns the name of the current user, as a plain string.
%
-spec get_user_name() -> 'false' | string().
get_user_name() ->
	os:getenv( "USER" ).



% Returns the home directory of the current user, as a plain string.
%
-spec get_user_home_directory() -> string().
get_user_home_directory() ->

	% Was: os:getenv( "HOME" )
	case init:get_argument( home ) of

		{ ok, [ [ Home ] ] } ->
			Home;

		Error ->
			throw( { home_directory_not_found, Error } )

	end.



% Lower-level services.


% Awaits the completion of a io:format request.
%
% Especially useful when displaying an error message on the standard output and
% then immediately halting the VM, in order to avoid a race condition between
% the displaying and the halting.
%
-spec await_output_completion() -> basic_utils:void().
await_output_completion() ->
	await_output_completion( _TimeOut=500 ).



% Awaits the completion of a io:format request, with a specified time-out, in
% milliseconds.
%
% Especially useful when displaying an error message on the standard output and
% then immediately halting the VM, in order to avoid a race condition between
% the displaying and the halting.
%
-spec await_output_completion(_) -> basic_utils:void().
await_output_completion( TimeOut ) ->

	% Not sure it is really the proper way of waiting, however should be still
	% better than timer:sleep(500):
	%
	% (we suppose that the time-out here is in milliseconds)

	% We added finally a short waiting, just out of safety:
	%
	timer:sleep( 200 ),

	sys:get_status( error_logger, TimeOut ).




% Erlang System-related functions.


% Returns the version informations of the current Erlang interpreter (actually
% the environment one, including the VM) being used.
%
% Returns a full version name (ex: "R13B04") or, if not available, a shorter one
% (ex: "R11B").
%
-spec get_interpreter_version() -> string().
get_interpreter_version() ->

	% Older versions (pre-R13A?) did not support the otp_release tag:
	try erlang:system_info( otp_release ) of

		StringVersion ->

			try list_to_integer( StringVersion ) of

				V ->
					% Ex: V=17 Newer release (ex: 17.0-rc1) do not comply to the
					% traditional scheme, applying it for uniformity and
					% nostalgia:
					io_lib:format( "R~BB", [ V ] )

			catch

				_:_ ->
					% Ex: StringVersion="R13B04":
					StringVersion

			end

	catch

		_:_ ->
			% Here we revert to another (older) solution:
			{ _OTPInfos, StringVersion } = init:script_id(),
			% Ex: StringVersion="R11B"
			StringVersion

	end.



% Returns the version information (as a 2 or 3-part tuple) corresponding to the
% specified Erlang application (ex: for 'kernel', could return {3,0} or
% {2,16,3}).
%
% Throws an exception if the information could not be retrieved.
%
-spec get_application_version( atom() ) -> basic_utils:any_version().
get_application_version( Application ) ->

	case application:get_key( Application, vsn ) of

		% Ex: "3.0" or "2.16.3":
		{ ok, VsnString } ->
			basic_utils:parse_version( VsnString );

		undefined ->
			throw( { application_version_not_found, Application } )

	end.



% Returns the size, in bytes, of a word of this Virtual Machine.
%
-spec get_size_of_vm_word() -> basic_utils:count().
get_size_of_vm_word() ->
	erlang:system_info( wordsize ).


% Returns the size of specified term, in bytes.
%
-spec get_size( term() ) -> byte_size().
get_size( Term ) ->
	erts_debug:flat_size( Term ) * get_size_of_vm_word().



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes, using GiB (Gibibytes, not Gigabytes), MiB (Mebibytes, not
% Megabytes), KiB (Kibibytes, not Kilobytes) and bytes.
%
% See http://en.wikipedia.org/wiki/Kibibyte
%
-spec interpret_byte_size( byte_size() ) -> string().
interpret_byte_size( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	ListWithGiga = case SizeInBytes div Giga of

					 0 ->
						 [];

					 GigaNonNull->
						 [ io_lib:format( "~B GiB", [ GigaNonNull ] ) ]

				   end,

	SizeAfterGiga = SizeInBytes rem Giga,
	%io:format( "SizeAfterGiga = ~B.~n", [ SizeAfterGiga ] ),

	ListWithMega = case SizeAfterGiga div Mega of

				 0 ->
						 ListWithGiga;

				 MegaNonNull->
					 [ io_lib:format( "~B MiB",
									  [ MegaNonNull ] ) | ListWithGiga ]

				   end,

	SizeAfterMega = SizeAfterGiga rem Mega,
	%io:format( "SizeAfterMega = ~B.~n", [ SizeAfterMega ] ),

	ListWithKilo = case SizeAfterMega div Kilo of

				 0 ->
					 ListWithMega;

				 KiloNonNull->
					[ io_lib:format( "~B KiB", [ KiloNonNull ] )
					  | ListWithMega ]

				   end,

	SizeAfterKilo = SizeAfterMega rem Kilo,
	%io:format( "SizeAfterKilo = ~B.~n", [ SizeAfterKilo ] ),

	ListWithByte = case SizeAfterKilo rem Kilo of

					 0 ->
						ListWithKilo ;

					 1->
						 [ "1 byte" | ListWithKilo ];

					 AtLeastTwoBytes ->
						 [ io_lib:format( "~B bytes", [ AtLeastTwoBytes ] )
						   | ListWithKilo ]

				   end,

	%io:format( "Unit list is: ~w.~n", [ ListWithByte ] ),

	case ListWithByte of

		[] ->
			"0 byte";

		[ OneElement ] ->
			OneElement;

		[ Smaller | Bigger ] ->
			text_utils:join( ", ", lists:reverse( Bigger ) )
				++ " and " ++ Smaller

	end.



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes, using the most appropriate unit among GiB (Gibibytes, not
% Gigabytes), MiB (Mebibytes, not Megabytes), KiB (Kibibytes, not Kilobytes) and
% bytes, rounding that value to 1 figure after the comma (this is thus an
% approximate value).
%
% See http://en.wikipedia.org/wiki/Kibibyte
%
-spec interpret_byte_size_with_unit( byte_size() ) -> string().
interpret_byte_size_with_unit( Size ) ->

	{ Unit, Value } = convert_byte_size_with_unit( Size ),

	case Unit of

		byte ->

			case Value of

				0 ->
					"0 byte";

				1 ->
					"1 byte";

				Other ->
					io_lib:format( "~B bytes", [ Other ] )

			end;

		kib ->
			io_lib:format( "~.1f KiB", [ Value ] );

		mib ->
			io_lib:format( "~.1f MiB", [ Value ] );

		gib ->
			io_lib:format( "~.1f GiB", [ Value ] )

	end.



% Converts the specified size, in bytes, as a value expressed in an appropriate
% size unit.
%
% Returns a { Unit, Value } pair, in which:
%
% - Unit is the largest size unit that can be selected so that the specified
% size if worth at least 1 unit of it (ex: we do not want a value 0.9, at least
% 1.0 is wanted); Unit can be 'gib', for GiB (Gibibytes), 'mib', for MiB
% (Mebibytes), 'kib' for KiB (Kibibytes), or 'byte', for Byte
%
% - Value is the converted byte size, in the specified returned unit, expressed
% either as an integer (for bytes) or as a float
%
% Ex: 1023 (bytes) translates to { byte, 1023 }, 1025 translates to
% { kib, 1.0009765625 }.
%
% Note that the returned value cannot be expected to be exact (rounded),
% therefore this function is mostly useful for user output.
%
-spec convert_byte_size_with_unit( byte_size() ) ->
	{ 'byte', integer() } | { 'kib', float() } | { 'mib', float() }
											 | { 'gib', float() }.
convert_byte_size_with_unit( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	case SizeInBytes div Giga of

		0 ->

			case SizeInBytes div Mega of

				0 ->

					case SizeInBytes div Kilo of

						0 ->
							%{ byte, float( SizeInBytes ) };
							{ byte, SizeInBytes };

						_ ->
							{ kib, SizeInBytes / Kilo }

					end;

				_ ->
					{ mib, SizeInBytes/Mega }

			end;

		_ ->
			{ gib, SizeInBytes / Giga }

	end.



% Returns a summary of the dynamically allocated memory currently being used by
% the Erlang emulator.
%
-spec display_memory_summary() -> basic_utils:void().
display_memory_summary() ->

	SysSize  = erlang:memory( system ),
	ProcSize = erlang:memory( processes ),

	Sum = SysSize + ProcSize,

	io:format( "  - system size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit( SysSize ),
			   text_utils:percent_to_string( SysSize / Sum ) ] ),

	io:format( "  - process size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit( ProcSize ),
			   text_utils:percent_to_string( ProcSize / Sum ) ] ).



% Returns the total installed physical volatile memory (RAM) of the local
% computer, expressed in bytes.
%
-spec get_total_physical_memory() -> byte_size().
get_total_physical_memory() ->

	% First check the expected unit is returned, by pattern-matching:
	UnitCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $3}'",

	"kB\n" = os:cmd( UnitCommand ),

	ValueCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $2}'",

	% The returned value of following command is like "12345\n", in bytes:
	MemorySizeString = text_utils:remove_ending_carriage_return(
								os:cmd( ValueCommand ) ),

	% They were kB (not kiB):
	list_to_integer( MemorySizeString ) * 1000.



% Returns the total installed physical volatile memory (RAM) of the computer on
% which specified node (specified as an atom) is running, expressed in bytes.
%
-spec get_total_physical_memory_on( net_utils:atom_node_name() ) -> byte_size().
get_total_physical_memory_on( Node ) ->

	% First check the expected unit is returned, by pattern-matching:
	UnitCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $3}'",
	"kB\n" = rpc:call( Node, os, cmd, [ UnitCommand ] ),

	ValueCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $2}'",
	ValueCommandOutput = rpc:call( Node, os, cmd, [ ValueCommand ] ),

	% The returned value of following command is like "12345\n", in bytes:
	MemorySizeString = text_utils:remove_ending_carriage_return(
								ValueCommandOutput ),

	% They were kB (not kiB):
	list_to_integer( MemorySizeString ) * 1000.



% Returns the total memory used, in bytes, by this instance of the Erlang VM,
% i.e. the total amount of memory currently allocated by the Erlang processes
% and by this emulator.
%
-spec get_memory_used_by_vm() -> byte_size().
get_memory_used_by_vm() ->
	erlang:memory( total ).



% Returns { UsedRAM, TotalRAM } where UsedRAM is the actual total memory used on
% the current host by all applications, in bytes, and TotalRAM is the total
% installed RAM, in bytes.
%
% The cached memory and the buffers used by the kernel are not taken into
% account into the returned count.
%
-spec get_total_memory_used() -> { byte_size(), byte_size() }.
get_total_memory_used() ->

	% Example of memory information as returned by the 'free' command:
	% (slightly edited with variable names)
	%
	% """
	%             total       used       free     shared    buffers    cached
	% Mem:       A=8202424  B=5588920  C=2613504  D=0       E=567480   F=3212392
	% -/+ buffers/cache:    G=1809048  H=6393376
	% """
	%
	% We have: H = C + D + E + F, and G = A - H. D is never used (obsolete).
	% We return here { G, A }, thus { G, G+H }.

	MemoryInfo = os:cmd(
				   "free -b | grep 'buffers/cache:' | awk '{print $3,$4}'" ),

	% Converts MemoryInfo from "a b\n" to ["a","b\n"]
	[ AppliUsedString, TotalFreeTermString ] = string:tokens( MemoryInfo, " " ),

	% This is G:
	AppliUsedSize = text_utils:string_to_integer( AppliUsedString ),

	TotalFreeString = text_utils:remove_ending_carriage_return(
														TotalFreeTermString ),

	% This is H:
	TotalFreeSize = text_utils:string_to_integer( TotalFreeString ),

	% { G, G+H }:
	{ AppliUsedSize, AppliUsedSize + TotalFreeSize }.






% Returns { UsedSwap, TotalSwap } where UsedSwap is the size of the used swap
% and TotalSwap is the total amount of swap space on the local host, both
% expressed in bytes.
%
-spec get_swap_status() -> { byte_size(), byte_size() }.
get_swap_status() ->

	SwapInfos = os:cmd( "free -b | grep 'Swap:' | awk '{print $2, $3}'" ),

	[ TotalSwapString, UsedSwapWith ] = string:tokens( SwapInfos, " " ),

	UsedSwapString = text_utils:remove_ending_carriage_return( UsedSwapWith ),

	TotalSwap = text_utils:string_to_integer( TotalSwapString ),
	UsedSwap = text_utils:string_to_integer( UsedSwapString ),

	{ UsedSwap, TotalSwap }.



% Returns the number of cores available on the local host.
%
% Throws an exception on failure.
%
-spec get_core_count() -> integer().
get_core_count() ->

	String = text_utils:remove_ending_carriage_return(
				os:cmd( "cat /proc/cpuinfo | grep -c processor" ) ),

	try

		text_utils:string_to_integer( String )

	catch

		{ integer_conversion_failed, String } ->
			throw( { could_not_determine_core_count, String } )

	end.



% Returns the number of live Erlang processes on the current node.
%
-spec get_process_count() -> basic_utils:count().
get_process_count() ->
	erlang:system_info( process_count ).



% Returns an aggregated view of the CPU usage (a float in [0;100]) based on the
% two specified sets of CPU counters, i.e. the average (on all cores of all
% processors of the local host) percentage of CPU utilization (all kinds of
% usage except idle) during the period which elapsed between the start and end
% measures (in that order).
%
% Typical usage:
%
% FirstMeasure = system_utils:get_cpu_usage_counters(),
% (do something)
% SecondMeasure = system_utils:get_cpu_usage_counters(),
%
% UsageInPercent = system_utils:compute_cpu_usage_between( FirstMeasure,
%   SecondMeasure )
%
-spec compute_cpu_usage_between( cpu_usage_info(), cpu_usage_info() ) ->
							   math_utils:percent().
compute_cpu_usage_between( StartCounters, EndCounters ) ->

	Percentages = compute_detailed_cpu_usage( StartCounters, EndCounters ),

	compute_cpu_usage_for( Percentages ).



% Returns an aggregated view of the CPU usage (a float in [0;100]) based on the
% specified detailed CPU percentages, i.e. the average (on all cores of all
% processors of the local host) percentage of CPU utilization (all kinds of
% usage except idle) during the period the input percentages correspond to.
%
% Returns 'undefined' iff the specified usage is itself undefined.
%
-spec compute_cpu_usage_for( basic_utils:maybe( cpu_usage_percentages() ) ) ->
							   basic_utils:maybe( math_utils:percent() ).
compute_cpu_usage_for( undefined ) ->
	undefined;

compute_cpu_usage_for( { UserPercent, NicePercent, SystemPercent, _IdlePercent,
						OtherPercent } ) ->

	% Every usage matters here, except idle:
	UserPercent + NicePercent + SystemPercent + OtherPercent.



% Returns a detailed view of the CPU usage, i.e. the average (on all cores of
% all processors of the local host) percentage of the various kinds of CPU
% utilization: { UserPercent, NicePercent, SystemPercent, IdlePercent,
% OtherPercent }, respectively for user mode, user mode with low priority
% (nice), system mode, idle task and all other usages (if any), between the two
% sets of measures.
%
% If the two sets of specified counters are equal, returns 'undefined', as no
% usage can be quantified then.
%
-spec compute_detailed_cpu_usage( cpu_usage_info(), cpu_usage_info() ) ->
	basic_utils:maybe( cpu_usage_percentages() ).
compute_detailed_cpu_usage( _StartCounters={ U1, N1, S1, I1, O1 },
							_EndCounters = { U2, N2, S2, I2, O2 } ) ->

	User = U2 - U1,

	Nice = N2 - N1,

	System = S2 - S1,

	Idle = I2 - I1,

	Other = O2 - O1,

	% This would be great if we could avoid a division by zero:
	case User + Nice + System + Idle + Other of

		% Yes, this happens:
		0.0 ->
			undefined;

		Sum ->

			RoundDigits = 1,

			UserPercent = math_utils:round_after( 100 * User / Sum,
												  RoundDigits ),

			NicePercent = math_utils:round_after( 100 * Nice / Sum,
												  RoundDigits ),

			SystemPercent = math_utils:round_after( 100 * System / Sum,
													RoundDigits ),

			IdlePercent = math_utils:round_after( 100 * Idle / Sum,
												  RoundDigits ),

			AllButOtherPercent = UserPercent + NicePercent + SystemPercent
				+ IdlePercent,

			% Avoids rounding errors:
			OtherPercent = math_utils:round_after( 100 - AllButOtherPercent,
										  RoundDigits ),

			{ UserPercent, NicePercent, SystemPercent, IdlePercent,
			  OtherPercent }

	end.



% Returns the instantaneous CPU counters, as maintained from boot.
%
% Note: mostly useful in terms of differences over time.
%
-spec get_cpu_usage_counters() -> cpu_usage_info().
get_cpu_usage_counters() ->

	% grep more versatile than: '| head -n 1':
	StatString = text_utils:remove_ending_carriage_return(
					   os:cmd( "cat /proc/stat | grep 'cpu '" ) ),

	% Ex: cpu  1331302 11435 364777 150663306 82509 249 3645 0 0

	% Tells the time spent in user mode, user mode with low priority (nice),
	% system mode, and the idle task.

	[ "cpu", UserString, NiceString, SystemString, IdleString | T ] =
						string:tokens( StatString, " " ),

	User   = text_utils:string_to_integer( UserString ),
	Nice   = text_utils:string_to_integer( NiceString ),
	System = text_utils:string_to_integer( SystemString ),
	Idle   = text_utils:string_to_integer( IdleString ),

	% Adapts to any architecture and update (iowait, irq, softirq, steal, guest,
	% etc.):
	Other = lists:sum( [ text_utils:string_to_integer( E ) || E <- T ] ),

	%io:format( "user = ~f, nice = ~f, system = ~f, idle = ~f, other = ~f, "
	%			"T = ~p~n", [ User, Nice, System, Idle, Other, T ] ),

	{ User, Nice, System, Idle, Other }.



% Returns the current usage of disks.
%
-spec get_disk_usage() -> string().
get_disk_usage() ->
	 text_utils:remove_ending_carriage_return( os:cmd( "df -h" ) ).



% Returns a string describing the current operating system.
%
-spec get_operating_system_description() -> string().
get_operating_system_description() ->

	IdentifierPath = "/etc/issue.net",

	case file_utils:is_existing_file( IdentifierPath ) of

		true ->
			BinString = file_utils:read_whole( IdentifierPath ),
			text_utils:trim_whitespaces(
						   text_utils:binary_to_string( BinString ) );

		false ->
			"(unknown operating system)"

	end.



% Returns a string describing the current state of the local system.
%
-spec get_system_description() -> string().
get_system_description() ->

	{ UsedRAM, TotalRAM } = get_total_memory_used(),

	{ UsedSwap, TotalSwap } = get_swap_status(),

	SwapInfo = "swap used: " ++ case TotalSwap of

				   0 ->
					   "none (no swap)";

				   _ ->
					   io_lib:format( "~s over a total of ~s (~s)",
									  [ interpret_byte_size( UsedSwap ),
										interpret_byte_size( TotalSwap ),
										text_utils:percent_to_string(
										  UsedSwap / TotalSwap ) ] )

	end,

	Subjects = [

		io_lib:format( "number of cores: ~B", [ get_core_count() ] ),

		io_lib:format( "size of a VM word: ~B bytes",
					  [ get_size_of_vm_word() ] ),

		io_lib:format( "operating system: ~s",
					  [ get_operating_system_description() ] ),

		io_lib:format( "number of existing Erlang processes: ~B ",
					  [ get_process_count() ] ),

		io_lib:format( "total physical memory: ~s",
					  [ interpret_byte_size( get_total_physical_memory() ) ] ),

		io_lib:format( "memory used by VM: ~s over a total of ~s (~s)",
					  [ interpret_byte_size( UsedRAM ),
					   interpret_byte_size( TotalRAM ),
					   text_utils:percent_to_string( UsedRAM / TotalRAM ) ] ),

		SwapInfo,

		io_lib:format( "user name: ~s", [ get_user_name() ] ),

		io_lib:format( "user home directory: ~s",
					  [ get_user_home_directory() ] ),

		io_lib:format( "current directory: ~s",
					  [ file_utils:get_current_directory() ] ),

		io_lib:format( "current disk usage:~n~s", [ get_disk_usage() ] )

				],

	text_utils:strings_to_string( Subjects ).
