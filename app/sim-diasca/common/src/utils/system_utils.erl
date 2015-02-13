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
-export([

		  execute_command/1, execute_background_command/1,

		  get_interpreter_version/0, get_application_version/1,

		  get_size_of_vm_word/0, get_size/1,
		  interpret_byte_size/1, interpret_byte_size_with_unit/1,
		  convert_byte_size_with_unit/1,

		  display_memory_summary/0, get_total_physical_memory/0,
		  get_total_physical_memory_on/1, get_memory_used_by_vm/0,
		  get_total_memory_used/0,

		  get_swap_status/0, get_core_count/0, get_process_count/0,
		  compute_cpu_usage_between/2, compute_cpu_usage_for/1,
		  compute_detailed_cpu_usage/2, get_cpu_usage_counters/0,

		  get_disk_usage/0, get_mount_points/0,
		  get_known_pseudo_filesystems/0, get_filesystem_info/1,
		  filesystem_info_to_string/1,

		  get_operating_system_description/0, get_system_description/0 ]).


% Size, in number of bytes:
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



% Known real, actual types of filesystems:
-type actual_filesystem_type() :: 'ext2' | 'ext3' | 'ext4' | 'vfat'.


% Known pseudo filesystems:
-type pseudo_filesystem_type() :: 'devtmpfs' | 'tmpfs'.


% All the known types of filesystems (atom, to capture even lacking ones):
-type filesystem_type() :: actual_filesystem_type()
						 | pseudo_filesystem_type()
						 | 'unknown' | atom().


% Stores information about a filesystem:
%
-record( fs_info, {

		   % Device name (ex: /dev/sda5):
		   filesystem :: file_utils:path(),

		   % Mount point (ex: /boot):
		   mount_point :: file_utils:path(),

		   % Filesystem type (ex: 'ext4'):
		   type :: filesystem_type(),

		   % Used size, in bytes:
		   used_size :: byte_size(),

		   % Available size, in bytes:
		   available_size :: byte_size(),

		   % Number of used inodes:
		   used_inodes :: basic_utils:count(),

		   % Number of available inodes:
		   available_inodes :: basic_utils:count()

		  } ).


-type fs_info() :: #fs_info{}.



% Describes a shell command:
%
-type command() :: text_utils:ustring().


% Return code of a shell command (a.k.a. exit status):
%
% (0 means success, strictly positive mean error)
%
-type return_code() :: basic_utils:count().


% Output of a shell command:
%
-type command_output() :: text_utils:ustring().


% All information returned by a shell command:
%
-type command_outcome() :: { return_code(), command_output() }.



-export_type([ byte_size/0, cpu_usage_info/0, cpu_usage_percentages/0,
			   host_static_info/0, host_dynamic_info/0,

			   actual_filesystem_type/0, pseudo_filesystem_type/0,
			   filesystem_type/0, fs_info/0,

			   command/0, return_code/0, command_output/0, command_outcome/0

			 ]).




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

-ifdef(debug_mode_is_enabled).

% Default time-out duration (one second):
await_output_completion() ->
	await_output_completion( _TimeOut=1000 ).

-else. % debug_mode_is_enabled


% Extended time-out (one minute), if for example being in production, on a
% possibly heavily loaded system:
%
await_output_completion() ->
	await_output_completion( _TimeOut=60000 ).

-endif. % debug_mode_is_enabled



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
	% better than timer:sleep( 500 ):
	%
	% (we suppose that the time-out here is in milliseconds)

	% We added finally a short waiting, just out of safety:
	%
	timer:sleep( 200 ),

	sys:get_status( error_logger, TimeOut ).





% Functions relative to the local Erlang system.



% Executes (synchronously) specified shell command (specified as a single,
% standalone one, or as a list of command elements), and returns its return code
% (exit status) and its outputs (both the standard and the error ones).
%
% We wish we could specify the command as a single, standalone one, or as a list
% of command elements, but the lack of a string type prevents it.
%
%-spec execute_command( command() | [ command() ] ) -> command_outcome().
%execute_command( Commands ) when is_list( Commands ) ->
-spec execute_command( command() ) -> command_outcome().
%execute_command( Commands ) ->

%	ActualCommand = text_utils:join( _Sep=" ", Commands ),

%	execute_command( ActualCommand );


execute_command( Command ) ->

	PortOpts = [ stream, exit_status, use_stdio, stderr_to_stdout, in, eof ],

	Port = open_port( { spawn, Command }, PortOpts ),

	read_port( Port, _Data=[] ).



% Helper to read command data from a port.
%
read_port( Port, Data ) ->

	receive

		{ Port, { data, NewData } } ->
			read_port( Port, [ NewData | Data ] );

		{ Port, eof } ->

			port_close( Port ),

			receive

				{ Port, { exit_status, ExitStatus } } ->

					% Otherwise we have an enclosing list and last character is
					% always "\n":
					%
					Output = text_utils:remove_ending_carriage_return(
							   lists:flatten( lists:reverse( Data ) ) ),

					{ ExitStatus, Output }

			 end


	 end.



% Executes asynchronously, in the background, specified shell command
%
% As a consequence it returns no return code (exit status) nor output.
%

% We wish we could specify the command as a single, standalone one, or as a list
% of command elements, but the lack of a string type prevents it.
%
%-spec execute_background_command( command() | [ command() ] ) ->
%										basic_utils:void().

-spec execute_background_command( command() ) -> basic_utils:void().


%execute_background_command( Commands ) when is_list( Commands ) ->

%	ActualCommand = text_utils:join( _Sep=" ", Commands ),

%	execute_background_command( ActualCommand );


execute_background_command( Command ) ->
	os:cmd( Command ++ " &" ).



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
					% traditional scheme, applying it for uniformity and maybe a
					% bit of nostalgia:
					%
					lists:flatten( io_lib:format( "R~BB", [ V ] ) )

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

	% With sharing taken into account:
	% use erts_debug:size/1 * get_size_of_vm_word()
	%
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

	case execute_command( UnitCommand ) of

		 { _ExitCode=0, _Output="kB" } ->

			% Ok, using kB indeed.

			ValueCommand =
				"cat /proc/meminfo | grep 'MemTotal:' | awk '{print $2}'",

			% The returned value of following command is like "12345\n", in
			% bytes:
			%
			case execute_command( ValueCommand ) of

				{ _ExitCode=0, MemSizeString } ->

					% They were probably kiB:
					list_to_integer( MemSizeString ) * 1024;

				{ ExitCode, ErrorOutput } ->
					throw( { total_physical_memory_inquiry_failed, ExitCode,
							 ErrorOutput } )

			end;

		{ ExitCode, ErrorOutput } ->
			throw( { total_physical_memory_inquiry_failed, ExitCode,
					 ErrorOutput } )

	end.






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

	% They were probably kiB:
	list_to_integer( MemorySizeString ) * 1024.



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

	% Avoid locale and greps 'buffers/cache:' (ex: on Debian) as well as
	% 'buff/cache' (ex: on Arch)
	%MemoryInfo = os:cmd( "LANG= free -b | grep '/cache' "
	%					 "| awk '{print $3,$4}'" ),

	% Converts MemoryInfo from "a b\n" to ["a","b\n"]
	%[ AppliUsedString, TotalFreeTermString ] =
	%  string:tokens( MemoryInfo, " " ),

	% Unfortunately on Arch we have quite different outputs, like:
	%          total        used        free      shared  buff/cache   available
	% Mem:   8047428     2476488     1124396      362228     4446544     4893712
	% Swap:        0           0           0


	% This is G:
	%AppliUsedSize = text_utils:string_to_integer( AppliUsedString ),

	%TotalFreeString = text_utils:remove_ending_carriage_return(
	%													TotalFreeTermString ),

	% This is H:
	%TotalFreeSize = text_utils:string_to_integer( TotalFreeString ),

	% { G, G+H }:
	%{ AppliUsedSize, AppliUsedSize + TotalFreeSize }.


	% So finally we prefered /proc/meminfo, used first to get MemTotal:
	%
	TotalString = case execute_command(
		  "cat /proc/meminfo|grep '^MemTotal:'|awk '{print $2,$3}'" ) of

		{ _TotalExitCode=0, TotalOutput } ->
			%io:format( "TotalOutput: '~p'~n", [ TotalOutput ] ),
			TotalOutput;

		{ TotalExitCode, TotalErrorOutput } ->
			throw( { total_memory_used_inquiry_failed, TotalExitCode,
					 TotalErrorOutput } )

	end,

	[ Total, "kB" ] = string:tokens( TotalString, " " ),

	TotalByte = text_utils:string_to_integer( Total ) * 1024,

	% MemAvailable does not seem always available:
	%
	FreeString = case execute_command(
						"cat /proc/meminfo|grep '^MemAvailable:'|awk "
						"'{print $2,$3}'" )  of

		{ _AvailExitCode=0, MemAvailOutput } ->
			%io:format( "## using MemAvailable~n" ),
			MemAvailOutput;

		{ _AvailExitCode, _AvailErrorOutput } ->

			% In some cases (ex: Debian 6.0), no 'MemAvailable' is defined, we
			% use 'MemFree' instead (we consider they are synonymous):

			%io:format( "## using MemFree~n" ),

			case execute_command(
				   "cat /proc/meminfo|grep '^MemFree:'|awk "
				   "'{print $2,$3}'" ) of

				{ _FreeExitCode=0, MemFreeOutput } ->
					MemFreeOutput;

				{ FreeExitCode, FreeErrorOutput } ->
					throw( { total_memory_used_inquiry_failed, FreeExitCode,
							 FreeErrorOutput } )

			end

	end,

	% The problem is that even if MemAvailable is not found, we have a zero exit
	% code (and an empty string):
	%
	FreeByte = case FreeString of

		[] ->

			% As a last resort we do as before, i.e. we use free:

			%io:format( "## using free~n" ),

			case execute_command(
				"free -b | grep '/cache' | awk '{print $3}'" ) of

				{ _ExitCode=0, FreeOutput } ->
					% Already in bytes:
					text_utils:string_to_integer( FreeOutput );

				{ ExitCode, ErrorOutput } ->
					throw( { total_memory_used_inquiry_failed, ExitCode,
							 ErrorOutput } )

			end;

		_ ->

			[ Free, "kB" ] = string:tokens( FreeString, " " ),

			text_utils:string_to_integer( Free ) * 1024

	end,

	UsedByte = TotalByte - FreeByte,

	{ UsedByte, TotalByte }.



% Returns { UsedSwap, TotalSwap } where UsedSwap is the size of the used swap
% and TotalSwap is the total amount of swap space on the local host, both
% expressed in bytes.
%
-spec get_swap_status() -> { byte_size(), byte_size() }.
get_swap_status() ->

	% Same reason as for get_total_memory_used/0:
	%SwapInfos = os:cmd( "free -b | grep 'Swap:' | awk '{print $2, $3}'" ),
	SwapTotalString = case execute_command(
		  "cat /proc/meminfo|grep '^SwapTotal:'|awk '{print $2,$3}'" ) of

		{ _TotalExitCode=0, TotalOutput } ->
			TotalOutput;

		{ TotalExitCode, TotalErrorOutput } ->
			throw( { swap_inquiry_failed, TotalExitCode, TotalErrorOutput } )

	end,

	[ TotalString, "kB" ] = string:tokens( SwapTotalString, " " ),

	TotalByte = text_utils:string_to_integer( TotalString ) * 1024,


	SwapFreeString = case execute_command(
		  "cat /proc/meminfo|grep '^SwapFree:'|awk '{print $2,$3}'" ) of

		{ _FreeExitCode=0, FreeOutput } ->
			FreeOutput;

		{ FreeExitCode, FreeErrorOutput } ->
			throw( { swap_inquiry_failed, FreeExitCode, FreeErrorOutput } )

	end,


	[ FreeString, "kB" ] = string:tokens( SwapFreeString, " " ),

	FreeByte = text_utils:string_to_integer( FreeString ) * 1024,

	UsedByte = TotalByte - FreeByte,

	{ UsedByte, TotalByte }.



% Returns the number of cores available on the local host.
%
% Throws an exception on failure.
%
-spec get_core_count() -> integer().
get_core_count() ->

	CoreString = case execute_command(
						"cat /proc/cpuinfo | grep -c processor" ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { core_count_inquiry_failed, ExitCode, ErrorOutput } )

	end,

	try

		text_utils:string_to_integer( CoreString )

	catch

		{ integer_conversion_failed, CoreString } ->
			throw( { could_not_determine_core_count, CoreString } )

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
	StatString = case execute_command( "cat /proc/stat | grep 'cpu '" ) of

		{ _ExitCode=0, Output } ->
						 Output;

		{ ExitCode, ErrorOutput } ->
						 throw( { cpu_counters_inquiry_failed, ExitCode,
								  ErrorOutput } )

	end,

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



% Returns the current usage of disks, as a human-readable string.
%
-spec get_disk_usage() -> text_utils:ustring().
get_disk_usage() ->

	case execute_command( "/bin/df -h" ) of

		{ _ExitCode=0, Output } ->
			Output;

		{ ExitCode, ErrorOutput } ->
			throw( { disk_usage_inquiry_failed, ExitCode,
					 ErrorOutput } )

	end.



% Returns a list of the known types of pseudo-filesystems.
%
-spec get_known_pseudo_filesystems() -> [ pseudo_filesystem_type() ].
get_known_pseudo_filesystems() ->

	% A list of all current filesystems can be obtained thanks to: 'df -T'.
	[ tmpfs, devtmpfs ].



% Returns a list of the current, local mount points (excluding the
% pseudo-filesystems).
%
-spec get_mount_points() -> [ file_utils:path() ].
get_mount_points() ->

	FirstCmd = "/bin/df -h --local --output=target"
		++ get_exclude_pseudo_fs_opt() ++ "|grep -v 'Mounted on'",

	case execute_command( FirstCmd ) of

		{ _FirstExitCode=0, ResAsOneString } ->
			%io:format( "## using direct df~n" ),
			text_utils:split( ResAsOneString, "\n" );

		{ _FirstExitCode, _FirstErrorOutput } ->

			% Older versions of df may not know the --output option:
			SecondCmd = "/bin/df -h --local "
				++ get_exclude_pseudo_fs_opt()
				++ "|grep -v 'Mounted on' | awk '{print $6}'",

			case execute_command( SecondCmd ) of

				{ _SecondExitCode=0, ResAsOneString } ->
					%io:format( "## using legacy df~n" ),
					text_utils:split( ResAsOneString, "\n" );

				{ SecondExitCode, SecondErrorOutput } ->
					throw( { mount_point_inquiry_failed, SecondExitCode,
							 SecondErrorOutput } )

			end

	end.



% (helper for df)
%
get_exclude_pseudo_fs_opt() ->

	Excludes = [ " --exclude-type=" ++ text_utils:atom_to_string( P )
				 || P <- get_known_pseudo_filesystems() ],

	text_utils:join( _Sep=" ", Excludes ).



% Returns information about the specified filesystem.
%
-spec get_filesystem_info( file_utils:bin_path() | file_utils:path() ) ->
								 fs_info().
get_filesystem_info( BinFilesystemPath ) when is_binary( BinFilesystemPath ) ->
	get_filesystem_info( text_utils:binary_to_string( BinFilesystemPath ) );

get_filesystem_info( FilesystemPath ) ->

	Cmd = "/bin/df --block-size=1K --local "
		++ get_exclude_pseudo_fs_opt()
		++ " --output=source,target,fstype,used,avail,iused,iavail '"
		++ FilesystemPath ++ "' | grep -v 'Mounted on'",

	case execute_command( Cmd ) of

		{ _ExitCode=0, ResAsOneString } ->
			% Order of the columns: 'Filesystem / Mounted on / Type / Used /
			% Avail / IUsed / IFree':
			%
			case text_utils:split( ResAsOneString, " " ) of

				[ Fs, Mount, Type, USize, ASize, Uinodes, Ainodes ] ->

					%io:format( "## using direct df~n" ),

					% df outputs kiB, not kB:
					#fs_info{
					  filesystem=Fs,
					  mount_point=Mount,
					  type=get_filesystem_type( Type ),
					  used_size = 1024 * text_utils:string_to_integer( USize ),
					  available_size = 1024 *
						  text_utils:string_to_integer( ASize ),
					  used_inodes = text_utils:string_to_integer( Uinodes ),
					  available_inodes =
						  text_utils:string_to_integer( Ainodes )
				 };

				_ ->
					get_filesystem_info_alternate( FilesystemPath )

			end;

		{ _ExitCode, _ErrorOutput } ->
			get_filesystem_info_alternate( FilesystemPath )

	end.




% Alternate version, if the base version failed.
%
get_filesystem_info_alternate( FilesystemPath ) ->

	% df must have failed, probably outdated and not understanding --output,
	% defaulting to a less precise syntax:

	%io:format( "## using alternate df~n" ),

	Cmd = "/bin/df --block-size=1K --local "
		++ get_exclude_pseudo_fs_opt() ++ " "
		++ FilesystemPath ++ "| grep -v 'Mounted on'",

	case execute_command( Cmd ) of

		{ _ExitCode=0, ResAsOneString } ->

			case text_utils:split( ResAsOneString, " " ) of

				[ Fs, _1KBlocks,  USize, ASize, _UsedPercent, Mount ] ->

					% df outputs kiB, not kB:
					#fs_info{
					  filesystem=Fs,
					  mount_point=Mount,
					  type=unknown,
					  used_size = 1024 * text_utils:string_to_integer( USize ),
					  available_size = 1024 *
						  text_utils:string_to_integer( ASize ),
					  used_inodes = 0,
					  available_inodes = 0
					};

				_ ->
					throw( { filesystem_inquiry_failed, FilesystemPath,
							 ResAsOneString } )

			end;

		{ _ExitCode, ErrorOutput } ->
			throw( { filesystem_inquiry_failed, FilesystemPath,
					 ErrorOutput } )

	end.



% Returns a textual description of the specified filesystem information.
%
-spec filesystem_info_to_string( fs_info() ) -> text_utils:ustring().
filesystem_info_to_string( #fs_info{ filesystem=Fs, mount_point=Mount,
									 type=Type,
									 used_size=USize, available_size=ASize,
									 used_inodes=Uinodes,
									 available_inodes=Ainodes } ) ->

	% For example vfat does not have inodes:
	InodeString = case Uinodes + Ainodes of

					  0 ->
						  "";

					  S ->
						  Percent = 100 * Uinodes / S,
						  text_utils:format( ", hence used at ~.1f%",
											 [ Percent ] )

	end,

	text_utils:format( "filesystem ~s mounted on ~s (type: ~s). "
					   "Used size: ~B bytes (i.e. ~s), available size: "
					   "~B bytes (i.e. ~s) hence used at ~.1f% "
					   "(total size: ~s), "
					   "using ~B inodes and having ~B of them available~s",
					   [ Fs, Mount, Type, USize,
						 interpret_byte_size_with_unit( USize ), ASize,
						 interpret_byte_size_with_unit( ASize ),
						 100 * USize / ( USize + ASize ),
						 interpret_byte_size_with_unit( USize + ASize ),
						 Uinodes, Ainodes, InodeString ] ).



-spec get_filesystem_type( text_utils:ustring() ) -> filesystem_type().
get_filesystem_type( TypeString ) ->
	% Better for now than relying on an uncomplete list:
	text_utils:string_to_atom( TypeString ).




% Returns a string describing the current operating system.
%
-spec get_operating_system_description() -> string().
get_operating_system_description() ->

	OSfile = "/etc/os-release",

	case file_utils:is_existing_file_or_link( OSfile ) of

		true ->

			case execute_command( "cat " ++ OSfile ++
					" | grep PRETTY_NAME | sed 's|^PRETTY_NAME=\"||1' "
					" | sed 's|\"$||1' 2>/dev/null" ) of

				{ _ExitCode=0, Output } ->
					Output;

				{ _ExitCode, _ErrorOutput } ->
					get_operating_system_description_alternate()

			end;

		false ->
			get_operating_system_description_alternate()

	end.


get_operating_system_description_alternate() ->

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

	% We use ~ts instead of ~s as in some cases, Unicode strings might be
	% returned:
	%
	Subjects = [

		io_lib:format( "number of cores: ~B", [ get_core_count() ] ),

		io_lib:format( "size of a VM word: ~B bytes",
					  [ get_size_of_vm_word() ] ),

		io_lib:format( "operating system: ~ts",
					  [ get_operating_system_description() ] ),

		io_lib:format( "number of existing Erlang processes: ~B ",
					  [ get_process_count() ] ),

		io_lib:format( "total physical memory: ~ts",
					  [ interpret_byte_size( get_total_physical_memory() ) ] ),

		io_lib:format( "memory used by VM: ~s over a total of ~s (~s)",
					  [ interpret_byte_size( UsedRAM ),
						interpret_byte_size( TotalRAM ),
						text_utils:percent_to_string( UsedRAM / TotalRAM ) ] ),

		SwapInfo,

		io_lib:format( "user name: ~ts", [ get_user_name() ] ),

		io_lib:format( "user home directory: ~ts",
					  [ get_user_home_directory() ] ),

		io_lib:format( "current directory: ~ts",
					  [ file_utils:get_current_directory() ] ),

		io_lib:format( "current disk usage:~n~ts", [ get_disk_usage() ] )

				],

	text_utils:strings_to_string( Subjects ).
