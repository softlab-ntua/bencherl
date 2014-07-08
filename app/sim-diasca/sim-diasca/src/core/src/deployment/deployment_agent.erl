% Copyright (C) 2008-2014 EDF R&D

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



% Agent to be sent, thanks to deployment workers, on all computing nodes, so
% that it can deploy automatically everything which is needed in order to run a
% simulation.
%
% Not using WOOPER here, in order to avoid needing extra dependencies and
% environment during this bootstrap phase.
%
-module(deployment_agent).


-export([ deploy/3 ]).


-export([ send_trace/2, send_trace_fmt/3 ]).



% Implementation notes:
%
% Includes are not a problem (they are seen at build time as any other module);
% however making use of other modules is a problem, as this pioneer module
% should be as self-contained as reasonably possible.
%
% Thus some functions defined in other modules had to be duplicated verbatim
% from other base modules (ex: system_utils), to avoid having to rely on too
% many prerequisite modules (pioneer must be lean and mean).



% For host_static_info record:
-include("system_utils.hrl").


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


% For TracingActivated:
-include("class_TraceEmitter.hrl").


% For the file_info record:
-include_lib("kernel/include/file.hrl").



% Performs the actual deployment; triggered by a rpc:cast/4 called by the
% associated computing host manager.
%
% io:format print-outs will end up in the user console.
%
% ComputerHostManagerPid is sent, as it is the main user-side interlocutor for
% remote deployment agents.
%
-spec deploy( pid(), unit_utils:seconds(), text_utils:bin_string() ) ->
					'onDatabaseStarted' | 'onDatabaseStopped'.
deploy( ComputerHostManagerPid, InterNodeTickTimeOut, BinDeployBaseDir ) ->

	% All nodes must behave the same:
	change_initiated = net_kernel:set_net_ticktime( InterNodeTickTimeOut ),

	TraceAggregatorPid = wait_for_global_registration_of(
		?trace_aggregator_name ),

	send_trace_fmt( TraceAggregatorPid,
		"Deployment agent running on node ~p, "
		"with version ~s of the virtual machine, "
		"requesting the simulation package from ~w. "
		"Current scheduler count: ~B.",
		[ node(), get_interpreter_version(), ComputerHostManagerPid,
		  erlang:system_info( schedulers ) ] ),

	ComputerHostManagerPid ! { requestPackage, node(), self() },

	% Prepare some operations in the meantime:
	{ DeployBaseDir, DeployBeamDir } = prepare_package( BinDeployBaseDir,
													   TraceAggregatorPid ),

	receive

		{ wooper_result, deploy_time_out } ->
			terminate();

		{ wooper_result, send_starting } ->

			send_trace( TraceAggregatorPid,
					"Receiving of the simulation package started." ),

			% The current directory is already correct:
			PackageFilename = receive_file( ComputerHostManagerPid ),

			send_trace( TraceAggregatorPid,
						"Simulation package fully received." ),

			PackageBin = file_utils:read_whole( PackageFilename ),

			manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
						   TraceAggregatorPid ),

			{ _UsedSwap, TotalSwap } = get_swap_status(),

			HostInfo = #host_static_info{

			  total_ram = get_total_physical_memory(),
			  total_swap = TotalSwap,
			  core_count = get_core_count(),
			  erlang_version = get_interpreter_version()

			 },

			ComputerHostManagerPid ! { onDeploymentReady, HostInfo },

			% Kept running, as could be useful later:
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
					BinDeployBaseDir )

	end.



% Prepares to receive the deployment package.
%
-spec prepare_package( text_utils:bin_string(), pid() ) ->
		{ file_utils:directory_name(), file_utils:directory_name() }.
prepare_package( BinDeployBaseDir, TraceAggregatorPid ) ->

	DeployBaseDir = binary_to_list( BinDeployBaseDir ),

	case file_utils:exists( DeployBaseDir ) of

		true ->

			send_trace_fmt( TraceAggregatorPid,
				"Deployment directory '~s' already existing "
				"as a filesystem element, removing it fully first.",
				[ DeployBaseDir ] ),

			Command = "/bin/rm -rf " ++ DeployBaseDir,

			[] = os:cmd( Command );


		false ->

			send_trace_fmt( TraceAggregatorPid,
				"Deployment directory '~s' not already existing, "
				"will create it.", [ DeployBaseDir ] )

	end,

	DeployBeamDir = filename:join( DeployBaseDir, "deployed-elements" ),

	file_utils:create_directory( DeployBeamDir, create_parents ),

	file_utils:set_current_directory( DeployBeamDir ),

	{ DeployBaseDir, DeployBeamDir }.



% Manages the received deployment package.
%
-spec manage_package( binary(), file_utils:directory_name(),
		 file_utils:directory_name(), pid() ) -> basic_utils:void().
manage_package( PackageBin, DeployBaseDir, DeployBeamDir,
			   TraceAggregatorPid ) ->

	send_trace_fmt( TraceAggregatorPid,
		"Received simulation package, whose size is ~B bytes, "
		"will extract it in deployment directory '~s'.~n",
		[ size( PackageBin ), DeployBeamDir ] ),

	FileNames = file_utils:zipped_term_to_unzipped_files( PackageBin ),

	send_trace_fmt( TraceAggregatorPid,
		"Following ~B files were extracted in '~s':~n~p.~n",
		[ length( FileNames ), DeployBeamDir, lists:sort( FileNames ) ] ),

	% Now updating the code path according to the layout of the deployed tree:

	% Some directories not containing BEAMs could be removed:
	BeamDirs = file_utils:find_directories_from( "." ),

	% Dealing with absolute directories is easier to debug:
	AbsoluteBeamDirs = [ filename:join( DeployBeamDir, D ) || D <-BeamDirs ],

	%io:format( "Added BEAM dirs: ~p.~n", [ AbsoluteBeamDirs ] ),

	ok = code:add_paths( AbsoluteBeamDirs ),

	send_trace_fmt( TraceAggregatorPid,
		"Following BEAM directories were added to code path:~n~p.~n",
		[ BeamDirs ] ),

	%io:format( "Updated code path:~n~p.~n", [ code:get_path() ] ),

	OutputDir = filename:join( DeployBaseDir, "outputs" ),

	file_utils:create_directory( OutputDir, create_parents ),

	file_utils:set_current_directory( OutputDir ).



% Final loop of this deploy agent.
%
final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
				BinDeployBaseDir ) ->

	receive

		{ start_database, CallerPid } ->

			% The mnesia directory must already have been set here (no
			% application:set_env( mnesia, dir, .. ) taken into account here).

			send_trace_fmt( TraceAggregatorPid,
						   "Deployment agent starting database on node ~s.",
						   [ node() ] ),

			%io:format( "Deployment agent starting database on node ~s.~n",
			%		  [ node() ] ),

			% No prior loading accepted:

			%%case application:load(mnesia) of

			%%	ok ->
			%%		ok;

			%%	LoadError ->
			%%		throw( { mnesia_load_failed, node(), LoadError } )

			%%end,

			case application:start( mnesia ) of

				ok ->
					ok;

				StartError ->
					throw( { mnesia_start_failed, node(), StartError } )

			end,

			%io:format( "Database started on node ~s.~n", [ node() ] ),

			CallerPid ! onDatabaseStarted,

			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir );


		{ stop_database, CallerPid } ->

			%io:format( "~w stopping database.~n", [ self() ] ),

			ok = application:stop( mnesia ),
			ok = application:unload( mnesia ),

			%io:format( "~w stopped database.~n", [ self() ] ),

			CallerPid ! onDatabaseStopped,

			% We must recurse, as we still want to properly terminate, otherwise
			% there would be lingering computing nodes:
			%
			final_main_loop( TraceAggregatorPid, ComputerHostManagerPid,
							 BinDeployBaseDir );


		terminate ->

			send_trace_fmt( TraceAggregatorPid,
					"Removing deployment directory '~s' and terminating now.",
					[ BinDeployBaseDir ] ),

			RemoveCommand = "/bin/rm -rf "
				++ binary_to_list( BinDeployBaseDir ),

			os:cmd( RemoveCommand ),

			terminate()

	end.



-spec terminate() -> no_return().
terminate() ->

	% We do not want nodes to wait any longer, otherwise old code of modules
	% could linger:
	%
	%io:format( "~n(deployment agent ~p terminating immediately)~n",
	%		   [ self() ] ),
	%timer:sleep( 1000 ),

	% Remote shutdown directly done by computing host manager:
	halt( 0 ).




% Duplication section: the deployment_agent is the only one which is to be run
% standalone (pioneer module with no prerequisite).



-ifdef(TracingActivated).


% Duplicated verbatim from class_TraceEmitter.erl:

% Returns the current time and date as a string, with correct format.
%
% Example: "14/04/2008 04:41:24".
%
current_time_to_string() ->
	{ { Year, Month, Day }, { Hour, Minute, Second } } = erlang:localtime(),
	lists:flatten( io_lib:format( "~B/~B/~B ~B:~B:~B",
		[ Day, Month, Year, Hour, Minute, Second ] ) ).



-spec send_trace( pid(), string() ) -> basic_utils:void().
send_trace( TraceAggregatorPid, Message ) ->

	% We keep only the hostname, not the FQDN, otherwise the (last) dot in the
	% name would be interpreted as subcategory in the traces:
	TraceAggregatorPid ! { send,
		[ self(), "Deployment agent on "
			++ hd( string:tokens( net_adm:localhost(), "." ) ),
			"Core.Deployment", _Tick=undefined, current_time_to_string(),
			node(), "Standalone.Deployment", _InfoPriority=4, Message ] }.

-spec send_trace_fmt( pid(), text_utils:format_string(), [ any() ] ) ->
							basic_utils:void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues ) ->

	% We keep only the hostname, not the FQDN, otherwise the (last) dot in the
	% name would be interpreted as subcategory in the traces:
	TraceAggregatorPid ! { send,
		[ self(), "Deployment agent on "
			++ hd( string:tokens( net_adm:localhost(), "." ) ),
			"Core.Deployment", _Tick=undefined, current_time_to_string(),
			node(), "Standalone.Deployment", _InfoPriority=4,
			io_lib:format( MessageFormat, FormatValues )
		 ] }.

-else.


% Avoids warnings:

deploy_trace_disabled( _, _ ) ->
	deploy_trace_disabled.


deploy_trace_disabled( _, _, _ ) ->
	deploy_trace_disabled.


-spec send_trace( pid(), string() ) -> basic_utils:void().
send_trace( TraceAggregatorPid, Message ) ->
	deploy_trace_disabled( TraceAggregatorPid, Message ).


-spec send_trace_fmt( pid(), string(), text_utils:format_string() ) ->
							basic_utils:void().
send_trace_fmt( TraceAggregatorPid, MessageFormat, FormatValues ) ->
	deploy_trace_disabled( TraceAggregatorPid, MessageFormat, FormatValues ).


-endif.





% Duplication section.

% As the initial deployment agent must be standalone, it is the only one which
% cannot rely on the helper modules, and thus have to duplicate some of their
% functions here, although it is definitively a bad practise.

% Note that the file_utils module will be already pre-deployed, and thus no code
% from it should be duplicated here.




% Duplicated verbatim from basic_utils.erl:



% Waits (up to 10 seconds) until specified name is globally registered.
%
% Returns the resolved PID, or throws
% { global_registration_waiting_timeout, Name }.
%
-spec wait_for_global_registration_of( basic_utils:registration_name() ) ->
											 pid().
wait_for_global_registration_of( Name ) ->
	wait_for_global_registration_of( Name, _Seconds=10 ).


wait_for_global_registration_of( Name, _Seconds=0 ) ->
	throw( { global_registration_waiting_timeout, Name } );

wait_for_global_registration_of( Name, SecondsToWait ) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep( 1000 ),
			wait_for_global_registration_of( Name, SecondsToWait - 1 );

		Pid ->
			Pid

	end.




% Duplicated verbatim from system_utils.erl:



% Returns the version informations of the current Erlang interpreter
% being used.
%
% Returns a full version name (ex: "R13B04") or, if not available, a shorter one
% (ex: "R11B").
%
-spec get_interpreter_version() -> string().
get_interpreter_version() ->

	% Older versions (pre-R13A?) did not support the otp_release tag:
	try erlang:system_info(otp_release) of

		V ->
			% Ex: V="R13B04"
			V

	catch

		_:_ ->
			% Here we revert to another (older) solution:
			{ _OTPInfos, V } = init:script_id(),
			% Ex: "R11B"
			V

	end.



% Returns the total installed physical volatile memory (RAM) of the local
% computer, expressed in bytes.
%
-spec get_total_physical_memory() -> system_utils:byte_size().
get_total_physical_memory() ->

	% First check the expected unit is returned, by pattern-matching:
	UnitCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $3}'",

	"kB\n" = os:cmd( UnitCommand ),

	ValueCommand = "cat /proc/meminfo | grep 'MemTotal:' | awk '{print $2}'",

	% The returned value of following command is like "12345\n", in bytes:
	MemorySizeString = remove_ending_carriage_return( os:cmd( ValueCommand ) ),

	% They were kB (not kiB):
	list_to_integer( MemorySizeString ) * 1000.



% Returns the number of cores available on the local host.
%
% Throws an exception on failure.
%
-spec get_core_count() -> integer().
get_core_count() ->

	String = remove_ending_carriage_return(
				os:cmd( "cat /proc/cpuinfo | grep -c processor" ) ),

	try

		list_to_integer( String )

	catch

		{ integer_conversion_failed, String } ->
			throw( { could_not_determine_core_count, String } )

	end.



% Returns { UsedSwap, TotalSwap } where UsedSwap is the size of the used swap
% and TotalSwap is the total amount of swap space on the local host, both
% expressed in bytes.
%
-spec get_swap_status() -> { system_utils:byte_size(),
							system_utils:byte_size() }.
get_swap_status() ->

	SwapInfos = os:cmd( "free -b | grep 'Swap:' | awk '{print $2, $3}'" ),

	[ TotalSwapString, UsedSwapWith ] = string:tokens( SwapInfos, " " ),

	UsedSwapString = remove_ending_carriage_return( UsedSwapWith ),

	TotalSwap = list_to_integer( TotalSwapString ),
	UsedSwap = list_to_integer( UsedSwapString ),

	{ UsedSwap, TotalSwap }.




% Duplicated verbatim from text_utils.erl:


% Converts a binary into a plain (list-based) string.
%
-spec binary_to_string( binary() ) -> string().
binary_to_string( Binary ) ->
	erlang:binary_to_list( Binary ).


% Removes the ending "\n" character(s) of specified string.
%
-spec remove_ending_carriage_return( string() ) -> string().
remove_ending_carriage_return( String ) when is_list(String) ->

	% See also: list_utils:remove_last_element/1.

	% 'Res ++ "\n" = String,Res' will not work:
	string:strip( String, right, $\n ).





% Duplicated verbatim from net_utils.erl:

-type ip_v4_address() :: { byte(), byte(), byte(), byte() }.



% Net-related transfers.
%
% They are not through a dedicated TCP/IP socket pair, using sendfile.
%
% For proper operation, a sufficient number of async threads should be
% available.
%
% The recipient acts as a server, while the emitter acts as a client.
%
% The sender is to use send_file/2 while the recipient is to use one of the
% receive_file/{1,2,3}. As they synchronize through messages, no specific order
% of these two calls matters (the first will wait for the second).

-type net_port() :: non_neg_integer().
-type tcp_port() :: net_port().



% We use an ephemeral port number by default:
-define( default_send_file_port, 0 ).



% Returns a list of the potentially usable non-local network interfaces on this
% host, trying to put in first position the "main" one, if any.
%
% Note: IPv6 support should be added.
%
-spec get_local_ip_addresses() -> [ ip_v4_address() ].
get_local_ip_addresses() ->

	IfList = case inet:getifaddrs() of

				 { ok, List } ->
					 List;

				 { error, Reason } ->
					 throw( { local_ip_look_up_failed, Reason } )

	end,

	%io:format( "Interface list:~n~p~n", [ IfList ] ),

	% Rules: put non-routable (network-local) interfaces last (including
	% loopback, i.e. "lo", which must be the very last one), try to put routable
	% "ethX"-like interfaces first, virtual interfaces (ex: "vmnetX")
	% last. Keeps only the actual address (addr).

	% More convenient than a queue:
	filter_interfaces( IfList, _FirstIfs=[], _LastIfs=[], _Loopback=undefined ).


% (helper)
filter_interfaces( _IfList=[], FirstIfs, LastIfs, _Loopback=undefined ) ->
	% No loopback here; quite surprising:
	filter_routable_first( FirstIfs ) ++ filter_routable_first( LastIfs );

filter_interfaces( _IfList=[], FirstIfs, LastIfs, Loopback ) ->
	% We need loopback never to take precedence over any other interface:
	filter_routable_first( FirstIfs ) ++ filter_routable_first( LastIfs )
		++ [ Loopback ];

filter_interfaces( _IfList=[ _If={ Name, Options } | T ], FirstIfs, LastIfs,
				   Loopback ) ->

	%io:format( "Examining interface named '~p', with options ~p.~n",
	%		   [ Name, Options ] ),

	case proplists:get_value( _K=addr, Options ) of

		% Ex: wlan0 might not have a configured address if down:
		undefined ->
			filter_interfaces( T, FirstIfs, LastIfs, Loopback );

		Address ->

			case Name of

				% Assuming up to one loopback, replacing any previous one:
				"lo" ->
					filter_interfaces( T, FirstIfs, LastIfs, Address );

				% For example, eth1:
				[ "eth" | _ ] ->
					filter_interfaces( T, [ Address | FirstIfs ], LastIfs,
									   Loopback );

				% For example, enp0s25:
				[ "enp" | _ ] ->
					filter_interfaces( T, [ Address | FirstIfs ], LastIfs,
									   Loopback );

				% Ex: vmnetX, etc.
				_ ->
					filter_interfaces( T, FirstIfs, [ Address | LastIfs ],
									   Loopback )

			end

	end.



% (helper)
filter_routable_first( IfList ) ->
	filter_routable_first( IfList, _RoutableAddrs=[], _NonRoutableAddrs=[] ).


filter_routable_first( _IfList=[], RoutableAddrs, NonRoutableAddrs ) ->
	RoutableAddrs ++ NonRoutableAddrs;

filter_routable_first( _IfList= [ If | T ], RoutableAddrs, NonRoutableAddrs ) ->

	case is_routable( If ) of

		true ->
			filter_routable_first( T, [ If | RoutableAddrs ],
								  NonRoutableAddrs );

		false ->
			filter_routable_first( T, RoutableAddrs, [ If | NonRoutableAddrs ] )

	end.



% Returns the "main" potentially usable non-local network interface on this
% host.
%
-spec get_local_ip_address() -> ip_v4_address().
get_local_ip_address() ->

	case get_local_ip_addresses() of

		[] ->
			throw( no_local_ip_address_established );

		[ Addr | _T ] ->
				Addr

	end.



% Address-related functions.


% Tells whether the specified IPv4 address is routable.
%
% Note: the loopback ({127,0,0,1}, or {0,0,0,0,0,0,0,1}) is deemed routable.
%
-spec is_routable( ip_v4_address() ) -> boolean().
is_routable( { 10, _, _, _ } ) ->
	false;

is_routable( { 172, N, _, _ } ) when N >= 16 andalso N < 32 ->
	false;

is_routable( { 192, 168, _, _ } ) ->
	false;

is_routable( _ ) ->
	true.



% Receives specified file out of band (through a dedicated TCP socket, not
% thanks to Erlang messages), the emitter being supposed to use send_file/2.
%
% The file will be written in current directory, and the default TCP port will
% be used.
%
% Returns the full path to the received file.
%
-spec receive_file( pid() ) -> file_utils:file_name().
receive_file( EmitterPid ) ->
	receive_file( EmitterPid, file_utils:get_current_directory() ).



% Receives specified file out of band (through a dedicated TCP socket, not
% thanks to Erlang messages) into specified pre-existing directory, the emitter
% being supposed to use send_file/2.
%
% The default TCP port will be used.
%
% Returns the full path to the received file.
%
-spec receive_file( pid(), file_utils:directory_name() ) -> basic_utils:void().
receive_file( EmitterPid, TargetDir ) ->
	receive_file( EmitterPid, TargetDir, ?default_send_file_port ).



% Receives specified file out of band (through a dedicated TCP socket, not
% thanks to Erlang messages) into specified pre-existing directory, the emitter
% being supposed to use send_file/2.
%
% The default TCP port will be used.
%
-spec receive_file( pid(), file_utils:directory_name(), tcp_port() ) ->
						  file_utils:file_name().
receive_file( EmitterPid, TargetDir, Port ) ->

	% We prefer relying on IP addresses rather than hostnames, as a surprisingly
	% high number of systems have no usable DNS service:
	%
	% BinHostname = text_utils:string_to_binary( localhost() ),
	LocalIP = get_local_ip_address(),

	receive

		{ sendFile, [ BinFilename, Permissions, EmitterPid ] } ->

			case gen_tcp:listen( Port, [ binary, { active, false },
										{ packet,0 } ] ) of

				{ ok, ListenSock } ->

					% An ephemeral port (0) may have been specified:
					{ ok, ActualPort } = inet:port( ListenSock ),

					EmitterPid ! { sendFileAcknowledged,
								  [ BinFilename, LocalIP, ActualPort ] },

					Filename = file_utils:join( TargetDir,
								 binary_to_string( BinFilename ) ),

					%io:format( "Writing received file in '~s'.~n",
					%		  [ Filename ] ),

					% Do not know the units for { delayed_write, Size, Delay }:
					OutputFile = file_utils:open( Filename, [ write, raw,
						binary, delayed_write ] ),

					% Mono-client, yet using a separate socket for actual
					% sending:
					%
					case gen_tcp:accept( ListenSock ) of

						{ ok, DataSocket } ->

							receive_file_chunk( DataSocket, OutputFile ),
							ok = gen_tcp:close( ListenSock );

						Other ->
							throw( { accept_failed, Other } )

					end,

					case file:write_file_info( Filename,
								 #file_info{ mode=Permissions } ) of

						ok ->
							Filename;

						{ error, WriteInfoReason } ->
							throw( { write_file_info_failed, WriteInfoReason } )

					end;

				{ error, Reason } ->
					throw( { listen_failed, Reason } )

			end

	end.



% Reads next chunk of transferred file.
%
% (helper)
%
receive_file_chunk( DataSocket, OutputFile ) ->

	inet:setopts( DataSocket, [ { active, once } ] ),

	receive

		{ tcp, DataSocket, Data } ->
			%io:format( "Received chunk of ~B elements.~n", [ size( Data ) ] ),
			file_utils:write( OutputFile, Data ),
			receive_file_chunk( DataSocket, OutputFile );

		{ tcp_closed, DataSocket } ->
			%io:format( "Connection closed.~n" ),
			ok = gen_tcp:close( DataSocket ),
			file_utils:close( OutputFile )

	end.
