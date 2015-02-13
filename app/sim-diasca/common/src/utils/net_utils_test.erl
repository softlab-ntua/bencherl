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



% Unit tests for the net_utils toolbox.
%
% See the net_utils.erl tested module.
%
-module(net_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Localhost = net_utils:localhost(),

	test_facilities:display( "Pinging now localhost, whose FQDN is '~s' "
							 "(short name: '~s').",
							 [ Localhost, net_utils:localhost( short ) ] ),


	case net_utils:ping( Localhost ) of

		true ->
			test_facilities:display( "Ping success of localhost.");

		false ->
			% Deactivated as a laptop using DHCP may not be able to resolve its
			% own name:
			% throw( could_not_ping_localhost )
			test_facilities:display( "Warning: the local host is not able to "
									"ping itself.")

	end,

	test_facilities:display( "(will ping a non-existing host, "
		"depending on the DNS settings the operation might be quite long)" ),

	case net_utils:ping( "non.existing.hostname" ) of

		true ->
			throw( could_ping_non_existing_host );

		false ->
			test_facilities:display(
				"Ping could not ping a non-existing host, as expected.")

	end,

	test_facilities:display( "Detected usable network interfaces: ~p",
				[ net_utils:get_local_ip_addresses() ] ),


	test_facilities:display( "Connected nodes are: ~w.",
			  [ net_utils:get_all_connected_nodes() ] ),


	NamingMode = net_utils:get_node_naming_mode(),

	test_facilities:display( "Naming mode for this node: ~w.",
							[ NamingMode ] ),

	test_facilities:display( "Naming-compliant hostname for '~s' is '~s'.",
		[ Localhost,
		  net_utils:get_naming_compliant_hostname( Localhost, NamingMode ) ] ),


	TestName = "I have \"<spaces>\" / \ & ~ # @ { } [ ] | $ * ? ! + , . ; :"
		"(and also 'I have quotes')",

	test_facilities:display( "Node name generated from '~s' is '~s'.",
		[TestName,net_utils:generate_valid_node_name_from(TestName)] ),


	NodeName = "hello",
	NodeNamingMode = short_name,
	EpmdSettings = 754,
	TCPSettings = {10000,14000},
	AdditionalOptions = "-noshell -smp auto +K true +A 8 +P 400000",

	test_facilities:display( "Example of node launching command: '~s'.", [
		  net_utils:get_basic_node_launching_command( NodeName, NodeNamingMode,
		  EpmdSettings, TCPSettings, AdditionalOptions ) ] ),


	FirstIP = {74,125,127,100},
	test_facilities:display( "Reverse look-up of ~p is '~s'.",
		[ net_utils:ipv4_to_string(FirstIP),
		  net_utils:reverse_lookup(FirstIP) ] ),


	SecondIP = {82,225,152,215},
	test_facilities:display( "Reverse look-up of ~p is '~s'.",
		[ net_utils:ipv4_to_string(SecondIP),
		  net_utils:reverse_lookup(SecondIP) ] ),


	ThirdIP = {90,59,94,64},
	test_facilities:display( "Reverse look-up of ~p is '~s'.",
		[ net_utils:ipv4_to_string(ThirdIP),
		  net_utils:reverse_lookup(ThirdIP) ] ),

	FourthIP = {10,22,22,22},
	test_facilities:display( "Reverse look-up of ~p is '~s'.",
		[ net_utils:ipv4_to_string(FourthIP),
		  net_utils:reverse_lookup(FourthIP) ] ),


	test_facilities:display( "All connected nodes are: ~w.",
			  [ net_utils:get_all_connected_nodes() ] ),


	test_facilities:display( "Testing node availability (various forms):" ),

	FirstNonExistingNodeName = non_existing,
	%FirstNonExistingNodeName = erlang:node(),

	SecondNonExistingNodeName = "Non existing",
	%SecondNonExistingNodeName = atom_to_list( FirstNonExistingNodeName ),

	ExistingNodeName = node(),

	CandidateNodeNames = [ FirstNonExistingNodeName, SecondNonExistingNodeName,
				ExistingNodeName ],

	[ test_facilities:display( "  + direct for ~p: ~p",
			  [ N, net_utils:check_node_availability( N ) ] )
	 || N <- CandidateNodeNames ],


	[ test_facilities:display( "  + immediate for ~p: ~p",
			  [ N, net_utils:check_node_availability( N, immediate ) ] )
	 || N <- CandidateNodeNames ],


	[ test_facilities:display( "  + with waiting for ~p: ~p",
			  [ N, net_utils:check_node_availability( N, with_waiting ) ] )
	 || N <- CandidateNodeNames ],


	%Durations = [ 0, 1, 10, 100, 200, 510, 1000, 2050 ],
	Durations = [ 0, 1, 10, 100, 200 ],

	[ [ test_facilities:display( "  + with duration ~B for ~p: ~p",
			  [ D, N, net_utils:check_node_availability( N, D )
			   ] ) || N <- CandidateNodeNames ] || D <- Durations ],

	test_facilities:display( "To test send_file/2, receive_file/1, "
							 "receive_file/2 and receive_file/3, two "
							 "nodes are needed, and two shells, A and B." ),

	test_facilities:display( "On A, launched by: "
							 "'erl -name node_a -setcookie abc', enter: "
							 "'basic_utils:register_as( self(), shell_a, "
							 "global_only ).'" ),

	% Sleep needed for the synchronization of the atom table:
	test_facilities:display( "On B, launched by: "
							 "'erl -name node_b -setcookie abc', enter: "
							 "pong = net_adm:ping( 'node_a@foobar.org' ), "
							 "timer:sleep(500), "
							 "basic_utils:register_as( self(), shell_b, "
							 "global_only ), "
							 "ShellA = basic_utils:get_registered_pid_for( "
							 "shell_a, global ), "
							 "net_utils:receive_file( ShellA, \"/tmp\" ).'" ),

	test_facilities:display( "Back on A: "
							 "'ShellB = basic_utils:get_registered_pid_for( "
							 "shell_b, global ), "
							 "net_utils:send_file( \"/home/joe/test-file.txt\","
							 " ShellB ).'" ),


	test_facilities:stop().
