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


% Unit tests for the implementation of trace listening.
%
% See the following modules:
%  - class_TraceListener
%  - class_TraceSupervisor
%  - class_TraceAggregator



% The mode of operation is to execute this test while the traceManagement_test
% is running:
%
% - run on a first terminal: 'make traceManagement_run'
%
% - then run on a second terminal: 'make traceListening_run'
%
% A new trace supervisor window should appear and allow to catch up all the past
% traces.
%
% One may check that the aggregator and the listener have exactly the same
% messages (ex: at least counts must match of both interfaces).
%
-module(traceListening_test).


% For trace facilities:
-include("traces_for_tests.hrl").


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").



% Sends (as fast as possible) the specified number of traces:
send_traces( _Count=0 ) ->
	ok;

send_traces( Count ) ->
	?test_trace_fmt( "Emitting trace  #~B from listener.", [ Count ] ),
	send_traces( Count - 1 ).



% Sends the specified number of traces with some waiting between sendings:
send_timed_traces( _Count=0 ) ->
	ok;

send_timed_traces( Count ) ->

	?test_trace_fmt( "Emitting timed trace #~B from listener.", [ Count ] ),

	timer:sleep( 100 ),

	send_timed_traces( Count - 1 ).



% The real code of the test, in a separate function to avoid an indentation
% offset.
%
-spec test_actual_body() -> no_return().
test_actual_body() ->

	NodeStringName = case net_utils:localnode() of

				   local_node ->
					   throw( { node_not_networked, node() } );

				   N ->
					   text_utils:atom_to_string( N )

	end,

	% Get rid of the test module prefix, just to have the node-specific suffix
	% (generally, the hostname):
	%
	[ _TestModuleName, NodeSuffix ] = string:tokens( NodeStringName, "@" ),

	TargetNodeName = text_utils:format( "traceManagement_run-~s@~s",
						[ system_utils:get_user_name(), NodeSuffix ] ),

	test_facilities:display( "Connecting to node '~s'.", [ TargetNodeName ] ),

	case net_adm:ping( text_utils:string_to_atom( TargetNodeName ) ) of

		pong ->
			ok;

		pang ->

			test_facilities:display( "Error, the trace management test "
				"should already be running.~nFor example, execute "
				"'make traceManagement_run' in another terminal." ),

			throw( { no_trace_aggregator_to_listen, TargetNodeName } )

	end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	test_facilities:display( "Globally registered names: ~w.",
							 [ global:registered_names() ] ),

	AggregatorName = ?trace_aggregator_name,

	test_facilities:display( "Looking up aggregator by name: ~s.",
							 [ AggregatorName ] ),

	AggregatorPid = basic_utils:get_registered_pid_for( AggregatorName,
														global ),

	test_facilities:display( "Sending initial traces to force "
							 "a real synchronization." ),

	?test_info( "First trace sent from listener." ),

	send_traces( _Count=40 ),

	% No ?test_start: we want to use the aggregator from the node named
	% 'traceManagement_run'.

	test_facilities:display( "Creating a test trace local listener." ),

	MyTraceListener = class_TraceListener:synchronous_new_link( AggregatorPid ),

	send_timed_traces( _TimedCount=20 ),

	?test_info( "Last trace sent from listener." ),

	% Could wait here for any event before stopping.

	test_facilities:display( "Deleting this test trace listener." ),

	% In a real test, we do not wait for traceManagement_run to finish until
	% launching the listener; however we do not want either that this listener
	% terminates *before* traceManagement_run terminates, otherwise of course
	% the listener trace file will this time have *more* entries that the
	% aggregator one:
	%
	%timer:sleep( 5000 ),

	% We want the listener to have enough time to properly write its traces
	% before shutdown:
	%
	wooper:delete_synchronously_instance( MyTraceListener ),

	% ?test_stop should not be used here as its wait_for_any_trace_supervisor
	% macro would wait for a non-launched supervisor.
	%
	% ?test_stop_without_waiting_for_trace_supervisor() is neither used, as no
	% aggregator was started from that test:
	%
	test_facilities:finished().



% Runs the test.
%
-spec run() -> no_return().
run() ->

	% No test_start here.

	test_facilities:display( "Testing module ~w. 'make traceManagement_run' "
							 "supposed to be already executed.", [ ?MODULE ] ),

	case executable_utils:is_batch() of

		true ->

			test_facilities:display(
				"Running in batch mode, no traceManagement_test "
				"supposed to be running, nothing done." ),

			% Nothing was started here:
			test_facilities:finished();

		false ->

			test_facilities:display( "Running in interactive mode, "
				"'make traceManagement_run' supposed to be already running." ),

			test_actual_body()

	end.
