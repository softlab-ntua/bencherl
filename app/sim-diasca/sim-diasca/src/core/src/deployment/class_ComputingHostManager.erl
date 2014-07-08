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




% Manager of a computing host, notably for deployment purpose.
%
% All computing host managers run on the user node.


% Such a manager will in turn, for the computing host it is in charge of:
%
% - set-up the host: check liveliness (with ping), perform a node-cleanup if
% requested, then launch a dedicated Erlang node
%
% - deploy the simulation on that host: send the pioneer modules, run the
% deployment agent, provide the resources it requests, ensure it reports its
% deployment is ready
%
% - report to the deployment manager that this host is finally ready to take
% part to the simulation
%
-module(class_ComputingHostManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, HostnameOptions, NodeOption,
		NetworkOptions, DeployOptions ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, delete/1 ).


% Member method declarations.
-define( wooper_method_export, setUpHost/1, requestPackage/2,
		 onDeploymentReady/2, startDatabase/2, stopDatabase/2 ).


% Static methods:
-define( wooper_static_method_export, get_fully_qualified_node_name/3,
		 get_host_deployment_duration_upper_bound/0 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Deployment.ComputingHostManager").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes.


% If connecting from X@a to Y@b, the connection may fail or wait for a user
% input (ex: modal window popped through SSH ask pass, if passwordless
% authentication failed) and thus stay stuck in os:cmd/1, indefinitively or long
% enough to be rejected (no way of stopping this command by message or
% time-out).

% As a series of potentially lenghty blocking operations are performed here, we
% try not to communicate with the deployment manager once we are already past
% the deployment time-out, as we do not want to interact with it whereas there
% is a 'delete' message already waiting for this manager to be read: the
% deployment manager would then not expect to receive such messages, having gone
% through the next steps.

% To start a remote node (via SSH), instead of using os:cmd/1, a slave node
% could be used, see:
% support.process-one.net/doc/display/ERL/Starting+a+set+of+Erlang+cluster+nodes
%
% However it seems to work great currently with os:cmd/1, and using a slave node
% seems to imply some consequences that may not be wanted, as discussed in:
% http://www.erlang.org/doc/man/slave.html. For example:
%
% - all TTY output produced at the slave will be sent back to the master node
%
% - file I/O is done via the master
%
% - the slave node should use the same file system at the master; at least,
% Erlang/OTP should be installed in the same place on both computers and the
% same version of Erlang should be used


% A problem with code_loader may happen due to the boot sequence of an Erlang
% node. Basically, net_adm:ping may reply positively *before* the code server
% can accept requests.




% Constructs a new manager for a given computing host, from following
% parameters:
%
% - HostnameOptions={ Hostname, Username }, Hostname being either the name (as a
% plain string) of the remote computing host to manage, or the 'localhost' atom,
% Username being the name of the user to rely on for that host
%
% - NodeOptions={ NodeBaseName, NodeNamingNode, NodeCleanupWanted, NodeCookie,
%     NodeSchedulerCount }, a tuple made of:
%
%  - NodeName, the base node name (a plain string, ex: "my_node"), without any
%   host name
%
%   - NodeNamingNode, a node naming mode (i.e. short or long names)
%
%   - NodeCleanupWanted, which tells whether an initial clean-up of any
%   previously existing node with that name is wanted: it is either false, or
%   the full path of the clean-up script to be used, as a binary (note: this
%   path must have been already validated once for all by the caller, it is
%   considered here as reliable)
%
%   - NodeCookie, the cookie that must be used to launch any new node
%
%   - NodeSchedulerCount :: 'undefined' | basic_utils:count() the number of
%   schedulers to create on the associated node
%
% - NetworkOptions={ EpmdPort, TCPPortRestriction }, a pair made of:
%
%   - EpmdPort is the EPMD port specification, with can be either the
%   'undefined' atom or the port number; note that if a non-default EPMD port is
%   specified for a new node, this implies that the current node usually has to
%   itself respect the same non-standard convention (ex: see the FIREWALL_OPT
%   make option in common/GNUmakevars.inc), otherwise available nodes will not
%   be found
%
%   - TCPPortRestriction is the TCP port restriction, with can be either the
%   'no_restriction' atom or a pair of integers {MinTCPPort,MaxTCPPort}; note
%   that if using a specific TCP/IP port range for a new node, the current node
%   may have to respect this constraint as well (see the FIREWALL_OPT make
%   option in common/GNUmakevars.inc), otherwise inter-node communication could
%   fail
%
% - DeployOptions is a { DeploymentManagerPid, DeployTimeOut,
% InterNodeTickTimeOut } triplet, where:
%
%   - DeploymentManagerPid: the PID of the deployment manager, which created
%   this manager, in order to be able to interact with it later
%
%   - DeployTimeOut is the maximum number of milliseconds which will be left to
%   this host to be deployed
%
%   - InterNodeTickTimeOut is the time-out for inter-node ticks, to be
%   transmitted to the deployment agent later
%
-spec construct( wooper_state(),

		{ 'localhost' | net_utils:string_host_name(), basic_utils:user_name() },

		{ net_utils:atom_node_name(), net_utils:node_naming_mode(),
		  'false' | file_utils:bin_path(), net_utils:cookie() },

		{ 'undefined' | net_utils:tcp_port(),
		  'no_restriction' | net_utils:tcp_port_range() },

		{ pid(), unit_utils:milliseconds(), unit_utils:milliseconds(),
		 text_utils:bin_string() } )

				-> wooper_state().
construct( State,

		   _HostnameOptions={ Hostname, Username },

		   _NodeOptions={ NodeName, NodeNamingNode, NodeCleanupWanted,
					   NodeCookie, NodeSchedulerCount },

		   _NetworkOptions={ EpmdPort, TCPPortRestriction },

		   _DeployOptions={ DeploymentManagerPid, DeployTimeOut,
						 InterNodeTickTimeOut, BinDeployBaseDir } ) ->

	{ MessageHostname, ActualHostname } = case Hostname of

					localhost ->
							{ "the user host", net_utils:localhost() };

					_ ->
							{ Hostname, Hostname }

	end,

	% First the direct mother classes:
	%
	% (we replace dots by semi-colons, otherwise LogMX would create branches in
	% the message tree)
	%
	NonDottedMessageHostname = re:replace( _Subject=MessageHostname,
		_RegExp="\\.", _Replacement=":", _Opts=[ {return,list}, global ] ),

	TraceState = class_TraceEmitter:construct( State,
		"Host manager for " ++ NonDottedMessageHostname ++ "" ),

	FullyQualifiedNodeName = get_fully_qualified_node_name( NodeName,
									ActualHostname, NodeNamingNode ),

	StartingState = setAttributes( TraceState, [

		% Either the name (as a plain string) of the remote computing host to
		% manage, or the 'localhost' atom:
		{ managed_host, Hostname },

		% The name of the user the with which we should connect to that node:
		{ user_name, Username },

		% Node name (a plain string). This is just the node name, ex: "my_node",
		% i.e. it is not fully-qualified.
		{ node_name, text_utils:atom_to_string( NodeName ) },

		% Fully-qualified node name (stored as an atom), ex: 'my_node@foo.org',
		% to be used by other nodes, to target the corresponding node.
		{ full_node_name, FullyQualifiedNodeName },

		% Node naming mode (i.e. short or long names):
		{ node_naming_mode, NodeNamingNode },

		% Tells whether an initial clean-up of any previously existing node with
		% that name is wanted (either false or the full path of the clean-up
		% script to be used, as a binary):
		{ node_cleanup, NodeCleanupWanted },

		% The cookie that must be used to launch any new node:
		{ node_cookie, NodeCookie },

		% The number of schedulers to be used to launch any new node:
		{ scheduler_count, NodeSchedulerCount },

		% The EPMD port specification, either the 'undefined' atom or the port
		% number:
		{ epmd_port, EpmdPort },

		% The TCP port restriction, either the 'no_restriction' atom or a pair
		% of integers { MinTCPPort, MaxTCPPort }
		{ tcp_port_range, TCPPortRestriction },

		% The PID of the deployment manager:
		{ deployment_manager_pid, DeploymentManagerPid },

		% The PID of the deployment agent:
		{ deployment_agent_pid, undefined },

		% The name of the deployed node, as an atom:
		{ deployed_node, undefined },

		% Time-out, in milliseconds, for this host to be fully deployed:
		{ deploy_time_out, DeployTimeOut },

		% The deployment base directory for all computing nodes (as a binary):
		{ deploy_base_dir, BinDeployBaseDir },

		% Start time, so that we can give up if a blocking operation made us
		% wait past the deployment time-out:
		{ start_time, basic_utils:get_timestamp() },

		% Time-out, in milliseconds, to configure the deployed node:
		{ tick_time_out, InterNodeTickTimeOut },

		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

												] ),

	?send_info_fmt( StartingState,
				   "Creating a new manager for computing host '~s', "
				   "with node name '~s'.", [ Hostname, NodeName ] ),

	% Direct asynchronous auto-activation:
	self() ! setUpHost,

	StartingState.




% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	%io:format( "Deleting computing host manager ~w.~n", [ self() ] ),

	% Class-specific actions:
	?trace( "Deleting computing-host manager." ),

	% Halts corresponding node (safe remote shutdown):
	case ?getAttr(deployment_agent_pid) of

		undefined ->
			ok;

		Pid ->
			%io:format( "Requesting deployment agent ~p to terminate.~n",
			%		   [ Pid ] ),
			%timer:sleep(1000),
			Pid ! terminate

	end,

	% Useless:
	%?getAttr(deployment_manager_pid) ! { notifyHostRemoval, self() },

	% Previous solution to halt corresponding node, used to trigger
	% 'noconnection' errors:

	%TargetNode = ?getAttr(full_node_name),

	%try

		%net_utils:shutdown_node( TargetNode )

	%catch

	%	E ->
	%		io:format( "Exception caught when shutting down node '~s': ~p.~n",
	%				  [ TargetNode, E ] )

	%end,

	?debug( "Computing-host manager deleted." ),

	% "Then" allow chaining:
	State.





% Methods section.



% Performs an initial set-up of the managed host, to prepare for deployment.
%
% (oneway, as it is a long-running task)
%
-spec setUpHost( wooper_state() ) -> oneway_return().
setUpHost( State ) ->

	ManagedHostname = ?getAttr(managed_host),

	NewState = case check_availability( ManagedHostname, State ) of

		true ->
			case is_already_too_late( State ) of

				false ->
					connect_to_host( ManagedHostname, State );

				_TimeOutString ->
					State

			end;

		false ->
			declare_deployment_failure( host_not_available, State )

	end,

	?wooper_return_state_only( NewState ).



% Requests the simulation package to be sent to the caller, expected to be the
% deployment agent.
%
% DeployedNode is the node on which the calling agent runs, specified as an
% atom.
%
% (request)
%
-spec requestPackage( wooper_state(), net_utils:atom_node_name() ) ->
		   request_return( 'deploy_time_out' | binary() ).
requestPackage( State, DeployedNode ) ->

	% This call is also a way of discovering the PID of the remote agent that
	% was created thanks to a rpc:cast.

	% We want to know if ever a deployment agent crashed:
	% (there is a race condition though, as the agent may crash before this link
	% is made, but apparently we cannot do better with rpc operations)
	AgentPid = ?getSender(),

	link( AgentPid ),

	% Checkings:
	undefined = ?getAttr(deployment_agent_pid),
	undefined = ?getAttr(deployed_node),

	case is_already_too_late( State ) of

		false ->

			?trace_fmt( "Node ~p, corresponding to deployment agent ~p, "
						"requested the package, which is being sent to it.",
						[ DeployedNode, AgentPid ] ),

			% This request is necessary to ensure that we wait for the package
			% to be built by the deployment manager:
			%
			?getAttr(deployment_manager_pid) ! { getPackage, [], self() },

			?trace( "Waiting for the package to be sent locally by "
					"the deployment manager." ),

			% Luckily sent big binaries are not copied when in the same node:
			{ PackageFilename, PackageFilenameBin } = receive

				{ wooper_result, FilenameBin } when is_binary( FilenameBin ) ->
					{ text_utils:binary_to_string( FilenameBin ), FilenameBin }

			end,

			% We must tell the agent that it is not already too late:
			AgentPid ! { wooper_result, send_starting },

			?trace_fmt( "Sending the package '~s' through sendfile.",
						[ PackageFilename ] ),

			% Now we send the package through sendfile (rather than using a too
			% large Erlang message for that); the deployment agent already
			% triggered its receive_file call:
			net_utils:send_file( PackageFilename, AgentPid ),

			?trace( "Package sent." ),

			% No reference kept on the package:
			?wooper_return_state_result( setAttributes( State, [

					{ deployment_agent_pid, AgentPid },
					{ deployed_node, DeployedNode }

																] ),
										 PackageFilenameBin );


		TimeOutString ->

			?info_fmt( "(already too late even to request "
					 "the deployment package: ~s)", [ TimeOutString ] ),

			?wooper_return_state_result( State, deploy_time_out )

	end.



% Notifies this manager than the deployment agent finished its deployment.
%
% (const oneway)
%
-spec onDeploymentReady( wooper_state(), system_utils:host_static_info() ) ->
							   oneway_return().
onDeploymentReady( State, HostInfo ) ->

	% Declare success if on time:
	case is_already_too_late( State ) of

		false ->
			?getAttr(deployment_manager_pid) !
				{ onHostDeploymentSuccess, [ self(), HostInfo ] };

		TimeOutString ->
			?info_fmt( "(already too late even to report that the "
					   "deployment succeeded: ~s)", [ TimeOutString ] ),
			ok

	end,

	?wooper_return_state_only( State ).



% Starts a database agent, on the corresponding computing host, generally on
% behalf of the deployment manager.
%
% (const oneway)
%
-spec startDatabase( wooper_state(), pid() ) -> oneway_return().
startDatabase( State, CallerPid ) ->

	DeployAgentPid = ?getAttr(deployment_agent_pid),

	DeployAgentPid ! { start_database, self() },

	receive

		onDatabaseStarted ->
			CallerPid ! { onDatabaseStarted, self() }

	end,

	?wooper_return_state_only( State ).



% Stops a database agent, on the corresponding computing host, generally on
% behalf of the deployment manager.
%
% (const oneway)
%
-spec stopDatabase( wooper_state(), pid() ) -> oneway_return().
stopDatabase( State, CallerPid ) ->

	DeployAgentPid = ?getAttr(deployment_agent_pid),

	%io:format( "Stopping database, requesting agent ~w.~n",
	%		   [ DeployAgentPid ] ),

	DeployAgentPid ! { stop_database, self() },

	receive

		onDatabaseStopped ->
			%io:format( "Database stopped by ~w.~n", [ DeployAgentPid ] ),
			CallerPid ! { onDatabaseStopped, self() }

	end,

	?wooper_return_state_only( State ).




% Section for static methods.


% Returns the fully-qualified node name (as an atom) corresponding to the
% specified node.
%
% NodeBaseName is expected to be already a valid node name.
%
% (static)
%
-spec get_fully_qualified_node_name( net_utils:atom_node_name(),
	net_utils:string_host_name(), net_utils:node_naming_mode() ) ->
										net_utils:atom_node_name().
get_fully_qualified_node_name( NodeBaseName, Hostname, NodeNamingNode ) ->
	list_to_atom( net_utils:get_fully_qualified_node_name( NodeBaseName,
											Hostname, NodeNamingNode ) ).



% Returns an upper bound to the duration, in milliseconds, of a host-level
% deployment.
%
-spec get_host_deployment_duration_upper_bound() -> unit_utils:milliseconds().
get_host_deployment_duration_upper_bound() ->

	% If an error was returned, chances are that the host will never answer, no
	% waiting is less useful:
	%
	LaunchDuration = max( get_time_out_for( launch_success ),
						  get_time_out_for( launch_error ) ),

	% We must take into account the duration of the operations that are needed
	% beyond node-level deployment; and another margin will be added by the
	% deployment manager later:
	%
	LaunchDuration + get_other_operations_duration().



% Returns the estimated maximum duration, in milliseconds, of all operations
% beyond node setup, for a host deployment.
%
-spec get_other_operations_duration() -> unit_utils:milliseconds().
get_other_operations_duration() ->
	1000.




% Section for helper functions.



% Tells whether, at this point in time, this manager is already too late to
% respect its deployment time-out. If yes, the deployment manager already
% ignored it and possibly went through next steps, and is not listening anymore.
%
% (helper function)
%
-spec is_already_too_late( wooper_state() ) -> 'false' | string().
is_already_too_late( State ) ->

	Now = basic_utils:get_timestamp(),

	% Milliseconds:
	TimeOut = ?getAttr(deploy_time_out),

	% get_duration returns seconds:
	case 1000 * basic_utils:get_duration( ?getAttr(start_time), Now ) of

		D when D > TimeOut ->
			io_lib:format( "already waited for ~s, "
						   "longer than time-out of ~s", [
						   text_utils:duration_to_string( D ),
						   text_utils:duration_to_string( TimeOut ) ] );

		_ ->
			false

	end.



% Connects to specified host, performs any required clean-up, and launch a
% corresponding node.
%
% (helper function)
%
-spec connect_to_host( net_utils:string_host_name(), wooper_state() ) ->
							 wooper_state().
connect_to_host( Hostname, State ) ->

	FullyQualifiedNodeName = ?getAttr(full_node_name),

	?debug_fmt( "Starting the creation of node '~s' on host '~s'.",
			   [ FullyQualifiedNodeName, Hostname ] ),

	case manage_node_cleanup( State ) of

		false ->
			?warning_fmt( "Deployment for host '~s' already too long after "
						  "clean-up, thus this host has already been "
						  "considered as unavailable.", [  Hostname ] ),
			State;

		true ->

			% Let's continue then:

			case launch_erlang_node( State ) of

				success ->

					?info_fmt( "Deployment succeeded for host '~s', notifying "
							   "the deployment manager.", [ Hostname ] ),

					% Another thing that can be done in parallel; returns an
					% udpated state:
					%
					case is_already_too_late( State ) of

						false ->

							send_deployment_agent( State );


						TimeOutString ->

							?warning_fmt( "Deployment succeeded for host '~s', "
								 "however it took too long compared to the "
								 "deployment time-out (~s), and thus this host "
								 "has already been considered as unavailable.",
								 [  Hostname, TimeOutString ] ),

							State

					end;


				{ failure, Reason } ->
					declare_deployment_failure( Reason, State )


			end

	end.



% Declares to the deployment manager that on this host the set-up failed, then
% triggers the deletion of this manager.
%
% Returns an updated state.
%
declare_deployment_failure( Reason, State ) ->

	ManagedHostname = ?getAttr(managed_host),
	LocalUsername = ?getAttr(user_name),

	ReasonString = class_DeploymentManager:interpret_host_failure( Reason ),

	case is_already_too_late( State ) of

		false ->

			?warning_fmt( "Deployment failed for host '~s' with user '~s' "
						  "(reason: ~s) and anyway it had already "
						  "taken too long compared to the "
						  "deployment time-out; notifying the deployment "
						  "manager and terminating.",
						  [ ManagedHostname, LocalUsername, ReasonString ] ),

			?getAttr(deployment_manager_pid) ! { onHostDeploymentFailure,
												 [ self(), Reason ] };


		TimeOutString ->

			?warning_fmt( "Deployment failed for host '~s' with user '~s' "
						  "(reason: ~s; ~s), notifying the deployment "
						  "manager and terminating.",
						  [ ManagedHostname, LocalUsername, ReasonString,
						  TimeOutString ] ),
			ok

	end,

	self() ! delete,

	State.



% Checks that the specified host is available and that no previous node is on
% the way.
%
% Returns whether the specified host is valid.
%
% (helper function)
%
check_availability( Hostname, State ) ->

	case check_host_availability( Hostname, State ) of

		true ->
			ensure_no_lingering_node( ?getAttr(full_node_name),
					  ?getAttr(node_name), Hostname, State ),

			true;

		false ->
			false

	end.



% Returns whether specified host seems to be reachable from the network.
%
% Checks with a ping that the specified host is available.
%
% (helper function)
check_host_availability( localhost, _State ) ->
	true;

check_host_availability( Hostname, State ) ->

	?debug_fmt( "Will ping now host '~s'. Depending on the DNS settings, "
				"if the host is not available, the operation may last "
				"for some time.", [ Hostname ] ),

	% Note: pinging a non-existing host may block this process for a few
	% seconds.
	case net_utils:ping( Hostname ) of

		true ->
			?debug_fmt( "Host '~s' is available (ping success).",
						[ Hostname ] ),
			true;

		false ->
			?warning_fmt( "Host '~s' not available, "
						  "no node checked nor launched.", [ Hostname ] ),
			false

	end.



% Ensures that no lingering node with specified name exists on the target host.
%
% The usefulness of this function is quite hypothetical now, as cookies should
% not match on purpose (new UUID already used here), and anyway a node cleaner
% script might be run afterwards.
%
% (helper function)
%
ensure_no_lingering_node( FullyQualifiedNodeName, NodeName, Hostname, State ) ->

	% 'Immediate', as it is not being launched here:
	%
	% (however this operation is long - typically 8-10 seconds on some contexts,
	% probably especially if the node is actually not available, which is by far
	% the most common case, so this checking alone might be responsible for a
	% time-out failure)
	%
	case net_utils:check_node_availability( FullyQualifiedNodeName,
										   immediate ) of

		{ true, _Duration } ->
			?warning_fmt( "Node ~s on host ~s was already available, stopping"
						  " (and, later relaunching) it to ensure it runs the "
						  "correct code version.", [ NodeName, Hostname ] ),

			% Preferred to unloading-purging/reloading our modules:
			% (this is a blocking operation)
			net_utils:shutdown_node( FullyQualifiedNodeName );

		{ false, _Duration } ->
			?debug_fmt( "Node ~s on host ~s is not available, it will be "
						"launched from scratch.", [ NodeName, Hostname ] )

	end.






% Performs a node-cleanup, if requested to do so.
%
% As we use at each simulation run, on purpose, unique (generated) cookies to
% avoid connecting by mistake to previously running instances of the same
% simulation case, we are not able to connect to such a pre-existing node to
% shutdown it.
%
% As a consequence it could remain on the way and prevent its host to take part
% to the simulation (until the node performs its automatic shutdown on idle
% time-out, which had to be set-up to a high value in order to support any
% possible cluster slow-down).
%
% Therefore the cookie system ensures no connection mismatch can ever happen,
% and the cleaner script allows to avoid at all that any such nodes gets ever in
% the way: a (normally successful) attempt to destroy them preventively can be
% performed.
%
% Returns whether the deployment shall continue afterwards.
%
-spec manage_node_cleanup( wooper_state() ) -> boolean().
manage_node_cleanup( State ) ->

	case ?getAttr(node_cleanup) of

		false ->
			% Ready to continue directly:
			true;

		ScriptFullPathAsBin ->

			ScriptFullPath = text_utils:binary_to_string(ScriptFullPathAsBin),

			CleanCommand = case ?getAttr(managed_host) of

				localhost ->
					get_clean_up_command_for_localhost( ScriptFullPath, State );

				Hostname ->
					get_clean_up_command_for_host( Hostname, ScriptFullPath,
												  State )

			end,

			?info_fmt( "Cleaning up was requested, with clean-up script '~s', "
					   "resulting in full command '~s'.~n",
					   [ ScriptFullPath, CleanCommand ] ),

			%io:format( "~nClean-up command:~n~s~n", [ CleanCommand ] ),

			CleanRes = os:cmd( CleanCommand ),

			%io:format( "~nClean-up command resulted in:~n~s~n", [ CleanRes ] ),

			?debug_fmt( "Result of clean-up: '~s'.", [ CleanRes ] ),

			% Tells whether we shall continue afterwards:
			case is_already_too_late( State ) of

				false ->
					true;

				_TimeOutString ->
					false

			end

	end.



% Clean-up the local computing node.
%
% We try to avoid a SSH connection from this node to itself, as it may not be
% already in its own known hosts.
%
get_clean_up_command_for_localhost( ScriptFullPath, State ) ->
	ScriptFullPath ++ " " ++ ?getAttr(node_name).



% Clean-up specified remote computing node.
%
% First, copies the script, then executes it there, then removes it.
get_clean_up_command_for_host( Hostname, ScriptFullPath, State ) ->

	% We suppose here we do not have anything to do, firewall-wise:
	SSHOption = executable_utils:get_ssh_mute_option(),
	Username = ?getAttr(user_name),

	% We used to suppose that the user home directory on this remote host was
	% the same as on this one, however the user name can change depending on
	% host. Instead of forging one with /home/USER, we just swap the user names,
	% not depending on the underlying path structure, so that it is preserved:

	%UserHomeDirectory = system_utils:get_user_home_directory(),
	%UserHomeDirectory = io_lib:format( "/home/~s", [Username] ),
	UserHomeDirectory = re:replace(
		_Subject=system_utils:get_user_home_directory(),
		_RegExp=system_utils:get_user_name(),
		_Replacement=Username,
		_Opts=[ {return,list} ] ),

	% Previously we attempted to use a one-liner with SSH but could not succeed,
	% so we had to write a specific script.
	%
	% Including a selection based on $USER, supposedly correctly set when
	% connected:
	%
	%"\"for p in `/bin/ps -o pid,cmd -u $USER|"
	%	"grep beam|grep -v grep|grep -v " ++ StringCookie
	%	++ "| grep " ++ NodeName
	%	++ " | cut -f 1 -d ' '` ; do kill $p ; done ; "
	%	++ BasicCommand ++ "\"";

	% Hidden yet being still clearly related to Sim-Diasca:
	RemoteCleanScriptName = ".sim-diasca-node-cleaner.sh",

	TargetScriptName = filename:join( UserHomeDirectory,
									 RemoteCleanScriptName ),

	% Like 'scp xx.sh joe@foo.org:/home/joe/yy.sh &&
	% ssh joe@foo.org "/home/joe/yy.sh NODE ; /bin/rm -f /home/joe/yy.sh"':
	RemoteCommand = "\"" ++ TargetScriptName ++ " " ++ ?getAttr(node_name)
		++ " ; /bin/rm -f " ++ TargetScriptName ++ "\"",

	text_utils:join( _Separator=" ", [
		executable_utils:get_default_scp_executable_path(),
		SSHOption,
		ScriptFullPath,
		Username ++ "@" ++ Hostname ++ ":" ++ TargetScriptName,
		"&&",
		executable_utils:get_default_ssh_client_path(),
		SSHOption,
		Username ++ "@" ++ Hostname,
		RemoteCommand
									  ] ).



% Launches on the remote host an appropriately configured Erlang node, on which
% first the deployment agent will be run.
%
% Returns either 'success' or {failure,Reason}.
%
-spec launch_erlang_node( wooper_state() ) -> 'success' | { 'failure',atom()}.
launch_erlang_node( State ) ->

	NodeName = ?getAttr(node_name),
	UserName = ?getAttr(user_name),
	Hostname = ?getAttr(managed_host),

	Command = get_erlang_launch_command( NodeName, UserName, Hostname, State ),

	?info_fmt( "Trying to launch node ~s on host ~s with user ~s "
			   "with following command: '~s'.",
			   [ NodeName, Hostname, UserName, Command ] ),

	%io:format( "### Launch: '~s'.~n", [ Command ] ),

	FullyQualifiedNodeName = ?getAttr(full_node_name),

	% We will try to ensure that host managers will not answer after the
	% deployment manager times-out:
	%
	MaxWaitingBudget = ?getAttr(deploy_time_out) -
		get_other_operations_duration(),

	% Node availability will be determined based on Erlang-level ping:
	case os:cmd( Command ) of

		[] ->

			% We will have to answer before the deployment manager times-out,
			% yet we want to let a node at the very least 2s for set-up,
			% otherwise waiting for node is pointless:
			ActualTimeOut = max( 2000, MaxWaitingBudget ),

			%io:format( "ActualTimeOut = ~B ms~n", [ActualTimeOut] ),

			% Node apparently successfully launched, checking it:
			case net_utils:check_node_availability( FullyQualifiedNodeName,
						ActualTimeOut ) of

				{ true,Duration } ->
					?info_fmt(
						"Node ~s on host ~s successfully launched and checked "
							  "(which took ~B ms on a time-out of ~B ms).",
						[ NodeName, Hostname, Duration, ActualTimeOut ] ),
					success;

				{ false, Duration } ->
					?error_fmt(
						"Node ~s on host ~s apparently successfully launched, "
						"but not responding (to Erlang ping), "
						"after ~B milliseconds (time-out duration: ~B). "
						"Maybe a node with the same name "
						"but with a different cookie was already existing "
						"(prior non-cleaned faulty launch)?",
						[ NodeName, Hostname, Duration, ActualTimeOut ] ),
					{ failure, launched_node_not_responding }

			end;

		ErrorMessage ->

			% Node apparently had trouble being launched, checking it:

			% Here we consider not using the full waiting budget, as apparently
			% something probably went wrong (otherwise we would wait for the
			% maximum duration regardless of command outcome):

			% (note that the previous launch command might have last a long
			% time, to the point that the simulation might already be finished)

			ActualTimeOut = max( 2000,
					 min( get_time_out_for(launch_error), MaxWaitingBudget ) ),

			%io:format( "ActualTimeOut = ~B ms~n", [ActualTimeOut] ),

			case net_utils:check_node_availability( FullyQualifiedNodeName,
						ActualTimeOut ) of

				{ true, Duration } ->

					%io:format( "ActualTimeOut = ~p, Duration = ~p.~n",
					%			[ ActualTimeOut, Duration ] ),

					?warning_fmt(
						"Node ~s on host ~s successfully launched and checked "
						"(in ~B ms out of the ~B time-out), "
						"despite following message: '~s'.",
						[ NodeName, Hostname, Duration, ActualTimeOut,
						 ErrorMessage ] ),

					success;

				{ false, Duration } ->

					io:format( "Error message: ~s.~n", [ ErrorMessage ] ),

					?error_fmt(
						"Node ~s on host ~s apparently failed to launch "
						"properly (message: '~s') and is not responding "
						"after ~B milliseconds (time-out duration: ~B). "
						"Are you using indeed a proper SSH password-less "
						"account for that host, and is Erlang available on it? "
						"One may try executing: 'ssh USER@HOST erl' to check." ,
						[ NodeName, Hostname, ErrorMessage, Duration,
						 ActualTimeOut ] ),

					{ failure, node_launching_failed }

			end

	end.




% Returns a command suitable to the launching of the corresponding Erlang node.
%
-spec get_erlang_launch_command( net_utils:string_node_name(),
		basic_utils:user_name(), net_utils:string_host_name(), wooper_state() )
							   -> string().
get_erlang_launch_command( NodeName, Username, Hostname, State ) ->

	% We replicate the settings of the user node on all computer nodes:
	%
	% (see the --max-process-count and --async-thread-count options of
	% common/src/scripts/launch-erl.sh)
	%
	AsynchThreadsCount = erlang:system_info( thread_pool_size ),
	MaxProcesses       = erlang:system_info( process_limit ),

	SeqOption = case ?getAttr(scheduler_count) of

					undefined ->
						"";

					Count ->
						io_lib:format( "+S ~B", [ Count ] )

	end,

	AdditionalOptions = io_lib:format(
			" -noshell -smp auto ~s +K true +A ~B +P ~B ",
			[ SeqOption, AsynchThreadsCount, MaxProcesses ] ),

	% The next command propagates the cookie of the user node to this newly
	% launched computing node (using -setcookie); however there seems to be a
	% short time window for a race condition, as (quite infrequently) we can see
	% a computing node reporting "** Connection attempt from disallowed node",
	% (that node being the user node); however this seems to be only a transient
	% error and the simulation overcomes it, as we saw it.
	%
	BasicCommand = net_utils:get_basic_node_launching_command(
		NodeName, ?getAttr(node_naming_mode), ?getAttr(epmd_port),
		?getAttr(tcp_port_range), AdditionalOptions ),

	case ?getAttr(managed_host) of

		localhost ->

			% We are on the current host, no need to perform a SSH login, as
			% already logged here:
			% (checking we are using indeed the same user)
			Username = system_utils:get_user_name(),

			BasicCommand ++ " &";

		_ ->
			% We target a remote host here:
			%
			% -f: Requests ssh to go to background just before command execution
			%
			% We suppose here we do not have anything to do, firewall-wise:
			executable_utils:get_default_ssh_client_path() ++ " "
			  ++ executable_utils:get_ssh_mute_option() ++ " -f "
			  ++ Username ++ "@" ++ Hostname ++ " " ++ BasicCommand

	end.



% Sends pioneer modules (e.g. the deployment agent with its prerequisites), that
% will then organise the deployment, based on the simulation archive which is
% expected to be received from the deployment manager afterwards (see the deploy
% function).
%
% Returns an udpated state.
%
-spec send_deployment_agent( wooper_state() ) -> wooper_state().
send_deployment_agent( State ) ->

	TargetNode = ?getAttr(full_node_name),

	% This system_info call may not work on ancient Erlang versions, see
	% system_utils:get_interpreter_version/0:
	%
	RemoteVersion = case rpc:call( TargetNode, _FirstModule=erlang,
								  _FirstFunction=system_info,
								  _FirstArgs=[ otp_release ] ) of

			{ badrpc, _Reason } ->

				case rpc:call( TargetNode, init, script_id, [] ) of

					{ badrpc, Reason } ->
						io_lib:format( "unknown version (reason: ~s)",
									  [ Reason ] );

					{ _OTPInfos, V } ->
						V

				end;

			Version ->
				Version

	end,

	?info_fmt( "Sending Sim-Diasca deployment agent and its prerequisites "
			   "to node ~s, which runs the following Erlang version: ~s.",
			   [ TargetNode, RemoteVersion ] ),

	% The file_utils module is badly needed by the deployment agent, so sending
	% it beforehand as well:
	ModulesToDeploy = [ file_utils, deployment_agent ],

	try

		% Only one target node here:
		basic_utils:deploy_modules( ModulesToDeploy, [ TargetNode ] )

	catch

		{ module_deployment_failed, _FileUtils, [ { error, badfile } ] } ->

			LocalVersion = system_utils:get_interpreter_version(),

			Message = io_lib:format(
				"Deployment failed, possibly due to a version mistmatch "
				"between the Erlang environments in the user node "
				"(~s, which relies on ~s) and the node ~s "
				"(which relies on version ~s).",
				[ net_utils:localhost(), LocalVersion, TargetNode,
				 RemoteVersion ] ),

			io:format( "~n~s~n", [ Message ] ),

			?fatal( Message ),

			throw( { module_deployment_failed,
					possibly_incompatible_erlang_versions,
					{ {local,LocalVersion}, {TargetNode,RemoteVersion} } } );

		Type:Exception ->
			throw( { module_deployment_failed, Type, Exception } )

	end,

	% Of course we do not want to wait for this deploy function to finish, as it
	% is itself waiting for the simulation to finish, so this is a non-blocking
	% call (obviously without a result being returned):
	%
	rpc:cast( TargetNode,
			  _SecondModule=deployment_agent,
			  _SecondFunction=deploy,
			  _SecondArgs=[ self(), ?getAttr(tick_time_out),
						   ?getAttr(deploy_base_dir) ] ),

	State.



-spec get_time_out_for( 'launch_success' | 'launch_error' ) ->
							unit_utils:milliseconds().



-ifdef(exec_target_is_production).


% Returns the duration, in milliseconds, that shall be waited until deciding a
% non-responding launched node is unavailable, depending on the value returned
% by its launch command.


% In production mode, we want to overcome situations where a few nodes might be
% especially long to set-up:
%
get_time_out_for( launch_success ) ->
	% 5 minutes:
	5 * 60 * 1000;

get_time_out_for( launch_error ) ->
	% 2 minutes:
	2 * 60 * 1000.


-else.


% In development mode, we want to be reactive, thus we rely on shorter
% durations:
%
get_time_out_for( launch_success ) ->
	% 400 seconds:
	400 * 1000;

get_time_out_for( launch_error ) ->
	% 5 seconds:
	5 * 1000.


-endif.
