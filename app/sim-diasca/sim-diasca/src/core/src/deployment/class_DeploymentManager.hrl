% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software (Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% The name under which the deployment manager is to be registered (globally):
% (this information must be shared with its clients)
%
-define( deployment_manager_name, sim_diasca_deployment_manager ).



% Section of shorthands for next record specifications.

% A list of hostnames, possibly with their associated user name:
-type host_list() :: net_utils:atom_host_name() |
		  [ { net_utils:atom_host_name(), basic_utils:atom_user_name() } ].


-type element_path() :: file_utils:path().

-type element_type() :: 'data' | 'code'.


-type element_option() :: { 'exclude_directories', [ file_utils:path() ] }
						| { 'exclude_suffixes', [ string() ] }
						| { 'keep_only_suffixes', [ string() ] }
						| 'rebuild' | 'no_rebuild'.


-type element_spec() :: { element_path(), element_type(), [ element_option() ] }
					  | { element_path(), element_type(), element_option() }
					  | { element_path(), element_type() }.


-type elements_to_deploy() :: [ element_spec() ].


-type firewall_options() :: { 'tcp_restricted_range',
							 net_utils:tcp_port_range()}
						  | { 'epmd_port', net_utils:tcp_port() }.



% Describes how the simulator deployment should be performed.
%
-record( deployment_settings, {

	% This field describes the eligible computing hosts that may take part to
	% the simulation. This field is either:
	%
	% - a list whose elements are either {Hostname,Username} pairs (both being
	% specified as atoms) or just Hostname, designating a set of computers and
	% associated logins that can take part to the simulation, like in: [
	% computer_a.foo.org, {computer_b.bar.org,boudevil} ]; if no username is
	% specified for a host, then the one of the user node will be selected for
	% that host; finally, specifying only such a list implies the use of the
	% local host, which will be automatically added to the list
	%
	% - or a pair made of the previous list and of an atom telling whether the
	% current node should be added to the node list: either 'include_localhost'
	% (may not be specified since, as already mentioned, this is true by
	% default) or 'exclude_localhost', if we do no want the user computer
	% from which the simulation is launched to take part to the computings
	% (ex: { [ computer_a.foo.org ], exclude_localhost })
	%
	% - or localhost_only, implying that the execution is not wanted to be
	% distributed, just being wanted to run on the local host (with the current
	% user)
	%
	% - or a pair made of an atom, which is use_host_file or
	% use_host_file_otherwise_local, and the filename of an existing text files
	% which lists candidates in terms of computing hosts, each filename being
	% either:
	%
	%    - alone on its line, specified as an atom (ex: 'myhost.example.org')
	%
	%    - or in a pair with a string-based comment (which is ignored by
	%    Sim-Diasca, its only purpose is to help to maintain the list), like in:
	%    {myhost.example.org,"Computer used by Olivier Boudeville in B-226"}
	%
	%    - or in a hostname-username-comment triplet, username being specified
	%    as an atom, like in: {myotherhost.example.org,'E21850',"My new Laptop"}
	% (supposing that E21850 is a valid username here)
	%
	% Each entry must be alone in its line, which must end with a dot (see
	% sim-diasca-host-candidates-sample.txt for a full example).
	%
	% The use_host_file atom requires such a file to be available and usable,
	% whereas the use_host_file_otherwise_local will not fail if such a file is
	% not available, it will just then fall-back to a localhost_only policy
	%
	% If using 'use_host_file', a third element can be added to the tuple,
	% 'include_localhost' or 'exclude_localhost', knowing that if none is
	% specified (the tuple is just a pair), the local host will be included.
	%
	% So examples for this field could be:
	% - [ computer_a, computer_b ] (which implies include_localhost)
	% - [ computer_a, {computer_b,joe} ] (which implies include_localhost)
	% - { [ computer_a, computer_b ], include_localhost }
	% - { [ computer_a, computer_b ], exclude_localhost }
	% - localhost_only
	% - { use_host_file,"my_hosts.txt" } (which implies include_localhost)
	% - { use_host_file,"my_hosts.txt", include_localhost }
	% - { use_host_file,"my_hosts.txt", exclude_localhost }
	% - { use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" }
	% - { use_host_file_otherwise_local, "sim-diasca-host-candidates.txt" }
	%
	% Of course if the local host is implied, it will be associated with the
	% current username.
	%
	% Note that even if an host is specified multiple times, it will be taken
	% into account as if it was specified only once.
	%
	computing_hosts = { use_host_file_otherwise_local,
					   "sim-diasca-host-candidates.txt" } ::
		host_list()
		| { host_list(), 'include_localhost' | 'exclude_localhost' }
		| 'localhost_only'
		| { 'use_host_file' | 'use_host_file_otherwise_local',
		   file_utils:file_name() }
		| { 'use_host_file', file_utils:file_name(),
		   'include_localhost' | 'exclude_localhost' },


	% This field tells whether an unavailable node can be tolerated (used both
	% by the deployment manager, initially, and then by the load balancer).
	%
	% Set to the 'fail_on_unavailable_node' atom if demanding that all nodes are
	% available before launching the simulation.
	%
	% Otherwise leave it to 'allow_unavailable_nodes'.
	%
	node_availability_tolerance = allow_unavailable_nodes ::
					   'allow_unavailable_nodes' | 'fail_on_unavailable_node',


	% The duration threshold, in seconds, above which a computing host, which
	% failed to report that its deployment succeeded, is considered to be faulty
	% and thus is excluded from the selected computing hosts for the simulation:
	% (more precisely it is the time-out which will be triggered once the last
	% computing node communicated to the deployment manager)
	%
	% Is either a number of seconds or the 'undefined' atom, to rely on the
	% default settings for the selected execution target.
	%
	%maximum_allowed_deployment_duration = 8,
	maximum_allowed_deployment_duration = undefined ::
								   'undefined' | unit_utils:seconds(),


	% This field tells how the deployment package should be managed.
	%
	% It is:
	%
	% - either the 'generate_deployment_package' atom (in which case the package
	% will be generated, saved as file and used)
	%
	% - or the {generate_and_save_deployment_package,TargetFilename} pair, where
	% TargetFilename is the name of the file in which the deployment package
	% should be saved, for a later use with the use_deployment_package option
	% (any pre-existing file with that name will be overwritten)
	%
	% - or the {use_deployment_package,SourceFilename} pair, where
	% SourceFilename is the name of a file containing the deployment package
	% that should be used for that simulation (probably obtained previously
	% thanks to the generate_and_save_deployment_package option)
	%
	% Although any extension can be specified for a deployment package, the
	% "sdar" extension, meaning 'Sim-Diasca Archive', should be preferred, for
	% the sake of clarity. Ex: "MySimPackage.sdar".
	%
	package_manager = generate_deployment_package ::
		  'generate_deployment_package'
		  | { 'generate_and_save_deployment_package', file_utils:file_name() }
		  | { 'use_deployment_package', file_utils:file_name() },


	% Tells whether an attempt to rebuild/update the build of the simulation
	% engine will be made if the generation of a deployment package is to be
	% done (note: deals specifically with the simulation *engine*):
	%
	rebuild_on_deployment_package_generation = true :: boolean(),


	% Allows to specify a list of third-party elements (code and/or data) that
	% are to be deployed in addition to the simulation engine itself (i.e. in
	% addition to Sim-Diasca and its own prerequisites).
	%
	% These elements may include the upper layer of the actual simulator itself
	% (the models and simulation cases that are to be run on top of the
	% Sim-Diasca simulation engine), simulation-specific data files, or anything
	% else.
	%
	% These elements will be added to the deployment archive, and thus a copy of
	% them will be sent and be available locally on each computing node during
	% the simulation.
	%
	% The general form of an element in this list is
	% { ElementPath, ElementType, ElementOptions }, where:
	%
	% - ElementPath is a path, specified as a plain string, to this element to
	% deploy; it is either an absolute path (ex:
	% "/home/boudeville/My-data/foo.dat"), or a path relative to the user
	% directory - i.e. the directory in which the run simulation case is (ex:
	% "../data/bootstrap-infos"; this will expect the 'data' directory to be in
	% the parent directory from the one of the case being run); if a
	% specified path refers to a file, that file will be added to the deployment
	% archive. If a specified path refers to a directory, then it will be
	% recursively scanned at archive creation, in order to look-up the content
	% to integrate to the archive
	%
	% - ElementType tells what is the content type of the specified element
	% path; it can be either:
	%
	%  - 'data', which implies that this content is opaque and will be used 'as
	%  is', without further management (a file will thus be included verbatim, a
	%  directory will be recursively scanned and all its content will be added
	%  to the deployment archive; tree structure will be preserved, thus the
	%  computing nodes will see the exact same tree, with the exact same content
	%  as found in the user node from which the deployment archive was created)
	%
	%  - 'code', which implies the deployment manager is to look-up BEAM files
	%  only, either specified explicitly (in this case one specific file is
	%  targeted, ex: ElementPath="my-simulator/ebin/my_sim_case.beam"), or as a
	%  whole from a parent directory (then the corresponding tree will be
	%  scanned, and all found BEAM files - and only them - will be added to the
	%  archive, ex: ElementPath="my-simulator/ebin/second-generation-models")
	%
	%  - ElementOptions is a (possibly empty) list of options that correspond to
	%  the element type and that will apply to the specified element path:
	%
	%  - options valid for all element types are:
	%
	%     - { exclude_directories, PathList } where PathList is a list of
	%     directories relative to ElementPath (supposed to be itself a
	%     directory): these directories will not be searched for content; for
	%     example: { exclude_directories, [ "ebin/v1", "test", "doc" ] }
	%
	%     - { exclude_suffixes, ExcludeSuffixList } where ExcludeSuffixList is a
	%     list of suffixes that will be blacklisted (filenames ending with them
	%     will be rejected); for example { exclude_suffixes, [ "_test.beam",
	%     "-local.dat", ".png" ] }
	%
	%     - { keep_only_suffixes, KeptSuffixList } where KeptSuffixList is a
	%     list of suffixes that will be white-listed (filenames ending with them
	%     will be accepted, and only them); this option will be processed last,
	%     i.e. *after* exclusions (usage example: for a given path, first a set
	%     of directories is excluded, then some suffixes are specifically
	%     excluded (ex: "_test.beam"), then only the ".beam" and ".dat" suffixes
	%     are kept, allowing to end up with exactly the targeted content
	%
	%     - 'rebuild' (the default for elements of type code): uses GNU 'make'
	%     to trigger any needed rebuild (note that code but also data may have
	%     to be rebuilt); if ElementPath designates a file, an attempt to
	%     rebuild that specific file will be performed prior to its integration
	%     into the deployment archive; if ElementPath designates a directory, a
	%     rebuild from that root will be attempted, before scanning it
	%     recursively for content to integrate into the deployment archive
	%
	%     - 'no_rebuild' (the default for elements of type data): no rebuild
	%     attempt will be performed, the content look-up will be directly
	%     triggered
	%
	%  - for element type 'data', no additional specific option can be specified
	%
	%  - for element type 'code', no additional specific option can be specified
	%
	% Finally, if only one element option is to be specified, the enclosing list
	% can be itself omitted.
	%
	% Should no specific element options have to be specified, this member can
	% be omitted as a whole.
	%
	% If a list-based option (i.e. exclude_directories, exclude_suffixes or
	% keep_only_suffixes) is specified more than once for a given element path,
	% then all these values will be automatically merged.
	%
	% The selection rules are always applied in the same order: first, the
	% exclusions (of directories and/or suffixes), then the inclusions (of kept
	% suffixes).
	%
	% As an example, a valid specification of additional elements to deploy
	%   could be: [ { "settings/models.dat", data }, { "/home/foo/coord.bin",
	%   data, [] }, { "class_MyModel.beam", code }, { "model-repository/v1.3",
	%   code, no_rebuild }, { "/var/Sim/ebin",code, [ rebuild, {
	%   exclude_suffixes, [ "_test.beam" ] }, { keep_only_suffixes, [ ".beam",
	%   ".dat" ] } ] } ].
	%
	additional_elements_to_deploy = [] :: elements_to_deploy(),


	% Allows to specify a list of plugin directories that will be scanned for
	% modules; all the BEAM files that will be found at deployment time will be
	% regarded as plugins.
	%
	% These directories can either be absolute or relative to the directory the
	% file corresponding to the simulation case is.
	%
	% These modules will be available on the user node only.
	%
	plugin_directories = [] :: [ file_utils:directory_name() ],


	% Tells whether, prior to the launching of any Erlang node, a previous
	% attempt to remove any lingering node is done, thanks to a killing of the
	% corresponding process.
	%
	% Note: unless an heavy debugging of the engine or of models is performed,
	% simulations are not expected to crash and thus no specific clean-up is to
	% be performed, knowing that this clean-up would blindly kill any Erlang
	% node owned by the simulation user and corresponding to the same simulation
	% case, on all hosts specified as eligible for the simulation.
	%
	% Nevertheless, there are many reasons for having a stale VM on some host
	% that may prevent a proper deployment, so we strongly advise to let this
	% clean-up enabled.
	%
	perform_initial_node_cleanup = true :: boolean(),


	% Tells whether there are firewall restrictions to be taken into account
	% between computing nodes:
	%
	% - either 'none', in which case communication can take place on arbitrary
	% TCP/IP ports (the general case on clusters)
	%
	% - or a list of options, containing possibly following options:
	%
	%    - { tcp_restricted_range, { Min, Max } }, to select which TCP/IP port
	%    numbers should be used for inter-node communication; for example: {
	%    tcp_restricted_range, { _MinPort=50000, _MaxPort=55000 } }
	%
	%    - { epmd_port, Port }, to override the default (standard) EPMD port,
	%    4369 (which is opened only if using long names); for example: {
	%    epmd_port, 4506 }
	%
	% Of course, the firewall of each node and these settings have to be
	% configured accordingly.
	%
	% For example let's suppose that all our nodes (user one and computing ones)
	% have their firewall configured so that the TCP ports between 20000 and
	% 26000 (bounds included) are not filtered. Then one could specify:
	%  firewall_restrictions = [ { epmd_port, 20000 },
	%						   { tcp_restricted_range, { 20001, 26000 } ].
	%
	% Note that the settings here will affect only the computing nodes (i.e. the
	% ones that will be subsequently launched by the initial user node), not the
	% user node.
	%
	% If wanting to change the settings of that user node, look at the
	% FIREWALL_OPT variable defined in common/GNUmakevars.inc. Otherwise, if the
	% settings of the two kinds of nodes do not match, no available computing
	% node will be found.
	%
	% Note also that the default (standard) EPMD port can always be overridden,
	% but any EPMD port (default or not) will be opened only if using long
	% names.
	%
	firewall_restrictions = [ { epmd_port, 4506 },
							  { tcp_restricted_range, { 50000, 55000 } } ]
								 :: 'none' | [ firewall_options() ],


	% Tells which directory shall be used, on each computing node, to store
	% temporary data (ex: some clusters may require to use '/local' rather than
	% '/tmp' for that, to benefit from more inodes, more space, etc.)
	%
	temporary_directory = "/tmp" :: file_utils:directory_name(),


	% Tells whether the data-logging service should be enabled.
	%
	enable_data_logger = false :: boolean(),


	% Tells whether the distributed data exchange service should be enabled.
	%
	% Can be set to false, or true, or { true, ConfigurationFileList } where
	% ConfigurationFileList is a list of plain strings corresponding to paths
	% (relative to the directory in which the simulation case file is) to the
	% configuration files that must be read from the deployment archive and
	% parsed.
	%
	% These files will be automatically be deployed, without needing to be
	% listed in the 'additional_elements_to_deploy' field. The recommended
	% extension for these files is ".cfg". Their content must be a list of
	% lines, each terminated by a dot, and containing either a { Key, Value,
	% Qualifier } triplet or a { Key, Value } pair (which implies
	% Qualifier=const), knowing that Key is an atom, Value is any Erlang term,
	% and Qualifier is either 'const' or 'mutable'.
	%
	% See the soda_parameters.cfg file and associated test cases of Soda Test
	% for a complete example.
	%
	enable_data_exchanger = true :: boolean()
							   | { 'true', [ file_utils:file_name() ] },


	% Tells whether the performance tracker should be enabled:
	%
	enable_performance_tracker = false :: boolean(),


	% Tells what is the required resilience of the simulation with regard to the
	% loss of computing hosts in the course of the simulation, which is bound to
	% happen in longer, large simulations (i.e. that run on many cores, each of
	% which that may fail at any time according to their individual, finite
	% MTBF).
	%
	% Either 'none' can be specified, in which case the simulation will crash as
	% soon as a computing node is deemed lost (in this case no overhead will
	% apply due to resilience), or a positive integer k, with designates the
	% maximum number of simultaneous losses of computing hosts that the
	% simulation will be able to overcome without crashing (which implies some
	% overhead) - and provided the remaining hosts are able to sustain the
	% demanded load.
	%
	% For example, k=0 is equivalent to 'none'; if k=3, then as long as "only"
	% 1, 2 ou 3 computing hosts fail at the same time during the simulation, it
	% will be nevertheless able to resist and continue. There is no upper bound
	% to the total losses in the simulation, provided that at any time the k
	% threshold is not exceeded and that the remaining hosts can sustain the
	% resulting load.
	%
	% Note that this resilience applies to random computing hosts; for example,
	% if the host that is lost is the one of the root time manager, the
	% simulation will crash regardless of the value of k. So, depending on the
	% simulation services that are enabled by a case, more single points of
	% failure may be introduced.
	%
	% Similarly, should a long-enough network split happen, the k threshold can
	% be immediately reached.
	%
	% Currently no support is offered for hosts that would join in the course of
	% the simulation, for example spare hosts dynamically added to compensate
	% for previous losses.
	%
	% This service is implemented by stopping the simulation at regular
	% milestones (in wall-clock time), serializing the state of the actors on
	% each computing hosts, and sending them to k well-chosen other hosts. Then,
	% as long as up to k hosts fail, the simulation can still rely on a snapshot
	% for the last met milestone, and restart from it (provided the remaining
	% hosts are powerful enough to support the whole simulation alone).
	%
	crash_resilience = none :: 'none' | basic_utils:count(),


	% As outages happen in real time (as opposed to virtual time), the duration
	% between two resilience milestones is to be expressed in real time as well.
	%
	% Specifies the minimal duration (in integer seconds) between two of these
	% milestones, which consist on serialising the whole simulation state in
	% order to be able to go back to them, should nodes be lost since then. Once
	% that delay is over, the serialisation will be triggered as soon as a a new
	% overall simulation tick is scheduled.
	%
	% By default, 2 hours will elapse between the end of a serialisation and the
	% beginning of the next one.
	%
	serialisation_period = 2 * 60 * 60 :: unit_utils:seconds()

} ).


% For convenience:
-type deployment_settings() :: #deployment_settings{}.




% Records the known information of a computing host.
%
-record( computing_host_info, {


		   % The PID of the host manager in charge of the corresponding
		   % computing host:
		   %
		   host_manager_pid :: pid(),

		   % The name of the corresponding computing host, as a binary (could
		   % be net_utils:atom_host_name()):
		   host_name :: text_utils:bin_string(),

		   % The name of the user to be retained for that host, as a binary
		   % (could be basic_utils:user_name()):
		   user_name :: text_utils:bin_string(),

		   % The name of the corresponding (fully qualified) node created for
		   % this host (as an atom):
		   node_name :: net_utils:atom_node_name(),

		   % The static information about an host:
		   host_infos :: system_utils:host_static_info()

} ).


-type computing_host_info() :: #computing_host_info{}.
