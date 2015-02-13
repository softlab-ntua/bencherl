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



% Data-logger class.
%
% Stores potentially numerous simulation results, notably on behalf of actors,
% thanks to virtual probes.
%
% The main data logger is an optional simulation service, managed from the
% deployment manager (see the 'enable_data_logger' field of the
% 'deployment_settings' record in class_DeploymentManager.hrl).

% The data logger is in soft real-time, it should provide fairly efficient
% services before (pre-processing), in the course of, and after
% (post-processing) a simulation.

% Generally the data-logger is a singleton, using only one instance of that
% class is more convenient.

% It allows notably to maintain a full database of all relevant simulation
% results, either intermediate or final, whatever they are, in order to store
% them reliably and to be able to run complex queries over their data sets at
% any time.
%
% The simulation being distributed, the database is itself distributed, in order
% to reduce the messages over the network, to distribute the need for resources
% (CPU, RAM, disk) and to perform most operations locally, yet being able to
% view all stored information as a unique common database.

% One of the roles of a data-logger is to offer alternatives to probes,
% alternatives which moreover are more scalable and feature-rich.

% Indeed, plain probes (class_Probe instances) imply some limitations that a
% data-logger can alleviate:
%
% - probes only keep track of a set of curves over time (less generic, no cross
% queries, etc.)
%
% - probes use one process per probe instance, which, in some cases, might be a
% bit expensive, resource-wise
%
% - on a single node, in general only up to 1024 simultaneous probe instances
% can coexist (due to file descriptor limit), otherwise they need to store their
% values in memory (thus resulting in a massive memory footprint over time)

% The data logger provides nevertheless an API very similar to the one of
% probes, so that it can replace them easily whenever needed, and is able to
% generate exactly the same kind of reports.

% A data-logger is able to manage a (potentially large) set of so-called
% "virtual probes".

% User code is expected to create a virtual probe by calling the
% create_virtual_probe/6 static method, instead of creating a new class_Probe
% instance.

% The usual features of probes are available, including:
%
% - ability to set samples to non-consecutive ticks
%
% - ability to send partial samples, where only a subset of the curves have
% values for a given tick
%
% - dynamic addition of curves
%
% - reordering of curves
%
% In addition we have:
%
% - (transactional) sample merging, when multiple sources write to the same
% virtual probe
%
% - synchronous counterparts of non-blocking operations to set or merge sample
% data
%
% - direct operations, from the data source (the owner of the virtual probe) and
% the local database node, not going through a potentially remote data-logger,
% which in addition could behave as a bottleneck
%
-module(class_DataLogger).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_ResultProducer ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, RegistrationOptions, MetaData ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, createVirtualProbe/8,
		 setData/4, setDataSynchronous/4,
		 mergeData/4, mergeDataSynchronous/4,
		 addCurve/3,
		 getCurveRenderOrder/2, setCurveRenderOrder/3, getProbeTable/2,
		 setPlotStyle/3, setFillStyle/3, setCanvasSize/4, setKeyOptions/3,
		 sendResults/2,
		 generateReport/2, generateReport/3 ).


% Static method declarations.
-define( wooper_static_method_export, create_main_datalogger/2,
		 get_main_datalogger/0,
		 create_virtual_probe/6, send_data/3,
		 set_data_synchronous/3, merge_data_synchronous/3 ).


% For probe_settings():
-include("class_Probe.hrl").


-type virtual_probe_id() :: basic_utils:count().


-type curve_count() :: basic_utils:count().


-type table_name() :: atom().





% Data structure maintained by the data-logger to record information about probe
% counterparts: not real probes, but emulation thereof, called virtual probes.
%
% The probe owner (PID) is not recorded here.
%
% TO-DO: add two fields (per probe), result_produced and result_collected, to
% perform more checkings at shutdown.
%
-record( virtual_probe, {

		 % A counter that identifies uniquely a virtual probe in the context of
		 % a data logger instance.
		 %
		 % This is a duplicated information, as a virtual probe record is
		 % already referenced in the associative table by a key, which is the
		 % ID.
		 %
		 id :: virtual_probe_id(),

		 % Name of the virtual probe, as a binary; the id field still remains
		 % its identifier, but using a name allows to be matched against the
		 % result specifications.
		 %
		 name :: curve_name(),

		 % Ordered list of { CurveCount, CurveName } pairs, with CurveCount
		 % keeping track of the order into which the curves were declared and
		 % fed (so that, prior to generating a report, curves can be reordered
		 % while being still associated to their values), and with curve names
		 % being binaries; the order of this list dictates the rendering order
		 % of curves.
		 %
		 curve_entries :: class_Probe:curve_entries(),

		 % List of { ZoneName, Bounds } pairs, where ZoneName is the name of
		 % that zone (as a binary) and Bounds={ FirstBound, SecondBound }
		 % delimits the zone, each bound being either a curve index or the
		 % 'abscissa_top' or 'abscissa_bottom' atom.
		 %
		 zone_entries:: class_Probe:zone_entries(),

		 % Number of known curves for that virtual probe (cached value for
		 % faster processing):
		 %
		 curve_count :: curve_count(),

		 % Corresponding Mnesia name for that virtual probe (an atom):
		 %
		 table_name :: atom(),

		 % A probe_settings record describing how reports should be rendered:
		 % (see class_Probe.hrl)
		 %
		 render_settings :: probe_settings()

						 }

		).


-type virtual_probe() :: #virtual_probe{}.



% Describes an entry of the table created for a virtual probe.
-record( probe_sample, {

		   % The time-stamp for that sample entry (it is a key, thus an index).
		   sample_tick :: probe_tick(),

		   % The value of that sample, a tuple containing numerical values.
		   sample_data :: sample_data()

						}
		).


-type probe_sample() :: #probe_sample{}.


-export_type([ probe_sample/0 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.DataLogging").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% The default name for the global data logger:
-define( default_data_logger_name, main_data_logger ).



% For qlc:
%-include_lib("stdlib/include/qlc.hrl").



% Implementation notes.

% The data logger uses Mnesia to store its data.
%
% The second best technology that was identified for that was CouchDB. Should
% these two be unable to withstand the needed storage volume, MySQL, PostgreSQL
% or Berkeley DB could be used, instead, or as a second storage level (Mnesia
% being then used as a smart cache that can be queried).

% We are not using currently the disc-only copies storage type for
% general-purpose data (as opposed to virtual probe data), as it would limit the
% size of tables to 2GB each (the one of a DETS table), and would be
% considerably slower in terms of read access.

% Therefore, if the data-logger runs on a 32-bit VM, the maximum size for a
% table is currently 4GB (4e9 bytes), whereas on a 64-bit VM it is 16 exabytes
% (16e18 bytes).

% Larger tables would require to be fragmented and possibly to be distributed,
% which we are doing here naturally on a distributed simulation anyway (both
% features being provided by Mnesia out-of-the-box).



% Virtual probe section.

% When used to gather data as probes do, a data logger allows to create virtual
% probes.

% For each virtual probe, in addition to some datalogger-level metadata
% (including a record describing everything about it except its sample data), a
% specific Mnesia table will be used, in order to store all its samples.

% As, during a simulation, we mostly, if not uniquely, write to probe tables
% (thus a RAM cache for reading is not needed) and we do not want to saturate
% the RAM with their data, we use disc_only_copies for virtual probes.
%
% As a result, a probe should not record more than 2GB of samples, which is a
% fairly high limit, and samples will be stored unordered (not even
% chronologically).

% A virtual probe has an owner (the supposedly unique process that will interact
% with the virtual probe), a unique virtual probe ID, a name, and each entry of
% that probe (tuple of sample data) will be referenced by a numerical integer
% timestamp (akin to a simulation tick), which will be an index of that table.

% When using a data-logger as a probe counterpart, writing a sample is done
% thanks to a dirty operation, for performance reason. We suppose that at any
% time up to one process, the data-logger instance to which the probe owner
% declared this probe, performs write accesses to it (no transaction needed).

% Note that if the samples happen to be sent faster than they are stored
% in-base, the data-logger mailbox will grow indefinitely and possibly exhaust
% the memory (no flow control).

% Writings performed with setData/4 will overwrite any previous sample entry for
% that tick, so partial sample sending (with some data being set to
% 'undefined', like for plain probe) cannot be done that way. A mergeData/4
% method is thus available instead, but it will use more resources, as merging
% samples implies a read-modify-write operation, thus a transaction.

% Contrary to plain probes, samples used in the context of a data-logger do not
% need to be sent in-order (i.e. in chronological order, with increasing
% timestamps), they will be reordered anyway at report generation. However,
% should a curve be dynamically added, all previous samples (i.e. all samples
% whose size does not take into account yet this new curve) should have been
% already stored (this is merely to be able to perform sanity check when a
% sample is received).

% Samples can also - and preferably - be written directly by the probe owner in
% the relevant table. This has for advantage to remove the bottleneck induced by
% the data-logger, to (hopefully) avoid unnecessary sending of sample messages
% over the network, and potentially also to perform some actor-level flow
% control with respect to the database.
%
% The two approaches to sample sending (direct or through the data-logger)
% should never be mixed for a given virtual probe, as otherwise concurrent
% accesses could happen, whereas we rely on dirty operations.

% The first field of a Mnesia record (i.e. the second element of the tuple) is
% the key, and thus need no extra index.

% Both files used for the generation of probe report (configuration file and
% data file) are created only when the generation is requested (ex: no
% create_command_file_initially, would be always set to false).


% Unlike basic probes, the data-logger does not have to declare anything
% special, as it always exists and is always requested to send its results, as
% virtual probes are created iff they produce selected results.


-type registration_options() :: basic_utils:registration_name()
		| { basic_utils:registration_name(), basic_utils:registration_scope() }.



% Class-specific attributes of a data-logger instance are:
%
% - next_probe_id ::basic_utils:count() is the ID (unsigned integer, used as an
% incrementing counter) of the next virtual probe to be created
%
% - probe_table :: dict:dict() is a table which associates to a key, a virtual
% probe id, the corresponding virtual probe record
%
% - database_activated :: boolean() tells whether this data-logger already
% requested to the deployment manager to activate the database
%
% - output_dir :: string() is a plain string recording the directory in which
% outputs should be produced
%
% - deployment_manager_pid :: pid() is the PID of the deployment manager (if
% any)
%
% - meta_data :: class_ResultManager:meta_data() record the meta-data to be
% passed to (virtual) probes
%
% - gnuplot_version is the version of the Gnuplot executable that will be
% used for virtual probes




% Constructs a new data logger.
%
% RegistrationOptions is:
%
% - either Name, corresponding to the name of this data logger (specified as an
% atom), under which it will register (globally) to
%
% - or { Name, RegistrationPolicy }, with RegistrationPolicy being in
% 'local_only', 'global_only', 'local_and_global', or 'none'
%
-spec construct( wooper:state(), registration_options(),
				class_ResultManager:meta_data() ) -> wooper:state().
construct( State, _RegistrationOptions={ Name, RegistrationType }, MetaData ) ->

	% First the direct mother classes:
	EmitterName = text_utils:uppercase_initial_letter( atom_to_list( Name ) ),

	TraceState = class_ResultProducer:construct( State, EmitterName ),

	% Then the class-specific actions:

	% Depending on its use (ex: if using numerous asynchronous operations), the
	% data-logger may become a bottleneck:
	erlang:process_flag( priority, _Level=high ),

	basic_utils:register_as( Name, RegistrationType ),

	class_InstanceTracker:register_agent( State ),

	InitState = setAttributes( TraceState, [

		% The datalogger is created unconditionally, so this very specific
		% result producer must disable the result_{produced,collected} checks:
		{ result_produced, true },

		% Must remain as (otherwise check will fail if collected):
		% { result_collected, false },

		{ next_probe_id, 1 },
		{ probe_table, dict:new() },
		{ database_activated, false },
		{ output_dir, file_utils:get_current_directory() },
		{ deployment_manager_pid, undefined },
		{ meta_data, MetaData },
		{ gnuplot_version, executable_utils:get_current_gnuplot_version() },
		{ trace_categorization, text_utils:string_to_binary(
								  ?TraceEmitterCategorization) }

	] ),

	?send_trace( InitState, "New data-logger created." ),

	InitState;

construct( State, _RegistrationOptions=Name, MetaData ) ->
	construct( State, { Name, global_only }, MetaData ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?trace( "Deleting data-logger." ),

	case ?getAttr(database_activated) of

		true ->

			case ?getAttr(deployment_manager_pid) of

				undefined ->
					% Should never happen, activating means storing the PID.
					ok;

				DeployPid ->

					% Acts as if was the only client:
					DeployPid ! { deactivateDatabase, [], self() }
					% Not interested in the request answer (ignored).

			end;

		false ->
			ok

	end,

	?debug( "Data-logger deleted." ),

	class_InstanceTracker:unregister_agent(),

	% basic_utils:unregister/2 useless and would require Name.

	% Then call the direct mother class counterparts and allow chaining:
	%
	% (we set result_collected to true so that the destructor of ResultProducer
	% does not raise an error in cases where the data-logger has been enabled -
	% which is the default - yet has not been used, while no simulation has been
	% run, no result has been requested, etc.)
	%
	setAttribute( State, result_collected, true ).





% Methods section.


% Creates a virtual probe, with specified name (specified as a binary), from
% specified node.
%
% Usually the node of the probe owner is specified, so that the sending of
% samples remains local, node-wise.
%
% Returns the ID of this new virtual probe, or the 'non_wanted_virtual_probe'
% atom should the result manager determine this probe will not be an awaited
% result.
%
% See the create_virtual_probe/6 static method, preferably to one to be
% called from the user code.
%
% (request)
%
-spec createVirtualProbe( wooper:state(), text_utils:bin_string(),
	   net_utils:atom_node_name(), [ class_Probe:declared_curve_name() ],
	   [ class_Probe:declared_zone() ], text_utils:title(), text_utils:label(),
	   text_utils:label() ) ->
	request_return( 'non_wanted_virtual_probe' | class_Probe:curve_index() ).
createVirtualProbe( State, BinProbeName, Node, CurveNames, Zones, Title,
					XLabel, YLabel ) ->

	%io:format( "Creating a virtual probe named '~s' on node ~s.~n",
	%	  [ ProbeName, Node ] ),

	% First, determines whether this probe should be created:
	?getAttr(result_manager_pid) ! { isResultProducerWantedWithOptions,
									 [ BinProbeName, virtual_probe ], self() },

	receive

		{ wooper_result, { true, Options } } ->

			%io:format( "Virtual probe ~s is wanted, options: ~p.~n",
			%		  [ BinProbeName, Options ] ),

			ActualOptions = case Options of

					undefined ->
							_DefaultOpts=[ data_and_plot ];

					Options ->
							Options

			end,

			% The deployment manager ensured the database is running on all
			% computing nodes (Mnesia started), we also have to ensure the
			% schema is created:
			NewState = ensure_database_activated( State ),

			% Note that the plots will be created on the same node as this
			% data-logger, so the Gnuplot version we determine in this function
			% is the correct one:
			Settings = class_Probe:get_probe_settings( Title, XLabel, YLabel,
													?getAttr(gnuplot_version) ),

			NewID = ?getAttr(next_probe_id),
			TableName = get_table_name_for( NewID ),

			% Results in [ {curve_index(),curve_name()} ]:
			CurveEntries = class_Probe:transform_curve_names( CurveNames ),

			% Results in [ zone_definition() ]:
			ZoneEntries = class_Probe:transform_declared_zones( Zones,
															   CurveEntries ),

			NewProbeRecord = #virtual_probe{

				id = NewID,

				name = BinProbeName,

				% Will be like [ {1,<<"A">>}, {2,<<"Curve B">>},
				% {3,<<"Foobar">>} ]:
				curve_entries = CurveEntries,

				zone_entries = ZoneEntries,

				curve_count = length( CurveEntries ),

				table_name = TableName,

				render_settings = Settings

			},


			% We create now the probe-specific table on the same node.
			%
			% Note that the most convenient type would be ordered_set, so that
			% we retrieve the probe samples already sorted according to
			% increasing ticks, however this type is not compatible with the
			% disc_only_copies storage type, which is strictly necessary as
			% probes should not have their data in RAM, lest long simulations
			% cannot fit in memory. As a consequence we are not able to rely on
			% ordered set, we just use set and we will reorder samples by
			% ourselves:
			%
			{ atomic, ok } = mnesia:create_table( TableName, [

			   { disc_only_copies, [ Node ] },
			   { record_name, probe_sample },
			   %{ type, ordered_set },
			   { type, set },
			   { attributes, record_info( fields, probe_sample ) }

			] ),

			ProbeDict = ?getAttr(probe_table),

			UpdatedState = setAttributes( NewState, [

					{ next_probe_id, NewID + 1 },
					{ probe_table, dict:store( _Key=NewID,
						 _Value={ NewProbeRecord, ActualOptions },
						 ProbeDict ) }

													 ] ),

			%io:format( "Probe ~s wanted and created.~n", [ BinProbeName ] ),

			?wooper_return_state_result( UpdatedState, NewID );

		{ wooper_result, false } ->

			io:format( "Virtual probe ~s was not wanted.~n", [ BinProbeName ] ),

			?wooper_return_state_result( State, non_wanted_virtual_probe )

	end.




% Set/Merge section, synchronous or not.



% Registers specified data sample for specified tick.
%
% This is an asynchronous (non-blocking) call, therefore it does not incur the
% overhead of the sending back of a synchronisation acknowledgement when the
% operation is over. On the other hand, no flow control can exist and too many
% simultaneous writers may outpace the database. See setDataSynchronous/4 for
% the synchronous (blocking) version of that operation.
%
% The specified sample will overwrite any previously defined entry for that
% tick. See mergeData/4 if not wanting this.
%
% The operation should preferably be done directly by the process that feeds the
% virtual probe, to avoid that the data-logger becomes a bottleneck and also to
% avoid that useless messages are sent over the network (knowing that usually
% the feeder process and the probe table are by construction on the same node).
%
% (oneway)
%
-spec setData( wooper:state(), virtual_probe_id(), probe_tick(),
			  sample_data() ) -> oneway_return().
setData( State, ProbeID, Tick, Sample ) ->

	%?debug_fmt( "setData called for probe #~B at tick ~B "
	%		   "with samples ~p.", [ ProbeID, Tick, Sample ] ),

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	ExpectedCount = ProbeRecord#virtual_probe.curve_count,

	% Integrity check (pattern-matching):
	ExpectedCount = size( Sample ),

	set_data_helper( ProbeRecord#virtual_probe.table_name, Tick, Sample ),

	?wooper_return_state_only( State ).



% Registers specified data sample for specified tick.
%
% This is a synchronous (blocking) call, therefore it incurs the overhead of the
% sending back of a synchronisation acknowledgement when the operation is
% over. On the other hand, this allows to perform a form of flow control, so
% that too many simultaneous writers will have to wait for the database to
% finish. See setData/4 for the asynchronous (non-blocking) version of that
% operation.
%
% The specified sample will overwrite any previously defined entry for that
% tick. See mergeData/4 if not wanting this.
%
% The operation should preferably be done directly by the process that feeds the
% virtual probe, to avoid that the data-logger becomes a bottleneck and also to
% avoid that useless messages are sent over the network (knowing that usually
% the feeder process and the probe table are by construction on the same node).
%
% (request)
%
-spec setDataSynchronous( wooper:state(), virtual_probe_id(),
		 probe_tick(), sample_data() ) ->
					request_return( 'datalogging_set_done' ).
setDataSynchronous( State, ProbeID, Tick, Sample ) ->

	%?debug_fmt( "setDataSynchronous called for probe #~B at tick ~B "
	%		   "with samples ~p.", [ ProbeID, Tick, Sample ] ),

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	ExpectedCount = ProbeRecord#virtual_probe.curve_count,

	% Integrity check (pattern-matching):
	ExpectedCount = size( Sample ),

	set_data_helper( ProbeRecord#virtual_probe.table_name, Tick, Sample ),

	% For synchronisation (flow control):
	?wooper_return_state_result( State, datalogging_set_done ).



% Merges specified data sample for specified tick.
%
% This is an asynchronous (non-blocking) call, therefore it does not incur the
% overhead of the sending back of a synchronisation acknowledgement when the
% operation is over. On the other hand, no flow control can exist and too many
% simultaneous writers may outpace the database. See mergeDataSynchronous/4 for
% the synchronous (blocking) version of that operation.
%
% Any sample element not set to undefined in the specified sample will fill any
% sample element set to undefined at the same index on the already existing
% sample entry (if any). If the pre-existing value is not undefined, then an
% exception is thrown (even if we try to replace that value with the same
% value).
%
% Note: merging samples involves a read-modify-write operation, thus a
% transaction, thus it is far more expensive than setData.
%
% (oneway)
%
-spec mergeData( wooper:state(), virtual_probe_id(), probe_tick(),
			  sample_data() ) -> oneway_return().
mergeData( State, ProbeID, Tick, Sample ) ->

	%?debug_fmt( "mergeData called for probe #~B at tick ~B "
	%		   "with samples ~w.", [ ProbeID, Tick, Sample ] ),

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	ExpectedCount = ProbeRecord#virtual_probe.curve_count,

	% Integrity check (pattern-matching):
	ExpectedCount = size( Sample ),

	% Here we know that this sample and any pre-existing one have the same
	% number of elements.

	merge_data( ProbeRecord#virtual_probe.table_name, Tick, Sample ),

	?wooper_return_state_only( State ).



% Merges specified data sample for specified tick.
%
% This is a synchronous (blocking) call, therefore it incurs the overhead of the
% sending back of a synchronisation acknowledgement when the operation is
% over. On the other hand, this allows to perform a form of flow control, so
% that too many simultaneous writers will have to wait for the database to
% finish. See mergeData/4 for the asynchronous (non-blocking) version of that
% operation.
%
% Any sample element not set to undefined in the specified sample will fill any
% sample element set to undefined at the same index on the already existing
% sample entry (if any). If the pre-existing value is not undefined, then an
% exception is thrown (even if we try to replace that value with the same
% value).
%
% Note: merging samples involves a read-modify-write operation, thus a
% transaction, thus it is far more expensive than setData.
%
% (request)
%
-spec mergeDataSynchronous( wooper:state(), virtual_probe_id(),
		 probe_tick(), sample_data() ) ->
					request_return( 'datalogging_merge_done' ).
mergeDataSynchronous( State, ProbeID, Tick, Sample ) ->

	%?debug_fmt( "mergeDataSynchronous called for probe #~B at tick ~B "
	%			"with samples ~w.", [ ProbeID, Tick, Sample ] ),

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	ExpectedCount = ProbeRecord#virtual_probe.curve_count,

	% Integrity check (pattern-matching):
	ExpectedCount = size( Sample ),

	% Here we know that this sample and any pre-existing one have the same
	% number of elements.

	merge_data( ProbeRecord#virtual_probe.table_name, Tick, Sample ),

	% For synchronisation (flow control):
	?wooper_return_state_result( State, datalogging_merge_done ).




% Declares an additional curve, whose name is specified, to the specified
% virtual probe.
%
% By default it will be rendered after the already declared curves.
%
% Note: all samples received afterwards are then expected to take it into
% account (sending one more value, or the atom 'undefined', for that curve).
%
% Probe options will not be modified.
%
% (oneway)
%
-spec addCurve( wooper:state(), virtual_probe_id(),
			   class_Probe:declared_curve_name() ) -> oneway_return().
addCurve( State, ProbeID, CurveName ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),

	NewCount = ProbeRecord#virtual_probe.curve_count + 1,

	NewNames = list_utils:append_at_end(
				  { NewCount, text_utils:string_to_binary( CurveName ) },
				 ProbeRecord#virtual_probe.curve_entries ),

	NewProbeRecord = ProbeRecord#virtual_probe{
						  curve_count = NewCount,
						  curve_entries = NewNames },

	NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts }, State ),

	?wooper_return_state_only( NewState ).



% Returns the list of curve names, as plain strings, sorted according to current
% rendering order, for the specified virtual probe.
%
% Useful then to reorder them and then to set them back thanks to
% setCurveRenderOrder/3.
%
% (const request)
%
-spec getCurveRenderOrder( wooper:state(), virtual_probe_id() ) ->
								 request_return( [ string() ] ).
getCurveRenderOrder( State, ProbeID ) ->

	ProbeRecord = get_virtual_probe( ProbeID, State ),
	CurveNames = ProbeRecord#virtual_probe.curve_entries,

	% Get rid of the curve index, order preserved:
	PlainNames = [ text_utils:binary_to_string( element( 2, NamePair ) )
				   || NamePair <- CurveNames ],

	?wooper_return_state_result( State, PlainNames ).



% Sets the list of curve names, sorted according to the desired rendering order,
% for the specified virtual probe.
%
% Names is a list of plain strings which must correspond to a permutation of the
% list which would be returned by getCurveNames/2.
%
% (oneway)
%
-spec setCurveRenderOrder( wooper:state(), virtual_probe_id(), [ string() ] ) ->
								 oneway_return().
setCurveRenderOrder( State, ProbeID, Names ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),
	CurveNames = ProbeRecord#virtual_probe.curve_entries,

	Len = length( CurveNames ),

	case length( Names ) of

		Len ->
			NewCurveNames = class_Probe:add_probe_index_back( Names,
															  CurveNames ),

			NewProbeRecord = ProbeRecord#virtual_probe{
						  curve_entries = NewCurveNames },

			NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts },
										  State ),

			?wooper_return_state_only( NewState );

		_Other ->
			throw( { invalid_name_count, Names, Len } )

	end.



% Returns the table name corresponding to the specified virtual probe
% identifier.
%
% Useful to perform afterwards direct thus efficient operations on the table,
% see the send_data/3, set_data_synchronous/3 and merge_data_synchronous/3
% static methods.
%
% (const request)
%
-spec getProbeTable( wooper:state(), virtual_probe_id() ) ->
						   request_return( table_name() ).
getProbeTable( State, ProbeID ) ->

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	?wooper_return_state_result( State, ProbeRecord#virtual_probe.table_name ).



% Sets the plot settings to the one specified as a plain string (ex:
% "histograms") for the specified virtual probe.
%
% (oneway)
%
-spec setPlotStyle( wooper:state(), virtual_probe_id(), string() ) ->
						  oneway_return().
setPlotStyle( State, ProbeID, NewPlotStyle ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),
	Settings = ProbeRecord#virtual_probe.render_settings,

	NewSettings = Settings#probe_settings{
		   plot_style=text_utils:string_to_binary( NewPlotStyle ) },

	NewProbeRecord = ProbeRecord#virtual_probe{
		   render_settings=NewSettings },

	NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts }, State ),

	?wooper_return_state_only( NewState ).



% Sets the fill settings, specified as a plain string (ex:
% "solid 1.0 border -1") for the specified virtual probe.
%
% (oneway)
%
-spec setFillStyle( wooper:state(), virtual_probe_id(), string() ) ->
						  oneway_return().
setFillStyle( State, ProbeID, NewFillStyle ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),
	Settings = ProbeRecord#virtual_probe.render_settings,

	NewSettings = Settings#probe_settings{
		   fill_style=text_utils:string_to_binary( NewFillStyle ) },

	NewProbeRecord = ProbeRecord#virtual_probe{
		   render_settings=NewSettings },

	NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts }, State ),

	?wooper_return_state_only( NewState ).



% Sets the size of the probe reports (canvas), in pixels.
%
% (oneway)
%
-spec setCanvasSize( wooper:state(), virtual_probe_id(), gui:length(),
					gui:length() ) -> oneway_return().
setCanvasSize( State, ProbeID, NewWidth, NewHeight ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),

	ProbeRecord = get_virtual_probe( ProbeID, State ),

	Settings = ProbeRecord#virtual_probe.render_settings,

	NewSettings = Settings#probe_settings{
			  canvas_width  = NewWidth,
			  canvas_height = NewHeight
										  },

	NewProbeRecord = ProbeRecord#virtual_probe{
		   render_settings=NewSettings },

	NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts }, State ),

	?wooper_return_state_only( NewState ).



% Sets the key (legend) settings, specified as a plain string (ex:
% "inside left") for the specified virtual probe.
%
% (oneway)
%
-spec setKeyOptions( wooper:state(), virtual_probe_id(), string() ) ->
						   oneway_return().
setKeyOptions( State, ProbeID, NewOptions ) ->

	{ ProbeRecord, Opts } = get_virtual_probe_and_options( ProbeID, State ),
	Settings = ProbeRecord#virtual_probe.render_settings,

	NewSettings = Settings#probe_settings{
		  key_options =text_utils:string_to_binary( NewOptions ) },

	NewProbeRecord = ProbeRecord#virtual_probe{
		   render_settings=NewSettings },

	NewState = set_virtual_probe( ProbeID, { NewProbeRecord, Opts }, State ),

	?wooper_return_state_only( NewState ).



% Sends the specified results to the caller (generally the result manager);
% implies generating the results of all virtual probes.
%
% Specified options are ignored, as each virtual probe is already associated to
% relevant ones.
%
% (const request, for synchronous yet concurrent operations)
%
-spec sendResults( wooper:state(), list() ) ->
	request_return( { pid(), 'no_result' } | { pid(), 'archive', binary() } ).
sendResults( State, _Options ) ->

	true = ?getAttr(result_produced),
	false = ?getAttr(result_collected),

	% By construction all virtual probes are wanted (this is checked when they
	% are created):
	%
	% (here, list of {_Key=ID,_Value={ProbeRecord,Opts} elements)
	%
	KeyValueList = dict:to_list( ?getAttr(probe_table) ),

	% Ideally, this should be done:
	% - in parallel
	% - in a distributed way, from each node each table was created on

	%io:format( "Probe table:~n~p~n", [ KeyValueList ] ),

	Files = lists:foldl( fun( { _Id, { ProbeRecord, Opts } }, L ) ->
			L ++ manage_probe_result( ProbeRecord, Opts, State ) end,
			_InitialAcc=[], _IteratedList=KeyValueList ),

	CollectedState = setAttribute( State, result_collected, true ),

	case Files of

		[] ->
			?wooper_return_state_result( CollectedState,
										 { self(), no_result } );

		FilenameList ->

			%io:format( "Data-logger to send an archive of: ~s, from ~p.~n",
			%		  [ text_utils:string_list_to_string( FilenameList ),
			%		   file_utils:get_current_directory() ] ),

			Bin = file_utils:files_to_zipped_term( FilenameList ),

			?wooper_return_state_result( CollectedState,
										 { self(), archive, Bin } )

	end.



% Manages the result from specified probe, with specified options.
%
% Returns a list of corresponding files, to be retrieved to the user node.
%
% (const helper function)
%
manage_probe_result( ProbeRecord, [ plot_only ], State ) ->

	% Generates everything, but select just the report for sending:
	{ _DataFilename, _CommandFilename, ReportFilename } =
		generate_report( ProbeRecord, State ),

	[ ReportFilename ];

manage_probe_result( ProbeRecord, [ data_only ], State ) ->

	% Generates data and command, and send them, no report:

	DataFilename = generate_data_file( ProbeRecord, ?getAttr(meta_data) ),

	ProbeBasename = text_utils:binary_to_string(
							ProbeRecord#virtual_probe.name ),

	CommandFilename = class_Probe:generate_command_file(
			ProbeBasename,
			ProbeRecord#virtual_probe.render_settings,
			ProbeRecord#virtual_probe.curve_entries,
			ProbeRecord#virtual_probe.zone_entries,
			?getAttr(output_dir) ),

	[ DataFilename, CommandFilename ];

manage_probe_result( ProbeRecord, [ data_and_plot ], State ) ->

	% Generates and sends everything:
	{ DataFilename, CommandFilename, ReportFilename } =
		generate_report( ProbeRecord, State ),

	[ DataFilename, CommandFilename, ReportFilename ].



% Generates a report corresponding to the current state of the specified virtual
% probe, and displays the result (the image) to the user.
%
% (request)
%
-spec generateReport( wooper:state(), virtual_probe_id() ) ->
			request_return( 'probe_report_generated' ).
generateReport( State, ProbeID ) ->
	generateReport( State, ProbeID, _DisplayWanted=true ).



% Generates a report corresponding to the current state of this probe.
%
% DisplayWanted is a boolean telling whether the generated report will be
% displayed to the user (if true).
%
% Returns the 'probe_report_generated' atom, merely for synchronisation purpose.
%
% (request)
%
-spec generateReport( wooper:state(), virtual_probe_id(), boolean() ) ->
			request_return( 'probe_report_generated' ).
generateReport( State, ProbeID, DisplayWanted ) ->

	{ _DataFilename, _CommandFilename, ReportFilename } =
		generate_report_from_id( ProbeID, State ),

	case DisplayWanted of

		true ->
			executable_utils:display_png_file( ReportFilename );

		false ->
			ok

	end,

	?wooper_return_state_result( State, probe_report_generated ).





% Generic interface.


% 'Static' methods (module functions):


% Creates the main (default) data-logger on specified node, specified as an
% atom.
%
% Note: the created instance is linked to the caller process.
%
% (static)
%
-spec create_main_datalogger( net_utils:atom_node_name(),
							 class_ResultManager:meta_data() ) -> pid().
create_main_datalogger( Node, MetaData ) ->
	remote_synchronous_timed_new_link( Node, ?default_data_logger_name,
									   MetaData ).



% Returns the PID of the main datalogger, which is to be created by default at
% deployment time.
%
-spec get_main_datalogger() -> pid().
get_main_datalogger() ->

	try basic_utils:wait_for_global_registration_of(
		  ?default_data_logger_name ) of

		Pid ->
			Pid

	catch

		_Exception ->
			throw( main_data_logger_not_found )

	end.



% Creates (synchronously) a new (virtual) probe, using the (default) main data
% logger:
%
% - ProbeName is the name, as a plain string, of that virtual probe (useful to
% match against the result specification)
%
% - CurveNames is a list containing the ordered names (as plain strings) of each
% curve to be drawn (hence the probe will expect receiving data in the form
% { Tick, {V1,V2,..} }); ex: [ "First curve", "Second curve" ]
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName, {
% ExtendedCurveNameOne, ExtendedCurveNameTwo } } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example { "My Zone", { "First curve", 'abscissa_bottom' } } defines a zone
% named "My Zone" and delimited by the curve named "First curve" and the
% abscissa axis (note: the order between the two elements defining a zone does
% not matter)

% - Title will be the graph title
%
% - XLabel will be the label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns either:
%
% - if the name of that virtual probe is acknowledged as a wanted result by the
% result manager, a { DataLoggerPid, VirtualProbeID } pair, where DataLoggerPid
% is the PID of the main data logger and VirtualProbeID is the virtual probe
% identifier, both of which are needed to send samples afterwards
%
% - if the result manager determined that this virtual probe is of no use in
% terms of results, the atom 'non_wanted_virtual_probe' is returned
%
% Throws an exception on failure.
%
% This is the recommended function to call from the user code, possibly as a
% replacement to the creation of a class_Probe instance.
%
% (static)
%
-spec create_virtual_probe( probe_name(), [ class_Probe:declared_curve_name() ],
	 class_Probe:zone_entries(), text_utils:title(),
	 text_utils:label(), text_utils:label() ) ->
			   'non_wanted_virtual_probe' | { pid(), virtual_probe_id() }.
create_virtual_probe( ProbeName, CurveEntries, ZoneEntries, Title, XLabel,
					  YLabel ) ->

	BinProbeName = text_utils:string_to_binary( ProbeName ),

	% Created preferably the Mnesia files on the local node (i.e. the caller
	% node, not necessarily the data-logger one) for better performances:
	TargetNode = net_utils:localnode(),

	DataLoggerPid = get_main_datalogger(),

	DataLoggerPid ! { createVirtualProbe, [ BinProbeName, TargetNode,
			CurveEntries, ZoneEntries, Title, XLabel, YLabel ], self() },

	receive

		{ wooper_result, non_wanted_virtual_probe } ->
			non_wanted_virtual_probe;

		{ wooper_result, ProbeID } when is_integer( ProbeID ) ->
			{ DataLoggerPid, ProbeID }

	end.



% Sends the specified sample data for the specified tick to the targeted probe,
% based on the specified "probe reference" (first parameter), which is the value
% returned by the result manager in answer to the initial creation request for
% that probe.
%
% This reference is either an actual PID (then data will be sent by this method)
% or the 'non_wanted_probe' atom, in which case nothing will be done.
%
% Note: this static method is to be used for basic probes, not virtual ones.
%
% (static method)
%
-spec send_data(
	 'non_wanted_virtual_probe' | { pid(), virtual_probe_id() } | table_name(),
	 probe_tick(), sample_data() ) -> basic_utils:void().

% In this case the probe creation was not acknowledged by the result manager:
send_data( non_wanted_virtual_probe, _Tick, _Samples )  ->
	undefined;

% In this case the user specified directly the probe reference returned by the
% creation request:
send_data( _ProbeRef={ DataLoggerPid, ProbeID }, Tick, Samples ) ->
	%io:format( "ProbeRef = ~p", [ ProbeRef ] ),
	DataLoggerPid ! { setData, [ ProbeID, Tick, Samples ] };

% In this case the user specified directly only the (supposedly local) table,
% thus we can shortcut the data-logger:
%
% (we added the guard to avoid mistakes between probe table and probe
% identifiers)
send_data( ProbeTable, Tick, Samples ) when is_atom( ProbeTable ) ->
	%io:format( "ProbeTable = ~p", [ ProbeTable ] ),
	set_data_helper( ProbeTable, Tick, Samples ).



% Registers specified data sample for specified tick.
%
% This is a synchronous (blocking) call, as it is evaluated directly in - and
% from - the calling process.
%
% The specified sample will overwrite any previously defined entry for that
% tick. See merge_data_synchronous/3 if not wanting this.
%
% The operation should preferably be done directly from the same node as the one
% from which the corresponding virtual probe was created, since then the sample
% table will be local.
%
% (static)
%
-spec set_data_synchronous( table_name(), probe_tick(), sample_data() ) ->
								  basic_utils:void().
set_data_synchronous( ProbeTable, Tick, Sample ) ->
	set_data_helper( ProbeTable, Tick, Sample ).



% Merges specified data sample for specified tick.
%
% This is a synchronous (blocking) call, as it is evaluated directly in - and
% from - the calling process.
%
% Any sample element not set to undefined in the specified sample will fill any
% sample element set to undefined at the same index on the already existing
% sample entry (if any). If the pre-existing value is not undefined, then an
% exception is thrown (even if we try to replace that value with the same
% value). See set_data_synchronous/3 if knowing that no prior entry can exist,
% or if not wanting to merge but to replace past sample entry.
%
% The operation should preferably be done directly from the same node as the one
% from which the corresponding virtual probe was created, since then the sample
% table will be local.
%
% Note: merging samples involves a read-modify-write operation, thus a
% transaction, thus it is far more expensive than just setting data.
%
% (static)
%
-spec merge_data_synchronous( table_name(), probe_tick(), sample_data() ) ->
								  basic_utils:void().
merge_data_synchronous( ProbeTable, Tick, Sample ) ->
	merge_data( ProbeTable, Tick, Sample ).




% Section for helper functions (not methods).


% Ensures that the Mnesia database is activated.
% Returns an updated state.
%
% (helper function)
%
-spec ensure_database_activated( wooper:state() ) -> wooper:state().
ensure_database_activated( State ) ->

	case ?getAttr(database_activated) of

		true ->
			%io:format( "Database already activated.~n" ),
			State;

		false ->
			%io:format( "Activating database.~n" ),
			DeployPid = class_DeploymentManager:get_deployment_manager(),

			DeployPid ! { activateDatabase, [], self() },
			receive

				{ wooper_result, database_already_running } ->
					%io:format( "(was already running).~n" ),
					ok;

				{ wooper_result, database_started } ->
					%io:format( "(had to be started).~n" ),
					ok

			end,

			%io:format( "Database activated.~n" ),

			% We record here the PID of the deployment manager, as we will need
			% it when deleting this instance, whereas this deployment manager
			% may have already been deleted.

			% We prefer sending a message to a defunct process rather than
			% looking up by name the deployment manager and time-out in doing
			% so:
			setAttributes( State, [ { database_activated, true },
									{ deployment_manager_pid, DeployPid } ] )

	end.



% Returns the Mnesia name (as an atom) for the table corresponding to specified
% probe ID.
%
% (helper function)
%
-spec get_table_name_for( virtual_probe_id() ) -> table_name().
get_table_name_for( Id ) ->
	% Avoids the need for flatten:
	list_to_atom( "virtual_probe_" ++ text_utils:integer_to_string( Id ) ).



% Returns a reference to the virtual probe (i.e. the virtual probe record) whose
% identifier is Id.
%
% (helper function)
%
-spec get_virtual_probe( virtual_probe_id(), wooper:state() ) ->
							   virtual_probe().
get_virtual_probe( Id, State ) ->

	case dict:find( _Key=Id, ?getAttr(probe_table) ) of

		{ ok, { ProbeRecord, _Opts } } ->
			ProbeRecord;

		error ->
			throw( { unknown_virtual_probe_id, Id } )

	end.



% Returns a reference to the virtual probe (i.e. the virtual probe record) whose
% identifier is Id, and the associated options.
%
% (helper function)
%
-spec get_virtual_probe_and_options( virtual_probe_id(), wooper:state() ) ->
									 { virtual_probe(), probe_options() }.
get_virtual_probe_and_options( ID, State ) ->

	case dict:find( _Key=ID, ?getAttr(probe_table) ) of

		%{ ok, { ProbeRecord, Opts } } ->
		{ ok, ProbePair } ->
			ProbePair;

		error ->
			throw( { unknown_virtual_probe_id, ID } )

	end.



% Returns an updated state, in which the specified probe ID is now associated to
% the specified probe (record and options).
%
% (helper function)
%
-spec set_virtual_probe( virtual_probe_id(),
	   { virtual_probe(), probe_options() }, wooper:state() ) -> wooper:state().
set_virtual_probe( ProbeID, NewProbePair, State ) ->

	%io:format( "set_virtual_probe for probe #~B: ~p.~n",
	%		   [ ProbeID, NewProbePair ] ),

	NewTable = dict:store( _Key=ProbeID, _Value=NewProbePair,
						   ?getAttr(probe_table) ),

	setAttribute( State, probe_table, NewTable ).



% Generates the appropriate file containing the data of specified probe.
%
% Returns the name, as a plain string, of the data file.
%
% (helper function)
%
-spec generate_data_file( virtual_probe(), class_ResultManager:meta_data() )
						-> file_utils:file_name().
generate_data_file( ProbeRecord, MetaData ) ->

	ProbeBasename = text_utils:binary_to_string(
							ProbeRecord#virtual_probe.name ),

	DataFilename = class_Probe:get_data_filename( ProbeBasename ),

	File = file_utils:open( DataFilename, [ write, delayed_write, raw ] ),

	class_Probe:write_header( File, ProbeRecord#virtual_probe.curve_entries,
			   ProbeRecord#virtual_probe.zone_entries,
			   ProbeRecord#virtual_probe.render_settings,
			   ProbeBasename, MetaData ),

	% Does not show any specific order, unless the table is an ordered_set:
	%
	% (anyway we prefer not using transactions, as it is only read access
	% (moreover accesses are synchronized, as they are done by the data logger)
	%
	%Query = qlc:q( [ { Tick, Sample } ||
	%			{ probe_sample, Tick, Sample } <- mnesia:table( Table ) ] ),
	%F = fun() -> qlc:e( Query ) end,
	%{ atomic, Val } = mnesia:transaction( F ),

	%mnesia:info(),

	% Not needing transactions, thus relying on dirty operations:
	%Val = mnesia:transaction( fun() ->
	%					get_all( Table, mnesia:first( Table ), [] ) end ),

	Table = ProbeRecord#virtual_probe.table_name,

	% We want to have { Tick, Sample } pairs sorted by increasing tick:
	% (dirty_first/1 may fail)
	SortedPairs = get_ordered_samples( Table, mnesia:dirty_first( Table ),
									   gb_trees:empty() ),

	%io:format( "Sorted pairs = ~p~n", [ SortedPairs ] ),

	case SortedPairs of

		[] ->
			throw( { no_available_data_sample,
					ProbeRecord#virtual_probe.table_name } );

		_NonEmpty ->
			ok

	end,

	write_data( File, SortedPairs ),

	ok = file:close( File ),

	DataFilename.



% Extracts and sorts chronologically the table entries, and returns an ordered
% list.
%
% (helper function)
%
get_ordered_samples( _Table, '$end_of_table', Tree ) ->
	gb_trees:to_list( Tree );

get_ordered_samples( Table, Key, Tree ) ->

	% An actual entry is available here:
	[ { probe_sample, _Key, Sample } ] = mnesia:dirty_read( Table, Key ),

	NewTree = gb_trees:insert( Key, Sample, Tree ),

	NewKey = mnesia:dirty_next( Table, Key ),

	get_ordered_samples( Table, NewKey, NewTree ).



% Writes the probe data, row by row.
%
% (helper function)
%
write_data( _File, _SortedPairs=[] ) ->
	ok;

write_data( File, _SortedPairs=[ { Tick, Sample } | T ] ) ->
	ok = file:write( File, class_Probe:format_row( Tick, Sample ) ),
	write_data( File, T ).


% Merges newer samples into the older one, and returns the merged sample.
%
% (helper function)
%
merge_samples( NewSample, OldSample ) ->
	merge_samples( erlang:tuple_to_list( NewSample ),
				  erlang:tuple_to_list( OldSample ), _Acc=[] ).


merge_samples( _New=[], _Old=[], Acc ) ->
	erlang:list_to_tuple( lists:reverse( Acc ) );

merge_samples( [ undefined | Tnew ], [ Any | Told ], Acc ) ->
	% Keep 'Any', whatever it is (value or undefined):
	merge_samples( Tnew, Told, [ Any | Acc ] );

merge_samples( [ Value | Tnew ], [ undefined | Told ], Acc ) ->
	% Found 'undefined', to be replaced by newer Value:
	merge_samples( Tnew, Told, [ Value | Acc ] );

merge_samples( New, Old, Acc ) ->
	% Here we have two values (either the same or not) instead of at least one
	% 'undefined', thus the merge is refused. Reconstructs the two samples:
	Beginning = lists:reverse( Acc ),
	throw( { sample_merging_conflict,
			erlang:list_to_tuple( Beginning ++ Old ),
			erlang:list_to_tuple( Beginning ++ New ) } ).



% Registers directly the specified data sample for specified tick.
%
% The operation will not go through the data-logger (thus avoiding this possible
% bottleneck) and should be purely local to the node of the caller (provided
% indeed it is the process that declared this virtual probe, as the table
% corresponding to a virtual probe is created in the node of its owner).
%
% Does not return anything useful.
%
% (helper function)
%
-spec set_data_helper( table_name(), probe_tick(), sample_data() ) ->
							 basic_utils:void().
set_data_helper( ProbeTable, Tick, Sample ) ->

	% No sanity check in term of element count done here.

	SampleEntry = #probe_sample{ sample_tick=Tick, sample_data=Sample },

	% Even concurrent dirty writes should not be a problem:
	ok = mnesia:dirty_write( ProbeTable, SampleEntry ).



% Merges directly the specified data sample for specified tick.
%
% The operation will not go through the data-logger (thus avoiding this possible
% bottleneck) and should be purely local to the node of the caller (provided
% indeed it is the process that declared this virtual probe, as the table
% corresponding to a virtual probe is created in the node of its owner).
%
% Does not return anything useful.
%
% (helper function)
%
-spec merge_data( table_name(), probe_tick(), sample_data() ) ->
							 basic_utils:void().
merge_data( ProbeTable, Tick, Sample ) ->

	% No sanity check in term of element count done here.

	F = fun() ->

				% We are thinking to an update, so we aim at at least a write
				% lock (as not currently replicated, not using sticky writes):
				case mnesia:read( _Tab=ProbeTable, _Key=Tick, _LockKind=write )
				of

					[] ->

						% No previous record, just write unconditionally (we
						% could have used a dirty write instead, if we had
						% known):
						SampleEntry = #probe_sample{ sample_tick=Tick,
													 sample_data=Sample },

						mnesia:write( _Tab=ProbeTable, _Record=SampleEntry,
									  _LockKind=write );


					[ PreviousEntry ] ->

						% Merging newer into older:
						MergedSample = merge_samples( _New=Sample,
								_Old=PreviousEntry#probe_sample.sample_data ),

						SampleEntry = #probe_sample{ sample_tick=Tick,
													 sample_data=MergedSample },

						mnesia:write( _Tab=ProbeTable, _Record=SampleEntry,
									 _LockKind=write )

				end

		end,

	{ atomic, _Res } = mnesia:transaction( F ).



% Generates the report for the specified virtual probe.
%
% Returns a triplet {DataFilename,CommandFilename,ReportFilename}.
%
% (helper function)
%
-spec generate_report_from_id( virtual_probe_id(), wooper:state() ) ->
   { file_utils:file_name(), file_utils:file_name(), file_utils:file_name() }.
generate_report_from_id( ProbeID, State ) ->
	ProbeRecord = get_virtual_probe( ProbeID, State ),
	generate_report( ProbeRecord, State ).



% Actual generation of the report for the specified virtual probe.
%
% Returns a { DataFilename, CommandFilename, ReportFilename } triplet.
%
% (helper)
%
-spec generate_report( virtual_probe(), wooper:state() ) ->
   { file_utils:file_name(), file_utils:file_name(), file_utils:file_name() }.
generate_report( ProbeRecord, State ) ->

	% Creates an appropriate base for the output filenames:
	ProbeBasename = text_utils:binary_to_string(
						  ProbeRecord#virtual_probe.name ),

	%?info_fmt( "Generation of report requested for virtual probe "
	%		  "named '~s'.", [ ProbeBasename ] ),

	%io:format( "Generation of report requested for virtual probe "
	%		  "named '~s'.", [ ProbeBasename ] ),

	CommandFilename = class_Probe:generate_command_file( ProbeBasename,
			ProbeRecord#virtual_probe.render_settings,
			ProbeRecord#virtual_probe.curve_entries,
			ProbeRecord#virtual_probe.zone_entries,
			?getAttr(output_dir)
														),

	DataFilename = generate_data_file( ProbeRecord, ?getAttr(meta_data) ),

	ReportFilename = class_Probe:get_report_filename( ProbeBasename ),

	% Gnuplot might issue non-serious warnings.
	% Generates a PNG:
	case os:cmd( "gnuplot " ++ CommandFilename ) of

		[] ->
			ok;

		Message ->
			?warning_fmt( "Report generation for '~s' resulted in "
						  "following output: ~s.", [ ReportFilename, Message ] )

	end,

	{ DataFilename, CommandFilename, ReportFilename }.
