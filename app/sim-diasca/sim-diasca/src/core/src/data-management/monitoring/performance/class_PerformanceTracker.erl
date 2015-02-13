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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)




% Performance Tracker.


% Its roles are to trace:
%
% 1. the memory consumptions on all nodes (i.e. on the user one and on each
% computing nodes)
%
% Thus there will be one (basic) facility probe per node, showing the following
% memory consumptions:
%
% - the available RAM memory (which is: free memory + buffers + cache)
%
% - the memory allocated to the Erlang virtual machine (i.e. the memory
% currently used by the simulator, plus any other Erlang program being executed
% at the same time)
%
% - the memory used by the other applications (i.e. all non-Erlang applications)
%
% - the used swap
%
% 2. the number of Erlang processes and instances (overall and on each node)
%
% 3. the total and per-class instance count (aggregated, i.e. regardless of
% their dispatching on computing nodes)
%
%
% All these metrics are tracked over time twice, based on simulation time
% (i.e. tick) and on wall-clock time (i.e. real time).


% Data collected in wall-clock time is sent periodically, but, depending on the
% computer load, the sampling might be non-uniform, time-wise. However, the
% values correspond to the relevant time (i.e. the time at which they were
% measured).


% The performance tracker is mostly expected to run on the user node, so that,
% in case of simulation crash, post-mortem analysis of left over files is made
% easier.



% Implementation notes.

% All probes used by this tracker are facility probes: they are not simulation
% results.
%
% The command file for each of these probes is created as soon as possible, so
% that, should this tracker crash (ex: the simulation as a whole crashed), they
% are nevertheless available.



% See also class_PerformanceTracker.hrl and class_PerformanceTracker_test.erl
%
-module(class_PerformanceTracker).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, PerformanceTrackerName, RegistrationType,
		ResultDirInfo ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, start/5, stop/1, setTickerPeriod/2,
		 onNewTick/2, onNewDiasca/3, generateMonitoringReports/1 ).


% Static method declarations (to be directly called from module)
-define( wooper_static_method_export, get_tracker/0 ).


% To avoid warnings:
-export([ record_new_sample/1 ]).




% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks,).


% Must be included before class_TraceEmitter header:
% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For performance tracker name, registration type and performance tracker
% settings:
%
-include("class_PerformanceTracker.hrl").


% For host_{static,dynamic}_info:
-include("system_utils.hrl").


-define(TraceEmitterCategorization,"Core.Tracker.Performance").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% The class-specific attributes of the performance tracker are:
%
% - started :: boolean(): tells whether this performance tracker is started
%
% - tracker_result_dir :: file_utils:directory_name() :: is the name of the
% directory in which this tracker should write its data files
%
% - root_time_manager_pid :: basic_utils:maybe( pid() ): the PID of the root
% time manager; initially, its value is undefined and then is set in start/4;
% this PID is obtained from the deployment manager
%
% - current_tick_offset :: class_TimeManager:tick_offset() allows this tracker
% to keep track of simulation ticks, as a listener of the root time manager
%
% - load_balancer_pid :: basic_utils:maybe( pid() ): the PID of the load
% balancer
%
% - instance_trackers :: [ pid() ]: a list of the PIDs of the instance trackers
% on all nodes (computing ones and user one - we want to monitor it as well,
% resource-wise)
%
% - resources_per_node_entries :: [ {net_utils:atom_node_name(),
%  node_static_info(), pid(), pid() } ] is the ordered list of nodes and
%  associated information (static one, and resource probe PIDs - respectively
%  tick and time), so that we can keep track of the order in which curves are
%  listed in samples for all probes aggregating node information, and so that we
%  can associate to each node its static settings and its two resource probes,
%  respectively over tick and over time; note that the first entry corresponds
%  to the user node, while the others are for computing nodes
%
% - nodes_in_tick_probe :: pid() is the PID of the probe tracking the Erlang
% process and instance count on each node and their total number, over
% simulation ticks; initially, its value is undefined and then is created when
% computing nodes are set
%
% - nodes_in_time_probe :: pid() is the PID of the probe tracking the Erlang
% process and instance count on each node and the total number, over wall-clock
% time; initially, its value is undefined and then is created when computing
% nodes are set
%
% - ordered_class_names :: [ class_name() ] is the ordered list of class names,
% so that we can keep track of the order in which curves are listed in samples
% for the two instance probes, over tick and over time; note that there is an
% implicit first entry (in display order) which corresponds to the overall
% instance count (regardless of any class)
%
% - classes_in_tick_probe :: pid() is the PID of the probe tracking the number
% of instances for each WOOPER class and the total one (in first position), over
% simulation ticks; curves are added as classes are discovered
%
% - classes_in_time_probe :: pid() is the PID of the probe tracking the number
% of instances for each WOOPER class and the total one (in first position), over
% wall-clock time; curves are added as classes are discovered
%
% - simulation_status: shows if the simulation is started. As this tracker is
% registered as a root time manager listener, it will receive a "simulation
% started" message from root time manager when simulation is started. Initially,
% this value is undefined
%
% - ticker_pid :: pid() is the PID of the ticker process, that triggers
% information updates
%
% - ticker_period :: unit_utils:milliseconds(): is the interval, in
% milliseconds, for performance tracker to send a new data sample to its probes




% Constructs a new performance tracker with following parameters:
%
% - PerformanceTrackerName is an atom
%
% - RegistrationScope: in 'local_only', 'global_only', 'local_and_global',
% 'none' depending on what kind of registration is requested
%
% - ResultDirInfo: either ResultDirName of { ResultDirName, do_not_create }
% where ResultDirName is the name of the result directory, as a plain string
%
-spec construct( wooper:state(), basic_utils:registration_name(),
		basic_utils:registration_scope(),
		file_utils:directory_name() |
			{ file_utils:directory_name(), 'do_not_create' } ) ->
					   wooper:state().
construct( State, ?wooper_construct_parameters )  ->

	%basic_utils:display( "Constructing performance tracker ~p",
	%						[ self() ] ),

	% Increases the chances that the tracker does not lag too much compared to
	% wall-clock time:
	erlang:process_flag( priority, _Level=high ),

	TraceState = class_TraceEmitter:construct( State,
					_EmitterName=atom_to_list( PerformanceTrackerName ) ),

	PreState = setAttribute( TraceState, trace_categorization,
				text_utils:string_to_binary( ?TraceEmitterCategorization ) ),

	RegistrationName = get_registration_name( PerformanceTrackerName ),

	basic_utils:register_as( RegistrationName, RegistrationType ),

	class_InstanceTracker:register_agent( RegistrationName ),

	ActualResultDirName = case ResultDirInfo of

		{ ResultDirName, do_not_create } ->
			ResultDirName;

		ResultDirName ->
			ResultDirName

	end,

	% Creating the result directory for the performance tracker:
	TrackerResultDir = file_utils:join( ActualResultDirName,
										"performance-monitoring" ),

	case ResultDirInfo of

		{ _ResultDirName, do_not_create } ->
			ok;

		_ ->
			file_utils:create_directory( TrackerResultDir )

	end,

	% The probes to monitor resources per node cannot be created here, as we do
	% not know yet the nodes of interest.

	{ InstancesPerNodeInTickProbe, InstancesPerNodeInTimeProbe } =
		create_node_probes( TrackerResultDir ),

	{ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe } =
		create_class_probes( TrackerResultDir ),


	%io:format( "Performance tracker ready.~n" ),

	% All memory-related and process-related probes will be created when the
	% this performance tracker will be started:
	FinalState = setAttributes( PreState, [

		{ started, false },
		{ tracker_result_dir, TrackerResultDir },
		{ root_time_manager_pid, undefined },
		{ current_tick_offset, undefined },
		{ load_balancer_pid, undefined },
		{ instance_trackers, [] },
		{ resources_per_node_entries, undefined },
		{ nodes_in_tick_probe, InstancesPerNodeInTickProbe },
		{ nodes_in_time_probe, InstancesPerNodeInTimeProbe },
		{ ordered_class_names, [] },
		{ classes_in_tick_probe, InstancesPerClassInTickProbe },
		{ classes_in_time_probe, InstancesPerClassInTimeProbe },
		{ simulation_status, undefined },
		{ ticker_pid, undefined },
		{ ticker_period, _Milliseconds=100 }

		] ),

	?send_info( FinalState, "Creating a performance tracker." ),

	FinalState.



% Overridden destructor to delete the performance tracker probes, and to
% unregister the tracker.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%basic_utils:display( "Deleting performance tracker ~w", [ self() ] ),

	% Class-specific actions:
	?info( "Deleting performance tracker." ),

	TickerState = stop_ticker( State ),

	% Deletes synchronously all probes:
	AllProbes = get_all_probes( TickerState ),

	[ class_Probe:delete_facility_probe( P ) || P <- AllProbes ],

	StoppedState = case ?getAttr(started) of

		   true ->
			   executeOneway( TickerState, stop );

		   false ->
			   State

	end,

	class_InstanceTracker:unregister_agent(),

	basic_utils:unregister( get_registration_name( ?performance_tracker_name ),
							?performance_tracker_registration_type ),

	?info( "Performance tracker is deleted." ),

	%basic_utils:display( "Performance tracker ~w deleted", [ self() ] ),

	% Then call the direct mother class counterparts and allow chaining:
	StoppedState.





% Method method section.


% Starts the performance tracker:
%
% - RootTimeManagerPid is the PID of the root time manager, needed to monitor
% the main simulation events
%
% - Nodes is a list of atom node names, starting with the user node
%
% - { LoadBalancerPid, LoadBalancerNode } allows to interact with the load
% balancer
%
% (oneway)
%
-spec start( wooper:state(), pid(), [ net_utils:atom_node_name() ],
			[ pid() ], pid() ) -> oneway_return().
start( State, RootTimeManagerPid, Nodes=[ UserNode | ComputingNodes ],
	  InstanceTrackers, LoadBalancerPid )->

	% Requests early all remote information:
	Message = { getStaticResourceInformation, [], self() },

	[ I ! Message || I <- InstanceTrackers ],

	% We need to timestamp probe samples:
	RootTimeManagerPid ! { addTimeListener, self() },

	% Creates blanck node entries to preserve the intended node order:
	ResourcesPerNodeEntries = [ { N, undefined, undefined, undefined }
							   || N <- Nodes ],

	% Declare a curve corresponding to each computing node on all relevant
	% probes:
	MultiNodesProbes =  [ ?getAttr(nodes_in_tick_probe),
						  ?getAttr(nodes_in_time_probe) ],

	[

	 [
	 begin

		 NodeCurveName = io_lib:format( "Computing Node ~s", [ N ] ),
		 P ! { addCurve, [ NodeCurveName ] }

	 end
		  || N <- ComputingNodes ]

		|| P <- MultiNodesProbes ],



	% The probe creation requires the static information to be already
	% available:
	UpdatedResourcesPerNodeEntries = wait_static_node_info(
		   ResourcesPerNodeEntries, InstanceTrackers, UserNode,
		   ?getAttr(tracker_result_dir) ),


	% Triggers the first update, before any instance is created yet, as the
	% ticker will wait for a full period before issuing its first request:
	self() ! record_new_sample,

	TickerState = launch_ticker( ?getAttr(ticker_period), State ),

	StartedState = setAttributes( TickerState, [

			{ started, true },
			{ instance_trackers, InstanceTrackers },
			{ resources_per_node_entries, UpdatedResourcesPerNodeEntries },
			{ root_time_manager_pid, RootTimeManagerPid },
			{ load_balancer_pid, LoadBalancerPid }

										  ] ),

	?wooper_return_state_only( StartedState ).



% Waits for the static node information to be received, and updates accordingly
% the node entries (with static information to probes).
%
wait_static_node_info( ResourcesPerNodeEntries, _InstanceTrackers=[],
					  _UserNode, _TrackerDir ) ->
	ResourcesPerNodeEntries;

wait_static_node_info( ResourcesPerNodeEntries, InstanceTrackers, UserNode,
					  TrackerDir ) ->

	receive

		{ wooper_result, { NodeName, NodeStaticInfo, InstanceTrackerPid } } ->

			{ TickProbe, TimeProbe } = create_node_resource_probes(
						NodeName, NodeStaticInfo, UserNode, TrackerDir ),

			UpdatedEntry = { NodeName, NodeStaticInfo, TickProbe, TimeProbe },

			% We update in-place a blanck entry:
			UpdatedResourcesPerNodeEntries = lists:keyreplace( _Key=NodeName,
					   _Index=1, ResourcesPerNodeEntries, UpdatedEntry ),

			wait_static_node_info( UpdatedResourcesPerNodeEntries,
						  lists:delete( InstanceTrackerPid, InstanceTrackers ),
						  UserNode, TrackerDir )

	end.



% To be called directly to stop the performance tracker, supposedly already
% started.
%
% (oneway)
%
-spec stop( wooper:state() ) -> oneway_return().
stop( State ) ->

	?getAttr(root_time_manager_pid) ! { removeTimeListener, self() },

	UpdatedState = stop_ticker( State ),

	?wooper_return_state_only( UpdatedState ).



% Called to stop (synchronously) the tracker sending of data to its probes.
%
% (helper)
%
-spec stop_ticker( wooper:state() ) -> wooper:state().
stop_ticker( State ) ->

	case ?getAttr(ticker_pid) of

		undefined ->
			%?warning( "Stop ticker request ignored: was not running." ),
			State;

		Pid ->

			?info( "Stopping the ticker." ),

			Pid ! { stop, self() },

			receive

				stopped ->
					ok

			end,

			% However there might be already 'record_new_sample' messages
			% sitting in the mailbox, let's flush them:
			basic_utils:flush_pending_messages( record_new_sample ),

			setAttribute( State, ticker_pid, undefined )

	end.



% Sets the ticker period, in milliseconds.
%
% The performance tracker will send data to its probes at this pace.
%
% Default period is 1s (1000 milliseconds).
%
% (oneway)
%
-spec setTickerPeriod( wooper:state(), unit_utils:milliseconds() ) ->
							 oneway_return().
setTickerPeriod( State, TickerPeriod ) ->

	% Changes dynamically the ticker frequency:
	StoppedState = stop_ticker( State ),

	TickerState = launch_ticker( TickerPeriod, StoppedState ),

	?wooper_return_state_only( TickerState ).



% Notifies this tracker (as a time listener) that a new tick is being scheduled.
%
% (oneway)
-spec onNewTick( wooper:state(), class_TimeManager:tick_offset() ) ->
					   oneway_return().
onNewTick( State, NewTickOffset ) ->
	?wooper_return_state_only(
				 setAttribute( State, current_tick_offset, NewTickOffset ) ).



% Notifies this tracker (as a time listener) that a new tick is being scheduled.
%
% (oneway)
-spec onNewDiasca( wooper:state(), class_TimeManager:tick_offset(),
		  class_TimeManager:diasca() ) -> oneway_return().
onNewDiasca( State, _TickOffset, _NewDiasca ) ->

	% No-op, the performance tracker does not track diascas currently:
	?wooper_return_state_only( State ).




% Called to generate all performance monitoring reports.
%
% (request)
%
-spec generateMonitoringReports( wooper:state() ) ->
									   request_return( 'report_generated' ).
generateMonitoringReports( State ) ->

	%io:format( "Generating performance monitoring reports.~n" ),

	% Checking we are the one (a bit useless):
	% true = (class_PerformanceTracker:get_tracker() =:= self()),

	% Done synchronously:
	StoppedState = stop_ticker( State ),

	% Generated in parallel:
	Message = { generateReport, _DisplayWanted=false, self() },

	ProbeList = send_to_all_probes( Message, StoppedState ),

	%io:format( "Requesting the generation of report for ~B probe(s): ~p.~n",
	%		  [ length( ProbeList ), ProbeList ] ),

	basic_utils:wait_for( _Message={ wooper_result, probe_report_generated },
		 _Count=length( ProbeList ), _Duration=2000,
		 "Still waiting for ~B probe()s to complete their report generation." ),

	?info( "Performance report correctly generated." ),

	case executable_utils:is_batch() of

		true ->
			ok;

		false ->
			executable_utils:browse_images_in( ?getAttr(tracker_result_dir) )

	end,

	%io:format( "Performance monitoring reports generated.~n" ),

	?wooper_return_state_result( StoppedState, report_generated ).





% Section for static methods.


% Returns either the PID of the performance tracker (if any), or undefined.
%
% (static)
%
-spec get_tracker() -> pid() | 'not_registered'.
get_tracker() ->

	RegisteredName = get_registration_name( ?performance_tracker_name ),

	basic_utils:is_registered( RegisteredName ).






% Helper functions.



-spec create_node_resource_probes( net_utils:atom_node_name(),
		system_utils:host_static_info(), net_utils:atom_node_name(),
						  file_utils:directory_name() ) -> { pid(), pid() }.
create_node_resource_probes( NodeName, NodeStaticInfo, UserNode,
							 TrackerResultDir ) ->

	TotalRam = system_utils:interpret_byte_size_with_unit(
				 NodeStaticInfo#host_static_info.total_ram ),

	TotalSwap = system_utils:interpret_byte_size_with_unit(
				  NodeStaticInfo#host_static_info.total_swap ),

	TotalUsed  = io_lib:format( "Total Memory Used (over a total of ~s)",
								[ TotalRam ] ),


	ErlangVersion = NodeStaticInfo#host_static_info.erlang_version,

	ErlangUsed = io_lib:format( "Memory Used by the Erlang VM (version ~s)",
								[ ErlangVersion ] ),

	SwapUsed = io_lib:format( "Swap Used (in GiB, over a total of ~s)",
							  [ TotalSwap ] ),

	CPUUtilization = "Percentage of CPU Used (Non-idle)",

	ProcessCount = io_lib:format( "Erlang Process Count (spread over ~B cores)",
							 [ NodeStaticInfo#host_static_info.core_count ] ),

	CurveNames = [ TotalUsed, ErlangUsed, SwapUsed, CPUUtilization,
				   ProcessCount ],

	Zones = [

			 { "Total available memory (free+buffers+cache)",
			  { 'abscissa_top', TotalUsed } },

			 { "Memory used by all other programs",
			  { ErlangUsed, TotalUsed } },

			 { io_lib:format(
					"Memory used for the simulation (over a total of ~s)",
					[ TotalRam ] ),

			  { 'abscissa_bottom', ErlangUsed } }

			 ],


	% Do not suppose anything about this tracker location:
	NodeDescription = case UserNode of

			NodeName ->
				"user";

			_ ->
				"computing"

	end,



	ResourceOverTickProbe = class_Probe:create_facility_probe(

		 { _TickProbeName=io_lib:format( "Resource Consumption Probe Over "
			  "Simulation Time for ~s node ~s", [ NodeDescription, NodeName ] ),
		   [ { create_command_file_initially, false } ] },

		 CurveNames,

		 Zones,

		 _TickTitle=io_lib:format( "Monitoring memory and swap consumptions "
								   "on ~s node ~s over wall-clock time",
								   [ NodeDescription, NodeName ] ),

		 _TickXLabel="Simulation duration, in simulation ticks",

		 _TickYLabel="Percentages of Memory Consumption, "
					 "Swap and CPU Use and Process Count",

		TrackerResultDir

		 ),

	[ ResourceOverTickProbe ! M || M <- [
			{ setKeyOptions, [ "bmargin center" ] },
			{ setOrdinateRange, [ 0, 100 ] } ,
			{ setPlotStyle, [ "linespoints" ] },
			setRotatedTickLabels,
			{ setPointSize, [ 2, _GenerateFile=true ] } ] ],


	ResourceOverTimeProbe = class_Probe:create_facility_probe(

		 {_TimeProbeName=io_lib:format( "Resource Consumption Probe Over "
					"Wallclock Time for ~s node ~s",
					[ NodeDescription, NodeName ] ),
		  [ { create_command_file_initially, false } ] },

		 CurveNames,

		 Zones,

		 _TimeTitle=io_lib:format( "Monitoring memory and swap consumptions "
							   "on ~s node ~s  over wall-clock time",
				[ NodeDescription, NodeName ] ),

		 _TimeXLabel="Wall-clock duration, in milliseconds",

		 _TimeYLabel="Percentages of Memory Consumption, "
					 "Swap and CPU Use and Process Count",

		TrackerResultDir

		 ),

	[ ResourceOverTimeProbe ! M || M <- [
			{ setKeyOptions, [ "bmargin center" ] },
			{ setOrdinateRange, [ 0, 100 ] } ,
			{ setPlotStyle, [ "linespoints" ] },
			setRotatedTickLabels,
			{ setPointSize, [ 2, _GenerateFile=true ] } ] ],

	{ ResourceOverTickProbe, ResourceOverTimeProbe }.





% Creates the two probes used to monitor per-node process and instance
% creations, over simulation ticks and over wallclock time.
%
% Each of these two probes will monitor all nodes at once (in a single plot),
% regarding instance count.
%
% Once the computing nodes will be known, the corresponding curves will be added
% to these probes.
%
-spec create_node_probes( file_utils:directory_name() ) -> { pid(), pid() }.
create_node_probes( TrackerResultDir ) ->

	InstancesPerNodeInTickProbe = class_Probe:create_facility_probe(

			{_TickProbeName="Per Node Instance Count Over Tick Probe",

			   [ { create_command_file_initially, true } ] },

			_TickCurveNames=[ "Overall Instance Count" ],

			_TickZones=[],

			_TickTitle="Monitoring the Overall and Per-Node Instance Count "
									 "Over Simulation Ticks",

			_TickXLabel="Simulation time, in ticks",

			_TickYLabel="Instance Count",

			TrackerResultDir ),


	InstancesPerNodeInTimeProbe = class_Probe:create_facility_probe(

			{_TimeProbeName="Per Node Instance Count Over Time Probe",

			   [ { create_command_file_initially, true } ] },

			_TimeCurveNames=[ "Overall Instance Count" ],

			_TimeZones=[],

			_TimeTitle="Monitoring the Overall and Per-Node Instance Count "
									 "Over Wall-clock Time",

			_TimeXLabel="Wall-clock duration, in milliseconds",

			_TimeYLabel="Instance Count",

			TrackerResultDir ),


	{ InstancesPerNodeInTickProbe, InstancesPerNodeInTimeProbe }.



% Creates the two probes used to monitor per-class instance creations over
% simulation ticks and over wallclock time.
%
-spec create_class_probes( file_utils:directory_name() ) -> { pid(), pid() }.
create_class_probes( TrackerResultDir ) ->

	InstancesPerClassInTickProbe = class_Probe:create_facility_probe(

			{_TickProbeName="Per Class Instance Count Over Tick Probe",

			   [ { create_command_file_initially, false } ] },

			_TickCurveNames=[ "Overall Instance Count" ],

			_TickZones=[],

			_TickTitle="Monitoring the Overall and Per-Class Instance Count "
									 "Over Simulation Ticks",

			_TickXLabel="Simulation time, in ticks",

			_TickYLabel="Instance Count",

			TrackerResultDir ),


	InstancesPerClassInTimeProbe = class_Probe:create_facility_probe(

			{_TimeProbeName="Per Class Instance Count Over Time Probe",

			   [ { create_command_file_initially, false } ] },

			_TimeCurveNames=[ "Overall Instance Count" ],

			_TimeZones=[],

			_TimeTitle="Monitoring the Overall and Per-Class Instance Count "
									 "Over Wall-clock Time",

			_TimeXLabel="Wall-clock duration, in milliseconds",

			_TimeYLabel="Instance Count",

			TrackerResultDir ),

	[ P ! { setRotatedTickLabels, _GenerateFile=true } ||
		P <- [ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe ] ],

	{ InstancesPerClassInTickProbe, InstancesPerClassInTimeProbe }.




% Main loop of the ticker process.
%
ticker_main_loop( PerformanceTrackerPid, TickerPeriod ) ->

	receive

		{ stop, CallerPid } ->
			%io:format( "The performance ticker received a stop request.~n" ),
			CallerPid ! stopped

	after TickerPeriod ->

			%io:format( "Tick!~n" ),

			% Tells, based on wall-clock time, the tracker to update its
			% statistics:
			PerformanceTrackerPid ! record_new_sample,

			ticker_main_loop( PerformanceTrackerPid, TickerPeriod )

	end.



% Returns the registering name of the performance tracker.
%
% (helper)
%
get_registration_name( PerformanceName ) ->
	PerformanceName.




% The following methods are called to set curves data and to send the data to
% the performance probe, at each ticker period (in wall-clock time).



% Called to send updated data to performance probes.
%
% Does not return anything useful.
%
-spec record_new_sample( wooper:state() ) -> basic_utils:void().
record_new_sample( State ) ->

	% First, requests the information from the load balancer, early as we need
	% the current tick offset first for all curves:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	LoadBalancerPid ! { getInstanceCounts, [], self() },

	% Same thing (in parallel) for node-related information:

	Request = { getDynamicResourceInformation, [], self() },

	InstanceTrackers = ?getAttr(instance_trackers),

	[ I ! Request || I <- InstanceTrackers ],

	{ CurrentWallclockTime, _T } = statistics( wall_clock ),

	% First, wait for the information from the load balancer, notably the tick
	% offset (set to zero if not defined):
	%
	{ InstancesPerClass, InstancesPerNode } = receive

		{ wooper_result, { instance_counts, InstancesClass, InstancesNode } } ->
			{ InstancesClass, InstancesNode }

	end,

	CurrentTickOffset = case ?getAttr(current_tick_offset) of

							undefined ->
								0;

							Tick ->
								Tick

	end,

	%io:format( "Tick offset #~p:~n- InstancesPerClass = ~p~n"
	%						"- InstancesPerNode = ~p~n",
	%			[ CurrentTickOffset, InstancesPerClass, InstancesPerNode ] ),


	{ ClassState, Count } = manage_class_monitoring( CurrentTickOffset,
		 CurrentWallclockTime, InstancesPerClass, State ),

	% Note the pattern-matching on Count, to check correctness of instance
	% count:
	{ NodeState, Count } = manage_node_monitoring( CurrentTickOffset,
		 CurrentWallclockTime, InstancesPerNode, ClassState ),

	% Unsorted list:
	NodeDynInfos = wait_dynamic_info_from_trackers( InstanceTrackers, NodeState,
													_Acc=[] ),

	LocalState = manage_node_dynamic_info( CurrentTickOffset,
					CurrentWallclockTime, NodeDynInfos, NodeState ),

	?wooper_return_state_only( LocalState ).




% Helper function for recording new samples:

% Helper function, const state.


% Manages the per-class instance monitoring.
%
% Returns an updated state and the total instance count.
%
manage_class_monitoring( CurrentTickOffset, CurrentWallclockTime,
						 InstancesPerClass, State ) ->

	OrderedClasses = ?getAttr(ordered_class_names),


	% Computes the data sample in the class order as was discovered:
	%
	% InstancesPerClass is a list of { ClassName, {CreationCount,DeletionCount}
	% } entries.
	%
	% We want to sort it in the order of the classes in OrderedClasses, and
	% replace the two counters by one, the current number of instance of that
	% class, i.e. CreationCounter - DeletionCounter.
	%
	% Note that new classes may appear in InstancesPerClass (while not being
	% known of OrderedClasses yet) and that the first element of OrderedClasses
	% is (implicitly - it does not exist there) the total number of instances.

	% Sort classes:
	{ TotalCount, SortedExistingClasses, NewClasses } = sort_classes(
										 InstancesPerClass, OrderedClasses ),


	TickProbe = ?getAttr(classes_in_tick_probe),

	TimeProbe = ?getAttr(classes_in_time_probe),

	Probes = [ TickProbe, TimeProbe ],

	% Let's now declare the new classes:
	[

	 begin

		 ClassName = text_utils:atom_to_string( C ),
		 [ P ! { addCurve, [ [ ClassName ] ] } || P <- Probes ]

	 end || { C, _Count } <- NewClasses

	],

	% And finally feed the probes with a full corresponding sample:
	AllSortedPairs = SortedExistingClasses ++ NewClasses,

	ExtractedValues = [ V || { _C, V } <- AllSortedPairs ],

	SampleValues = list_to_tuple( [ TotalCount | ExtractedValues ] ),

	TickProbe ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbe ! { setData, [ CurrentWallclockTime, SampleValues ] },

	{ setAttribute( State, ordered_class_names,
				   [ C || { C, _V } <- AllSortedPairs ] ),
	 TotalCount }.



% Sorts InstancesPerClass in the order in OrderedClasses, and determines the
% list of classes that were not known.
%
% Returns { TotalCount, SortedExistingClasses, NewClasses }, an overall instance
% count and the two lists (one ordered, one listing the new classes), each made
% of {ClassName,InstanceCount} entries.
%
sort_classes( InstancesPerClass, OrderedClasses ) ->

	% First, computes the current instance count for each class, and sums it
	% also:
	%
	{ NewInstancesPerClass, OverallCount } = lists:foldl(

			fun( { ClassName, { CreationCount, DeletionCount } },
				_Acc={ ClassList, TotalCount } ) ->

					ExistingCount = CreationCount - DeletionCount,

					NewClassList = [ {ClassName,ExistingCount} | ClassList ],

					NewTotalCount = TotalCount + ExistingCount,

					_NewAcc={ NewClassList, NewTotalCount }

			end,

			_FirstAcc={_ClassList=[],_TotalCount=0},

			_List=InstancesPerClass

				),

	% Now re-order and split:
	{ SortedExistingClasses, NewClasses } = reorder_classes(
					 NewInstancesPerClass, OrderedClasses, _ExistingAcc=[] ),

	{ OverallCount, SortedExistingClasses, NewClasses }.



% We iterate here over the ordered classes, moving the found classes from
% InstancesPerClass to ExistingAcc; the remaining ones are by design the new
% ones.
%
% Here we exhausted the known classes:
reorder_classes( InstancesPerClass, _OrderedClasses=[], ExistingAcc ) ->

	% Existing classes will then be correctly reordered:
	SortedExistingClasses = lists:reverse( ExistingAcc ),

	% InstancesPerClass is then a list corresponding to new classes:
	{ SortedExistingClasses, _NewClasses=InstancesPerClass };


reorder_classes( InstancesPerClass, _OrderedClasses=[ Class | T ], 
				 ExistingAcc ) ->

	% Finds and removes 'Class':
	case lists:keytake( _Key=Class, _Index=1, InstancesPerClass ) of

		{ value, ClassEntry, NewInstancesPerClass } ->
			reorder_classes( NewInstancesPerClass, T,
							 [ ClassEntry | ExistingAcc ] );

		false ->
			throw( { class_not_found, Class, InstancesPerClass } )

	end.



% Waits for all instance trackers to answer, returns the list of corresponding
% node information records.
%
-spec wait_dynamic_info_from_trackers( [ pid() ], wooper:state(),
	[ system_utils:host_dynamic_info() ] ) ->
									  [ system_utils:host_dynamic_info() ].
wait_dynamic_info_from_trackers( _InstanceTrackers=[], _State, Acc ) ->
	Acc;

wait_dynamic_info_from_trackers( InstanceTrackers, State, Acc ) ->

	receive

		{ wooper_result, { NodeDynInfo, TrackerPid } } ->

			RemainingTrackers = lists:delete( TrackerPid, InstanceTrackers ),

			wait_dynamic_info_from_trackers( RemainingTrackers, State,
											[ NodeDynInfo | Acc ] )

	after 5000 ->

			?warning_fmt( "Still waiting for the dynamic information "
						  "from instance trackers ~p", [ InstanceTrackers ] ),

			wait_dynamic_info_from_trackers( InstanceTrackers, State, Acc )

	end.



% Manages the per-node instance monitoring.
%
% Returns an updated state and the total instance count.
%
manage_node_monitoring( CurrentTickOffset, CurrentWallclockTime,
					   InstancesPerNode, State ) ->

	% Otherwise would include the user node (in first position):
	[ { _UserNode, _Static, _P1, _P2 } | OrderedNodes ] =
		?getAttr(resources_per_node_entries),

	%basic_utils:display( "manage_node_monitoring: OrderedNodes = ~p",
	%						[ OrderedNodes ] ),

	% By design there is no instance on the user node:
	NewInstancesPerNode = reorder_nodes( InstancesPerNode, OrderedNodes,
										 _Acc=[] ),

	ExtractedValues = [ V || { _C, V } <- NewInstancesPerNode ],

	TotalCount = lists:sum( ExtractedValues ),

	SampleValues = list_to_tuple( [ TotalCount | ExtractedValues ] ),

	%basic_utils:display( "manage_node_monitoring: SampleValues = ~p",
	%						[ SampleValues ] ),

	TickProbe = ?getAttr(nodes_in_tick_probe),

	TimeProbe = ?getAttr(nodes_in_time_probe),

	TickProbe ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbe ! { setData, [ CurrentWallclockTime, SampleValues ] },

	{ State, TotalCount }.



% We iterate here over the ordered nodes, moving the found nodes from
% InstancesPerNode to ExistingAcc.
%
% Here we exhausted the known nodes:
% (note that the two lists should be exhausted simultaneously)
%
reorder_nodes( _InstancesPerNode=[], _OrderedNodes=[], Acc ) ->

	% Returns a properly reordered {Node,InstanceCount} list:
	lists:reverse( Acc );

reorder_nodes( InstancesPerNode,
			   _OrderedNodes=[ {NodeName,_NodeStaticInfo,_Probe1,_Probe2} | T ],
			   Acc ) ->

	% Finds and removes 'Node':
	case lists:keytake( _Key=NodeName, _Index=1, InstancesPerNode ) of

		{ value, NodeEntry, NewInstancesPerNode } ->
			reorder_nodes( NewInstancesPerNode, T, [ NodeEntry | Acc ] );

		false ->
			throw( { node_not_found, NodeName, InstancesPerNode } )

	end.



% Manages the per-node resource monitoring (RAM and swap).
%
% Returns an updated state.
%
manage_node_dynamic_info( CurrentTickOffset, CurrentWallclockTime,
						  NodeDynInfos, State ) ->

	% Iterates on the list of node entries and feeds appropriately the
	% corresponding probes:

	% basic_utils:display( "manage_node_dynamic_info at tick #~p:~n"
	%						" - NodeDynInfos = ~p~n - NodeEntries = ~p",
	%			 [ CurrentTickOffset, NodeDynInfos,
	%			   ?getAttr(resources_per_node_entries) ] ),

	manage_dyn_nodes( ?getAttr(resources_per_node_entries), NodeDynInfos,
					  CurrentTickOffset, CurrentWallclockTime ),

	State.


% We iterate on the records as we want to use 'keytake' for the node list:
manage_dyn_nodes( _NodeEntries=[], _NodeDynInfos=[], _CurrentTickOffset,
				  _CurrentWallclockTime ) ->
	ok;

manage_dyn_nodes( NodeEntries,
				 _NodeDynInfos=[ #host_dynamic_info{
			node_name=NodeName,
			swap_used=Swap,
			cpu_usage=CPUPercentages,
			ram_use={ PercentRamUsedBySimulation, PercentRamUsedByOthers },
			process_count=ProcessCount } | T ],
				 CurrentTickOffset,
				 CurrentWallclockTime ) ->

	TotalUsed = PercentRamUsedBySimulation + PercentRamUsedByOthers,

	% For the moment we just focus on an aggregated non-idle use:
	% (undefined might be returned, the probe manages this natively)
	CPUPercent = system_utils:compute_cpu_usage_for( CPUPercentages ),

	SampleValues = list_to_tuple( [ TotalUsed, PercentRamUsedBySimulation,
									Swap, CPUPercent, ProcessCount ] ),

	% Now retrieves the corresponding probes:
	{ value,  { NodeName, _NodeStaticInfo, TickProbePid, TimeProbePid },
	 NewNodeEntries } = lists:keytake( _K=NodeName, _Index=1, NodeEntries ),

	TickProbePid ! { setData, [ CurrentTickOffset, SampleValues ] },

	TimeProbePid ! { setData, [ CurrentWallclockTime, SampleValues ] },

	manage_dyn_nodes( NewNodeEntries, T, CurrentTickOffset,
					  CurrentWallclockTime );

% Catch-all crash clause:
manage_dyn_nodes( NodeEntries, NodeDynInfos, CurrentTickOffset,
				  CurrentWallclockTime ) ->

	throw( { node_inconsistency, NodeEntries, NodeDynInfos, CurrentTickOffset,
			 CurrentWallclockTime } ).



% Sends the specified message to all probes used by this tracker, returns the
% list of their PIDs.
%
% (helper)
%
-spec send_to_all_probes( any(), wooper:state() ) -> [ pid() ].
send_to_all_probes( Message, State ) ->

	SingleProbeList = get_all_probes( State ),

	% More parallel by chunks:
	[ P ! Message || P <- SingleProbeList ],

	% Returns the full PID list:
	lists:foldl(

			fun( { _NodeName, _StaticInfos, TickProbe, TimeProbe }, Acc ) ->
					TickProbe ! Message,
					TimeProbe ! Message,
					[ TickProbe, TimeProbe | Acc ]
			end,

			_InitialAcc=SingleProbeList,

			?getAttr(resources_per_node_entries)

				).



% Launches the ticker process.
%
% Returns an updated state.
%
% (helper)
%
launch_ticker( TickerPeriod, State ) ->

	% Allows to track the wall-clock time:
	% (closure used to avoid exporting the function)

	%io:format( "Creating a ticker process whose period is ~B ms.~n",
	%		  [ TickerPeriod ] ),

	% Beware of closures!
	TrackerPid = self(),

	% Creating a ticker process, to trigger regular data updates:
	TickerPid = spawn_link( fun() ->
						 ticker_main_loop( TrackerPid, TickerPeriod ) end ),

	setAttributes( State, [

			{ ticker_pid, TickerPid },
			{ ticker_period, TickerPeriod }

						  ] ).



% Returns a list of the PIDs of all probes.
%
get_all_probes( State ) ->
	[ ?getAttr(nodes_in_tick_probe), ?getAttr(nodes_in_time_probe),
	  ?getAttr(classes_in_tick_probe), ?getAttr(classes_in_time_probe) ].



% Hooks for serialisation/deserialisation.

% We do not want the serialiser function to see the PIDs of the local worker
% process (the ticker one), as of course they are not meant to be resolved by
% the instance tracker (this would fail).



% Triggered just before serialisation.
%
% The state used here is dedicated to serialisation (i.e. it is not the actual
% state).
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	% Just one here, the ticker:
	PrivateProcesses = [ ticker_pid ],

	% In this state, private processes have been replaced by restoration
	% markers:
	%
	wooper_serialisation:handle_private_processes( PrivateProcesses, State ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
-spec post_serialise_hook( class_name(),
		wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								 wooper_serialisation:term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->

	% We have to recreate all private helper processes that were running,
	% i.e. just the ticker one:
	%
	case ?getAttr(ticker_pid) of

		undefined ->
			State;

		?process_restoration_marker ->
			% We restore the ticker (of course the wallclock time will register
			% a sudden delay):
			launch_ticker( ?getAttr(ticker_period), State )

	end.
