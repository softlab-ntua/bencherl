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



% Probe base class.
%
% Aggregates a series of values for a series of ticks and generates an
% appropriate data file for gnuplot.
%
% Note: ticks are expected to arrive correctly ordered (increasing timestamps),
% although gnuplot seems able to overcome it.
%
% A probe named 'Test probe' will result in the creation of two files:
%
% - Test_probe.p, with the relevant gnuplot commands
%
% - Test_probe.dat, with the probe data
%
% The writing of the received data samples can be either performed on-the-fly or
% deferred until a report generation is required.
%
% Note that not storing data (the default) allows to reduce the memory
% footprint.
%
% Preferring a deferred writing of the command file (this is the case by
% default) allows, regardless of the writing of the data being itself deferred
% or not), to support the dynamic addition of new columns: additional curves and
% zones may be declared, provided that the report generation has not been
% requested yet.
%
% For any number of ticks, no sample at all can be sent (i.e. reported values
% may no be consecutive), and partial samples can be sent (use the atom
% 'undefined' to specify that a given data element has no known value).
%
% Maybe in the future the possibility of merging samples could be supported (as
% it is already the case with the data-logger), if deferred_data_writes is
% on. For example, if for the same tick samples S1={ 1, undefined, 3 } and S2={
% undefined, 2, undefined } were sent, then the probe would store S={ 1, 2, 3 }.
%
% The rule would be that only undefined elements could be overridden. For
% example S3={ 1, 2, undefined } could not be merged with S1 because they both
% defined their first element (even if it is with the same value).

% Note that samples are tuples (ex: Sample={ 2, 1, undefined, 4 }), even if
% there is only one curve (ex: Sample={ 7 }, not Sample=7).

% Based on the enabled_producer attribute, a probe could decide not to perform
% anything if deactivated, to avoid wasting resources.

% If the data writes are not deferred, the header will be written directly at
% probe creation, thus any subsequent change (like addition of a curve, curves
% reordering, etc.) will not be taken in account in this probe based on an
% already-written header.





% Probes can be used as result producers (the usual case) or, in some specific
% cases, as mere technical helpers (ex: for the tracking of the simulation
% performance).
%
% In the first case, they will be declared either from a actor (then
% class_Actor:declare_probe/6 should be used for that) or directly from the
% simulation test case (then class_Probe:declare_from_{test,case}/6 should be
% used; these two forms are synonyms). In all these situations, they will be
% managed as results: created iff being requested results, and if yes they will
% be created in the directory for temporary data (typically '/tmp') and later
% retrieved iff the simulation succeeds.
%
% In the second case, probes are not result producers, just logging facilities,
% and they should be created (not declared, as their creation will not be
% decided upon by the result manager) thanks to
% class_Probe:create_facility_probe/7. In this case their files will be directly
% written into the specified directory so that, in case of crash or simulation
% failure, their files will linger there, on the local computer, already in the
% appropriate target directory.



% Probe serialisation.
%
%
% The state of a probe is written in a serialisation stream (file) that way:
%
% - 16-bit header telling that the following content corresponds to a probe
% (type id corresponding to ?serialised_probe_instance)
%
% - 32-bit unsigned integer telling on how many bytes the binary corresponding
% to the full state of the probe, its command file (if any) and its data file
% (if any) is spreading afterwards
%
% - then this binary itself (corresponding to a { ClassName, AttributeEntries,
% BinCommand, BinData } tuple)
%
% Probes thus need specific hooks to manage the information (command, data) that
% is not stored directly in its state.
%
% See: the serialisation hooks.



% See class_Probe_test.erl
%
% See http://www.gnuplot.info/docs/gnuplot.html for graph generation.
%
% Needs:
%
% - gnuplot version 4.2 or higher; see comments in get_probe_settings/4
% (key_options) to support older gnuplot versions
%
% - an image viewer, eog (eye of gnome); see the executable_utils module for
% viewers
%
-module(class_Probe).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_ResultProducer ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, NameOptions, CurveNames, Title, Zones,
		 XLabel, YLabel, MetaData ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/7, new_link/7,
		 synchronous_new/7, synchronous_new_link/7,
		 synchronous_timed_new/7, synchronous_timed_new_link/7,
		 remote_new/8, remote_new_link/8, remote_synchronous_new/8,
		 remote_synchronous_new_link/8, remote_synchronisable_new_link/8,
		 remote_synchronous_timed_new/8, remote_synchronous_timed_new_link/8,
		 construct/8, delete/1 ).



% Member method declarations.
-define( wooper_method_export, setTickOffset/2, setData/3,
		 addCurve/2, getCurveRenderOrder/1, setCurveRenderOrder/2,
		 setPlotStyle/2, setPlotStyle/3,
		 setFillStyle/2, setFillStyle/3,
		 setCanvasSize/3, setCanvasSize/4,
		 setPointSize/2, setPointSize/3,
		 setFilledCurvesOptions/3, setFilledCurvesOptions/4,
		 setKeyOptions/2, setKeyOptions/3,
		 setRotatedTickLabels/1, setRotatedTickLabels/2,
		 setAbscissaRange/3, setAbscissaRange/4,
		 setOrdinateRange/3, setOrdinateRange/4,
		 addLabel/3, addLabel/4, addLabel/5, addLabel/6,
		 setDirectory/2, sendResults/2,
		 generateCommandFile/1, generateReport/1, generateReport/2,
		 toString/1 ).



% Static method declarations.
-define( wooper_static_method_export, declare_result_probe/6,
		 declare_test_probe/6, delete_test_probe/1,
		 declare_case_probe/6, delete_case_probe/1,

		 create_facility_probe/6, create_facility_probe/7,
		 delete_facility_probe/1,

		 send_data/3, generate_report_for/1, get_gnuplot_reference_version/0,

		 deserialise/4 ).



% Helper functions.
-export([ get_probe_settings/4, write_row/3 ]).



% Export to share code with the data-logger and al:
-export([ transform_curve_names/1, transform_declared_zones/2,
		  add_probe_index_back/2, get_command_filename/1,
		  generate_command_file/5, get_data_filename/1, get_report_filename/1,
		  get_plot_command/4, check_probe_directory/1, get_basic_options/1,
		  get_label_definitions/1, get_x_range_option/1, get_y_range_option/1,
		  format_row/2, write_header/6 ]).


% For the probe settings:
-include("class_Probe.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.Probe.Basic").


% Not all plot features are available in older gnuplot versions:
-define( gnuplot_reference_version, { 4, 2 } ).



% To centralize basic open flags:
%
% (exclusive used to avoid that two probes bearing the same name by mistake end
% up writing to the same files simultaneously)
%
-define( base_open_flags, [ raw, write, exclusive ] ).



% Type section of external interactions with a probe:
% (we use plain strings here, as opposed to the internal representation)

-type special_curve_names() :: 'abscissa_top' | 'abscissa_bottom'.

-type declared_curve_name() :: string().
-type declared_extended_curve_name() :: string() | special_curve_names().

-type declared_zone_name() :: string().

-type declared_zone() :: { declared_zone_name(),
		 { declared_extended_curve_name(), declared_extended_curve_name() } }.



% Type section for internal data:


% Extended to allow for zone definitions:
% (knowing that the name of a curve is a binary)
%
-type extended_curve_name() :: curve_index() | special_curve_names().



% The factor by which the default point size should be multiplied:
-type point_size_factor() :: pos_integer().


-type name_options() :: probe_name() | { probe_name(), probe_options() }.


% A probe may not be a wanted result producer:
-type probe_pid() :: 'non_wanted_probe' | pid().


% Curves are numbered internally, and correspond to the position of data in sent
% samples:
%
-type curve_index() :: basic_utils:count().

-type curve_entry() :: { curve_index(), curve_name() }.


-type zone_name() :: text_utils:bin_string().


-type zone_definition() :: { zone_name(),
							{ extended_curve_name(), extended_curve_name() } }.


-type curve_entries() :: [ curve_entry() ].
-type zone_entries()  :: [ zone_definition() ].

% Exported so that for example class_Actor can reference them:
-export_type([ name_options/0, declared_curve_name/0, declared_zone_name/0,
			   declared_zone/0, probe_pid/0, curve_index/0, curve_entries/0,
			   zone_entries/0 ]).


% To have rotated tick labels:
-define(rotate_option,"rotate by - 45").


% Probes require specific (de)serialisations:
-define( wooper_serialisation_hooks,).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% For app_info*:
-include("traces.hrl").




% The class-specific attributes of a probe instance are:
%
% - settings :: probe_settings() is an instance of a probe_settings record
%
% - command_file_up_to_date :: boolean() tells whether the command file is
% considered as 'clean', i.e. existing and up-to-date
%
% - deferred_data_writes :: boolean() tells whether the data should be stored in
% memory and written just before generating the report (if true; thus increasing
% the memory footprint and not surviving a crash) or written over time as sample
% data is sent (if false; thus involving slower I/O and many writings)
%
% - is_tracked_producer :: boolean() tells whether this probe is a tracked
% result producer, i.e. a producer which will be requested by the result manager
% to return actual simulation results
%
% - probe_dir :: file_utils:directory_name() corresponds to the directory where
% the relevant probe files will be written; by default, it is the current
% working directory
%
% - curve_count :: basic_utils:count() is number of the curves carrying the data
% of the probe, corresponding to the number of sample data this probe is to be
% fed with (cached, precomputed value)
%
% - curve_entries :: [ { curve_index(), curve_name() } ] is an ordered list of {
% CurveIndex, BinCurveName } pairs, with CurveIndex keeping track of the order
% into which the curves were declared and fed (so that, prior to generating a
% report, curves can be reordered while being still associated to their values),
% and with curve names being binaries; the order in this list dictates the
% actual rendering order of curves that will be done
%
% - zone_entries :: [ zone_definition() ] is a list of
% { BinZoneName, { ExtendedCurveName1, ExtendedCurveName2 } entries
%
% - tick_offset :: probe_tick() is a value, by default set to zero, that will be
% subtracted to all the ticks sent for sample data; for example, if tick_offset
% is set to 1000 and samples are received for (supposedly absolute) ticks 1005
% and 1007, then the corresponding samples will be associated to abscissas 5 and
% 7; this allows for example to be able to rely on tick offsets rather than
% simulation absolute ticks, which would be generally a lot larger - thus more
% difficult to interpret; note: we could have defined a system which would, when
% the first sample is received, store this tick and subtract it from all the
% next sample ticks; however this is not what is generally wanted, as the origin
% of time would then be probe-specific, whereas we want generally to use the
% simulation start tick as common origin; as the probe cannot guess it, a call
% to setTickOffset/2 seems necessary
%
% - data_table :: [ { probe_tick(), sample_data() } ] records the sample data
% this probe was fed with; it is not a hashtable because gnuplot may prefer that
% the rows be ordered, and immediate writing might be requested. It is an
% ordered list (in reverse chronological order, as new samples are added at the
% head) which contains { TickOffset, Samples } entries, with Samples being a
% tuple whose size can increase over time, if updateCurveInformation/2 is called
% before the report generation (using default settings for graph rendering)
%
% - data_filename :: text_utils:binary() is the path of the probe data file; it
% is a complete path (including the probe directory), stored as a binary
%
% - data_file :: basic_utils:maybe( file:io_device() ) is the file object (if
% any) in which sample data is written
%
% - gnuplot_version :: basic_utils:two_digit_version() is the version of gnuplot
% that will be used on this computer during this simulation
%
% - meta_data :: class_ResultManager:meta_data() corresponds to the meta-data to
% be added into probe-generated data files



% Constructs a new (basic) probe, from following parameters: NameOptions,
% CurveNames, Title, Zones, XLabel, YLabel.
%
%
% - NameOptions is either:
%
%  - directly the name of this probe (specified as a plain string), which will
%  be used for the generated data and command files; note that no two probes
%  should have the same name, otherwise they will end up writing in the same
%  files and corrupting them (so the result manager will reject such attempts)
%
%  - or { Name, ProbeOptions } where ProbeOptions is a list of pairs, in:
%
%   - { create_command_file_initially, Boolean }: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter, but remaining
%   available even if the simulation was to be interrupted (default: false)
%
%   - { deferred_data_writes, boolean() }: if true, received sample data will be
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become then very significant)
%
%   - { register_as_tracked_producer, boolean() }: if true (the default), this
%   probe will register itself to the result manager, and be driven by it
%
%   - { probe_directory, ProbeDir } where ProbeDir is the directory in which the
%   files related to this probe (ex: *.p, *.data, *.png) should be written
%
% - CurveNames is an (ordered) list containing the names (as plain strings) of
% each curve to be drawn (hence the probe will expect receiving data in the form
% of { Tick, {V1,V2,..} }). For example, CurveNames=[ "First curve","Second
% curve" ] will lead to expect to receive samples like: { MyTick,
% { ValueForFirstCurve, ValueForSecondCurve } }
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo} } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'} } defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the title of the plot (as a plain string)
%
% - XLabel will be the label of the abscissa axis (as a plain string)
%
% - YLabel will be the label of the ordinate axis (as a plain string)
%
% - MetaData corresponds to the meta-data to be added into probe-generated data
% files
%
-spec construct( wooper_state(),
		  probe_name() | { probe_name(), probe_options() },
		  [ declared_curve_name() ], text_utils:title(), [ declared_zone() ],
		  text_utils:label(), text_utils:label(),
		  class_ResultManager:meta_data() ) -> wooper_state().
construct( State, { Name, ProbeOptions }, CurveNames, Zones, Title,
		  XLabel, YLabel, MetaData ) ->

	%basic_utils:display( "Creating probe ~s", [ Name ] ),

	% First the direct mother classes:

	ProducerState = class_ResultProducer:construct( State, Name ),

	% Then the class-specific actions:

	% Results in [ { curve_index(), curve_name() } ]:
	CurveEntries = transform_curve_names( CurveNames ),

	%io:format( "Initial curve entries: ~p~n", [ CurveEntries ] ),

	% Results in [ zone_definition() ]:
	ZoneEntries = transform_declared_zones( Zones, CurveEntries ),

	%io:format( "Initial zone entries: ~p~n", [ ZoneEntries ] ),

	GnuplotVersion = executable_utils:get_current_gnuplot_version(),

	ProbeSettings = get_probe_settings( Title, XLabel, YLabel, GnuplotVersion ),

	{ CreateCommandFileInitially, DeferredDataWrites, IsTrackedProducer,
	 ProbeDir } = interpret_options( ProbeOptions ),

	DataFilename = file_utils:join( ProbeDir, get_data_filename( Name ) ),

	StartState = setAttributes( ProducerState, [

		{ settings, ProbeSettings },
		{ command_file_up_to_date, false },
		{ deferred_data_writes, DeferredDataWrites },
		{ is_tracked_producer, IsTrackedProducer },
		{ probe_dir, ProbeDir },
		{ curve_count, length( CurveNames ) },
		{ curve_entries, CurveEntries },
		{ zone_entries, ZoneEntries },
		{ tick_offset, 0 },
		{ data_table, [] },
		{ data_filename, text_utils:string_to_binary( DataFilename ) },
		{ data_file, undefined },
		{ gnuplot_version, GnuplotVersion },
		{ meta_data, MetaData },
		{ trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization) }

	] ),

	CommandState = case CreateCommandFileInitially of

		true ->
			generate_command_file( StartState );

		false ->
			StartState

	end,

	DeferredState = case DeferredDataWrites of

		 true ->
			CommandState;

		 false ->

			check_probe_directory( ProbeDir ),

			file_utils:remove_file_if_existing( DataFilename ),

			% We perform "immediate" writes here (i.e. not storing samples),
			% however we rely on the underlying delayed raw writes. Probes do
			% not have to be too much responsive (hence the 2s delay), but as
			% they may be numerous we choose not a too big buffer (2KB), to
			% reduce the overall memory footprint:
			%
			DataFile = file_utils:open( DataFilename,
			  [ { delayed_write, _Size=2*1024, _Delay=2000 }
			   | ?base_open_flags ] ),


			% io_lib:format used merely for a portable '\n':
			ok = file:write( DataFile, io_lib:format(
					"# Warning: using immediate writes here, thus this "
					"header might be~n"
					"# inaccurate, should subsequent curve reordering or "
					"addition be performed.~n"
					"# Only initially created curves are listed below, "
					"the dynamically added ones~n"
					"# are not visible in this file.~n~n", [] ) ),

			write_header( DataFile, CurveEntries, ZoneEntries, ProbeSettings,
						 Name, MetaData ),

			setAttribute( CommandState, data_file, DataFile )

	end,

	% Finally registers to the result manager if needed:

	getAttribute( DeferredState, result_manager_pid ) !
				{ declareProbe, [ text_utils:string_to_binary(Name),
								 IsTrackedProducer ], self() },

	% What is true now may not be true anymore later, if the state of
	% the result manager changes:
	receive

		{ wooper_result, output_not_requested } ->

			?send_trace_fmt( DeferredState,
							"The probe ~s does not produce an expected result.",
							[ Name ] ),

			setAttribute( DeferredState, enabled_producer, false );

		{ wooper_result, output_requested } ->

			% Default is enabled_producer set to true:
			?send_trace_fmt( DeferredState,
							"The probe ~s will produce an expected result.~n",
							[ Name ] ),

			DeferredState

	end;


construct( State, Name, CurveNames, Zones, Title, XLabel, YLabel, MetaData ) ->

	% Will be using default settings here:
	construct( State, { Name, _DefaultOptions=[] }, CurveNames, Zones, Title,
			  XLabel, YLabel, MetaData ).




% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	%io:format( "Deleting probe ~s.~n", [ ?getAttr(name) ] ),

	% Class-specific actions:

	%?trace( "Deleting probe." ),

	case ?getAttr(deferred_data_writes) of

		true ->
			ok;

		false ->
			% We were performing immediate writes here (due to the delayed_write
			% option, close may return an old write error and not even try to
			% close the file. In that case we try to close it another time):
			%
			DataFile = ?getAttr(data_file),

			case file:close( DataFile ) of

				{ error, _Reason } ->
					file:close( DataFile ) ;

				ok  ->
					ok

			end

	end,

	%?debug( "Probe deleted." ),

	% Then call the direct mother class counterparts and allow chaining:
	State.





% Methods section.



% Sets the tick offset, that will be subtracted from the tick of all samples
% that will be received next. Then the abscissa axis will at least start with
% clearer, shorter, more tractable labels.
%
% For an example, refer to the soda vending machine example: these machines
% manage their probe so that it uses an offset to the simulation initial tick.
%
% (oneway)
%
-spec setTickOffset( wooper_state(), probe_tick() ) -> oneway_return().
setTickOffset( State, Offset ) ->
	?wooper_return_state_only( setAttribute( State, tick_offset, Offset ) ).



% Registers specified samples in the probe.
%
% Samples is a tuple which contains the value (either integer or floating-point)
% corresponding to the specified tick for each known curve.
%
% Should a curve have no relevant sample to be defined, the 'undefined' atom
% should be specified instead.
%
% (oneway)
%
-spec setData( wooper_state(), probe_tick(), sample_data() ) -> oneway_return().
setData( State, Tick, Samples ) ->

	%?debug_fmt( "setData called for tick ~B with samples ~p.",
	%  [ Tick, Samples ] ),

	ExpectedCount = ?getAttr(curve_count),

	case size(Samples) of

		ExpectedCount ->
			ok;

		_Other ->
			throw( { invalid_sample_size, Samples, ExpectedCount } )

	end,

	RecordedTick = Tick - ?getAttr(tick_offset),

	case ?getAttr(deferred_data_writes) of

		true ->
			% Watch out the memory footprint in long simulations!
			?wooper_return_state_only( appendToAttribute( State, data_table,
							  { RecordedTick, Samples } ) );

		false ->
			% Lower-level I/O will attempt a bit of deferred write nevertheless:
			write_row( ?getAttr(data_file), RecordedTick, Samples ),
			?wooper_return_state_only( State )

	end.




% Declares an additional curve, whose name is specified (as a plain string).
%
% By default it will be rendered after the already declared curves.
%
% Note: all samples received afterwards are then expected to take it into
% account (sending one more value, or the atom 'undefined', for that curve).
%
% (oneway)
%
-spec addCurve( wooper_state(), string() ) -> oneway_return().
addCurve( State, CurveName ) ->

	%io:format( "addCurve '~s' for probe '~s'.~n",
	%		  [ CurveName, ?getAttr(name) ] ),

	NewCurveCount = ?getAttr(curve_count) + 1,

	NewCurveEntries = list_utils:append_at_end(
				  { NewCurveCount, text_utils:string_to_binary(CurveName) },
				  ?getAttr(curve_entries) ),


	?wooper_return_state_only( setAttributes( State, [

		{ curve_count, NewCurveCount },
		{ curve_entries,NewCurveEntries },

		% Forcing a later re-creation:
		{ command_file_up_to_date, false }

		 ] ) ).



% Returns the list of curve names, as plain strings, sorted according to current
% rendering order.
%
% Useful then to reorder them and then to set them back thanks to
% setCurveRenderOrder/2.
%
% (const request)
%
-spec getCurveRenderOrder( wooper_state() ) -> request_return( [ string() ] ).
getCurveRenderOrder( State ) ->

	CurveEntries = ?getAttr(curve_entries),

	% Get rid of the curve index, order preserved:
	PlainNames = [ text_utils:binary_to_string( element(2,NamePair) )
				  || NamePair <- CurveEntries ],

	%io:format( "Returned curve render order: ~p~n", [ PlainNames ] ),

	?wooper_return_state_result( State, PlainNames ).



% Sets the list of curve names, sorted according to the desired rendering order.
%
% Names is a list of plain strings which must correspond to a permutation of the
% list which would be returned by getCurveEntries/1.
%
% (oneway)
%
-spec setCurveRenderOrder( wooper_state(), [ string() ] ) -> oneway_return().
setCurveRenderOrder( State, Names ) ->

	CurveEntries = ?getAttr(curve_entries),
	Len = length(CurveEntries),
	case length(Names) of

		Len ->

			NewCurveEntries = add_probe_index_back( Names, CurveEntries ),

			%io:format( "Set curve render order: ~p~n", [ NewCurveEntries ] ),

			% We force the (possible re-) generation of a command file when a
			% report will be requested:
			?wooper_return_state_only( setAttributes( State, [

					 { curve_entries, NewCurveEntries },
					 { command_file_up_to_date, false }
															  ] ) );

		_Other ->
			throw( { invalid_name_count, Names, Len } )

	end.



% Sets the plot settings to the one specified as a plain string (ex:
% "histograms", "linespoints"; "lines" is the default).
%
% (oneway)
%
-spec setPlotStyle( wooper_state(), string() ) -> oneway_return().
setPlotStyle( State, NewPlotStyle ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttributes( State, [

			   { settings, Settings#probe_settings{ plot_style =
				   text_utils:string_to_binary(NewPlotStyle) } },

				% Forcing a later re-creation:
				{ command_file_up_to_date, false }

				] ) ).



% Sets the plot settings to the one specified as a plain string (ex:
% "histograms", "linespoints"; "lines" is the default) and forces to regenerate
% the command file for taking into account the new settings.
%
% (oneway)
%
-spec setPlotStyle( wooper_state(), string(), boolean() ) -> oneway_return().
setPlotStyle( State, NewPlotStyle, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ plot_style =
				   text_utils:string_to_binary(NewPlotStyle) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the fill settings, specified as a plain string (ex: "solid 1.0 border
% -1").
%
% (oneway)
%
-spec setFillStyle( wooper_state(), string() ) -> oneway_return().
setFillStyle( State, NewFillStyle ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttributes( State, [

				  { settings, Settings#probe_settings{ fill_style =
				   text_utils:string_to_binary(NewFillStyle) } },

				  % Forcing a later re-creation:
				  { command_file_up_to_date, false }

				   ] ) ).



% Sets the fill settings, specified as a plain string (ex: "solid 1.0 border
% -1") and forces to regenerate the command file for taking into account the new
% settings.
%
% (oneway)
%
-spec setFillStyle( wooper_state(), string(), boolean() ) ->
	 oneway_return().
setFillStyle( State, NewFillStyle, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ fill_style =
				   text_utils:string_to_binary(NewFillStyle) } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the size of the probe reports (canvas), in pixels.
%
% (oneway)
%
-spec setCanvasSize( wooper_state(), gui:length(), gui:length() ) ->
						   oneway_return().
setCanvasSize( State, NewWidth, NewHeight ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttributes( State, [

			  { settings, Settings#probe_settings{

			  canvas_width  = NewWidth,
			  canvas_height = NewHeight

								} },

			   % Forcing a later re-creation:
			   { command_file_up_to_date, false }

			   ] ) ).



% Sets the size of the probe reports (canvas), in pixels and forces to
% regenerate the command file for taking into account these new settings, if
% requested.
%
% (oneway)
%
-spec setCanvasSize( wooper_state(), gui:length(), gui:length(), boolean() ) ->
						   oneway_return().
setCanvasSize( State, NewWidth, NewHeight, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{

			  canvas_width  = NewWidth,
			  canvas_height = NewHeight

								} ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the size of the plot point.
%
% Point size means that the shown points will be of PointSize times the default
% point size.
%
% (oneway)
%
-spec setPointSize( wooper_state(), point_size_factor() ) -> oneway_return().
setPointSize( State, PointSize ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttributes( State, [

			   { settings, Settings#probe_settings{

			  point_size = PointSize

								} },

				% Forcing a later re-creation:
				{ command_file_up_to_date, false }

				] ) ).



% Sets the size of the plot point and forces to regenerate the command file in
% order to take into account the new settings.
%
% Point size means that the shown points will be of PointSize times the default
% point size.
%
% (oneway)
%
-spec setPointSize( wooper_state(), point_size_factor(), boolean() ) ->
		oneway_return().
setPointSize( State, PointSize, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState =  setAttribute( State, settings,
		Settings#probe_settings{

			  point_size  = PointSize

								} ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the key (legend) settings, specified as a plain string (ex: "inside
% left", "bottom center").
%
% (oneway)
%
-spec setKeyOptions( wooper_state(), string() ) -> oneway_return().
setKeyOptions( State, NewOptions ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttribute( State, settings,
		Settings#probe_settings{

				key_options = text_utils:string_to_binary(NewOptions)

								} ) ).


% Sets the key (legend) settings, specified as a plain string (ex: "inside
% left", "bottom center") and forces to regenerate the command file for taking
% into account the new settings, if requested.
%
% (oneway)
%
-spec setKeyOptions( wooper_state(), string(), boolean() ) -> oneway_return().
setKeyOptions( State, NewOptions, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{

			   key_options =  text_utils:string_to_binary(NewOptions)

								} ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the abscissa range for the plot.
%
% MinX and MaxX are integers.
%
% (oneway)
%
-spec setAbscissaRange( wooper_state(), gui:coordinate(), gui:coordinate() ) ->
							  oneway_return().
setAbscissaRange( State, MinX, MaxX ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttribute( State, settings,

		Settings#probe_settings{ x_range = {MinX,MaxX} }

											) ).


% Sets the abscissa range for the plot and forces to regenerate the command file
% for taking into account the new settings.
%
% MinX and MaxX are integers.
%
% (oneway)
%
-spec setAbscissaRange( wooper_state(), gui:coordinate(), gui:coordinate(),
					   boolean() ) -> oneway_return().
setAbscissaRange( State, MinX, MaxX, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,
		Settings#probe_settings{ x_range={MinX,MaxX} } ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Sets the ordinate range for the plot.
%
%  MinY and MaxY are integers.
%
% (oneway)
%
-spec setOrdinateRange( wooper_state(), gui:coordinate(), gui:coordinate() ) ->
							 oneway_return().
setOrdinateRange( State, MinY, MaxY ) ->

	Settings = ?getAttr(settings),

	?wooper_return_state_only( setAttribute( State, settings,

		Settings#probe_settings{ y_range = {MinY,MaxY} }

											) ).



% Sets the ordinate range for the plot and forces to regenerate the command file
% for taking into account the new settings.
%
% MinY and MaxY are integers.
%
% (oneway)
%
-spec setOrdinateRange( wooper_state(), gui:coordinate(), gui:coordinate(),
					   boolean() ) -> oneway_return().
setOrdinateRange( State, MinY, MaxY, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState =  setAttribute( State, settings,

		Settings#probe_settings{ y_range = {MinY,MaxY} }

								 ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Ensures that the generated reports will rely on rotated tick labels, so that
% labels will never overlap, however long they are.
%
% (oneway)
%
-spec setRotatedTickLabels( wooper_state() ) -> oneway_return().
setRotatedTickLabels( State ) ->

	Settings = ?getAttr(settings),

	%?wooper_return_state_only( setAttribute( State, settings,
	%	Settings#probe_settings{ xtic = text_utils:string_to_binary(
	%		   "format \"%.0f\" border out rotate by 90 offset 0,graph 0.05"

	?wooper_return_state_only( setAttributes( State, [
				{ settings, Settings#probe_settings{

		   xtic = text_utils:string_to_binary( ?rotate_option )

								} },

				{ command_file_up_to_date, false }
											 ] ) ).



% Ensures that the generated reports will rely on rotated tick labels, so that
% labels will never overlap, however long they are, and regenerates the command
% file if requested.
%
-spec setRotatedTickLabels( wooper_state(), boolean() ) -> oneway_return().
setRotatedTickLabels( State, GenerateFile ) ->

	Settings = ?getAttr(settings),

	UpdatedState = setAttribute( State, settings,

		Settings#probe_settings{


			xtic = text_utils:string_to_binary( ?rotate_option )

								} ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).




% Adds a specific text label, specified as a plain string, at the specified
% location ({X,Y} integer coordinates).
%
% (oneway)
%
-spec addLabel( wooper_state(), string(), gui:point() ) -> oneway_return().
addLabel( State, Text, Location ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{

		location=Location,
		text=text_utils:string_to_binary( Text ),
		color=blue,
		position=center,
		orientation=upright

							},

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	?wooper_return_state_only( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified location
% ({X,Y} integer coordinates), with specified color (ex: 'magenta', or
% "#4B00820").
%
% (oneway)
%
-spec addLabel( wooper_state(), string(), gui:point(), gui_color:color() ) ->
					  oneway_return().
addLabel( State, Text, Location, Color ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{

		location=Location,
		text=text_utils:string_to_binary( Text ),
		color=Color,
		position=center,
		orientation=upright

							},

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	?wooper_return_state_only( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified location
% ({X,Y} integer coordinates), with specified color (ex: magenta, or "#4B00820")
% and orientation, either 'upright' (the default), or { rotate, Angle }, Angle
% being an angle in degrees (as a floating-point value).
%
% (oneway)
%
-spec addLabel( wooper_state(), string(), gui:point(), gui_color:color(),
			label_orientation() ) -> oneway_return().
addLabel( State, Text, Location, Color, Orientation ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{

		location=Location,
		text=text_utils:string_to_binary( Text ),
		color=Color,
		position=center,
		orientation=Orientation

							},

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	?wooper_return_state_only( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Adds a specific text label, specified as a plain string, at specified Location
% ({X,Y} integer coordinates), with specified color (ex: magenta), orientation,
% either 'upright' (the default), or { rotate, Angle }, Angle being an angle in
% degrees (as a floating-point value) and position (an atom, either left, center
% or right, the default being center).
%
% (oneway)
%
-spec addLabel( wooper_state(), string(), gui:point(), gui_color:color(),
			label_orientation(), label_position() ) -> wooper_state().
addLabel( State, Text, Location, Color, Orientation, Position ) ->

	Settings = ?getAttr(settings),

	% Adds default values where none was specified:
	NewLabel = #probe_label{

		location=Location,
		text=text_utils:string_to_binary( Text ),
		color=Color,
		position=Position,
		orientation=Orientation

							},

	Labels = [ NewLabel | Settings#probe_settings.labels ],

	?wooper_return_state_only( setAttribute( State, settings,
			Settings#probe_settings{ labels=Labels } ) ).



% Generates the appropriate gnuplot command file.
%
% (oneway, so that it can be overridden)
%
generateCommandFile( State ) ->

	NewState = generate_command_file( State ),

	?wooper_return_state_only( NewState ).



% Generates a report corresponding to the current state of this probe, and
% displays the result (the image) to the user.
%
% (request)
%
-spec generateReport( wooper_state() ) ->
						request_return( 'probe_report_generated' ).
generateReport( State ) ->
	generateReport( State, _DisplayWanted=true ).



% Generates a report corresponding to the current state of this probe.
%
% DisplayWanted is a boolean telling whether the generated report will be
% displayed to the user (if true).
%
% Returns the 'probe_report_generated' atom, merely for synchronisation purpose.
%
% (request)
%
-spec generateReport( wooper_state(), boolean() ) ->
						request_return( 'probe_report_generated' ).
generateReport( State, DisplayWanted ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	%basic_utils:display( "generateReport for probe '~s'", [ Name ] ),

	ReportState = generate_report( Name, State ),

	ReportName = get_report_filename( Name ),

	case DisplayWanted of

		true ->
			executable_utils:display_png_file(
				file_utils:join( ?getAttr(probe_dir), ReportName ) );

		false ->
			ok

	end,

	?wooper_return_state_result( ReportState, probe_report_generated ).



% Sets the probe directory: all further probe files (command, data, locally
% generated plots) will be created there.
%
% (oneway)
%
-spec setDirectory( wooper_state(), file_utils:directory_name() ) ->
						  oneway_return().
setDirectory( State, NewProbeDirectory ) ->

	% data_filename is the only precomputed path (thus the only one to be
	% updated):
	DataFilename = file_utils:join( NewProbeDirectory,
			get_data_filename( class_TraceEmitter:get_plain_name(State) ) ),

	?wooper_return_state_only( setAttributes( State, [

			{ probe_dir, NewProbeDirectory },
			{ data_filename, text_utils:string_to_binary( DataFilename ) }

													] ) ).



% Sends the specified results to the caller (generally the result manager).
%
% (const request, for synchronous operations)
%
-spec sendResults( wooper_state(), class_ResultProducer:producer_options() ) ->
				request_return( class_ResultProducer:producer_result() ).
sendResults( State, [ data_only ] ) ->

	% Here we will send an archive term containing the data and command files:
	CommandState = ensure_command_file_available( State ),
	ensure_data_file_available( CommandState ),

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	PathLessDataFilename = filename:basename( DataFilename ),

	Name = class_TraceEmitter:get_plain_name( CommandState ),

	CommandFilename = get_command_filename( Name ),

	FileList = [ PathLessDataFilename, CommandFilename ],

	?trace_fmt( "Creating binary data-only archive term for ~p.",
			   [ FileList ] ),

	BinArchive = file_utils:files_to_zipped_term( FileList ),

	ProbeDir = ?getAttr(probe_dir),

	FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

	% Performing clean-up here is useful, as (basic) probes created directly
	% from the simulation case will not be created under the directory for
	% temporary data (by default '/tmp') - like it is the case for computing
	% nodes - but in the current directory.
	file_utils:remove_files( FilesToRemove ),

	?wooper_return_state_result( CommandState,
								{ self(), archive, BinArchive } );


sendResults( State, [ plot_only ] ) ->

	Name = class_TraceEmitter:get_plain_name( State ),

	ReportState = generate_report( Name, State ),

	ReportFilename = get_report_filename( Name ),
	?trace_fmt( "Creating binary plot-only raw term for ~p.",
			   [ ReportFilename ] ),

	ReportFilenameFullPath = file_utils:join( ?getAttr(probe_dir),
											 ReportFilename ),

	BinContent = file_utils:read_whole( ReportFilenameFullPath ),

	BinReportFilename = text_utils:string_to_binary( ReportFilename ),

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	CommandFilename = get_command_filename( Name ),

	ProbeDir = ?getAttr(probe_dir),

	FileList = [ CommandFilename, ReportFilename ],

	FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

	% DataFilename is already a full path:
	file_utils:remove_files( [ DataFilename | FilesToRemove ] ),

	?wooper_return_state_result( ReportState, { self(), raw,
										 { BinReportFilename, BinContent } } );


sendResults( State, [ data_and_plot ] ) ->

	% Here we will send an archive term containing the data, command and plot
	% files:

	Name = class_TraceEmitter:get_plain_name( State ),

	% Generates the report, thus the data and command files:
	ReportState = generate_report( Name, State ),

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	CommandFilename = get_command_filename( Name ),

	ReportFilename = get_report_filename( Name ),

	PathLessDataFilename = filename:basename( DataFilename ),

	FileList = [ PathLessDataFilename, CommandFilename, ReportFilename ],

	ProbeDir = ?getAttr(probe_dir),

	?trace_fmt( "Creating binary plot-and-data archive term for ~p, from ~s.",
			   [ FileList, ProbeDir ] ),

	BinArchive = file_utils:files_to_zipped_term( FileList, ProbeDir ),

	FilesToRemove = [ file_utils:join( ProbeDir, F ) || F <- FileList ],

	file_utils:remove_files( FilesToRemove ),

	?wooper_return_state_result( ReportState, { self(), archive, BinArchive } ).




% Returns a textual description of this probe instance.
%
% (const request)
%
-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->

	CleanCommandWord = case ?getAttr(command_file_up_to_date) of

							true ->
								"";

							false ->
								"not "

	end,

	DeferredWord = case ?getAttr(deferred_data_writes) of

							true ->
								"";

							false ->
								"not "

	end,

	Text = io_lib:format( "Probe '~s', whose command file is ~s cleaned, "
				  "and ~sperforming deferred data writes",
				  [ ?getAttr(name), CleanCommandWord, DeferredWord ] ),

	?wooper_return_state_result( State, Text ).





% Generic interface.


% 'Static' methods (module functions):




% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - directly the name of this probe (specified as a plain string), which will
%  be used for the generated data and command files
%
%  - or {Name,ProbeOptions} where ProbeOptions is a list of pairs, in:
%
%   - {create_command_file_initially,Bool}: if true, the gnuplot command file
%   will be written at probe start-up, thus preventing the taking into account
%   of any subsequent change in the rendering parameter (default: false)
%
%   - {deferred_data_writes,Bool}: if true, received sample data will stored in
%   memory instead of being directly written to disk (default: false, as the
%   memory footprint might become significant) where Bool is true or false
%
% - CurveNames is an (ordered) list containing the names (as plain strings) of
% each curve to be drawn (hence the probe will expect receiving data in the form
% of { Tick, {V1,V2,..} }). For example, CurveNames=["First curve","Second
% curve"] will lead to expect to receive samples like: { MyTick,
% {ValueForFirstCurve,ValueForSecondCurve} }
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName,
% {ExtendedCurveNameOne,ExtendedCurveNameTwo} } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'} } defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - XLabel will be the label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns either the PID of this newly created probe (if the name of that probe
% is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
% (static)
%
-spec declare_result_probe( name_options(), [ declared_curve_name() ],
		 [ declared_zone() ], text_utils:title(), text_utils:label(),
		 text_utils:label() ) -> probe_pid().
declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ) ->

	ActualName = case NameOptions of

		 { Name, _ProbeOptions } ->
			Name;

		 Name when is_list(Name) ->
			Name

	end,

	ActualBinName = text_utils:string_to_binary( ActualName ),

	ResultManagerPid = class_ResultManager:get_result_manager(),

	ResultManagerPid ! { isResultProducerWanted,
						 [ ActualBinName, _Nature=basic_probe ], self() },

	receive

		{ wooper_result, { true, Metadata } } ->
			% Created in current directory (i.e. the one for temporary data):
			class_Probe:synchronous_new_link( NameOptions, CurveEntries,
						   ZoneEntries, Title, XLabel, YLabel, Metadata );

		{ wooper_result, false } ->
			non_wanted_probe

	end.



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a test case.
%
% - NameOptions is either:
%
%  - directly the name of this probe (specified as a plain string), which will
%  be used for the generated data and command files; note that no two probes
%  should have the same name, otherwise they will end up writing in the same
%  files and corrupting them
%
%  - or { Name, ProbeOptions } where ProbeOptions is a list of pairs, in:
%
%   - { create_command_file_initially, Bool }: if true, the gnuplot command file
%   will be written at probe start-up, thus preventing the taking into account
%   of any subsequent change in the rendering parameter (default: false)
%
%   - { deferred_data_writes, Bool }: if true, received sample data will stored
%   in memory instead of being directly written to disk (default: false, as the
%   memory footprint might become significant) where Bool is true or false
%
% - CurveNames is an (ordered) list containing the names (as plain strings) of
% each curve to be drawn (hence the probe will expect receiving data in the form
% of { Tick, {V1,V2,..} }). For example, CurveNames=["First curve","Second
% curve"] will lead to expect to receive samples like: { MyTick,
% { ValueForFirstCurve, ValueForSecondCurve } }
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName, {
% ExtendedCurveNameOne, ExtendedCurveNameTwo } } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example {"My Zone", {"First curve",'abscissa_bottom'} } defines a zone named
% "My Zone" and delimited by the curve named "First curve" and the abscissa axis
% (note: the order between the two elements defining a zone does not matter)
%
% - Title will be the graph (plot) title
%
% - XLabel will be the label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
% Returns either the PID of this newly created probe (if the name of that probe
% is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
% (static)
%
-spec declare_test_probe( name_options(), [ declared_curve_name() ],
		 [ declared_zone() ], text_utils:title(), text_utils:label(),
		 text_utils:label() ) -> probe_pid().
declare_test_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ) ->

	% They are synonyms:
	declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ).



% Declares (synchronously) a new (basic) probe, to be seen as a result producer,
% and be created directly from a simulation case.
%
% See declare_test_probe/6 (just above) for more information.
%
% (static)
%
-spec declare_case_probe( name_options(), [ declared_curve_name() ],
		 [ declared_zone() ], text_utils:title(), text_utils:label(),
			 text_utils:label() ) -> probe_pid().
declare_case_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ) ->

	case declare_result_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ) of

		non_wanted_probe ->
			non_wanted_probe;

		ProbePid ->

			% Directly from the simulation case, we cannot work in the current
			% case directory: (preferring a directory under /tmp):

			Nodename = atom_to_list( node() ),

			% Removes the @host trailing part:
			SimulationName = string:substr( Nodename, 1,
										   string:chr( Nodename, $@ ) - 1 ),

			FinalDir = "sim-diasca-case-"
				++ file_utils:convert_to_filename( SimulationName ),

			% (initial empty string allows to forge '/tmp/..'):
			%
			% (note: for these probes, which are generally not really numerous,
			% we prefer using /tmp even if another temporary directory was
			% specified - we do not want here to depend on the deployment
			% settings)
			%
			ProbeDir = filename:join( [ "", "tmp", FinalDir ] ),

			file_utils:create_directory( ProbeDir, create_parents ),

			ProbePid ! { setDirectory, [ ProbeDir ] },

			ProbePid

	end.



% Deletes specified test probe (as returned by declare_test_probe, i.e. actually
% created or not).
%
-spec delete_test_probe( probe_pid() ) -> basic_utils:void().
delete_test_probe( Any ) ->
	% Synonyms:
	delete_case_probe( Any ).


% Deletes specified case probe (as returned by declare_case_probe, i.e. actually
% created or not).
%
-spec delete_case_probe( probe_pid() ) -> basic_utils:void().
delete_case_probe( non_wanted_probe ) ->
	ok;

delete_case_probe( Pid ) ->
	Pid ! delete.



% Creates in the current directory a facility probe, i.e. a lingering probe, to
% be created (unilaterally) from a test case, and will not to considered as a
% result.
%
% - NameOptions is either:
%
%  - directly the name of this probe (specified as a plain string), which will
%  be used for the generated data and command files; note that no two probes
%  should have the same name, otherwise they will end up writing in the same
%  files and corrupting them
%
%  - or { Name, ProbeOptions } where ProbeOptions is a list of pairs, in:
%
%   - { create_command_file_initially, Boolean }: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter, but remaining
%   available even if the simulation was to be interrupted (default: false)
%
%   - { deferred_data_writes, Boolean }: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant)
%
% - CurveNames is an (ordered) list containing the names (as plain strings) of
% each curve to be drawn (hence the probe will expect receiving data in the form
% of { Tick, {V1,V2,..} }). For example, CurveNames=[ "First curve", "Second
% curve" ] will lead to expect to receive timestamped samples like: { MyTick, {
% ValueForFirstCurve, ValueForSecondCurve } }
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName, {
% ExtendedCurveNameOne, ExtendedCurveNameTwo } } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example { "My Zone", { "First curve", 'abscissa_bottom' } } defines a zone
% named "My Zone" which is delimited by the curve named "First curve" and the
% abscissa axis (note: the order between the two elements defining a zone does
% not matter)
%
% - Title will be the graph (plot) title
%
% - XLabel will be the label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
%
% Returns the PID of this newly created probe.
%
% (static)
%
-spec create_facility_probe( name_options(), [ declared_curve_name() ],
	[ declared_zone() ], text_utils:title(), text_utils:label(),
	text_utils:label() ) -> pid().
create_facility_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel ) ->

	ProbeDirectory = file_utils:get_current_directory(),

	create_facility_probe( NameOptions, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel, ProbeDirectory ).



% Creates a facility probe, i.e. a lingering probe, to be created (unilaterally)
% from a test case, and will not to considered as a result.
%
% - NameOptions is either:
%
%  - directly the name of this probe (specified as a plain string), which will
%  be used for the generated data and command files
%
%  - or { Name, ProbeOptions } where ProbeOptions is a list of pairs, in:
%
%   - { create_command_file_initially, Boolean }: if true, the gnuplot command
%   file will be written at probe start-up, thus preventing the taking into
%   account of any subsequent change in the rendering parameter, but remaining
%   available even if the simulation was to be interrupted (default: false)
%
%   - { deferred_data_writes, Boolean }: if true, received sample data will
%   stored in memory instead of being directly written to disk (default: false,
%   as the memory footprint might become significant)
%
% - CurveNames is an (ordered) list containing the names (as plain strings) of
% each curve to be drawn (hence the probe will expect receiving data in the form
% of { Tick, {V1,V2,..} }). For example, CurveNames=[ "First curve", "Second
% curve" ] will lead to expect to receive timestamped samples like: { MyTick, {
% ValueForFirstCurve, ValueForSecondCurve } }
%
% - Zones, which correspond to specific areas between two curves being defined,
% are specified as a (potentially empty) list of { ZoneName, {
% ExtendedCurveNameOne, ExtendedCurveNameTwo } } entries, where ZoneName is the
% name of this zone (as a plain string), and ExtendedCurveNameOne and
% ExtendedCurveNameTwo are each either a plain string designating a curve (ex:
% "Second curve") already defined in CurveNames, or a special atom designating
% the plot boundaries, i.e. either 'abscissa_bottom' or 'abscissa_top'. For
% example { "My Zone", { "First curve", 'abscissa_bottom' } } defines a zone
% named "My Zone" which is delimited by the curve named "First curve" and the
% abscissa axis (note: the order between the two elements defining a zone does
% not matter)
%
% - Title will be the graph (plot) title
%
% - XLabel will be the label of the abscissa axis
%
% - YLabel will be the label of the ordinate axis
%
%
% Returns the PID of this newly created probe.
%
% (static)
%
-spec create_facility_probe( name_options(), [ declared_curve_name() ],
		[ declared_zone() ], text_utils:title(), text_utils:label(),
		text_utils:label(), file_utils:directory_name() ) -> pid().
create_facility_probe( { Name, Options }, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel, ProbeDirectory ) ->

	% Put in end to ensure it overrides any previous probe directory definition:

	NewOptions = Options ++ [ { register_as_tracked_producer, false },
							  { probe_directory, ProbeDirectory } ],

	class_Probe:synchronous_new_link( { Name, NewOptions }, CurveEntries,
				ZoneEntries, Title, XLabel, YLabel, _MetaData=[] );


create_facility_probe( Name, CurveEntries, ZoneEntries, Title,
					 XLabel, YLabel, ProbeDirectory) ->

	Options = [ { register_as_tracked_producer, false },
				{ probe_directory, ProbeDirectory } ],

	class_Probe:synchronous_new_link( { Name, Options }, CurveEntries,
				ZoneEntries, Title, XLabel, YLabel, _MetaData=[] ).



% Deletes specified facility probe (knowing that the other kinds of probes are
% results, and thus are managed by the result manager).
%
-spec delete_facility_probe( pid() ) -> basic_utils:void().
delete_facility_probe( ProbePid ) ->
	% This is necessary a PID, not a 'non_wanted_probe' atom:
	ProbePid ! delete.



% Sends the specified sample data for the specified tick to the targeted probe,
% based on the specified probe (first parameter), which is the value
% returned by the result manager in answer to the initial creation request for
% that probe.
%
% This parameter is either an actual PID (then data will be sent by this method)
% or the 'non_wanted_probe' atom (as potentially sent back by the result
% manager), in which case nothing will be done.
%
% Note: this static method is to be used for basic probes, not virtual ones.
%
% (static method)
%
-spec send_data( probe_pid(), probe_tick(), sample_data() ) ->
		basic_utils:void().
send_data( non_wanted_probe, _Tick, _Samples )  ->
	undefined;

% The guard should be useless here:
send_data( ProbePid, Tick, Samples ) when is_pid(ProbePid) ->
	ProbePid ! { setData, [ Tick, Samples ] }.



% Allows to define whether the probe report should be displayed to the user,
% after generation.
%
% Now superseded by the use of the result manager.
%
% (static method)
%
-spec generate_report_for( probe_pid() ) -> basic_utils:void().
generate_report_for( ProbePid ) ->

	case executable_utils:is_batch() of

		true ->
			ProbePid ! { generateReport, _DisplayWanted=false, self() };

		false ->
			ProbePid ! { generateReport, _DisplayWanted=true, self() }

	end,

	receive

		{ wooper_result, probe_report_generated } ->
			?notify_info( "Probe report correctly generated." )

	end.



% Section for helper functions (not methods).


% Returns a probe_settings record with specified informations (expressed as
% plain strings) and default values for the other fields.
%
-spec get_probe_settings( text_utils:title(), text_utils:label(),
		 text_utils:label(), basic_utils:two_digit_version() ) ->
								probe_settings().
get_probe_settings( Title, XLabel, YLabel, GnuplotVersion ) ->

	{ Xtic, KeyOption } = get_basic_options( GnuplotVersion ),

	#probe_settings{

		title = text_utils:string_to_binary(Title),

		xtic = Xtic,

		key_options = KeyOption,

		%image_format = text_utils:string_to_binary("svg");

		ytic = text_utils:string_to_binary("auto"),

		xlabel = text_utils:string_to_binary(XLabel),

		ylabel = text_utils:string_to_binary(YLabel),

		x_range = undefined,

		y_range = undefined,

		plot_style = text_utils:string_to_binary("linespoints"),

		fill_style = text_utils:string_to_binary("empty")

					}.



% Returns some basic options, depending on the current gnuplot version.
%
% (helper, for code sharing)
%
-spec get_basic_options( basic_utils:two_digit_version() ) ->
							   { binary(), binary() }.
get_basic_options( GnuplotVersion ) ->

	% If using a very old version of gnuplot (ex: < 4.2), these key options
	% use default values:

	case basic_utils:compare_versions( GnuplotVersion,
									  get_gnuplot_reference_version() ) of

		second_bigger ->
			% Here we only have access to an older gnuplot:
			{

			 _Xtic2 = text_utils:string_to_binary( "auto" ),
			 _KeyOption2 = text_utils:string_to_binary( "" )

			 };

		_ ->
			% Here we have a recent enough gnuplot:
			{

			 % By default we prefer not having rotated ticks:

			 %_Xtic1=text_utils:string_to_binary( "rotate by - 45 auto" ),
			 _Xtic1=text_utils:string_to_binary( "auto" ),

			 _KeyOption1=
			 text_utils:string_to_binary( "bmargin center horizontal" )

			 }

			%image_format = text_utils:string_to_binary( "svg" );

	end.



% Adds back the index in the Names list, as read from the CurveEntries
% list, without changing the order of the Names list.
%
% Transforms plain strings in binaries as well.
%
% Ex: add_probe_index_back( [ "b", "c", "a" ], CurveEntries ) with
% CurveEntries=[ {3,<<"a">>}, {2,<<"b">>}, {1,<<"c">>} ] should return:
% [ {2,<<"b">>}, {1,<<"c">>}, {3,<<"a">>} ], i.e. the items of Names, in their
% original order in Names, with their index added back.
%
% (helper function)
%
-spec add_probe_index_back( [ string() ], curve_entries() ) -> curve_entries().
add_probe_index_back( Names, CurveEntries ) ->
	add_probe_index_back( Names, CurveEntries, _Acc=[] ).


add_probe_index_back( _Names=[], _CurveEntries, Acc ) ->
	lists:reverse(Acc);

add_probe_index_back( [ Name | T ], CurveEntries, Acc ) ->

	BinName = text_utils:string_to_binary( Name ),

	% We do not check for duplicated names and removed ones resulting in a
	% correct length of the name list:
	case lists:keyfind( BinName, 2, CurveEntries ) of

		false ->
			throw( { unknown_curve, Name } );

		{ Index, _Name } ->
			add_probe_index_back( T, CurveEntries,
								 [ { Index, BinName } | Acc ] )

	end.



% Interprets the creation-time probe options:
%
-spec interpret_options( probe_options() ) ->
			{ boolean(), boolean(), boolean(), file_utils:directory_name() }.
interpret_options( ProbeOptions ) ->
	% Defaults:
	interpret_options( ProbeOptions, _CreateCommandFileInitially=false,
			_DeferredDataWrites=false, _IsTrackedProducer=true,
			_ProbeDir=file_utils:get_current_directory() ).



-spec interpret_options( probe_options(), boolean(), boolean(), boolean(),
						file_utils:directory_name() ) ->
			{ boolean(), boolean(), boolean(), file_utils:directory_name() }.
interpret_options( [], CreateCommandFileInitially, DeferredDataWrites,
	   IsTrackedProducer, ProbeDir ) ->
	{ CreateCommandFileInitially, DeferredDataWrites, IsTrackedProducer,
	 ProbeDir };

interpret_options( [ {create_command_file_initially,Value} | OtherOptions ],
				  _CreateCommandFileInitially, DeferredDataWrites,
				  IsTrackedProducer, ProbeDir ) when is_boolean(Value) ->
	interpret_options( OtherOptions, Value, DeferredDataWrites,
					  IsTrackedProducer, ProbeDir );

interpret_options( [ {create_command_file_initially,Value} | _OtherOptions ],
				  _CreateCommandFileInitially, _DeferredDataWrites,
				  _IsTrackedProducer, _ProbeDir ) ->
	throw( { option_must_be_boolean, create_command_file_initially, Value } );

interpret_options( [ {deferred_data_writes,Value} | OtherOptions ],
				  CreateCommandFileInitially, _DeferredDataWrites,
				  IsTrackedProducer, ProbeDir ) when is_boolean(Value) ->
	interpret_options( OtherOptions, CreateCommandFileInitially, Value,
					  IsTrackedProducer, ProbeDir );

interpret_options( [ {deferred_data_writes,Value} | _OtherOptions ],
				  _CreateCommandFileInitially, _DeferredDataWrites,
				  _IsTrackedProducer, _ProbeDir ) ->
	throw( { option_must_be_boolean,deferred_data_writes, Value } );

interpret_options(
		  [ {register_as_tracked_producer,IsTrackedProducer} | OtherOptions ],
				  CreateCommandFileInitially, DeferredDataWrites, _WasProducer,
				  _ProbeDir ) when is_boolean(IsTrackedProducer) ->

	interpret_options( OtherOptions, CreateCommandFileInitially,
					  DeferredDataWrites, IsTrackedProducer, _ProbeDir );

interpret_options(
		  [ {register_as_tracked_producer,IsTrackedProducer} | _OtherOptions ],
				  _CreateCommandFileInitially, _DeferredDataWrites,
				  IsTrackedProducer, _ProbeDir ) ->
	throw( { option_must_be_boolean, register_as_tracked_producer,
			IsTrackedProducer } );

interpret_options( [ {probe_directory,Value} | OtherOptions ],
				  CreateCommandFileInitially, DeferredDataWrites,
				  IsTrackedProducer, _ProbeDir ) ->

	% No case file_utils:is_directory(Value) of... performed here, as we cannot
	% report warnings through logs (no State available) and a non-existing
	% directory may not be an error as it might be created later (ex: case of a
	% probe created from a test whereas the simulation is not launched yet)
	interpret_options( OtherOptions, CreateCommandFileInitially,
					   DeferredDataWrites, IsTrackedProducer, Value );

interpret_options( [ {UnknownOption,_Value} | _OtherOptions ],
				  _CreateCommandFileInitially, _DeferredDataWrites,
				  _IsTrackedProducer, _ProbeDir ) ->
	throw( { unknown_option, UnknownOption } );

interpret_options( InvalidOption, _CreateCommandFileInitially,
				  _DeferredDataWrites, _IsTrackedProducer, _ProbeDir ) ->
	throw( { invalid_option, InvalidOption } ).



% Generates the appropriate gnuplot command file.
%
% Returns an updated state.
%
% (helper)
%
-spec generate_command_file( wooper_state() ) -> wooper_state().
generate_command_file( State ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	case ?getAttr(command_file_up_to_date)
		andalso file_utils:is_existing_file( CommandFileName ) of

		true ->
			State;

		false ->

			Settings = ?getAttr(settings),

			CurveEntries = ?getAttr(curve_entries),

			ZoneEntries = ?getAttr(zone_entries),

			% Returned path ignored:
			generate_command_file( Name, Settings, CurveEntries, ZoneEntries,
						  ProbeDir ),

			setAttribute( State, command_file_up_to_date, true )

	end.





% Generates unconditionally the appropriate gnuplot command file.
%
% Returns the name, as a plain string, of the command file.
%
% Helper function defined to be shared with the data-logger.
%
-spec generate_command_file( string(), probe_settings(), curve_entries(),
	   zone_entries(), file_utils:directory_name() ) -> file_utils:file_name().
generate_command_file( Name, Settings, CurveEntries, ZoneEntries,
						  ProbeDir ) ->

	%io:format( "generate_command_file for probe '~s'.~n", [ Name ] ),

	LabelDefs = get_label_definitions( Settings#probe_settings.labels ),

	DataFilename = get_data_filename( Name ),

	PlotCommand = get_plot_command( Settings, CurveEntries, ZoneEntries,
					DataFilename ),


	XrangeOpt = get_x_range_option( Settings ),

	YrangeOpt = get_y_range_option( Settings ),


	check_probe_directory( ProbeDir ),

	PNGFilename = get_report_filename( Name ),

	CommandFilename = get_command_filename( Name, ProbeDir ),

	% Possible race condition if two processes try to create it at once:
	file_utils:remove_file_if_existing( CommandFilename ),

	% No 'delayed_write' I/O option useful here:
	File = file_utils:open( CommandFilename, ?base_open_flags ),

	%io:format( "Generating command file '~s'.~n", [ CommandFilename ] ),

	% Use 'undefined' in sample if not having available data for an element.
	% Set terminal png *transparent* could be used as well.
	ok = file:write( File, io_lib:format(
							   "set autoscale~n"
							   "unset log~n"
							   "set grid~n"
							   "set style data ~s~n"
							   "set style fill ~s~n"
							   "set key box ~s~n"
							   "set pointsize ~B~n"
							   "set xtic ~s~n"
							   "set ytic ~s~n"
							   "~s~n"
							   "~s~n"
							   "set title \"~s\"~n"
							   "set xlabel \"~s\"~n"
							   "set ylabel \"~s\"~n"
							   "set datafile missing 'undefined'~n"
							   "set terminal ~s size ~B, ~B~n"
							   "~s~n"
							   "set output \"~s\"~n"
							   "~s",
							  [
							   Settings#probe_settings.plot_style,
							   Settings#probe_settings.fill_style,
							   Settings#probe_settings.key_options,
							   Settings#probe_settings.point_size,
							   Settings#probe_settings.xtic,
							   Settings#probe_settings.ytic,
							   XrangeOpt,
							   YrangeOpt,
							   Settings#probe_settings.title,
							   Settings#probe_settings.xlabel,
							   Settings#probe_settings.ylabel,
							   Settings#probe_settings.image_format,
							   Settings#probe_settings.canvas_width,
							   Settings#probe_settings.canvas_height,
							   LabelDefs,
							   PNGFilename,
							   PlotCommand
							   ] )
					),

	ok = file:close( File ),

	CommandFilename.


% Returns the full path to the command file corresponding to specified settings.
%
% (helper)
%
get_command_filename( Name, ProbeDir ) ->
	file_utils:join( ProbeDir, get_command_filename( Name ) ).


% Generates the appropriate file containing probe data.
generate_data_file( State ) ->

	%io:format( "Generating data file '~s'.~n", [ ?getAttr(data_filename) ] ),

	% Sanity check:
	undefined = ?getAttr(data_file),

	DataTable = ?getAttr(data_table),

	case DataTable of

		[] ->
			throw( { no_available_data_sample,
					class_TraceEmitter:get_plain_name( State ) } );

		_NonEmpty ->
			ok

	end,

	% Apparently gnuplot is able to deal with unordered data sample, but we
	% prefer clearer data files anyway:
	%
	ReversedDataTable = lists:reverse( DataTable ),

	%io:format( "Data table: ~p~n", [ ReversedDataTable ] ),

	FormattedData = format_rows( ReversedDataTable, _Acc=[] ),

	DataFilename = text_utils:binary_to_string( ?getAttr(data_filename) ),

	file_utils:remove_file_if_existing( DataFilename ),

	% delayed_write would not be terribly useful here, if not
	% counter-productive:
	File = file_utils:open( DataFilename, ?base_open_flags ),

	write_header( File, ?getAttr(curve_entries), ?getAttr(zone_entries),
				 ?getAttr(settings), ?getAttr(name), ?getAttr(meta_data) ),

	ok = file:write( File, FormattedData ),
	ok = file:close( File ).



% Writes the probe header to the data file.
%
-spec write_header( file:io_device(), curve_entries(), zone_entries(),
	probe_settings(), string(), class_ResultManager:meta_data() )
				  -> basic_utils:void().
write_header( File, CurveEntries, ZoneEntries, Settings, Name, Metadata ) ->

	{ Year, Month, Day } = erlang:date(),
	{ Hour, Minute, Second } = erlang:time(),

	% Curves might have been reordered, of course we want the order of the names
	% to match the order in the data samples, so we reorder according to the
	% curve index (first element of the curve entry pair):
	ReorderedCurveEntries = lists:keysort( _Index=1, CurveEntries ),

	%io:format( "Listed curve entries: ~p~nReordered: ~p~n",
	%		  [ CurveEntries, ReorderedCurveEntries ] ),

	CurveDescriptions = format_curve_info( ReorderedCurveEntries, _Acc=[] ),

	%io:format( "Curve descriptions: ~p~n", [ CurveDescriptions ] ),

	ZoneDescriptions = format_zone_info( ZoneEntries),

	Title = Settings#probe_settings.title,

	MetadataAllStrings = [ text_utils:binary_to_string( BinText ) ||
					{ _Key, BinText } <- Metadata ],

	MetadataString = text_utils:string_list_to_string( MetadataAllStrings,
												  _Sep="# - " ),

	ok = file:write( File, io_lib:format(
		"# This time series data file has been written on ~B/~B/~B, at "
		"~B:~2..0B:~2..0B, on~n"
		"# host ~s (node: ~s).~n~n"
		"# Probe name: '~s'.~n"
		"# Probe title: '~s'.~n~n"
		"# Associated meta-data:~s~n~n"
		"# First column corresponds to the abscissa, "
		"expressed in tick offsets.~n"
		"# Next columns correspond to following curve names "
		"(in that order):~n~s~n"
		"# ~s",
		[ Day, Month, Year, Hour, Minute, Second,
		 net_utils:localhost(), net_utils:localnode(),
		 Name, Title, MetadataString, CurveDescriptions, ZoneDescriptions ] ) ).



format_curve_info( _CurveInfoList=[], Acc ) ->
	lists:reverse( Acc );

format_curve_info( [ { Num, BinName } | T ], Acc ) ->
	Entry = io_lib:format( "# - curve #~B: '~s'~n", [ Num, BinName ] ),
	format_curve_info( T, [Entry|Acc] ).


format_zone_info( _ZoneInfoList=[] ) ->
	io_lib:format( "No zone defined.~n~n", [] );

format_zone_info( ZoneInfoList ) ->
	io_lib:format( "Following zones were defined:~n"
				  ++ format_zone_info( ZoneInfoList, _Acc=[] )
				  ++ io_lib:format( "~n~n", [] ), [] ).


format_zone_info( _ZoneInfoList=[], Acc ) ->
	lists:reverse( Acc );

format_zone_info( [ { BinName, {FirstBound,SecondBound} } | T ], Acc ) ->
	Entry = io_lib:format( "# - zone '~s', extending from ~p to ~p~n",
						  [ BinName, FirstBound, SecondBound ] ),
	format_zone_info( T, [ Entry | Acc ] ).



% Triggers unconditionally an update of the command file, if requested, and
% returns an updated state.
%
% (helper)
%
trigger_command_file_update( _UpdateRequested=false, State ) ->
	State;

trigger_command_file_update( _UpdateRequested=true, State ) ->

	% Forces the update:
	generate_command_file(
					setAttribute( State, command_file_up_to_date, false ) ).



% Returns the gnuplot command filename.
%
-spec get_command_filename( string() ) -> file_utils:file_name().
get_command_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".p" ).



% Returns the gnuplot data filename.
%
-spec get_data_filename( string() ) -> file_utils:file_name().
get_data_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".dat" ).



% Returns the report filename.
%
-spec get_report_filename( string() ) -> file_utils:file_name().
get_report_filename( Name ) ->
	file_utils:convert_to_filename( Name ++ ".png" ).
	%file_utils:convert_to_filename( Name ++ ".svg" ).



% Formats the rows of the data table
%
% (could be a list comprehension as well)
%
format_rows( _DataTable=[], Acc ) ->
	Acc;

format_rows( [ { Tick, DataTuple } | T ], Acc ) ->
	% Not sure it is really efficient, maybe a list accumulated, reversed then
	% joined would behave better:
	format_rows( T, Acc ++ format_row( Tick, DataTuple ) ).


% Formats specified row of data, using specified tick.
%
-spec format_row( probe_tick(), sample_data() ) -> string().
format_row( Tick, DataTuple ) ->
	io_lib:format( "~B ~s~n", [ round(Tick), make_data_row(DataTuple) ] ).



% Returns a string contained the samples.
%
make_data_row( DataTuple ) ->
	make_data_row( erlang:tuple_to_list(DataTuple), [] ).



% Transforms the list into a string.
%
make_data_row( [], Acc ) ->
	lists:reverse(Acc);

make_data_row( [ H | T ], Acc ) ->
	make_data_row( T, io_lib:format( "~w ", [H] ) ++ Acc ).



% Used for direct data writing.
%
-spec write_row( file:io_device(), probe_tick(), sample_data() ) ->
					   basic_utils:void().
write_row( File, Tick, DataTuple ) ->
	ok = file:write( File, format_row( Tick, DataTuple ) ).



% Creates or updates an entry for specified curve.
%
% (oneway)
%
-spec setFilledCurvesOptions( wooper_state(), string(), curve_index() ) ->
									wooper_state().
setFilledCurvesOptions( State, CurveName, ColumnSpecifier ) ->
	setFilledCurvesOptions( State, CurveName, ColumnSpecifier,
						   _GenerateFile=false ).




% Creates or updates an entry for specified curve.
%
% (oneway)
%
-spec setFilledCurvesOptions( wooper_state(), string(), curve_index(),
							 boolean() ) -> oneway_return().
setFilledCurvesOptions( State, CurveName, ColumnSpecifier, GenerateFile ) ->

	SpecifiedFilledCurve = { CurveName,
							_Plot_style="filledcurves",
							_ColumnSpecifier=ColumnSpecifier },

	FilledCurveList = ?getAttr(filled_curve_list),

	% Verifying whether the curve exits already in the filled curve list;
	% if yes, its setting option will be replaced by the new one:
	UpdatedList = case lists:keyfind( CurveName, 1, FilledCurveList ) of

			false ->
				% Just appends:
				[ SpecifiedFilledCurve | FilledCurveList ];

			Tuple ->
				% Erases and replaces:
				NewList = lists:delete( Tuple, FilledCurveList ),
				[ SpecifiedFilledCurve | NewList ]

		end,

	UpdatedState = setAttribute( State, filled_curve_list, UpdatedList ),

	% Forces the regeneration of the command file if requested:
	CommandState = trigger_command_file_update( GenerateFile, UpdatedState ),

	?wooper_return_state_only( CommandState ).



% Returns the Gnuplot command appropriate to render all registered labels.
%
-spec get_label_definitions( [ probe_label() ] ) -> string().
get_label_definitions( Labels ) ->
	text_utils:join( _Separator="\n",
					get_label_definitions( Labels, _Acc=[] ) ).


get_label_definitions( _Labels=[], Acc ) ->
	% Nothing to reverse, labels will end up being rendered in the order they
	% were specified:
	Acc;

get_label_definitions( [ #probe_label{ location={X,Y}, text=BinText,
	  color=Color, position=Position, orientation=Orientation } | T ], Acc ) ->

	% For a list of supported colors, see:
	% www.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm
	%
	ActualColor = gui_color:get_color_for_gnuplot( Color ),

	LabelString = io_lib:format(
			 "set label \"~s\" at ~p,~p ~s ~s textcolor rgbcolor \"~s\"",
			 [ text_utils:binary_to_string( BinText ), X, Y, Position,
			  get_formatted_orientation( Orientation ),  ActualColor] ),

	get_label_definitions( T, [ LabelString | Acc ] ).



% Returns the abscissa range options as read from the specified settings.
%
-spec get_x_range_option( probe_settings() ) -> string().
get_x_range_option( Settings ) ->

	case Settings#probe_settings.x_range of

			undefined ->
				 "# No xrange set.";

			{ MinX, MaxX } when is_integer(MinX) andalso is_integer(MaxX) ->
				 io_lib:format( "set xrange [~B:~B]", [ MinX, MaxX ] )

	end.



% Returns the ordinate range options as read from the specified settings.
%
-spec get_y_range_option( probe_settings() ) -> string().
get_y_range_option( Settings ) ->

	case Settings#probe_settings.y_range of

			undefined ->
				 "# No yrange set.";

			{ MinY, MayY } when is_integer(MinY) andalso is_integer(MayY) ->
				 io_lib:format( "set yrange [~B:~B]", [ MinY, MayY ] )

	end.



% Returns the Gnuplot command appropriate to render that probe output.
%
% Defines one plot curve per declared curve, with current plot settings, and
% defines as well any specified zone.
%
-spec get_plot_command( probe_settings(), curve_entries(), zone_entries(),
					  file_utils:file_name() ) -> string().
get_plot_command( Settings, CurveEntries, ZoneEntries, DataFilename ) ->

	case length(CurveEntries) + length(ZoneEntries) of

		0 ->
			throw( no_curve_nor_zone_defined );

		_ ->
				  ok

	end,

	% Typical expected output:
	%
	% plot 'silver.dat' using 1:2 with lines, 'silver.dat' using 1:3 with lines,
	% 'silver.dat' using 1:2:3 with filledcurves

	% For us it is: "plot " ++ tl( join( _Prefix=", 'silver.dat' using 1:",
	%  [ "2 with lines", "3 with lines", "2:3 with filledcurves" ]
	%
	% (knowing that tl is used to remove the prefix head (','), i.e. the first
	% extra comma)
	%
	% So:

	Prefix = io_lib:format( ", \"~s\" using 1:", [ DataFilename ] ),

	% We prefer not rendering curves that are used to delimit zones:
	FilteredCurveEntries = remove_zone_specific_curves( CurveEntries,
									ZoneEntries ),

	CurvePlots = get_plot_commands_for_curves( FilteredCurveEntries, Settings ),

	ZonePlots = get_plot_commands_for_zones( ZoneEntries, Settings ),

	% Note that we specify the zones first, otherwise the curves would be hidden
	% below:
	JoinedCommand = text_utils:join( Prefix, ZonePlots ++ CurvePlots ),

	% tl to remove prefix head (','):
	io_lib:format( "plot ~s~s~n", [ tl(Prefix), JoinedCommand ] ).


% Returns a list of curve entries in which there is no more curves that are used
% to define a zone among the ones specified.
%
-spec remove_zone_specific_curves( curve_entries(), zone_entries() ) ->
				curve_entries().
remove_zone_specific_curves( CurveEntries, ZoneEntries ) ->

	CurvesToRemove = select_curves( ZoneEntries, _Acc=[] ),

	Selector = fun( { CurveIndex, _CurveName } ) ->
					   not lists:member( CurveIndex, CurvesToRemove ) end,

	lists:filter( Selector, CurveEntries ).



% Selects all curve index that are mentioned (possibly with some duplicates).
%
select_curves( _ZoneEntries=[], Acc ) ->
	Acc;

select_curves( _ZoneEntries=[ { _ZoneName, {abscissa_top,C} } | T ], Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, {abscissa_bottom,C} } | T ], Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, {C1,C2} } | T ], Acc ) ->
	select_curves( T, [ C1, C2 | Acc ] ).



-spec get_plot_commands_for_curves( curve_entries(), probe_settings() ) ->
			[ string() ].
get_plot_commands_for_curves( CurveEntries, Settings ) ->

	% After some potential reordering, curve entries might be:
	% [ {3,<<"c">>}, {1,<<"a">>}, {2,<<"b">>} ]
	%
	% We expect: [ "4 title \"c\"", "2 title \"a\"", "3 title \"b\"" ]
	%
	% (note that each curve index is incremented, as the first column is the
	% tick)
	%
	% We simply write them in-order, with an appropriate title:
	[ get_curve_command( C, Settings ) || C <- CurveEntries ].


% Returns a command element suitable to render specified curve.
%
% (helper)
%
-spec get_curve_command( curve_entry(), probe_settings() ) -> string().
get_curve_command( { CurveIndex, BinCurveName }, _Settings ) ->

	Title = text_utils:binary_to_string( BinCurveName ),

	% +1 to accound for the abscissa column:
	io_lib:format( "~B title \"~s\"", [ CurveIndex+1, Title ] ).




-spec get_plot_commands_for_zones( zone_entries(), probe_settings() ) ->
			[ string() ].
get_plot_commands_for_zones( ZoneEntries, Settings ) ->

	% Zone entries are a list of:
	% {BinZoneName,{ExtendedCurveName1,ExtendedCurveName2}}
	%
	% We expect returned values to be either "3:5 with filledcurves" (for a zone
	% between curves 2 and 4) or "3 with filledcurves x1" (for a zone between
	% curve 2 and the abscissa axis):
	[ get_zone_command( Z, Settings ) || Z <- ZoneEntries ].



% Returns a command element suitable to render specified zone.
%
% (helper)
%
-spec get_zone_command( zone_definition(), probe_settings() ) -> string().
get_zone_command( _ZoneEntry={ BinZoneName,
			{ FirstExtendedCurve, SecondExtendedCurve } }, _Settings ) ->


	FirstPart = case FirstExtendedCurve of

		'abscissa_top' ->
			% The other is necessarily an index (+1 as the first column is the
			% tick):
			ActualCurveIndex = SecondExtendedCurve + 1,
			io_lib:format( "~B with filledcurves x2", [ ActualCurveIndex ] );

		'abscissa_bottom' ->
			% The other is necessarily an index (+1 as the first column is the
			% tick):
			ActualCurveIndex = SecondExtendedCurve + 1,
			io_lib:format( "~B with filledcurves x1", [ ActualCurveIndex ] );

		_BinCurveName ->
			ActualFirstCurveIndex = FirstExtendedCurve + 1,
			ActualSecondCurveIndex = SecondExtendedCurve + 1,
			io_lib:format( "~B:~B with filledcurves",
						  [ ActualFirstCurveIndex, ActualSecondCurveIndex ] )

	end,

	FirstPart ++ io_lib:format( " title \"~s\"",
						 [ text_utils:binary_to_string( BinZoneName ) ] ).




% Actual (synchronous) generation of the report.
%
% Returns an updated state.
%
% (helper function)
%
generate_report( Name, State ) ->

	TargetFilename = get_report_filename( Name ),

	%% case file_utils:is_existing_file( TargetFilename ) of

	%%	true ->
	%%		?warning_fmt( "Warning, file '~s' was already existing, "
	%%					"it has been removed.", [ TargetFilename ] ),
	%%		file_utils:remove_file( TargetFilename );

	%%	false ->
	%%			ok

	%% end,

	file_utils:remove_file_if_existing( TargetFilename ),

	?info_fmt( "Generation of probe report '~s' requested.",
			  [ TargetFilename ] ),

	% To allow specialized probes to override the command generation:
	CommandState = executeOneway( State, generateCommandFile ),

	ensure_data_file_available( CommandState ),

	ProbeDir = ?getAttr(probe_dir),

	% Generates a PNG:
	% (Gnuplot might issue non-serious warnings)
	%
	CommandFilename = get_command_filename( Name ),

	%io:format( "Generating plot based on ~s.~n", [ GeneratingFile ] ),

	% We must change the current directory (in the command, preferably not the
	% one of the whole VM) otherwise the PNG will be created in the test case
	% directory; specifying in the command file an absolute path for the PNG is
	% not an option either, as we move the files to the result directory
	Message = case os:cmd( "cd " ++ ProbeDir ++ " && gnuplot "
						  ++ CommandFilename ) of

		[] ->
			[];

		M ->
			% No trailing dot, as at least some messages already end with a new
			% line:
			%
			?warning_fmt( "Report generation resulted in following output: ~s",
				[ M ] ),
			M

	end,

	case file_utils:is_existing_file(
								file_utils:join( ProbeDir, TargetFilename ) ) of

		true ->
			ok;

		false->
			case Message of

				[] ->
					throw( { report_generation_failed_for, Name } );

				_ ->

					throw( { report_generation_failed_for, Name, Message } )

			end

	end,

	CommandState.



% Ensures that the command file for this probe is available.
%
% Returns an updated state.
%
% (helper)
%
ensure_command_file_available( State ) ->

	Name = text_utils:binary_to_string( ?getAttr(name) ),

	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	case file_utils:is_existing_file( CommandFileName ) of

		true ->
			State;

		false ->
			% Returned state is thrown away:
			executeOneway( State, generateCommandFile )

	end.




% Ensures that the data file for this probe is available.
%
% (const helper function, not returning anything useful)
%
ensure_data_file_available( State ) ->

	case ?getAttr(deferred_data_writes) of

		true ->
			generate_data_file( State );

		false ->
			% Here we need to have the data file available, regardless of
			% buffering:
			ok = file:sync( ?getAttr(data_file) )

	end.



% Returns the Gnuplot reference version for us.
%
% (static)
%
-spec get_gnuplot_reference_version() -> basic_utils:two_digit_version().
get_gnuplot_reference_version() ->
	?gnuplot_reference_version.



% Serialisation section.
%
% Hooks are defined so that the WOOPER-provided serialise/3 request is
% customised for probes.


% Triggered just before serialisation.
%
% We are to fix file handles here. The PIDs (none is internal to a probe) will
% be converted later by the entry transformer.
%
-spec pre_serialise_hook( wooper_state() ) -> wooper_state().
pre_serialise_hook( State ) ->

	NewDataFileValue = case ?getAttr(data_file) of

		undefined ->
			undefined;

		_File ->
			resilience_recreate_data_file

	end,

	setAttributes( State, [

			{ data_filename, undefined },
			{ data_file, NewDataFileValue },
			{ probe_dir, undefined },
			{ gnuplot_version, undefined }

							] ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
% Instead of returning a mere { Classname, Entries } tuple for binarisation, a
% probe returns a more complete { Classname, Entries, BinCommand, BinData }
% tuple.
%
-spec post_serialise_hook( class_name(), term_serialisation(), wooper_state() )
						 -> term().
post_serialise_hook( Classname, Entries, State ) ->

	% Content of the command file (if any):

	Name = text_utils:binary_to_string( ?getAttr(name) ),
	ProbeDir = ?getAttr(probe_dir),

	CommandFileName = get_command_filename( Name, ProbeDir ),

	BinCommand = case file_utils:is_existing_file( CommandFileName ) of

		true ->
			file_utils:read_whole( CommandFileName );

		false ->
			undefined

	end,

	% Content of the data file (if any):

	DataFilename = file_utils:join( ProbeDir, get_data_filename( Name ) ),

	BinData = case file_utils:is_existing_file( DataFilename ) of

		true ->
			file_utils:read_whole( DataFilename );

		false ->
			undefined

	end,

	{ Classname, Entries, BinCommand, BinData }.





% Triggered just before deserialisation.
%
% Here we mostly perform the reverse operations done in post_serialise_hook/3.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, _Entries, _BinCommand,
										   _BinData }, _UserData ) ->
	%Entries.
	throw( fixme ).


% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper_state() ) -> wooper_state().
post_deserialise_hook( State ) ->
	State.



% Deserialises a probe from specified binaries.
%
% Never returns: the probe takes ownership of the process.
%
% (static)
%
-spec deserialise( binary(), binary(), binary(), pid() ) -> no_return().
deserialise( BinContent, BinCommand, _BinData, ReaderPid ) ->

	% Let's start with the files:
	{ _CommandFilename, _CommandBinContent } = binary_to_term( BinCommand ),

	{ Classname, RawEntries } = binary_to_term( BinContent ),

	% Too early to transform serialisation markers, as we need up-to-date
	% instance trackers beforehand:
	%
	% (we just update the local tracker with information about that actor)
	%
	% Will never return:
	%
	apply( Classname, wooper_deserialise, [ RawEntries,
			_EntryTransformer=undefined, _UserData=undefined,
			_ListenerPid=ReaderPid ] ),

	throw( fixme_not_implemented_yet ).



% Returns a Gnuplot-compatible rotation specification.
%
% (static)
%
-spec get_formatted_orientation( label_orientation() ) -> string().
get_formatted_orientation( upright ) ->
	"norotate";

get_formatted_orientation( Angle ) when is_number(Angle) ->
	io_lib:format( "rotate by ~p", [ Angle ] ).




% Helper section.


% Transforms a list of names into a list of pairs {Number,Name} where Number is
% the index of the name in the list (starting at 1), and Name is a binary name.
%
% Respects the order of specified names.
%
% Ex: transform_curve_names( [ "a", "b", "c" ] ) should result in:
% [ {1,<<"a">>}, {2,<<"b">>}, {3,<<"c">>} ].
%
-spec transform_curve_names( [ declared_curve_name() ] ) ->
					curve_entries().
transform_curve_names( NameList ) ->
	transform_curve_names( NameList, _Acc=[], _Count=1 ).


transform_curve_names( _NameList=[], Acc, _Count ) ->
	lists:reverse( Acc );

transform_curve_names( [ H | T ], Acc, Count ) ->
	transform_curve_names( T, [ {Count,text_utils:string_to_binary(H)} | Acc ],
					 Count+1 ).






% Transforms a list of zone declarations into actual zone entries, while
% checking them against the curve names.
%
-spec transform_declared_zones( [ declared_zone() ], curve_entries() )
							  -> [ zone_definition() ].
transform_declared_zones( DeclaredZones, CurveEntries ) ->
	transform_declared_zones( DeclaredZones, CurveEntries, _Acc=[] ).


transform_declared_zones( _DeclaredZones=[], _CurveEntries, Acc ) ->
	% We preserve order here as well, otherwise zones will listed in the keys in
	% reverse order:
	lists:reverse(Acc);

transform_declared_zones( [ Z={ ZoneName,
							{ FirstCurveName, SecondCurveName } } | T ],
							CurveEntries, Acc ) ->

	First = get_curve_index_for( FirstCurveName, CurveEntries ),
	Second = get_curve_index_for( SecondCurveName, CurveEntries ),

	% We want to ensure:
	%
	%  1. that at least one actual curve is referenced (not two 'abscissa_*'
	%  atoms)
	%
	%  2. that if there is one 'abscissa_*' atom, it ends up in first position
	%  of the returned pair
	%
	NewBounds = case First of

		_ when First == 'abscissa_top' orelse First == 'abscissa_bottom' ->

			case Second of

				_ when Second == 'abscissa_top' orelse
					   Second == 'abscissa_bottom' ->
					throw( { curveless_zone, Z } );

				_ ->
					{ First, Second }

			end;

		_ ->
			% So that we are sure that any abscissa_* atom would end up in first
			% position:
			{Second,First}

	end,

	ZoneBinName = text_utils:string_to_binary( ZoneName ),

	transform_declared_zones( T, CurveEntries,
							 [ { ZoneBinName, NewBounds } | Acc ] ).




% Returns an appropriate curve index to define internally a zone.
%
-spec get_curve_index_for( declared_extended_curve_name(), curve_entries() ) ->
								 extended_curve_name().
get_curve_index_for( CurveName='abscissa_top', _CurveEntries ) ->
	CurveName;

get_curve_index_for( CurveName='abscissa_bottom', _CurveEntries ) ->
	CurveName;

get_curve_index_for( CurveName, CurveEntries ) ->

	BinCurveName = text_utils:string_to_binary( CurveName ),

	case lists:keyfind( _Key=BinCurveName, _Index=2, CurveEntries ) of

		false ->
			throw( { zone_specified_unknown_curve, CurveName, CurveEntries } );

		{ CurveIndex, _BinCurveName } ->
			CurveIndex

	end.



% Checks that the (specified) probe directory is indeed existing.
%
check_probe_directory( ProbeDir ) ->

	case file_utils:is_existing_directory( ProbeDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_probe_directory, ProbeDir } )

	end.
