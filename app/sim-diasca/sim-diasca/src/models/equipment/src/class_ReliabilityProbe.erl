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


% Reliability Probe class, regarding failures and reparations of an equipment.
% See class_Probe.erl
%
-module(class_ReliabilityProbe).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Probe ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Name, Title ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		synchronous_new/2, synchronous_new_link/2,
		synchronous_timed_new/2, synchronous_timed_new_link/2,
		remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		construct/3, delete/1 ).


% Member method declarations.
-define( wooper_method_export, generateCommandFile/1 ).


% For the probe settings:
-include("class_Probe.hrl").



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Probe.Reliability").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% Green when ok:
-define( nominal_color, "#3ab001" ).

% Red when failed:
-define( failed_color, "#ec0505" ).



% Constructs a new Probe:
%
% - Name is the name of this probe, and will be used for the generated data and
% command files
%
% - Title will be the graph title
%
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	ProbeState = class_Probe:construct( State, Name,
		_Curves=[ "Equipment State" ],
		_Zones=[],
		Title,
		_XLabel="Duration",
		_YLabel="Failure state",
		_MetaData=[] ),

	% Updates the inherited settings:
	ProbeSettings = update_probe_settings(
									 getAttribute( ProbeState, settings ) ),

	% Then the class-specific actions:
	%
	% Overrides probe default settings, sets the probe output to reliability
	% mode, to track the changes in the state of an equipment:
	%
	% (curve_count set to 1 whereas two curve names given: above/below)
	StartState = setAttributes( ProbeState, [

		{settings,ProbeSettings},
		{trace_categorization,?TraceEmitterCategorization}

											 ] ),

	?send_trace( ProbeState, "New reliability probe created." ),

	StartState.



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?trace( "Deleting reliability probe." ),

	?debug( "Reliability probe deleted." ),

	% Then allow chaining:
	State.




% Methods section.



% Generates the appropriate gnuplot command file.
%
% Note: mostly defined to override its inherited version and branch to the
% helper just below.
%
% (oneway)
%
generateCommandFile( State ) ->

	Settings = ?getAttr(settings),

	Name = class_TraceEmitter:get_plain_name( State ),

	LabelDefs = class_Probe:get_label_definitions(
											  Settings#probe_settings.labels ),

	PlotCommand = get_plot_command( Name, State ),

	ProbeDir = ?getAttr(probe_dir),

	class_Probe:check_probe_directory( ProbeDir ),

	PNGFilename = class_Probe:get_report_filename( Name ),

	CommandFilename = file_utils:join( ProbeDir,
								  class_Probe:get_command_filename( Name ) ),

	XrangeOpt = class_Probe:get_x_range_option( Settings ),

	YrangeOpt = class_Probe:get_y_range_option( Settings ),

	% No 'delayed_write' I/O option useful here:
	File = file_utils:open( CommandFilename, [ raw, write, exclusive ] ),

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

	?wooper_return_state_only( State ).




% Helper section.


% Returns (as a plain string) an appropriate gnuplot command for this probe.
%
% (helper)
%
get_plot_command( Name, State ) ->

	% Not wanting a full path here:
	DataFilename = class_Probe:get_data_filename( Name ),

	Settings = ?getAttr(settings),

	PlotStyle = Settings#probe_settings.plot_style,

	FirstTitle = "Equipment is functional",

	SecondTitle = "Equipment is non-functional",

	io_lib:format( "plot \"~s\" using 1:2:(0.0) title \"~s\" with ~s above "
			"lt rgb \"~s\",  \"~s\" using 1:2:(0.0) title \"~s\" with ~s below "
			"lt rgb \"~s\"",
			[ DataFilename, FirstTitle,  PlotStyle, ?nominal_color,
			  DataFilename, SecondTitle, PlotStyle, ?failed_color ] ).



% Returns a probe_settings record with updated informations (expressed as
% plain strings) and default values for the other fields.
%
-spec update_probe_settings( probe_settings() ) -> probe_settings().
update_probe_settings( Settings ) ->

	Settings#probe_settings{

		xtic=text_utils:string_to_binary( "auto" ),

		ytic=text_utils:string_to_binary( "0" ),

		y_range={ -2, 2 },

		xlabel=text_utils:string_to_binary(
										 "Equipment in Nominal Conditions" ),

		ylabel=text_utils:string_to_binary( "Equipment in Failure" ),

		plot_style=text_utils:string_to_binary( "filledcurves" )

					}.
