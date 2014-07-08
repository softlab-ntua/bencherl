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


% The name of a probe (ex: to be checked against a result specification):
-type probe_name() :: class_ResultProducer:producer_name().


% The tick for a probe corresponds to a tick offset:
-type probe_tick() :: class_TimeManager:tick_offset().



% Corresponds to the 2D coordinates of the label on the plot:
-type label_location() :: linear_2D:point().


% Actual text of the label:
-type label_text() :: text_utils:bin_string().


% Color of the text (default: "blue"):
-type label_color() :: gui_color:color().


% Describes the position of the text based on to the specified location for the
% label:
-type label_position() :: 'left' | 'center' | 'right'.


% Describes whether the text of the label should be rendered with an angle from
% the abscissa axis:
%
-type label_orientation() :: 'upright' | unit_utils:int_degrees().


% Fully defines a label on a probe rendering:
-record( probe_label, {

		% 2D coordinates of the label on the plot:
		location :: label_location(),

		% Actual text of the label:
		text :: label_text(),

		% Color of the text:
		color :: label_color(),

		% Position of the text based on to the location for the label:
		position :: label_position(),

		% The label may be rendered with an angle from the abscissa axis:
		orientation :: label_orientation()

					  } ).


-type probe_label() :: #probe_label{}.



% Records the (rendering) settings of a probe.
%
% Used by plain probes and by the data-logger.
%
-record( probe_settings, {

	% Title of any probe report (as a binary):
	title :: binary(),

	% Key (legend) options (as a binary):
	key_options :: binary(),

	% Label for the abscissa axis (as a binary):
	xlabel :: binary(),

	% Label for the ordinate axis (as a binary):
	ylabel :: binary(),

	% Settings for layout along the abscissa axis (as a binary):
	xtic :: binary(),

	% Settings for layout along the ordinate axis (as a binary):
	ytic :: binary(),

	% Abscissa range (pair of {MinX,MaxX} integers, or 'undefined'):
	x_range :: { gui:coordinate(), gui:coordinate() },

	% Ordinate range (pair of {MinY,MaxY} integers, or 'undefined'):
	y_range :: { gui:coordinate(), gui:coordinate() },

	% Defines how graphs should be rendered (as a binary):
	% (default is 'lines', use 'linespoints' to add a symbol on top of each
	% point)
	plot_style :: binary(),

	% Defines the size of each point, set pointsize 2 means the point size is
	% twice the default size.
	point_size = 1 :: non_neg_integer(),

	% Defines how areas like histograms should be filled (as a binary):
	fill_style :: binary(),

	% Defines the width of the canvas, i.e. the actual width, in pixels, of the
	% corresponding plot:
	canvas_width = 1600 :: gui:length(),

	% Defines the height of the canvas, i.e. the actual height, in pixels, of
	% the corresponding plot:
	canvas_height = 800 :: gui:length(),

	% The default image format for probe rendering (as a binary):
	image_format = <<"png">> :: binary(),

	% Lists the arbitrary labels that may be defined over the probe rendering:
	labels = [] :: [ probe_label() ]

}).


-type probe_settings() :: #probe_settings{}.


-type probe_option() ::  { 'create_command_file_initially', boolean() }
					   | { 'deferred_data_writes', boolean() }
					   | { 'probe_directory', file_utils:directory_name() }.


-type probe_options() :: [ probe_option() ].


% The internal name for a curve:
-type curve_name() :: text_utils:bin_string().


% A tuple of data (numbers) to be sent as sample to a probe_like result
% producer:
%
-type sample_data() :: tuple().
