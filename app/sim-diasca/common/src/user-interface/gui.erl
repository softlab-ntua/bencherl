% Copyright (C) 2010-2014 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% Gathering of various facilities for Graphical User Interfaces.
%
% See gui_test.erl for the corresponding test.
%
-module(gui).


% For canvas record:
-include("gui_canvas.hrl").


-include("polygon.hrl").




% Rendering of GUI elements.
%
% Formerly based on the gs module, now on the wx one.



% Implementation notes:

% This module used to rely on the gs module, whose API was quite simple and
% elegant.
%
% As GS was replaced (quite quickly, unfortunately) by WxWindows, now we rely on
% the latter.
%
% The general convention is still to put as first argument the object on which
% the operation is to be applied (ex: the window).
%
% See also: gui_canvas.erl for all canvas-related operations.



% For colors, refer to the color module.


% Basic GUI operations.
%
-export([ start/0, start/1, set_debug_level/1, stop/0 ]).




% Widget-related section.
%



% General-purpose:
%
-export([ id_to_widget/1, connect/2, connect/3, disconnect/1, set_tooltip/2 ]).


% Windows:
%
-export([ create_window/0, create_window/1, create_window/2, create_window/5,
		  set_background_color/2, set_sizer/2, show/1, hide/1, get_size/1 ]).


% Frames:
%
-export([ create_frame/0, create_frame/1, create_frame/3, create_frame/4,
		  create_frame/6 ]).


% Panels:
%
-export([ create_panel/0, create_panel/1, create_panel/2, create_panel/5,
		  create_panel/6 ]).


% Buttons:
%
-export([ create_button/6 ]).


% Sizers:
%
-export([ create_sizer/1, create_sizer_with_box/2,
		  create_sizer_with_labelled_box/3, add_to_sizer/2, add_to_sizer/3,
		  clear_sizer/1, clear_sizer/2 ]).


% Status bars:
%
-export([ create_status_bar/1, push_status_text/2 ]).


% Gui_Canvas: defined in canvas.erl.




% For related defines:
-include("gui.hrl").



% Type declarations:

-type length() :: linear:distance().
-type coordinate() :: linear:integer_coordinate().


% linear_2D:point() would allow for floating-point coordinates:
-type point() :: linear_2D:integer_point().
-type position() :: point() | 'auto'.

-type size() :: { linear:integer_distance(), linear:integer_distance() }
			  | 'auto'.


% Identifier of a GUI elements are integers.


% Allows to specify a 'void' (null) ID of a GUI element:
%
% (note: this type is defined and exported, yet reported unknown by Dialyzer)
%
-type id() :: 'undefined' | integer().


% See any_id, no_parent, etc. as defined in gui.hrl.


% A vertical orientation means piling elements top to bottom for example, while
% an horizontal means left to right, for example.
%
-type orientation() :: 'vertical' | 'horizontal'.


% Widget types.

-type gui_object() :: wx:wx_object().

-type window() :: 'undefined' | wxWindow:wxWindow() | gui_canvas:canvas().

-type frame() :: wxFrame:wxFrame().

-type panel() :: wxPanel:wxPanel().

-type button() :: wxButton:wxButton().

-type sizer() :: wxSizer:wxSizer().

-type sizer_item() :: wxSizerItem:wxSizerItem().

-type status_bar() :: wxStatusBar:wxStatusBar().

-type bitmap() :: wxBitmap:wxBitmap().

-type back_buffer() :: wxMemoryDC:wxMemoryDC().





-type void() :: basic_utils:void().


-type text() :: text_utils:unicode_string().

-type title() :: text().
-type label() :: text().


% User data, as specified in the connect call:
-type user_data() :: any().



% Event types.
%
% In general we promote managing events thanks to messages rather than thanks to
% callbacks, as the former can access easily to the full context of the program
% whereas the latter is only executed into a transient process.


-type event_source() :: wxEvtHandler:wxEvtHandler() | gui_canvas:canvas().


% Using the wx-event type, leaked by wx.hrl:
%-type event_type() :: event().
-type event_type() :: EventType::wxEventType().


% A #wx record comprises:
%
% - the ID of the event source
% - a reference onto it (ex: {wx_ref,35,wxFrame,[]})
% - user-specified data (typically [])
% - the information about the event itself (ex: {wxClose,close_window})
%
% The actual event source can be found either directly (through its reference)
% or from its ID (see id_to_widget/1). Best option seems to be the first one, in
% the general case.
%
% Same as: -record( wx,...
%
-type gui_event() :: { 'wx', id(), gui_object(), user_data(), event_type() }.


% Options for windows, see:
% http://docs.wxwidgets.org/stable/wx_wxwindow.html and
% http://docs.wxwidgets.org/stable/wx_windowstyles.html#windowstyles

-type window_style_opt() ::  'default'
						   | 'simple_border'
						   | 'double_border'
						   | 'sunken_border'
						   | 'raised_border'
						   | 'static_border'
						   | 'theme_border'
						   | 'no_border'
						   | 'transparent'
						   | 'tab_traversable'
						   | 'grab_all_keys'
						   | 'with_vertical_scrollbar'
						   | 'with_horizontal_scrollbar'
						   | 'never_hide_scrollbars'
						   | 'clip_children'
						   | 'grab_all_keys'
						   | 'full_repaint_on_resize'.


-type window_style() :: window_style_opt() | [ window_style_opt() ].

-type window_option() :: { pos, point() }
					   | { size, size() }
					   | {style, [ window_style_opt() ] }.

% Unused: -type window_options() :: [ window_option() ].



% Options for frames, see:
% http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe
%
-type frame_style_opt() ::   'default'
						   | 'caption'
						   | 'minimize'
						   | 'minimize_box'
						   | 'maximize'
						   | 'maximize_box'
						   | 'close_box'
						   | 'stay_on_top'
						   | 'system_menu'
						   | 'resize_border'
						   | 'tool_window'
						   | 'no_taskbar'.


-type frame_style() :: frame_style_opt() | [ frame_style_opt() ].


% Options for panels, see:
% http://docs.wxwidgets.org/stable/wx_wxpanel.html#wxpanelwxpanel
%
-type panel_option() :: window_option().

-type panel_options() :: [ panel_option() ].



% Options for button style, see:
% http://docs.wxwidgets.org/stable/wx_wxbutton.html#wxbuttonwxbutton
%
-type button_style_opt() ::  'default'
						   | 'left_justified'
						   | 'right_justified'
						   | 'top_justified'
						   | 'bottom_justified'
						   | 'exact_fit'
						   | 'flat'.


-type button_style() :: button_style_opt() | [ button_style_opt() ].



% Options for sizers, see:
% http://docs.wxwidgets.org/stable/wx_wxsizer.html
%
-type sizer_flag_opt() ::  'default'
						   | 'top_border'
						   | 'bottom_border'
						   | 'left_border'
						   | 'right_border'
						   | 'all_borders'
						   | 'expand_fully'
						   | 'expand_shaped'
						   | 'fixed_size'
						   | 'counted_even_if_hidden'
						   | 'align_center'
						   | 'align_left'
						   | 'align_right'
						   | 'align_top'
						   | 'align_bottom'
						   | 'align_center_vertical'
						   | 'align_center_horizontal'.


-type sizer_flag() :: sizer_flag_opt() | [ sizer_flag_opt() ].

-type sizer_option() :: { 'proportion', integer() }
					  | { 'flag', sizer_flag() }
					  | { 'border', integer() }
					  | { 'userData', gui_object() }.


-type sizer_options() :: [ sizer_option() ].


% Options for event management connections:
%
-type connect_opt() ::   { 'id', integer() }
					   | { lastId, integer() }
					   | { skip, boolean() }
					   |   callback
					   | { callback, function() }
					   | { userData, term() }.


-type connect_options() :: connect_opt() | [ connect_opt() ].

-type debug_level_opt() :: 'none' | 'calls' | 'life_cycle'.
-type debug_level() :: debug_level_opt() | [ debug_level_opt() ].

-type error_message() :: term().



-export_type ([
				length/0, coordinate/0, point/0, id/0,
				orientation/0,
				window/0, frame/0, panel/0, button/0, sizer/0, status_bar/0,
				bitmap/0, back_buffer/0,
				gui_event/0,
				window_style/0, frame_style/0, button_style/0,
				sizer_flag/0,
				error_message/0
			  ]).




% Section for basic GUI operations.


% Starts the GUI sub-system.
%
-spec start() -> gui_object().
start() ->
	%wx:new().
	wx:new().



% Starts the GUI sub-system, with specified debug level.
%
-spec start( debug_level() ) -> gui_object().
start( DebugLevel ) ->
	ServerId = wx:new(),
	set_debug_level( DebugLevel ),
	ServerId.



-spec set_debug_level( debug_level() ) -> void().
set_debug_level( DebugLevels ) when is_list( DebugLevels ) ->
	wx:debug( [ convert_debug_level( L ) || L <- DebugLevels ] );

set_debug_level( DebugLevel ) ->
	set_debug_level( [ DebugLevel ] ).



% Stops the GUI sub-system.
%
-spec stop() -> void().
stop() ->
	ok = wx:destroy().





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Widget section.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Common section.


% Returns the widget corresponding to the specified identifier.
%
-spec id_to_widget( id() ) -> window().
id_to_widget( Id ) ->
	wxWindow:findWindowById( Id ).


% Requests the specified widget to send a message-based event when the specified
% kind of event happens, overriding its default behaviour.
%
-spec connect( event_source(), event_type() ) -> void().
connect( #canvas{ panel=Panel }, EventType ) ->
	connect( Panel, EventType );

connect( EventSource, EventType ) ->
	wxEvtHandler:connect( EventSource, EventType ).


% Requests the specified widget to send a message-based event when the specified
% kind of event happens, overriding its default behaviour based on specified
% options.
%
-spec connect( event_source(), event_type(), connect_options() ) -> void().
connect( #canvas{ panel=Panel }, EventType, Options ) ->
	connect( Panel, EventType, Options );

connect( EventSource, EventType, Options ) ->
	wxEvtHandler:connect( EventSource, EventType, Options ).



% Unsubscribes the event source, for all event types.
%
-spec disconnect( event_source() ) -> boolean().
disconnect( #canvas{ panel=Panel } ) ->
	disconnect( Panel );

disconnect( EventSource ) ->
		wxEvtHandler:disconnect( EventSource ).



% Attaches a tooltip to specified widget.
%
-spec set_tooltip( window(), label() ) -> void().
set_tooltip( #canvas{ panel=Panel }, Label ) ->
	set_tooltip( Panel, Label );

set_tooltip( Window, Label ) ->
	wxWindow:setToolTip( Window, Label ).



% Window section.
%
% Base class for all windows and represents any visible object on screen. All
% controls, top level windows and so on are windows. Sizers and device contexts
% are not, however, as they do not appear on screen themselves.


% Creates a new window.
%
-spec create_window() -> window().
create_window() ->
	wxWindow:new().


-spec create_window( id(), window() ) -> window().
create_window( Id, Parent ) ->

	ActualId = get_id( Id ),
	ActualParent = get_parent( Parent ),

	wxWindow:new( ActualParent, ActualId ).


-spec create_window( size() ) -> window().
create_window( Size ) ->

	ActualId = get_id( undefined ),
	ActualParent = get_parent( undefined ),

	Options =  [ convert_size( Size ) ],

	wxWindow:new( ActualParent, ActualId, Options ).


-spec create_window( position(), size(), window_style(), id(), window() )
				   -> window().
create_window( Position, Size, Style, Id, Parent ) ->

	Options = [ get_position( Position ), convert_size( Size ),
				 { style, window_style_to_bitmask( Style ) } ],

	ActualId = get_id( Id ),
	ActualParent = get_parent( Parent ),

	wxWindow:new( ActualParent, ActualId, Options ).



% Sets the background color of the specified window.
%
-spec set_background_color( window(), gui_color:color() ) -> void().
set_background_color( #canvas{ back_buffer=BackBuffer }, Color ) ->

	% Must not be used, other double-deallocation core dump:
	%_PreviousBrush = wxMemoryDC:getBrush( BackBuffer ),
	%wxBrush:destroy( PreviousBrush ),

	ActualColor = gui_color:get_color( Color ),

	NewBrush = wxBrush:new( ActualColor ),

	wxMemoryDC:setBackground( BackBuffer, NewBrush );


set_background_color( Window, Color ) ->

	ActualColor = gui_color:get_color( Color ),

	wxWindow:setBackgroundColour( Window, ActualColor ).



% Associates specified sizer to specified window.
%
-spec set_sizer( window(), sizer() ) -> void().
set_sizer( #canvas{ panel=Panel }, Sizer ) ->
	set_sizer( Panel, Sizer );

set_sizer( Window, Sizer ) ->
	wxWindow:setSizer( Window, Sizer ).



% Shows (renders) specified window.
%
% Returns whether anything had to be done.
%
-spec show( window() ) -> boolean().
show( Window ) ->
	wxWindow:show( Window ).



% Hides specified window.
%
% Returns whether anything had to be done.
%
-spec hide( window() ) -> boolean().
hide( Window ) ->
	wxWindow:show( Window, [ {show,false} ] ).



% Returns the size (as a 2D vector, i.e. {Width,Height}) of specified window.
%
-spec get_size( window() ) -> linear_2D:vector().
get_size( Window ) ->
	wxWindow:getSize( Window ).




% Frame section.
%
% A frame is a window whose size and position can (usually) be changed by the
% user. It usually has thick borders and a title bar, and can optionally contain
% a menu bar, toolbar and status bar. A frame can contain any window that is not
% a frame or dialog.
%
% Source: http://docs.wxwidgets.org/stable/wx_wxframe.html#wxframewxframe


% Creates a new frame, with default title, ID, parent, position, size and style.
%
-spec create_frame() -> frame().
create_frame() ->
	wxFrame:new().


% Creates a new frame, with default position, size, style, ID and parent.
%
-spec create_frame( title() ) -> frame().
create_frame( Title ) ->
	wxFrame:new( get_parent( undefined), get_id( undefined ), Title ).


% Creates a new frame, with default position, size and style.
%
-spec create_frame( title(), id(), window() ) -> frame().
create_frame( Title, Id, Parent ) ->
	wxFrame:new( get_parent( Parent ), get_id( Id ), Title ).


% Creates a new frame, with default ID and parent.
%
-spec create_frame( title(), position(), size(), frame_style() ) -> frame().
create_frame( Title, Position, Size, Style ) ->

	Options =  [ get_position( Position ), convert_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	%io:format( "create_frame options: ~w.~n", [ Options ] ),

	wxFrame:new( get_parent(undefined), get_id( undefined ), Title, Options ).



% Creates a new frame.
%
-spec create_frame( title(), position(), size(), frame_style(), id(), window() )
				  -> frame().
create_frame( Title, Position, Size, Style, Id, Parent ) ->

	Options =  [ get_position( Position ), convert_size( Size ),
				 { style, frame_style_to_bitmask( Style ) } ],

	ActualId = get_id( Id ),

	ActualParent = get_parent( Parent ),

	wxFrame:new( ActualParent, ActualId, Title, Options ).



% Panel section.


% Creates a new panel.
%
-spec create_panel() -> panel().
create_panel() ->
	wxPanel:new().



% Creates a new panel, associated to specified parent.
%
-spec create_panel( window() ) -> panel().
create_panel( Parent ) ->
	wxPanel:new( Parent ).


% Creates a new panel, associated to specified parent and with specified
% options.
%
-spec create_panel( window(), panel_options() ) -> panel().
create_panel( Parent, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, ActualOptions ).



% Creates a new panel, associated to specified parent and with specified
% position and dimensions.
%
-spec create_panel( window(), coordinate(), coordinate(),
					length(), length() ) -> panel().
create_panel( Parent, X, Y, Width, Height ) ->
	wxPanel:new( Parent, X, Y, Width, Height ).



% Creates a new panel, associated to specified parent, with specified
% position, dimensions and options.
%
-spec create_panel( window(), coordinate(), coordinate(),
					length(), length(), panel_options() ) -> panel().
create_panel( Parent, X, Y, Width, Height, Options ) ->

	ActualOptions = get_panel_options( Options ),

	wxPanel:new( Parent, X, Y, Width, Height, ActualOptions ).




% Button section.


% Creates a new button.
%
-spec create_button( label(), position(), size(), button_style(), id(),
					 window() ) -> button().
create_button( Label, Position, Size, Style, Id, Parent ) ->

	Options = [ { label, Label }, get_position( Position ),
				convert_size( Size ),
				{ style, button_style_to_bitmask( Style ) } ],

	%io:format( "Button options for ID #~B: ~w.~n", [ Id, Options ] ),

	wxButton:new( Parent, Id, Options ).




% Sizer section.
%
% Sizers correspond actually to wxBoxSizer (wxSizer is an abstract class).


% Creates a sizer operating on specified orientation.
%
-spec create_sizer( orientation() ) -> sizer().
create_sizer( Orientation ) ->
	ActualOrientation = get_orientation( Orientation ),
	wxBoxSizer:new( ActualOrientation ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around.
%
-spec create_sizer_with_box( orientation(), window() ) -> sizer().
create_sizer_with_box( Orientation, Parent ) ->

	ActualOrientation = get_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent ).



% Creates a sizer operating on specified orientation, within specified parent,
% with a box drawn around bearing specified label.
%
-spec create_sizer_with_labelled_box( orientation(), window(), label() ) ->
											sizer().
create_sizer_with_labelled_box( Orientation, Parent, Label ) ->

	ActualOrientation = get_orientation( Orientation ),

	wxStaticBoxSizer:new( ActualOrientation, Parent, [ { label, Label } ] ).



% Adds specified element to the specified sizer.
%
-spec add_to_sizer( sizer(), window() | sizer() ) -> sizer_item().
add_to_sizer( Sizer, _Element=#canvas{ panel=Panel } ) ->
	add_to_sizer( Sizer, Panel );

add_to_sizer( Sizer, Element ) ->
	wxSizer:add( Sizer, Element ).



% Adds specified element, with options, to the specified sizer.
%
-spec add_to_sizer( sizer(), window() | sizer(), sizer_options() )
				  -> sizer_item().
add_to_sizer( Sizer, _Element=#canvas{ panel=Panel }, Options ) ->
	add_to_sizer( Sizer, Panel, Options );

add_to_sizer( Sizer, Element, Options ) ->

	ActualOptions = get_sizer_options( Options ),

	wxSizer:add( Sizer, Element, ActualOptions ).



% Clears specified sizer, detaching and deleting all its child windows.
%
-spec clear_sizer( sizer() ) -> basic_utils:void().
clear_sizer( Sizer ) ->
	clear_sizer( Sizer, _DeleteWindows=true ).



% Clears specified sizer, detaching all its child windows, and deleting them iff
% requested.
%
-spec clear_sizer( sizer(), boolean() ) -> basic_utils:void().
clear_sizer( Sizer, DeleteWindows ) ->
	wxSizer:clear( Sizer, [ { delete_windows, DeleteWindows } ] ).



% Status bar section.
%
% Status bars are very useful to trace events.


% Creates and attaches a status bar to specified widget.
%
-spec create_status_bar( window() ) -> status_bar().
create_status_bar( Window ) ->
	% No interesting option:
	wxFrame:createStatusBar( Window ).



% Pushes specified text in specified status bar.
%
-spec push_status_text( text(), status_bar() ) -> void().
push_status_text( Text, StatusBar ) ->
	wxStatusBar:pushStatusText( StatusBar, Text ).



% Section for back-end specific helpers.



% Debug section.


% Converts from our API to the one of the current GUI back-end.
%
% (helper)
%
convert_debug_level( _DebugLevel=none ) ->
	none;

convert_debug_level( _DebugLevel=calls ) ->
	trace;

convert_debug_level( _DebugLevel=life_cycle ) ->
	driver.




% Windows section.


% Converts specified window style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec window_style_to_bitmask( window_style() ) -> basic_utils:bit_mask().
window_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> window_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

window_style_to_bitmask( _Style=default ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=simple_border ) ->
	?wxBORDER_SIMPLE;

window_style_to_bitmask( _Style=double_border ) ->
	?wxBORDER_DOUBLE;

window_style_to_bitmask( _Style=sunken_border ) ->
	?wxBORDER_SUNKEN;

window_style_to_bitmask( _Style=raised_border ) ->
	?wxBORDER_RAISED;

window_style_to_bitmask( _Style=static_border ) ->
	?wxSTATIC_BORDER;

window_style_to_bitmask( _Style=theme_border ) ->
	?wxBORDER_THEME;

window_style_to_bitmask( _Style=no_border ) ->
	?wxBORDER_NONE;

window_style_to_bitmask( _Style=transparent ) ->
	?wxTRANSPARENT_WINDOW;

window_style_to_bitmask( _Style=tab_traversable ) ->
	?wxTAB_TRAVERSAL;

window_style_to_bitmask( _Style=grab_all_keys ) ->
	?wxWANTS_CHARS;

window_style_to_bitmask( _Style=with_vertical_scrollbar ) ->
	?wxVSCROLL;

window_style_to_bitmask( _Style=with_horizontal_scrollbar ) ->
	?wxHSCROLL;

window_style_to_bitmask( _Style=never_hide_scrollbars ) ->
	?wxALWAYS_SHOW_SB;

window_style_to_bitmask( _Style=clip_children ) ->
	?wxCLIP_CHILDREN;

window_style_to_bitmask( _Style=full_repaint_on_resize ) ->
	?wxFULL_REPAINT_ON_RESIZE.



% Converts specified window options into the appropriate back-end specific
% options.
%
% (helper)
%
get_window_options( Options ) ->
	get_window_options( Options, _Acc=[] ).


get_window_options( _Options=[], Acc ) ->
	Acc;

get_window_options( _Options=[ { style, Style } | T ], Acc ) ->
	get_window_options( T,
					[ { style, window_style_to_bitmask( Style ) } | Acc ] );

get_window_options( _Options=[ H | T ], Acc ) ->
	get_window_options( T, [ H | Acc ] ).




% Frames section.


% Converts specified frame style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec frame_style_to_bitmask( frame_style() ) -> basic_utils:bit_mask().
frame_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> frame_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

frame_style_to_bitmask( _Style=default ) ->
	?wxDEFAULT_FRAME_STYLE;

frame_style_to_bitmask( _Style=caption ) ->
	?wxCAPTION;

frame_style_to_bitmask( _Style=minimize ) ->
	?wxMINIMIZE;

frame_style_to_bitmask( _Style=minimize_box ) ->
	?wxMINIMIZE_BOX;

frame_style_to_bitmask( _Style=maximize ) ->
	?wxMAXIMIZE;

frame_style_to_bitmask( _Style=maximize_box ) ->
	?wxMAXIMIZE_BOX;

frame_style_to_bitmask( _Style=close_box ) ->
	?wxCLOSE_BOX;

frame_style_to_bitmask( _Style=stay_on_top ) ->
	?wxSTAY_ON_TOP;

frame_style_to_bitmask( _Style=system_menu ) ->
	?wxSYSTEM_MENU;

frame_style_to_bitmask( _Style=resize_border ) ->
	?wxRESIZE_BORDER;

frame_style_to_bitmask( _Style=tool_window ) ->
	?wxFRAME_TOOL_WINDOW;

frame_style_to_bitmask( _Style=no_taskbar ) ->
	?wxFRAME_NO_TASKBAR.



% Panels section.


% Converts specified panel options into the appropriate back-end specific
% options.
%
% (helper)
%
get_panel_options( Options ) ->
	get_window_options( Options ).




% Buttons section.


% Converts specified button style into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec button_style_to_bitmask( button_style() ) -> basic_utils:bit_mask().
button_style_to_bitmask( StyleList ) when is_list( StyleList ) ->

	lists:foldl( fun( S, Acc ) -> button_style_to_bitmask( S ) bor Acc end,
				 _InitialAcc=0,
				 _List=StyleList );

button_style_to_bitmask( _Style=default ) ->
	0;

button_style_to_bitmask( _Style=left_justified ) ->
	?wxBU_LEFT;

button_style_to_bitmask( _Style=right_justified ) ->
	?wxBU_RIGHT;

button_style_to_bitmask( _Style=top_justified ) ->
	?wxBU_TOP;

button_style_to_bitmask( _Style=bottom_justified ) ->
	?wxBU_BOTTOM;

button_style_to_bitmask( _Style=exact_fit ) ->
	?wxBU_EXACTFIT;

button_style_to_bitmask( _Style=flat ) ->
	throw( not_implemented ).




% Sizers section.


% Converts specified sizer flag into the appropriate back-end specific bit
% mask.
%
% (helper)
%
-spec sizer_flag_to_bitmask( sizer_flag() ) -> basic_utils:bit_mask().
sizer_flag_to_bitmask( FlagList ) when is_list( FlagList ) ->

	lists:foldl( fun( F, Acc ) -> sizer_flag_to_bitmask( F ) bor Acc end,
				 _InitialAcc=0,
				 _List=FlagList );

sizer_flag_to_bitmask( _Flag=default ) ->
	0;

sizer_flag_to_bitmask( _Flag=top_border ) ->
	?wxTOP;

sizer_flag_to_bitmask( _Flag=bottom_border ) ->
	?wxBOTTOM;

sizer_flag_to_bitmask( _Flag=left_border ) ->
	?wxLEFT;

sizer_flag_to_bitmask( _Flag=right_border ) ->
	?wxRIGHT;

sizer_flag_to_bitmask( _Flag=all_borders ) ->
	?wxALL;

sizer_flag_to_bitmask( _Flag=expand_fully ) ->
	?wxEXPAND;

sizer_flag_to_bitmask( _Flag=expand_shaped ) ->
	?wxSHAPED;

sizer_flag_to_bitmask( _Flag=fixed_size ) ->
	?wxFIXED_MINSIZE;

sizer_flag_to_bitmask( _Flag=counted_even_if_hidden ) ->
	?wxRESERVE_SPACE_EVEN_IF_HIDDEN;

sizer_flag_to_bitmask( _Flag=align_center ) ->
	?wxALIGN_CENTER;

sizer_flag_to_bitmask( _Flag=align_left ) ->
	?wxALIGN_LEFT;

sizer_flag_to_bitmask( _Flag=align_right ) ->
	?wxALIGN_RIGHT;

sizer_flag_to_bitmask( _Flag=align_top ) ->
	?wxALIGN_TOP;

sizer_flag_to_bitmask( _Flag=align_bottom ) ->
	?wxALIGN_BOTTOM;

sizer_flag_to_bitmask( _Flag=align_center_vertical ) ->
	?wxALIGN_CENTER_VERTICAL;

sizer_flag_to_bitmask( _Flag=align_center_horizontal ) ->
	?wxALIGN_CENTER_HORIZONTAL.


% Converts to back-end sizer options.
%
% (helper)
%
get_sizer_options( Options ) ->
	get_sizer_options( Options, _Acc=[] ).

get_sizer_options( _Options=[], Acc ) ->
	Acc;

get_sizer_options( _Options=[ { flag, Flag } | T ], Acc ) ->
	get_sizer_options( T, [ { flag, sizer_flag_to_bitmask( Flag ) } | Acc ] );

get_sizer_options(_Options=[ H | T ], Acc ) ->
	get_sizer_options( T, [ H | Acc ] ).



% General-purpose section.


% Converts to back-end widget identifier.
%
% (helper)
%
get_id( undefined ) ->
	?any_id;

get_id( Other ) ->
	Other.



% Converts to back-end parent window.
%
% (helper)
%
get_parent( undefined ) ->
	?no_parent;

get_parent( Other ) ->
	Other.



% Converts to back-end position (with defaults).
%
% (helper)
%
get_position( _Position=auto ) ->
	{ pos, {-1,-1} };

get_position( Position ) ->
	{ pos, Position }.



% Converts to back-end size (with defaults).
%
% (helper)
%
convert_size( _Size=auto ) ->
	{ size, {-1,-1} };

convert_size( Size ) ->
	{ size, Size }.



% Converts to back-end orientation.
%
% (helper)
%
get_orientation( vertical ) ->
	?wxVERTICAL;

get_orientation( horizontal ) ->
	?wxHORIZONTAL.
