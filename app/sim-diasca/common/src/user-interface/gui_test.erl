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



% Unit tests for the GUI toolbox.
%
% See the gui.erl tested module.
%
-module(gui_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% For gui-related defines:
-include("gui.hrl").


% Utility functions:
-export([ get_name/1 ]).


% To remove:
-export([ gui_main_loop/1, get_canvas_width/0, get_canvas_height/0,
		  render_test/1 ]).


% State of the program, passed between event handlers.
%
-record( test_state,
		{

		  main_frame,
		  render_shape_button,
		  render_mec_button,
		  clear_canvas_button,
		  add_point_button,
		  load_image_button,
		  quit_button,
		  canvas,
		  point_count = 0,
		  render_mode = test :: 'test' | 'mec'

		  }).


-spec get_main_window_width() -> linear:coordinate().
get_main_window_width() ->
	800.


-spec get_main_window_height() -> linear:coordinate().
get_main_window_height() ->
	600.



-spec get_canvas_width() -> linear:coordinate().
get_canvas_width() ->
	640.


-spec get_canvas_height() -> linear:coordinate().
get_canvas_height() ->
	480.





% Lists all the declared names of identifiers.
%
get_all_id_names() ->
	[ 'MainFrame', 'RenderShapeButton', 'RenderMECButton',
	  'AddPointButton', 'LoadImageButton', 'ClearCanvasButton', 'QuitButton' ].



% Returns the numerical ID corresponding to the specified name.
%
% (a good target for a parse transform)
%
-spec get_id( atom() ) -> gui:id().
get_id( Name ) ->
	list_utils:get_index_of( Name, get_all_id_names() ).



% Returns the name (as an atom) of the specified widget (expected to be named).
%
-spec get_name( gui:id() ) -> atom().
get_name( Id ) ->

	Names = get_all_id_names(),

	Len = length(Names),

	case Id of

		Id when Id < 1 orelse Id > Len ->
			unknown;

		_ ->
			lists:nth( Id, Names )

	end.



-spec init_test_gui() -> no_return().
init_test_gui() ->

	gui:start(),

	FrameSize = { get_main_window_width(), get_main_window_height() },

	MainFrame = gui:create_frame( _Title="GUI Test", _FramePos=auto,
			FrameSize, _FrameStyle=default, _Id=get_id('MainFrame'),
			_Parent=undefined ),

	gui:connect( MainFrame, close_window ),

	%gui:set_background_color( MainFrame, red ),
	%gui:set_background_color( LeftPanel, blue ),
	%gui:set_background_color( RightPanel, green ),

	StatusBar = gui:create_status_bar( MainFrame ),

	gui:push_status_text( "Waiting for points to be added.", StatusBar ),

	LeftPanel = gui:create_panel( MainFrame ),

	RightPanel = gui:create_panel( MainFrame ),

	MainSizer = gui:create_sizer( horizontal ),

	% Constant width:
	gui:add_to_sizer( MainSizer, LeftPanel,
					  [ {proportion,0}, {flag,[ expand_fully ]} ] ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, RightPanel,
					  [ {proportion,2}, {flag,[ expand_fully ]} ] ),

	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical, LeftPanel,
											"Controls" ),

	% Adding the buttons to the control panel:

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = default,
	ParentButton = LeftPanel,

	RenderShapeButton = gui:create_button( "Render a few random shapes",
		Position, ButtonSize, ButtonStyle, get_id('RenderShapeButton'),
		ParentButton ),

	gui:connect( RenderShapeButton , command_button_clicked ),


	RenderMECButton = gui:create_button( "Render MEC",
		Position, ButtonSize, ButtonStyle, get_id('RenderMECButton'),
		ParentButton ),

	gui:connect( RenderMECButton , command_button_clicked ),


	AddPointButton = gui:create_button( "Add point", Position, ButtonSize,
		ButtonStyle, get_id('AddPointButton'), ParentButton ),

	gui:connect( AddPointButton, command_button_clicked ),


	LoadImageButton = gui:create_button( "Load image", Position, ButtonSize,
		ButtonStyle, get_id('LoadImageButton'), ParentButton ),

	gui:connect( LoadImageButton, command_button_clicked ),


	ClearCanvasButton = gui:create_button( "Clear canvas", Position, ButtonSize,
		ButtonStyle, get_id('ClearCanvasButton'), ParentButton ),

	gui:connect( ClearCanvasButton, command_button_clicked ),


	QuitButton = gui:create_button( "Quit", Position, ButtonSize,
		ButtonStyle, get_id('QuitButton'), ParentButton ),

	gui:connect( QuitButton, command_button_clicked ),



	gui:set_tooltip( LeftPanel, "Controls for the GUI test" ),

	% Not working apparently:
	gui:set_tooltip( AddPointButton, "Add a point to the\ncurrent polygon" ),

	ButtonOpt = [ { flag, [ expand_fully ] } ],

	gui:add_to_sizer( ControlBoxSizer, RenderShapeButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, RenderMECButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, AddPointButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, LoadImageButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, ClearCanvasButton, ButtonOpt ),

	gui:add_to_sizer( ControlBoxSizer, QuitButton, ButtonOpt ),

	gui:set_sizer( LeftPanel, ControlBoxSizer ),

	PolyBoxSizer = gui:create_sizer_with_labelled_box( vertical, RightPanel,
											"Polygon View" ),

	Canvas = gui_canvas:create( RightPanel ),

	gui_canvas:set_background_color( Canvas, pink ),

	gui:connect( Canvas, paint ),
	gui:connect( Canvas, size ),

	gui:add_to_sizer( PolyBoxSizer, Canvas,
					  [ {proportion,1}, {flag,[ expand_fully ]} ] ),

	gui:set_tooltip( Canvas, "Random polygons and their MEC\n"
							 "(Minimum Enclosing Circle Box) are drawn here." ),

	gui:set_sizer( RightPanel, PolyBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),



	% Sets the GUI to visible:
	gui:show( MainFrame ),

	InitialPointCount = 3,

	InitialState = #test_state{  main_frame=MainFrame,
								 render_shape_button=RenderShapeButton,
								 render_mec_button=RenderMECButton,
								 clear_canvas_button=ClearCanvasButton,
								 add_point_button=AddPointButton,
								 load_image_button=LoadImageButton,
								 quit_button=QuitButton,
								 canvas=Canvas,
								 point_count=InitialPointCount,
								 render_mode = test },

	gui_main_loop( InitialState ),

	gui:stop().





-spec render_test( gui_canvas:canvas() ) -> basic_utils:void().
render_test( Canvas ) ->

	test_facilities:display( "Rendering shape test." ),

	gui_canvas:set_background_color( Canvas, yellow ),

	gui_canvas:clear( Canvas ),

	P1 = { 20,10 },
	P2 = { 100, 200 },

	gui_canvas:draw_line( Canvas, P1, P2 ),

	P3 = {300,50},
	Purple = gui_color:get_color( blue ),

	gui_canvas:draw_line( Canvas, P2, P3, Purple ),
	P4 = {400,250},

	gui_canvas:set_draw_color( Canvas, red ),
	gui_canvas:draw_lines( Canvas, [ P1, P3, P4 ] ),


	gui_canvas:set_draw_color( Canvas, black ),
	gui_canvas:draw_cross( Canvas, {36,26}, _FirstEdgeLength=6 ),

	LabelPosition = {72,300},
	gui_canvas:draw_label( Canvas, LabelPosition, "A simple label, the cross "
					"indicating its specified location" ),
	gui_canvas:draw_cross( Canvas, LabelPosition ),

	gui_canvas:draw_labelled_cross( Canvas, {36,86}, _SecondEdgeLength=4,
							 "Cross label" ),

	gui_canvas:set_draw_color( Canvas, firebrick ),
	gui_canvas:set_fill_color( Canvas, chartreuse ),
	gui_canvas:draw_circle( Canvas, _CircleCenter={80,80}, _Radius=80 ),

	gui_canvas:set_fill_color( Canvas, none ),
	gui_canvas:draw_circle( Canvas, _OtherCircleCenter={180,180},
							_OtherRadius=180 ),

	% Taken from polygon_test.erl:
	MyTriangle = polygon:update_bounding_box( lazy_circle,
	   polygon:set_edge_color( fuchsia,
			  polygon:get_triangle( {110,110}, {550,155}, {420,335} ) ) ),

	MyUprightSquare = polygon:update_bounding_box( lazy_circle,
	   polygon:set_edge_color( steelblue,
			  polygon:get_upright_square( _Center={250,250},
										  _EdgeLength=50 ) ) ),

	polygon:render( MyTriangle, Canvas ),
	polygon:render( MyUprightSquare, Canvas ),

	gui_canvas:blit( Canvas ),

	Canvas.



% Renders the MEC view.
-spec render_mec( gui_canvas:canvas(), integer() ) -> gui_canvas:canvas().
render_mec( Canvas, PointCount ) ->

	%test_facilities:display( "Rendering MEC with ~B points.", [ PointCount ] ),

	gui_canvas:set_background_color( Canvas, blue ),

	gui_canvas:clear( Canvas ),

	gui_canvas:set_draw_color( Canvas, white ),

	RandomPoints = [ { random_utils:get_random_value(200) + 300,
					   random_utils:get_random_value(300) + 100 }
					|| _Count <- lists:seq( 1, PointCount ) ],

	%test_facilities:display( "Random points: ~w.", [ RandomPoints ] ),

	{ Pivot, RemainingPoints } = linear_2D:find_pivot( RandomPoints ),

	%test_facilities:display( "Pivot: ~w, remaining: ~w.",
	% [ Pivot, RemainingPoints ] ),



	SortedPoints = linear_2D:sort_by_angle( Pivot, RemainingPoints ),

	%test_facilities:display( "Sorted points: ~w.", [ SortedPoints ] ),

	gui_canvas:draw_lines( Canvas, [ Pivot | SortedPoints ] ++ [ Pivot ],
						   green ),

	HullPoints = linear_2D:compute_convex_hull( RandomPoints ),

	%test_facilities:display( "Hull points: ~w.", [ HullPoints ] ),

	%test_facilities:display( "Number of hull/set points: ~B/~B.",
	%		   [ length( HullPoints ), PointCount ] ),

	{ ExactCenter, SquareRadius } =
		bounding_box:get_minimal_enclosing_circle_box( HullPoints ),

	Center = linear_2D:roundify( ExactCenter ),

	Radius = math:sqrt( SquareRadius ),

	%test_facilities:display( "Bounding Minimal Enclosing Circle: "
	%		   "center = ~p, radius = ~f.~n", [ Center, Radius ] ),

	gui_canvas:draw_labelled_cross( Canvas, Center, 5, purple, "MEC center" ),

	gui_canvas:draw_circle( Canvas, Center, round( Radius ) ),

	gui_canvas:draw_lines( Canvas, [ Pivot | HullPoints ], red ),

	% Draws the crosses last to have them on top:
	gui_canvas:draw_labelled_cross( Canvas, Pivot, _OtherEdgeLength=10, black,
							 "Pivot" ),

	gui_canvas:set_draw_color( Canvas, white ),

	gui_canvas:draw_numbered_points( Canvas, SortedPoints ),

	gui_canvas:blit( Canvas ),

	Canvas.




%-spec gui_main_loop( gs_object(), integer(), gui_canvas:canvas() | undefined )
%				   -> no_return().
gui_main_loop( State=#test_state{ main_frame=MainFrame,
								  render_shape_button=RenderShapeButton,
								  render_mec_button=RenderMECButton,
								  add_point_button=AddButton,
								  load_image_button=LoadImageButton,
								  clear_canvas_button=ClearCanvasButton,
								  quit_button=QuitButton,
								  canvas=Canvas,
								  point_count=PointCount,
								  render_mode=Mode
								} ) ->

	test_facilities:display( "~nEntering main loop, mode is ~p, "
							 "point count is ~B.", [ Mode, PointCount ] ),

	Update = receive

		#wx{ obj=MainFrame, event={wxClose,close_window} } ->
			test_facilities:display( "Quitting GUI test." ),
			quit;

		#wx{ obj=RenderShapeButton,
			 event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Render shape button clicked." ),
			NewCanvas = render_test( Canvas ),
			State#test_state{ canvas=NewCanvas, render_mode=test  };

		#wx{ obj=RenderMECButton,
			 event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Render MEC button clicked." ),
			NewCanvas = render_mec( Canvas, PointCount ),
			State#test_state{ canvas=NewCanvas, render_mode=mec };

		#wx{ obj=AddButton, event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Add button clicked." ),
			NewPointCount = PointCount + 1,
			NewCanvas = render_mec( Canvas, NewPointCount ),
			State#test_state{
			  canvas=NewCanvas,
			  point_count=NewPointCount,
			  render_mode=mec
			 };

		#wx{ obj=LoadImageButton,
			 event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Load image clicked." ),
			gui_canvas:load_image( Canvas, { 40, 20 },
								   "../../doc/example.bmp" ),
			gui_canvas:blit( Canvas ),
			State;

		#wx{ obj=ClearCanvasButton,
			 event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Clear canvas button clicked." ),
			gui_canvas:clear( Canvas ),
			gui_canvas:blit( Canvas ),
			State;

		#wx{ obj=QuitButton,
			 event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Quit button clicked." ),
			quit;

		#wx{ obj=Any, event=#wxCommand{ type=command_button_clicked } } ->
			test_facilities:display( "Following button clicked: ~w.", [ Any ] ),
			quit;


		% Received for example when another window overlapped:
		#wx{ event=#wxPaint{} } ->

			case Mode of

				test ->
					render_test( Canvas );

				mec ->
					% Triggers a redraw:
					render_mec( Canvas, PointCount )

			end,
			State ;


		#wx{ event=#wxSize{ size=NewSize } } ->

			test_facilities:display( "Resizing to ~w.", [ NewSize ] ),

			NewCanvas = gui_canvas:resize( Canvas, NewSize ),

			case Mode of

				test ->
					render_test( NewCanvas );

				mec ->
					% Triggers a redraw:
					render_mec( NewCanvas, PointCount )

			end,

			State#test_state{ canvas=NewCanvas };


		Any ->
			test_facilities:display( "GUI test got event '~w' (ignored).",
									[ Any ] ),
			State

	end,

	case Update of

		quit ->
			% Simply stop recursing:
			ok;

		NewState ->
			gui_main_loop( NewState )

	end.




% Runs the test.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the GUI test, "
									"being in batch mode)" );

		false ->
			init_test_gui()

	end,

	test_facilities:stop().
