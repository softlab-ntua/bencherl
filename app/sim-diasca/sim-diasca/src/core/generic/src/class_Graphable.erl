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


% Graphable class, base of all instances able to output a textual description of
% their state in the context of graph rendering.
%
% See also: core/mesh/src/class_Mesh.erl
%
-module(class_Graphable).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, OptionParameters ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, delete/1 ).


% Method declarations.
-define( wooper_method_export, getNodeName/1, getLabel/1, setLabel/2,
		 getGraphInformation/1, getGraphOptions/1,
		 getGraphOptions/2, setGraphOptions/2 ).


% Helper functions.
-export([ forge_node_name/0, forge_node_name/1, graph_options_to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


-type dot_option_name() :: 'label' | 'style' | 'height' | 'width' | 'fixedsize'
	 | 'shape' | 'fillcolor' | 'color' | 'bgcolor' | 'penwidth' | 'pencolor'.


-define( dot_option_list, [ label, style, height, width, fixedsize, shape,
	fillcolor, color, bgcolor, penwidth, pencolor ] ).


-type dot_option_value() :: any().


-type options() :: string() | [ { dot_option_name(), dot_option_value() } ].



% Implementation Note:
%
% Being a trace emitter is not strictly needed here, as it would lead to useless
% diamond-shaped multiple inheritance.




% Constructs a new graphable instance.
%
% OptionParameters is:
%
% - either a label, like "hello"
%
% - or a list of option pairs like { dot_option_name, option_value } in which at
% least the label is defined
%
% Ex: [ { label, "hello" }, { color, red } ].
%
% Note that options must be valid dot options.
%
% See the dot_option_list macro.
%
-spec construct( wooper_state(), options() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	NamedState = setAttribute( State, node_name, forge_node_name() ),

	interpret_option_list( OptionParameters, NamedState ).



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	% Class-specific actions:
	% Then call the direct mother class counterparts and allow chaining:
	State.




% Methods section.


% Returns a name, derived from current PID, adequate to be a node identifier.
%
% (const request)
%
-spec getNodeName( wooper_state() ) -> request_return( string() ).
getNodeName( State ) ->
	?wooper_return_state_result( State, ?getAttr(node_name) ).



% Returns the description of this Graphable.
%
% (const request)
%
-spec getLabel( wooper_state() ) -> request_return( string() ).
getLabel( State ) ->
	?wooper_return_state_result( State, ?getAttr(label) ).



% Sets the label of this Graphable.
%
% (oneway)
%
-spec setLabel( wooper_state(), string() ) -> oneway_return().
setLabel( State, NewLabel ) ->
	?wooper_return_state_only( setAttribute( State, label, NewLabel ) ).



% Returns { GraphableName, OptionList } where GraphableName is the generated
% name for this graphable, and OptionList is the list of all attribute
% name/value pairs corresponding to dot options for that Graphable.
%
% (const request)
%
-spec getGraphInformation( wooper_state() ) ->
			request_return( { string(), options() } ).
getGraphInformation( State ) ->
	?wooper_return_state_result( State, { ?getAttr(node_name),
		select_attributes_from( wooper_get_all_attributes(State) ) } ).



% Returns the list of all attribute name/value pairs corresponding to dot
% options for that Graphable.
%
% (const request)
%
-spec getGraphOptions( wooper_state() ) -> request_return( options() ).
getGraphOptions( State ) ->
	?wooper_return_state_result( State,
		select_attributes_from( wooper_get_all_attributes(State) ) ).



% Returns the list of all attribute name/value pairs corresponding to dot
% options for that Graphable.
%
% Triggers back a setGraphOptions actor call.
%
% Note: supposed to be called on an Actor instance.
%
% (const actor oneway)
%
-spec getGraphOptions( wooper_state(), pid() ) ->
							 class_Actor:actor_oneway_return().
getGraphOptions( State, CallerPid ) ->

	Options = select_attributes_from( wooper_get_all_attributes(State) ),

	% Note the double list for options:
	SentState = class_Actor:send_actor_message( CallerPid,
		{ setGraphOptions, [ Options ] }, State ),

	?wooper_return_state_only( SentState ).



% Sets the specified option list, regarding graphable parameters.
%
% (oneway)
%
-spec setGraphOptions( wooper_state(), options() ) -> oneway_return().
setGraphOptions( State, OptionParameters ) ->
	?wooper_return_state_only(
							interpret_option_list( OptionParameters, State ) ).




% Section for helper functions (not methods).

% Interprets the option list specified for a graphable.



% Sets the relevant options in state.
%
interpret_option_list( _Options=[], State ) ->
	State;

interpret_option_list( _Options=[ { label, Label } | T ], State ) ->
	interpret_option_list( T,
		setAttribute( State, label, transform_label( Label ) ) );

interpret_option_list( [ { OptionName, OptionValue } | T ], State ) ->

	case lists:member( OptionName, ?dot_option_list ) of

		true ->
			interpret_option_list( T,
				setAttribute( State, OptionName, OptionValue ) );

		false ->
			throw( { unknown_dot_option, OptionName } )

	end;

interpret_option_list( Label, State ) ->
	interpret_option_list( _Options=[],
		setAttribute( State, label, transform_label( Label ) )  ).



-spec forge_node_name() -> string().
forge_node_name() ->
	forge_node_name( self() ).


-spec forge_node_name( pid() ) -> string().
forge_node_name( Pid ) when is_pid(Pid) ->

	% Ex: "<0.59.0>":
	PidAsString = hd( io_lib:format( "~w", [ Pid ] ) ),

	% Ex: "0.59.0":
	string:substr( PidAsString, 2, length( PidAsString ) - 2 ).



% Splits specified label, one word per line.
%
transform_label( Label ) ->
	separate_in_lines( string:tokens( Label, " " ) ).


separate_in_lines( WordList ) ->
	separate_in_lines( WordList, "" ).


separate_in_lines( _WordList=[], ResultingString ) ->
	ResultingString;

separate_in_lines( _WordList=[ H | T ], ResultingString ) ->
	separate_in_lines( T, ResultingString ++ H ++ "\\n" ).



% Select only the known dot attributes.
%
select_attributes_from( AttributeList ) ->
	select_attributes_from( AttributeList, _Acc=[] ).


select_attributes_from( _AttributeList=[], Acc ) ->
	Acc;

select_attributes_from( _AttributeList=[ { Name, Value } | T ], Acc ) ->
	case lists:member( Name, ?dot_option_list ) of

		true ->
			select_attributes_from( T, [ { Name, Value } | Acc ] );

		false ->
			select_attributes_from( T, Acc )
	end.



-spec graph_options_to_string( options() ) -> string().
graph_options_to_string( OptionsList ) ->
	graph_options_to_string( OptionsList, [] ).


graph_options_to_string( _OptionsList=[], Acc ) ->
	Acc;

graph_options_to_string( _OptionsList=[ { label, Label } | T ], Acc ) ->
	graph_options_to_string( T,
		Acc ++ io_lib:format( " label='~s'", [ Label ] ) );

graph_options_to_string( _OptionsList=[ { OptionName, OptionValue } | T ],
						 Acc ) ->
	graph_options_to_string( T,
		Acc ++ io_lib:format( " ~s=~w", [ OptionName, OptionValue ] ) ).
