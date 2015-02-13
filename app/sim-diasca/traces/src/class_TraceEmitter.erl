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
% Creation date: July 1, 2007.


% Base class for all emitters of traces.
%
% A trace emitter has a notion of time (execution tick) as it needs to
% timestamp its traces.
%
% See class_TestTraceEmitter.erl and class_TraceEmitter_test.erl
%
-module(class_TraceEmitter).


% Name of a trace emitter:
-type name() :: string().

-export_type([ name/0 ]).



% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
% (TraceEmitterCategorization will be set in the trace_categorization attribute
% of each child class when coming down the inheritance hierarchy, so that the
% latest child class sets its targeted trace_categorization value)
%
-define( wooper_construct_parameters, TraceEmitterName ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
%
-export([ new/1, new_link/1, synchronous_new/1, synchronous_new_link/1,
		  synchronous_timed_new/1, synchronous_timed_new_link/1,
		  remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		  remote_synchronous_new_link/2, remote_synchronous_timed_new/2,
		  remote_synchronous_timed_new_link/2, remote_synchronisable_new_link/2,
		  construct/2, destruct/1 ]).



% Member method declarations:
%
-define( wooper_method_export, getName/1, setName/2, setCategorization/2,
		 getInitialTick/1, setInitialTick/2,
		 getCurrentTickOffset/1, setCurrentTickOffset/2,
		 getCurrentTick/1,
		 display/1, toString/1 ).


% Static method declarations:
%
-define( wooper_static_method_export, send_from_test/2, send_from_test/3,
		 send_standalone/2, send_standalone/3, send_standalone/5,
		 get_emitter_node_as_binary/0,
		 get_priority_for/1, get_channel_name_for_priority/1 ).



% Helper functions:
%
-export([ init/1, set_categorization/2, send/3, send/4, send/5,
		  get_current_tick/1, get_current_tick_offset/1, get_plain_name/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Trace.Emitter").


% For trace_aggregator_name:
-include("class_TraceEmitter.hrl").


% For DefaultEmitterCategorization:
-include("class_TraceAggregator.hrl").


-define(LogPrefix,"[Trace Emitter]").




% Implementation notes:

% As traces are timestamped, a trace emitter has to have some notion of time,
% here based on (integer) execution ticks.
%
% Its current tick, to be obtained with the getCurrentTick/1 method or the
% get_current_tick/1 function, is determined based on the addition of:
%
% - the initial emitter tick (initial_tick), a supposedly absolute time
% reference (possibly a very large integer), whatever this reference may be
% (creation tick for that instance, initial execution tick, etc.)
%
% - the current tick offset of the emitter (current_tick_offset), defined
% relatively (i.e. as an offset) to initial_tick; this offset is generally able
% to fit in a platform-native integer, therefore, for increased performances,
% processings should be based preferably on offsets rather than on absolute time
% references
%
% Currently the timestamps reported are tick offsets rather than ticks, as
% otherwise the trace browsing usually involves very long integers, which, for
% humans, are not convenient to read, remember, compare, etc.

% To reduce the memory footprint in the trace aggregator mailbox and the size of
% messages sent over the network, most of the time binaries are used instead of
% plain strings.





% Constructs a new Trace emitter.
%
% EmitterName is a plain string containing the name of this trace emitter, ex:
% "MyObject 16".
%
% Note: this constructor should be idempotent, as a given instance might very
% well inherit (directly or not) from that class more than once.
%
-spec construct( wooper:state(), string() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	%io:format( "~s Creating a trace emitter whose name is ~s, "
	%	"whose PID is ~w and whose categorization is ~s.~n",
	%	[ ?LogPrefix, TraceEmitterName, self(), ?TraceEmitterCategorization ] ),


	% Note: the 'name' attribute is stored as a binary, to reduce the memory
	% footprint. Use text_utils:binary_to_string/1 to get back a plain string
	% or, preferably, the class_TraceEmitter:get_plain_name/1 static method.

	InitState = init( State ),

	setAttributes( InitState, [

		{ name, text_utils:string_to_binary( TraceEmitterName ) },
		{ initial_tick, undefined },
		{ current_tick_offset, undefined },

		% Should be converted to binary each time when set (but will not crash
		% if remaining a plain string):
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

						   ] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%io:format( "~s Deleting Trace Emitter.~n", [ ?LogPrefix ] ),

	%io:format( "~s Trace Emitter deleted.~n", [ ?LogPrefix ] ).

	State.






% Methods section.



% Generic interface.


% Returns the name of this trace emitter, as a binary.
%
% Note: use text_utils:binary_to_string/1 to get back a plain string.
%
% (const request)
%
-spec getName( wooper:state() ) -> request_return( binary() ).
getName( State ) ->
	?wooper_return_state_result( State, ?getAttr(name) ).



% Sets the name of this trace emitter from specified plain string.
%
% (oneway)
%
-spec setName( wooper:state(), string() ) -> oneway_return().
setName( State, NewName ) ->
	?wooper_return_state_only( setAttribute( State, name,
								text_utils:string_to_binary( NewName ) ) ).



% Sets the trace categorization for this trace emitter to specified plain
% string.
%
% Setting the trace categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter correctly gathered in the
% same trace category, which is a lot clearer when browsing afterwards.
%
% (oneway)
%
-spec setCategorization( wooper:state(), string() ) -> oneway_return().
setCategorization( State, TraceCategorization ) ->

	NewState = set_categorization( TraceCategorization, State ),

	?wooper_return_state_only( NewState ).



% Returns the initial tick of this trace emitter.
%
% (const request)
%
-spec getInitialTick( wooper:state() ) -> request_return( traces:tick() ).
getInitialTick( State ) ->
	?wooper_return_state_result( State, ?getAttr(initial_tick) ).




% Sets the initial tick of this trace emitter.
%
% Note: does not update the tick offset, therefore the current tick is not
% preserved.
%
% (oneway)
%
-spec setInitialTick( wooper:state(), traces:tick() ) -> oneway_return().
setInitialTick( State, NewInitialTick ) ->
	?wooper_return_state_only( setAttribute( State, initial_tick,
											 NewInitialTick ) ).




% Returns the current tick offset of this trace emitter.
%
% (const request)
%
-spec getCurrentTickOffset( wooper:state() ) -> request_return( traces:tick() ).
getCurrentTickOffset( State ) ->
	?wooper_return_state_result( State, ?getAttr(current_tick_offset) ).



% Sets the current tick offset of this trace emitter.
%
% (oneway)
%
-spec setCurrentTickOffset( wooper:state(), traces:tick() ) ->
								  oneway_return().
setCurrentTickOffset( State, NewCurrentTickOffset ) ->
	?wooper_return_state_only(
		setAttribute( State, current_tick_offset, NewCurrentTickOffset ) ).



% Returns the current tick of this trace emitter.
%
% (const request)
%
-spec getCurrentTick( wooper:state() ) -> request_return( traces:tick() ).
getCurrentTick( State ) ->
	?wooper_return_state_result( State, get_current_tick( State ) ).



% Displays the state in the console.
%
% (const oneway)
%
-spec display( wooper:state() ) -> oneway_return().
display( State ) ->
	wooper:display_instance( State ),
	?wooper_return_state_only( State ).


% Returns a textual description of this emitter.
%
% (const request)
%
-spec toString( wooper:state() ) -> request_return( string() ).
toString( State ) ->
	?wooper_return_state_result( State, wooper:state_to_string( State ) ).




% 'Static' methods (module functions).




% Section for static methods.



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_test( traces:message_type(), traces:message() ) ->
							 basic_utils:void().
send_from_test( TraceType, Message ) ->
	send_from_test( TraceType, Message, ?DefaultTestEmitterCategorization ).



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_from_test( traces:message_type(), traces:message(),
					traces:emitter_categorization() ) -> basic_utils:void().
send_from_test( TraceType, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name(?trace_aggregator_name) of

		undefined ->

			error_logger:info_msg( "class_TraceEmitter:send_from_test: "
								   "trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
				basic_utils:get_textual_timestamp() ),

			% Not State available here:
			EmitterNode = get_emitter_node_as_binary(),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=
					 text_utils:string_to_binary( "(test)" ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _Tick=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( "Test" ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message() ) ->
							 basic_utils:void().
send_standalone( TraceType, Message ) ->
	send_standalone( TraceType, Message,
					 ?DefaultStandaloneEmitterCategorization ).



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message(),
					  traces:emitter_categorization() ) -> basic_utils:void().
send_standalone( TraceType, Message, EmitterCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			error_logger:info_msg( "class_TraceEmitter:send_standalone: "
				"trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
				basic_utils:get_textual_timestamp() ),

			% No State available here:
			EmitterNode = get_emitter_node_as_binary(),

			% Visual noise:
			%PidName = "(anonymous)",

			% Not wanting dots in PID here (otherwise this would be interpreted
			% as sub-categories in the traces:
			PidName = text_utils:substitute( $., $-, pid_to_list( self() ) ),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=text_utils:string_to_binary( PidName ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _Tick=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( "Standalone" ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }

	end.



% Sends all types of traces without requiring a class_TraceEmitter state.
%
% Uses default trace aggregator, supposed to be already available and
% registered.
%
% (static)
%
-spec send_standalone( traces:message_type(), traces:message(),
					   traces:emitter_name(), traces:emitter_categorization(),
					   traces:message_categorization() ) -> basic_utils:void().
send_standalone( TraceType, Message, EmitterName, EmitterCategorization,
				 MessageCategorization ) ->

	% Follows the order of our trace format; oneway call:
	case global:whereis_name( ?trace_aggregator_name ) of

		undefined ->

			error_logger:info_msg( "class_TraceEmitter:send_standalone: "
				"trace aggregator not found." ),

			throw( trace_aggregator_not_found );

		AggregatorPid ->

			TimestampText = text_utils:string_to_binary(
				basic_utils:get_textual_timestamp() ),

			% No State available here:
			EmitterNode = get_emitter_node_as_binary(),

			AggregatorPid ! { send,
				[
				 _TraceEmitterPid=self(),
				 _TraceEmitterName=text_utils:string_to_binary( EmitterName ),
				 _TraceEmitterCategorization=
					 text_utils:string_to_binary( EmitterCategorization ),
				 _Tick=none,
				 _Time=TimestampText,
				 _Location=EmitterNode,
				 _MessageCategorization=
					 text_utils:string_to_binary( MessageCategorization ),
				 _Priority=get_priority_for( TraceType ),
				 _Message=text_utils:string_to_binary( Message )
				] }


	end.



% Returns the name of the node this emitter is on, as an atom.
%
% (static)
%
-spec get_emitter_node_as_binary() -> binary().
get_emitter_node_as_binary() ->
	erlang:atom_to_binary( net_utils:localnode(), _Encoding=latin1 ).



% Returns the priority of specified trace type (i.e. fatal, error, etc.).
%
% Note: now that LogMX v1.3.2 and later only support 5 levels of detail
% (stack/error, warning/warn, info, fine, finest/debug, i.e. no more trace),
% fatal and error messages have been put at the same priority level, and
% Ceylan trace level has been kept, whereas others have been offset.
%
% See also: get_channel_name_for_priority/1.
%
% (static)
%
-spec get_priority_for( traces:message_type() ) -> traces:priority().
% Corresponds to stack/error:
get_priority_for( fatal ) ->
	1 ;

% Corresponds to stack/error:
get_priority_for( error ) ->
	2 ;

% Corresponds to warning/warn:
get_priority_for( warning ) ->
	3 ;

% Corresponds to info:
get_priority_for( info ) ->
	4 ;

% Corresponds to fine:
get_priority_for( trace ) ->
	5 ;

% Corresponds to finest/debug:
get_priority_for( debug ) ->
	6.



% Returns the name of the trace channel corresponding to the trace priority.
%
% See also: get_priority_for/1
%
% (static)
%
-spec get_channel_name_for_priority( traces:priority() ) ->
										traces:message_type().
get_channel_name_for_priority( 1 ) ->
	fatal;

get_channel_name_for_priority( 2 ) ->
	error;

get_channel_name_for_priority( 3 ) ->
	warning;

get_channel_name_for_priority( 4 ) ->
	info;

get_channel_name_for_priority( 5 ) ->
	trace;

get_channel_name_for_priority( 6 ) ->
	debug.





% Section for helper functions.


% Initializes some context-specific informations.
%
% (helper)
%
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->

	% Context-specific, useful to re-use, for example for deserialisation:

	% Retrieves the trace aggregator (false: do not launch it if not available,
	% otherwise the creation of multiple emitters would result in a race
	% condition that would lead to the creation of multiple aggregators):
	%
	AggregatorPid = class_TraceAggregator:get_aggregator(
												  _DoLaunchAggregator=false ),

	setAttributes( State, [

		{ emitter_node, get_emitter_node_as_binary() },
		{ trace_aggregator_pid, AggregatorPid }

						   ] ).



% Implementation of functions used by trace macros.


% Sets the trace categorization (part of the full emitter categorization) for
% this trace emitter to specified plain string.
%
% Setting the trace categorization early in the constructor, before sending any
% trace, allows to have all traces for a given emitter correctly gathered in the
% same trace category, which is a lot clearer when browsing afterwards.
%
% (helper)
%
-spec set_categorization( traces:emitter_categorization(), wooper:state() ) ->
								wooper:state().
set_categorization( TraceCategorization, State ) ->
	setAttribute( State, trace_categorization,
				  text_utils:string_to_binary( TraceCategorization ) ) .



% Sends a trace from that emitter.
% Message is a plain string.
%
% All information are available here, except the tick and the message
% categorization.
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message() ) ->
	basic_utils:void().
send( TraceType, State, Message ) ->
	send( TraceType, State, Message, ?DefaultMessageCategorization ).



% Message and MessageCategorization are plain strings.
% All informations available but the tick, determining its availability:
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message(),
		   traces:message_categorization() ) -> basic_utils:void().
send( TraceType, State, Message, MessageCategorization ) ->
	send( TraceType, State, Message, MessageCategorization,
		get_current_tick_offset( State ) ).



% The function used to send all types of traces:
%
% (helper)
%
-spec send( traces:message_type(), wooper:state(), traces:message(),
		   traces:message_categorization(), traces:tick() )
		  -> basic_utils:void().
send( TraceType, State, Message, MessageCategorization, Tick ) ->

	TimestampText = text_utils:string_to_binary(
	   basic_utils:get_textual_timestamp() ),

	% Follows the order of our trace format; oneway call:
	?getAttr(trace_aggregator_pid) ! { send,

	%io:format( "PID = ~w, name = ~s, emitter categorization = ~s, "
	%	"tick = ~w, user time = ~s, location = ~s, "
	%	"message categorization = ~s, trace type = ~w, message = ~s ~n",

		[
		 _TraceEmitterPid=self(),
		 _TraceEmitterName=?getAttr(name),
		 _TraceEmitterCategorization=?getAttr(trace_categorization),
		 _Tick=Tick,
		 _Time=TimestampText,
		 _Location=?getAttr(emitter_node),
		 _MessageCategorizatio=text_utils:string_to_binary(
								 MessageCategorization ),
		 _Priority=get_priority_for( TraceType ),
		 _Message=text_utils:string_to_binary( Message )
		]
	% ).
	}.






% Returns the current (numerical) execution tick, expressed in execution ticks,
% or the atom 'none' if the emitter time is not known.
%
% (helper)
%
-spec get_current_tick( wooper:state() ) -> traces:tick().
get_current_tick( State ) ->

	%io:format( "get_current_tick called for ~w, initial tick is ~w, "
	%	"current tick offset is ~w.~n",
	%	[ self(), ?getAttr(initial_tick), ?getAttr(current_tick_offset) ] ),

	InitialEmitterTick = ?getAttr(initial_tick),

	case InitialEmitterTick of

		undefined ->
			none;

		InitialTick ->
			CurrentTickOffset = ?getAttr(current_tick_offset),
			case CurrentTickOffset of

				undefined ->
					none;

				TickOffset ->
					%io:format( "InitialTick = ~p, TickOffset = ~p~n",
					%  [ InitialTick, TickOffset ] ),
					InitialTick + TickOffset

			end

	end.



% Returns the current (numerical) execution tick offset, expressed in execution
% ticks, or the atom 'none' if the emitter time is not known.
%
% (helper)
%
-spec get_current_tick_offset( wooper:state() ) -> traces:tick().
get_current_tick_offset( State ) ->

	%io:format( "get_current_tick_offset called for ~w, initial tick is ~w, "
	%	"current tick offset is ~w.~n",
	%	[ self(), ?getAttr(initial_tick), ?getAttr(current_tick_offset) ] ),

	InitialEmitterTick = ?getAttr(initial_tick),

	case InitialEmitterTick of

		undefined ->
			none;

		_InitialTick ->
			CurrentTickOffset = ?getAttr(current_tick_offset),
			case CurrentTickOffset of

				undefined ->
					none;

				TickOffset ->
					TickOffset

			end

	end.



% Returns the name of this trace emitter, as a plain string, not as a binary.
%
% (helper)
%
-spec get_plain_name( wooper:state() ) -> string().
get_plain_name( State ) ->
	text_utils:binary_to_string( ?getAttr(name) ).
