% Copyright (C) 2012-2014 Olivier Boudeville
%
% This file is part of the WOOPER library.
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


% Module containing some facilities for WOOPER users, better placed in a
% standalone module than duplicated in each class.
%
-module(wooper).




% First, all the various exports:


% Very generic:
%
-export([ get_class_name/1, state_to_string/1 ]).


% Communication helpers:
%
-export([ send_requests/3,

		  send_requests_and_wait_acks/4, send_requests_and_wait_acks/5,
		  wait_for_request_answers/2, wait_for_request_answers/3,

		  obtain_results_for_requests/3,
		  send_and_listen/3, receive_result/0

		]).



% Creation helpers:
%
-export([ create_hosting_process/2,
		  construct_and_run/2, construct_and_run_synchronous/3 ]).



% Destruction helpers:
%
-export([ delete_any_instance_referenced_in/2,
		  delete_synchronously_any_instance_referenced_in/2,
		  delete_synchronously_instance/1, delete_synchronously_instances/1
		]).


% Infrequently-called functions for state management:
%
-export([ get_all_attributes/1 ]).



-ifdef(wooper_debug).

% State-related helpers (only available in debug mode):
-export([
		 virtual_table_to_string/1,
		 instance_to_string/1,
		 display_state/1,
		 display_virtual_table/1,
		 display_instance/1
		]).

-endif. % wooper_debug



% Basics:
-export([ get_class_manager/0, default_exit_handler/3 ]).


% Defined here because embodied instances rely on the main loop which needs that
% information to destruct the corresponding instance:
%
-define( wooper_superclasses, [] ).


% For the name of the registered process that keeps the per-class method
% hashtables:
%
-include("wooper_class_manager.hrl").


% For synchronous_time_out, state_holder and al:
-include("wooper_defines_exports.hrl").


% For type request_result/1:
-include("wooper_types_exports.hrl").


% Otherwise getClassName/1, get_superclasses/0, etc. are unused:
-include("wooper_classes_exports.hrl").

% Otherwise wooper_execute_method_with/4 is unused:
-include("wooper_execute_internal_exports.hrl").


%-opaque state() :: #state_holder{}.
-type state() :: #state_holder{}.

% We prefer having it prefixed by wooper:
-export_type([ state/0 ]).



% For getAttr and al:
-include("wooper_state_exports.hrl").

% Otherwise executeRequest/3 and all reported as unused:
-include("wooper_execute_exports.hrl").


% Now, function definitions:


% For getAttribute/2 and al:
-include("wooper_state_functions.hrl").

% For executeRequest/2:
-include("wooper_execute_functions.hrl").

% For wooper_execute_method/3:
-include("wooper_execute_internal_functions.hrl").

% For get_superclasses/1:
-include("wooper_classes_functions.hrl").





% Section for communication helpers.



% Sends specified request (based on its names and arguments) to each of the
% specified target instances.
%
% (helper)
%
-spec send_requests( request_name(), method_arguments(), [ pid() ] ) ->
						  basic_utils:void().
send_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	Request = { RequestName, RequestArgs, self() },

	[ InstancePid ! Request || InstancePid <- TargetInstancePIDs ].





% Sends specified request (based on its name and arguments) to each of the
% specified target instances, and waits (indefinitively) for their
% acknowledgement, which shall be their only result (i.e. these methods should
% be requests only for synchronisation).
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
								   [ pid() ], atom() ) -> basic_utils:void().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
					AckAtom ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_indefinitively_for_request_answers( TargetInstancePIDs, AckAtom ).



% Sends specified request (based on its names and arguments) to each of the
% specified target instances, and waits for their acknowledgement; returns
% whether it succeeded or if some instances triggered a time-out.
%
-spec send_requests_and_wait_acks( request_name(), method_arguments(),
				[ pid() ], basic_utils:time_out(), atom() ) ->
										 requests_outcome().
send_requests_and_wait_acks( RequestName, RequestArgs, TargetInstancePIDs,
					   Timeout, AckAtom ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_for_request_answers( TargetInstancePIDs, Timeout, AckAtom ).



% Waits for an acknowledgement answer, based on specified atom, from the
% specified requested instances, indefinitively (no time-out).
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ pid() ], atom() ) -> requests_outcome().
wait_for_request_answers( RequestedPidList, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ).




% Waits for an acknowledgement answer, based on specified atom, from the
% specified requested instances, unless the specified time-out is exceeded
% (specified as integer milliseconds or the 'infinity' atom).
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ pid() ], basic_utils:time_out(), atom() ) ->
					  requests_outcome().
wait_for_request_answers( RequestedPidList, _Timeout=infinity, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom );

wait_for_request_answers( RequestedPidList, Timeout, AckAtom ) ->

	InitialTimestamp = basic_utils:get_timestamp(),

	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  AckAtom ).





% Waits until end of time if necessary.
%
% (helper)
%
wait_indefinitively_for_request_answers( _RequestedPidList=[], _AckAtom ) ->
	success;

wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_indefinitively_for_request_answers( NewPidList, AckAtom )

	end.




% Wait until specified time-out is reached.
%
% (helper)
%
wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  AckAtom ) ->
	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  _DefaultPollDuration=1000, AckAtom ).


wait_for_request_answers( _RequestedPidList=[], _InitialTimestamp, _Timeout,
						  _PollDuration, _AckAtom ) ->
	success;

wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  PollDuration, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_for_request_answers( NewPidList, InitialTimestamp, Timeout,
									  PollDuration, AckAtom )

	after PollDuration ->

			NewDuration = basic_utils:get_duration( InitialTimestamp,
											  basic_utils:get_timestamp() ),

			case NewDuration > Timeout of

				true ->
					{ failure, RequestedPidList };

				false ->
					% Still waiting then:
					wait_for_request_answers( RequestedPidList,
						   InitialTimestamp, Timeout, PollDuration, AckAtom )

			end

	end.





% Sends the specified request to all specified instances for execution, in
% parallel, and returns the corresponding results, in indiscriminated order.
%
% Note: no specified order is enforced in the result list; hence this helper is
% meant to be used when we can collect each result regardless of its specific
% sender.
%
% (exported helper)
%
-spec obtain_results_for_requests( request_name(), method_arguments(),
						  [ pid() ] ) -> [ any() ].
obtain_results_for_requests( RequestName, RequestArgs, TargetInstancePIDs ) ->

	send_requests( RequestName, RequestArgs, TargetInstancePIDs ),

	% Of course we expect that no previously received WOOPER message is
	% remaining in the queue.

	collect_wooper_messages( _Count=length( TargetInstancePIDs ), _Acc=[] ).



% Collects specified number of WOOPER messages, and returns a list of the
% corresponding results.
%
% (helper)
%
collect_wooper_messages( _Count=0, Acc ) ->
	Acc;

collect_wooper_messages( Count, Acc ) ->

	receive

		{ wooper_result, Res } ->
			collect_wooper_messages( Count-1, [ Res | Acc ] )

	end.




% Section for creation helpers.



% Creates (asynchronously) a blank process, waiting to embody a WOOPER instance
% once it will have received its class and construction parameters, and link it
% to the caller and to the specified process.
%
-spec create_hosting_process( net_utils:node_name(), pid() ) -> pid().
create_hosting_process( Node, ToLinkWithPid ) ->

	WaitFun = fun() ->

		% Closure; not atomic:
		erlang:link( ToLinkWithPid ),

		receive

			  { embody, [ Class, ConstructionParameters ] } ->

				%io:format( "Process ~w becoming asynchronously an instance "
				%		   "of class '~s', constructed from following "
				%		   "parameters:~n~p.~n",
				%		   [ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run( Class, ConstructionParameters );


			  % We might need to notify another process than the caller:
			  { embody, [ Class, ConstructionParameters ], ToNotifyPid } ->

				%io:format( "Process ~w becoming synchronously an instance "
				%		   "of class '~s', constructed from following "
				%		   "parameters:~n~p.~n",
				%		   [ self(), Class, ConstructionParameters ] ),

				% Never returns:
				construct_and_run_synchronous( Class, ConstructionParameters,
											ToNotifyPid )

		end

	end,

	spawn_link( Node, WaitFun ).



% Constructs the initial state of an instance of specified class, using
% specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run( class_name(), [ method_argument() ] ) ->
							   no_return().


-ifdef(wooper_debug).

construct_and_run( Classname, ConstructionParameters ) ->

	%io:format( "wooper:construct_and_run for class ~p and parameters ~p.~n",
	%		   [ Classname, ConstructionParameters ] ),

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			ReadyState = ConstructState#state_holder{
						   attribute_table=TunedTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			apply( Classname, wooper_main_loop, [ ReadyState ] );


		Other ->

			error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned '~p' instead."
				" Construction parameters were:~n~p.~n",
				[ self(), Classname, Other, ConstructionParameters ] ),

			% Wait a bit as error_msg seems asynchronous:
			timer:sleep( ?wooper_error_display_waiting ),

			throw( { invalid_constructor, Classname } )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end.



-else. % wooper_debug


construct_and_run( Classname, ConstructionParameters ) ->

	BlankState = get_blank_state( Classname ),

	ConstructState = try

		apply( Classname, construct, [ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end,

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),


	ReadyState = ConstructState#state_holder{ attribute_table=TunedTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	apply( Classname, wooper_main_loop, [ ReadyState ] ).


-endif. % wooper_debug






% Constructs synchronously the initial state of an instance of specified class,
% using specified construction parameters, and enters its main loop.
%
% (helper)
%
-spec construct_and_run_synchronous( class_name(), [ method_argument() ],
									 pid() ) -> no_return().


-ifdef(wooper_debug).

construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	BlankState = get_blank_state( Classname ),

	try apply( Classname, construct,
			   [ BlankState | ConstructionParameters ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Notify early:
			SpawnerPid ! { spawn_successful, self() },

			% Enforces a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			%
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			ReadyState = ConstructState#state_holder{
						   attribute_table=TunedTable },

			% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper'
			% instead of the right class:
			%
			% (never returns)
			%
			apply( Classname, wooper_main_loop, [ ReadyState ] );



		Other ->

			error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned '~p' instead."
				" Construction parameters were:~n~p.~n",
				[ self(), Classname, Other, ConstructionParameters ] ),

			% Wait a bit as error_msg seems asynchronous:
			timer:sleep( ?wooper_error_display_waiting ),

			throw( { invalid_constructor, Classname } )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end.



-else. % not in wooper_debug:


construct_and_run_synchronous( Classname, ConstructionParameters,
							   SpawnerPid ) ->

	BlankState = get_blank_state( Classname ),

	% Faulty returns (non-state) not detected here:
	ConstructState = try

			 apply( Classname, construct,
					[ BlankState | ConstructionParameters ] )

	catch

		Reason:ErrorTerm ->
			trigger_error( Reason, ErrorTerm, Classname,
						   ConstructionParameters )

	end,

	% Notify early:
	SpawnerPid ! { spawn_successful, self() },

	% Enforces a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	%
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

	ReadyState = ConstructState#state_holder{ attribute_table=TunedTable },

	% Otherwise, in wooper_destruct/1 and all, ?MODULE will be 'wooper' instead
	% of the right class:
	%
	% (never returns)
	%
	apply( Classname, wooper_main_loop, [ ReadyState ] ).


-endif. % not wooper_debug.



% Helpers.


% Returns the state of a blank WOOPER instance of specified class.
%
% (helper)
%
-spec get_blank_state( class_name() ) -> wooper:state().
get_blank_state( Classname ) ->

	#state_holder{

		virtual_table   = retrieve_virtual_table( Classname ),

		attribute_table = ?wooper_hashtable_type:new(
									?wooper_attribute_count_upper_bound ),

		actual_class    = Classname,

		request_sender  = undefined

	}.



% Returns the WOOPER Class Manager.
%
% If it is already running, finds it and returns its atom, otherwise launches
% it, and returns that same atom as well.
%
-spec get_class_manager() -> basic_utils:registration_name().
get_class_manager() ->

	case lists:member( ?wooper_class_manager_name, registered() ) of

		true ->
			?wooper_class_manager_name;

		_ ->
			spawn( ?wooper_class_manager_name, start, [ self() ] ),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			receive

				class_manager_registered ->
					?wooper_class_manager_name

			% 10-second time-out:
			after 10000 ->

				error_logger:error_msg( "wooper:get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand.~n"
				),
				undefined

			end

	end.




% WOOPER default EXIT handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWOOPERExitReceived/3 method.
%
-spec default_exit_handler( wooper:state(), pid(), any() ) -> wooper:state().
default_exit_handler( State, Pid, ExitType ) ->

	error_logger:warning_msg( "WOOPER default EXIT handler of the ~w "
	  "instance ~w ignored following EXIT message from ~w:~n'~p'.~n~n",
	  [ State#state_holder.actual_class, self(), Pid, ExitType ] ),

	State.




% Returns the virtual table corresponding to the specified class.
%
% (helper)
%
-spec retrieve_virtual_table( class_name() ) ->
				   ?wooper_hashtable_type:?wooper_hashtable_type().
retrieve_virtual_table( Classname ) ->

	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).
	get_class_manager() ! { get_table, Classname, self() },
	receive

		{ virtual_table, Table } ->
			%?wooper_hashtable_type:display( Table ),
			Table

	end.



% Triggers specified construction error (notify and throw).
%
% (helper)
%
-spec trigger_error( 'throw' | 'exit' | 'error', term(), class_name(),
					 [ method_arguments() ] ) -> no_return().
trigger_error( Reason, ErrorTerm, Classname, ConstructionParameters ) ->

	% Construction failed:
	% (error term would often be unreadable with ~p)

	Arity = length( ConstructionParameters ) + 1,

	error_logger:error_msg( "~nWOOPER error for PID ~w, "
		"constructor (~s:construct/~B) failed (cause: ~p):~n~n"
		" - with error term:~n~p~n~n"
		" - stack trace was (latest calls first):~n~p~n~n"
		" - for parameters:~n~p~n~n",
		[ self(), Classname, Arity, Reason, ErrorTerm, erlang:get_stacktrace(),
		  ConstructionParameters ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),

	% Terminates the process:
	throw( { wooper_constructor_failed, self(), Classname, Arity,
			 ConstructionParameters, ErrorTerm } ).





% Methods for getting information about an instance.


% Returns the actual class name of the current instance.
%
% (helper)
%
-spec get_class_name( wooper:state() ) -> class_name().
get_class_name( State ) ->

	% Note: a mere ?MODULE would not work (ex: case of an inherited method,
	% compiled with the module name of the parent class).

	State#state_holder.actual_class.



% Returns a textual representation of the attributes of the specified state.
%
-spec state_to_string( wooper:state() ) -> string().
state_to_string( State ) ->

	Attributes = ?wooper_hashtable_type:enumerate(
								   State#state_holder.attribute_table ),

	% We prefer having the attributes sorted by their name, in alphabetical
	% order:
	%
	SortedAttributes = lists:keysort( _Index=1, Attributes ),

	lists:foldl(

		fun( { AttName, AttrValue }, Acc ) ->
			Acc ++ io_lib:format(
				"     * ~s = ~s~n",
				[
					text_utils:term_to_string( AttName ),
					text_utils:term_to_string( AttrValue, _MaxDepth=16,
											_MaxLength=100 )
				] )

		end,

		io_lib:format( "State of ~w:~nInstance of ~s with ~B attribute(s):~n",
			[ self(), get_class_name( State ), length( Attributes ) ] ),

		SortedAttributes ).



-ifdef(wooper_debug).


% Returns a textual representation of the virtual table corresponding to the
% specified state.
%
% (helper)
%
-spec virtual_table_to_string( wooper:state() ) -> string().
virtual_table_to_string( State ) ->

	lists:foldl(

		fun( { { Name, Arity }, Module }, String ) ->
			String ++ io_lib:format( "     * ~s/~B -> ~s~n",
				[ Name, Arity, Module ] )
		end,

		io_lib:format( "Virtual table of ~w:~n"
					   "(method name/arity -> module defining that method)~n",
					   [ self() ] ),

		?wooper_hashtable_type:enumerate( State#state_holder.virtual_table ) ).



% Returns a textual representation of this instance, including its state and
% virtual table.
%
% (helper)
%
-spec instance_to_string( wooper:state() ) -> string().
instance_to_string( State ) ->
	io_lib:format( "Inspection of instance ~w:~n~n  + ~s~n  + ~s",
		[ self(), state_to_string( State ),
			virtual_table_to_string( State ) ] ).



% Displays the inner state of this instance.
%
% This is not a method.
%
-spec display_state( wooper:state() ) -> basic_utils:void().
display_state( State ) ->
	error_logger:info_msg( "~s~n", [ state_to_string( State ) ] ).



% Displays the virtual table of this instance.
%
% This is not a method.
%
-spec display_virtual_table( wooper:state() ) -> basic_utils:void().
display_virtual_table( State ) ->
	error_logger:info_msg( "~s~n", [ virtual_table_to_string( State ) ] ).


% Displays information about this instance.
%
% This is not a method.
%
-spec display_instance( wooper:state() ) -> basic_utils:void().
display_instance( State ) ->
	error_logger:info_msg( "~s~n", [ instance_to_string( State ) ] ).


-endif. % wooper_debug



% Returns all the attributes of this instance, as a list of { AttributeName,
% AttributeValue } pairs.
%
-spec get_all_attributes( wooper:state() ) ->
									   ?wooper_hashtable_type:entries().
get_all_attributes( State ) ->
	?wooper_hashtable_type:enumerate( State#state_holder.attribute_table ).



% Helper function to test requests.
%
% Allows to test from the shell an instance by sending it requests (hence
% needing a receive, whereas the caller is the shell), and waiting for any kind
% of message sent back.
%
% Returns the actual result or received value.
%
% Available even when debug mode is off.
%
-spec send_and_listen( pid(), request_name(), method_arguments() ) -> term().
send_and_listen( InstancePid, RequestName, Arguments ) ->

	InstancePid ! { RequestName, Arguments, self() },

	receive

		{ wooper_result, Result } ->

			%io:format( "Result of call to '~w' with arguments '~w': ~s~n",
			%	[ RequestName, Arguments,
			%	 text_utils:term_to_string( Result ) ] ),

			Result;

		Anything ->

			%io:format( "Answer to call to '~w' with arguments '~w': ~s~n",
			%	[ RequestName, Arguments,
			%	  text_utils:term_to_string( Anything ) ] ),

			Anything

	end.



% Returns the result corresponding to the first pending WOOPER request (the
% latest sent one), or blocks.
%
% (helper)
%
-spec receive_result() -> request_result( any() ).
receive_result() ->

	receive

		{ wooper_result, R } ->
			R

	end.






% Deletion-related section.



% Deletes (asynchronously: "fire and forget") the WOOPER instance(s) potentially
% stored in the specified attribute list.
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: DeleteState = delete_any_instance_referenced_in( [
% first_pid_attr, second_pid_attr ], State ) or
% delete_any_instance_referenced_in( my_pid_attr, State ).
%
% (helper)
%
-spec delete_any_instance_referenced_in( [ attribute_name() ], wooper:state() )
									   -> wooper:state().
delete_any_instance_referenced_in( _Attributes=[], State ) ->
	State;


delete_any_instance_referenced_in( [ PidAttribute | T ], State ) ->

	NewState = case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end,
	delete_any_instance_referenced_in( T, NewState );


delete_any_instance_referenced_in( PidAttribute, State ) ->

	case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end.





% Deletes (synchronously, in a parallel yet blocking manner) the WOOPER
% instance(s) potentially stored in specified attribute list (a standalone
% attribute may be specified as well).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], State ) or
% delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper:state() ) -> wooper:state().
delete_synchronously_any_instance_referenced_in( _Attributes=[], State ) ->
	State;

delete_synchronously_any_instance_referenced_in( Attributes, State )
  when is_list( Attributes ) ->

	% Triggers the deletion of selected instances:
	{ TargetAttributes, TargetPids } = delete_pid_from( Attributes, State ),

	%io:format( "delete_synchronously_any_instance_referenced_in:~n"
	%			" - attributes are: ~p~n"
	%			" - PIDs are: ~p~n", [ TargetAttributes, TargetPids ] ),

	% Waits for their completion:
	wait_for_deletion_ack( TargetPids ),

	%io:format( "(all deletion acks received for ~p)~n", [ TargetAttributes ] ),

	% Erases deleted PIDs:
	UndefinedAttributes = [ { AttrName, undefined } ||
							  AttrName <- TargetAttributes ],

	setAttributes( State, UndefinedAttributes );


delete_synchronously_any_instance_referenced_in( Attribute, State ) ->

	case ?getAttr(Attribute) of

		undefined ->
			State;

		Pid when is_pid( Pid ) ->
			Pid ! { synchronous_delete, self() },

			receive

				{ deleted, Pid } ->
					setAttribute( State, Attribute, undefined )

			end

	end.




% Helper, which sends delete messages to all PIDs found in the list of
% attributes, and returns a list of the attributes and a list of the PÏDs.
%
delete_pid_from( Attributes, State ) ->

	DeleteMessage = { synchronous_delete, self() },

	delete_pid_from( Attributes, DeleteMessage, State, _AccAttr=[],
					_AccPid=[] ).


delete_pid_from( _Attributes=[], _DeleteMessage, _State, AccAttr, AccPid ) ->
	{ AccAttr, AccPid };

delete_pid_from( [ Attr | T ], DeleteMessage, State, AccAttr, AccPid ) ->

	case ?getAttr( Attr ) of

		undefined ->
			delete_pid_from( T, DeleteMessage, State, AccAttr, AccPid ) ;

		Pid when is_pid(Pid) ->

			%io:format( "Deleting now ~s (PID: ~w).~n", [ Attr, Pid ] ),

			Pid ! DeleteMessage,
			delete_pid_from( T, DeleteMessage, State, [ Attr | AccAttr ],
					[ Pid | AccPid ] )

	end.



% Deletes specified instance synchronously.
%
% Will wait forever the effective termination of the specified instance.
%
-spec delete_synchronously_instance( pid() ) -> basic_utils:void().
delete_synchronously_instance( InstancePid ) ->

	%io:format( "delete_synchronously_instance for ~p.~n", [ InstancePid ] ),

	InstancePid ! { synchronous_delete, self() },

	receive

		{ deleted, InstancePid } ->
			ok

	end.



% Deletes specified instances synchronously (yet in parallel).
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long) .
%
% (exported helper)
%
-spec delete_synchronously_instances( [ pid() ] ) -> basic_utils:void().
delete_synchronously_instances( InstanceList ) ->

	%io:format( "delete_synchronously_instances for ~p.~n", [ InstanceList ] ),

	DeleteMessage = { synchronous_delete, self() },

	[ I ! DeleteMessage || I <- InstanceList ],

	wait_for_deletion_ack( InstanceList ).



% Helper used to wait for the receiving of all deletion acknowledgements:
%
% Could almost use basic_utils:wait_for_acks/3.
%
wait_for_deletion_ack( _WaitedPids=[] ) ->
	ok;

wait_for_deletion_ack( WaitedPids ) ->

	receive

		{ deleted, Pid } ->

			case lists:member( Pid, WaitedPids ) of

				false ->
					throw( { unexpected_pid_deletion, Pid } );

				true ->
					NewWaitedPids = lists:delete( Pid, WaitedPids ),
					wait_for_deletion_ack( NewWaitedPids )

			end

	% Note that this time-out is reset at each ack:
	after ?synchronous_time_out ->

			case examine_waited_deletions( WaitedPids, _Acc=[] ) of

				[] ->
					ok;

				NewWaitedPids ->
					io:format( "(still waiting for the synchronous deletion of "
							   "following live WOOPER instance(s): ~p)~n",
							   [ NewWaitedPids ] ),

					% Warns, but does not trigger failures:
					wait_for_deletion_ack( NewWaitedPids )

			end

	end.



examine_waited_deletions( _WaitedPids=[], Acc ) ->
	Acc;

examine_waited_deletions( _WaitedPids=[ Pid | T ], Acc ) ->

	case erlang:is_process_alive( Pid ) of

		true ->
			examine_waited_deletions( T, [ Pid | Acc ] );

		false ->
			io:format( "Stopped waiting for the deletion of instance "
					   "whose PID is ~p: not found alive.~n", [ Pid ] ),

			examine_waited_deletions( T, Acc )

	end.
