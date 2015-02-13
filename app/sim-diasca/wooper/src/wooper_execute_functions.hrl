% Modular WOOPER header gathering all execute{Request,Oneway}* primitives
% provided to write methods (function definitions here).


% Request specs.

-spec executeRequest( wooper:state(), request_name() ) ->
	{ wooper:state(), method_internal_result() }.


-spec executeRequest( wooper:state(), request_name(), method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.


-spec executeRequestWith( wooper:state(), class_name(), request_name() ) ->
	{ wooper:state(), method_internal_result() }.


-spec executeRequestWith( wooper:state(), class_name(), request_name(),
			method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.



% Oneway specs.

-spec executeOneway( wooper:state(), oneway_name() ) -> wooper:state().


-spec executeOneway( wooper:state(), oneway_name(), method_arguments() ) ->
						   wooper:state().



-spec executeOnewayWith( wooper:state(), class_name(), oneway_name() ) ->
							   wooper:state().


-spec executeOnewayWith( wooper:state(), class_name(), oneway_name(),
				method_arguments() ) -> wooper:state().




-ifdef(wooper_debug).



% Parameter-less request, calling implicitly any overridden version of the
% method.
%
% Returns an updated state.
%
executeRequest( State, RequestAtom ) when is_record( State, state_holder )
		andalso is_atom(RequestAtom) ->

	%io:format("executeRequest/2: executing ~s() from ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
									   RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


executeRequest( State, RequestAtomError )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: '~p' is not an atom.~n",
		[ self(), State#state_holder.actual_class, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtomError } );


executeRequest( StateError, RequestAtom ) when is_atom( RequestAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequest( StateError, RequestAtomError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
						   "when executing request: '~p' is not a state and "
						   "'~p' is not an atom.~n",
						   [ self(), ?MODULE, StateError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, StateError, RequestAtomError } ).





% Allows to call synchronously from the code of a given class its actual
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequest( State, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )
		andalso is_list( ArgumentList ) ->

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
					   RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Here the third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )->

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
						RequestAtom, SenderAwareState, [ StandaloneArgument ] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Catches all errors:
executeRequest( StateError, RequestAtom, _LastArg )
		when is_atom( RequestAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequest( _State, RequestAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: '~p' is not an atom.~n",
		[ self(), ?MODULE, RequestAtomError] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtomError } ).





% Parameter-less request, calling the version of the method as defined in the
% specified class.
%
executeRequestWith( State, ClassName, RequestAtom )
	when is_record( State, state_holder ) andalso is_atom( ClassName )
		andalso is_atom( RequestAtom ) ->

	%io:format("executeRequestWith/3: executing ~s() from ~s with ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


executeRequestWith( StateError, ClassName, RequestAtom )
		when is_atom( ClassName ) andalso is_atom( RequestAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p in the context of class ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequestWith( _State, ClassNameError, RequestAtomError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request in a class context: "
		"'~p' and '~p' should both be atoms.~n",
		[ self(), ?MODULE, ClassNameError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, ClassNameError, RequestAtomError } ).





% Allows to call synchronously from the code of a given class overridden methods
% (requests, here) as defined in specified classes.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequestWith( State, ClassName, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( ClassName )
		andalso is_atom( RequestAtom ) andalso is_list( ArgumentList ) ->

	%io:format("executeRequestWith/4 with list: executing ~s(~w) from ~s "
	%  "with ~s.~n", [ RequestAtom, ArgumentList,
	% State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result};


% Here the third parameter is not a list:
executeRequestWith( State, ClassName, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( ClassName )
		andalso is_atom( RequestAtom ) ->

	%io:format("executeRequestWith/3 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [StandaloneArgument] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Error cases below:
executeRequestWith( StateError, ClassName, RequestAtom, _LastArg )
		when is_atom( ClassName ) andalso is_atom( RequestAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


% Catches all remaining errors:
executeRequestWith( _State, ClassNameError, RequestAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: both '~p' (classn name) and "
		"'~p' (request name) should be atoms.~n",
		[ self(), ?MODULE, ClassNameError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_request_call, ClassNameError, RequestAtomError } ).




% Parameter-less oneway.
executeOneway( State, OnewayAtom ) when is_record( State, state_holder )
		andalso is_atom( OnewayAtom ) ->

	%io:format("executeOneway/2: executing ~s() from ~s.~n",
	%   [ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [] ),

	% Returns:
	NewState;


executeOneway( State, OnewayError ) when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
		"the oneway name should be an atom, not '~p'.~n",
		[ self(), State#state_holder.actual_class, OnewayError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayError } );


executeOneway( StateError, OnewayAtom ) when is_atom( OnewayAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOneway( StateError, OnewayError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~s' is not a state and '~p' is not an atom.~n",
		[ self(), ?MODULE, StateError, OnewayError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayError } ).





% Allows to call synchronously from the code of a given class its actual
% overridden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeOneway should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOneway( State, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( OnewayAtom )
		andalso is_list( ArgumentList ) ->

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( OnewayAtom ) ->

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState;


% All errors caught below:
executeOneway( StateError, OnewayAtom, _LastArg ) when is_atom( OnewayAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOneway( State, OnewayAtomError, _LastArg )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~p' is not an atom.~n",
		[ self(), State#state_holder.actual_class, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } );

executeOneway( _State, OnewayAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~p' is not an atom.~n",
		[ self(), ?MODULE, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } ).






% Parameter-less oneway, relying on the specified actual class to be used,
% instead of determining it from the instance virtual table.
executeOnewayWith( State, ClassName, OnewayAtom )
		when is_record( State, state_holder ) andalso is_atom( ClassName )
			andalso is_atom( OnewayAtom ) ->

	%io:format("executeOnewayWith/3: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method_with( ClassName, OnewayAtom, State, [] ),

	% Returns:
	NewState;


executeOnewayWith( State, ClassName, OnewayAtom )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: "
		"'~p' (oneway name) and '~p' (class name) should be both atoms.~n",
		[ self(), State#state_holder.actual_class, OnewayAtom, ClassName ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOnewayWith( StateError, ClassName, OnewayAtom ) ->

	% Catches all:

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p with ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } ).





% Allows to call synchronously from the code of a given class the oneway defined
% in specified class, instead of determining it from the instance virtual table.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOnewayWith( State, ClassName, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( ClassName )
		andalso is_atom( OnewayAtom ) andalso is_list( ArgumentList ) ->

	%io:format("executeOneway/4 with list: executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOnewayWith( State, ClassName, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( ClassName )
		andalso is_atom( OnewayAtom ) ->

	%io:format("executeOneway/4 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState;


executeOnewayWith( StateError, ClassName, OnewayAtom, _LastArg )
		when is_atom( ClassName ) andalso is_atom( OnewayAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p with ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


% Catches all remaining errors:
executeOnewayWith( _State, ClassName, OnewayAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway with ~s: both '~p' and '~p' should be atoms.~n",
		[ self(), ?MODULE, ClassName, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } ).






-else. % wooper_debug





% Not in debug mode here (wooper_debug not set):



% Parameter-less request.
executeRequest( State, RequestAtom ) ->

	%io:format("executeRequest/2: executing ~s() from ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% No special checking performed in release mode:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
							  RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Allows to call synchronously from the code of a given class its actual
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeRequest should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequest( State, RequestAtom, ArgumentList )
  when is_list( ArgumentList ) ->

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% No special checking performed in release mode:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
						 RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };



% Here third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) ->

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, StandaloneArgument,
	% State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{request_sender=self()},

	% No special checking performed in release mode:
	{ NewState, { wooper_result, Result } } = wooper_execute_method(
				   RequestAtom,	SenderAwareState, [ StandaloneArgument ] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Parameter-less request, calling the version of the method as defined in the
% specified class.
executeRequestWith( State, ClassName, RequestAtom ) ->

	%io:format("executeRequestWith/3: executing ~s() from ~s with ~s.~n",
	%	[ RequestAtom , State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result}.




% Allows to call synchronously from the code of a given class overridden methods
% (requests, here) as defined in specified classes.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequestWith( State, ClassName, RequestAtom, ArgumentList )
  when is_list(ArgumentList) ->

	%io:format("executeRequestWith/4 with list: executing ~s(~w) from ~s "
	%  "with ~s.~n", [ RequestAtom, ArgumentList,
	% State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Here the third parameter is not a list:
executeRequestWith( State, ClassName, RequestAtom, StandaloneArgument ) ->

	%io:format("executeRequestWith/3 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, { wooper_result, Result } } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [ StandaloneArgument ] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Parameter-less oneway.
executeOneway( State, OnewayAtom ) ->

	%io:format("executeOneway/2: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom, State, [] ),

	% Returns:
	NewState.



% Allows to call synchronously from the code of a given class its actual
% overridden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeOneway should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOneway( State, OnewayAtom, ArgumentList ) when is_list(ArgumentList) ->

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom,
										  State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) ->

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom,
										  State, [ StandaloneArgument ] ),

	% Returns:
	NewState.




% Parameter-less oneway, relying on the specified actual class to be used,
% instead of determining it from the instance virtual table.
%
executeOnewayWith( State, ClassName, OnewayAtom ) ->

	%io:format("executeOnewayWith/3: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with( ClassName,
										  OnewayAtom, State, [] ),

	% Returns:
	NewState.



% Allows to call synchronously from the code of a given class the oneway defined
% in specified class, instead of determining it from the instance virtual table.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOnewayWith( State, ClassName, OnewayAtom, ArgumentList )
  when is_list( ArgumentList ) ->

	%io:format("executeOneway/4 with list: executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOnewayWith( State, ClassName, OnewayAtom, StandaloneArgument ) ->

	% io:format("executeOneway/4 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState.



-endif. % wooper_debug
