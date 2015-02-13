% Modular WOOPER header gathering all direct primitives for method execution,
% both in debug and non-debug mode (the two versions shall be one after the
% other and shall share their documentation):
%
% - wooper_execute_method/3
% - wooper_execute_method_with/4
% - wooper_effective_method_execution/4
% - wooper_handle_oneway_execution/3

% Much code is duplicated because no '-ifdef' clause can be defined in case
% clauses.



% Documentations and signatures, common to debug and non-debug modes:



% Executes the specified method, designated by its atom, with specified instance
% state and list of parameters.
%
% If the method is not found (either in the class module or in its ancestor
% trees), an error tuple beginning with the atom 'wooper_method_not_found' is
% returned with an unchanged state.
%
% If the method is found but its execution fails, an error tuple beginning with
% the atom 'wooper_method_failed' is returned with an unchanged state.
%
% If it does not fail but returns an unexpected result (i.e. not a tuple
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
%
% If its execution succeeds, then {wooper_result,Result} is returned (with
% Result being the actual result of the method call) with an updated state.
%
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returned, which allows a caller that sent his
% PID to be warned it is useless, as no answer should be expected.
%
% In debug mode, the error logs have been added, as debugging faulty oneways is
% more difficult: they cannot return any error to the caller, they can just
% crash and notify any linked or monitoring process.
%
% Note: atom and state checking in guards should be superfluous.
%
-spec wooper_execute_method( method_name(), wooper:state(),
							 method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.



% Exactly as wooper_execute_method, except that the target module (class) is
% directly specified, instead of being determined from the instance virtual
% table.
%
-spec wooper_execute_method_with( class_name(), method_name(), wooper:state(),
								  method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.



% Triggers the actual method execution.
%
-spec wooper_effective_method_execution( module(), method_name(),
										 wooper:state(), method_arguments() ) ->
	{ wooper:state(), method_internal_result() }.






% Section for wooper_execute_method/3.



-ifdef(wooper_debug).


wooper_execute_method( MethodAtom, State, Parameters )
  when is_atom( MethodAtom ) andalso is_record( State, state_holder )
	   andalso is_list( Parameters ) ->

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]),

	% +1: take into account the State additional parameter:
	case wooper_lookup_method( State, MethodAtom, length( Parameters ) + 1 ) of

		{ value, LocatedModule } ->

			%io:format("wooper_execute_method: executing ~s:~s(~w) from ~s.~n",
			%   [ ?MODULE, MethodAtom, Parameters, LocatedModule ]),

			% Returns { NewState, PossibleResult }:
			wooper_effective_method_execution( LocatedModule, MethodAtom,
				State, Parameters );

		hashtable_key_not_found ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple
					% elements, as if in a single string ("M/A"), the result
					% is displayed as a list:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"oneway method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length( Parameters ) + 1, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_not_found, self(),
							 State#state_holder.actual_class,
							 MethodAtom, length( Parameters ) + 1,
							 Parameters} );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"request method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length( Parameters ) + 1, Parameters ] ),

					{ State, { wooper_method_not_found, self(),
							   State#state_holder.actual_class,
							   MethodAtom, length( Parameters ) + 1,
							   Parameters } }


			% No other term can be returned.

			end

	end.



-else. % not in wooper_debug:



wooper_execute_method( MethodAtom, State, Parameters ) ->

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]),

	% +1: take into account the State additional parameter:
	case wooper_lookup_method( State, MethodAtom, length(Parameters)+1 ) of


		{ value, LocatedModule } ->

			%io:format("wooper_execute_method: executing ~s:~s(~w) from ~s.~n",
			%   [ ?MODULE, MethodAtom, Parameters, LocatedModule ]),

			wooper_effective_method_execution( LocatedModule, MethodAtom,
				State, Parameters );


		hashtable_key_not_found ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple elements,
					% as if in a single string ("M/A"), the result is displayed
					% as a list:
					error_logger:error_msg( "WOOPER error for PID ~w, "
						"oneway method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length( Parameters ) + 1, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_not_found, self(),
							 State#state_holder.actual_class,
							 MethodAtom, length( Parameters ) + 1,
							 Parameters } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "WOOPER error for PID ~w, "
						"request method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length( Parameters ) + 1, Parameters ] ),

					{ State, { wooper_method_not_found, self(),
							   State#state_holder.actual_class,
							   MethodAtom, length( Parameters ) + 1,
							   Parameters } }

			end

		% No other term can be returned.

	end.


-endif. % not wooper_debug.



% Looks-up specified method (Method/Arity, ex: toString/0) to be found in
% heritance tree and returns either { 'value', Module } with Module
% corresponding to the class that implements that method, or
% 'hashtable_key_not_found'.
%
% Note: uses the pre-built virtual table for this class.
%
% (helper)
%
-spec wooper_lookup_method( wooper:state(), method_name(), arity() ) ->
		   { 'value', class_name() } | 'hashtable_key_not_found'.
wooper_lookup_method( State, MethodAtom, Arity ) ->
	?wooper_hashtable_type:lookupEntry( { MethodAtom, Arity },
		State#state_holder.virtual_table ).




% Section for wooper_execute_method_with/4.
%
% (same implementation in debug or not)



wooper_execute_method_with( ClassName, MethodAtom, State, Parameters )
  when is_atom( ClassName ) andalso is_atom( MethodAtom )
	   andalso is_record( State, state_holder )
	   andalso is_list( Parameters ) ->

	% One check should be added: ClassName must be a super-class
	% (direct or not) of the actual class.
	%
	wooper_effective_method_execution( ClassName, MethodAtom, State,
		Parameters ).






% Section for wooper_effective_method_execution/4.



-ifdef(wooper_debug).


wooper_effective_method_execution( SelectedModule, MethodAtom, State,
		Parameters ) ->

	%io:format( "WOOPER: effective execution of ~p:~p.~n",
	%		  [ SelectedModule, MethodAtom ] ),

	try apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Void method (no result returned, only a state):
		% ?wooper_return_state_only:
		NewState when is_record( NewState, state_holder ) ->
			{ NewState, wooper_method_returns_void };

		% Method returning a result (and a state of course):
		% ?wooper_return_state_result:
		{ NewState, Result } when is_record( NewState, state_holder ) ->
			{ NewState, { wooper_result, Result } };

		% Neither a wooper result nor an exception: faulty return.
		Other ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"oneway method ~s:~s/~B made a faulty return "
						"'~p', parameters were:~n~p~n",
						[ self(), SelectedModule, MethodAtom,
							length( Parameters ) + 1, Other, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_faulty_return, self(),
						SelectedModule, MethodAtom,
						length( Parameters ) + 1, Parameters, Other } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"request method ~s:~s/~B made a faulty return "
						"'~p', parameters were:~n~p~n",
						[ self(), SelectedModule, MethodAtom,
							length( Parameters ) + 1, Other, Parameters ] ),

					{ State,
						{ wooper_method_faulty_return, self(),
							SelectedModule, MethodAtom,
							length( Parameters ) + 1, Parameters, Other } }

			end

	catch

		% All next cases are error cases.
		%
		% One option is to return an appropriate error term, but it is useful
		% only for requests, as oneways send back no result.
		%
		% Another option is to let the faulty process crash: oneways would not
		% send more answers, but linked and monitoring processes could
		% nevertheless be notified.

		% Finally, failed requests result in a log, an error answer being
		% returned, then a crash if the error is due to internal reasons,
		% whereas failed oneways result in a log then a crash, similarly if the
		% error is due to internal reasons.

		% User-defined exceptions are not caught, they will be rethrown and make
		% the process fail.

		% This the counterpart of {'EXIT',ErrorTerm}.
		% What would be the counterpart of {'EXIT',Pid,ErrorTerm}?
		% Reason can be: throw, error or exit.
		Reason:ErrorTerm ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
								"oneway method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
								  length( Parameters ) + 1, Reason, ErrorTerm,
								  Parameters, erlang:get_stacktrace(),
								  wooper:state_to_string( State ) ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					% Terminates the process:
					throw( { wooper_oneway_failed, self(), SelectedModule,
						MethodAtom, length( Parameters ) + 1, Parameters,
						ErrorTerm } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash: (error term would
					% often be unreadable with ~p)
					error_logger:error_msg( "~nWOOPER error for PID ~w, request"
								" method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
								  length( Parameters ) + 1, Reason, ErrorTerm,
								  erlang:get_stacktrace(), Parameters,
								  wooper:state_to_string( State )
								 ] ),

					% Wait a bit (yes, even for requests, as often the VM will
					% be halted just before) as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					{ State,
						{ wooper_method_failed, self(), SelectedModule,
							MethodAtom, length(Parameters)+1,
							Parameters, ErrorTerm } }

			end

	end.



-else. % not in wooper_debug:



wooper_effective_method_execution( SelectedModule, MethodAtom, State,
		Parameters ) ->

	%io:format( "WOOPER: effective execution of ~p:~p.~n",
	%		  [ SelectedModule, MethodAtom ] ),

	try apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Method returning a result (and a state of course):
		% ?wooper_return_state_result:
		%
		{ NewState, Result }  ->
			{ NewState, { wooper_result, Result } };

		% Void method (no result returned, only a state):
		% ?wooper_return_state_only:
		NewState ->
			{ NewState, wooper_method_returns_void }

	catch

		% All next cases are error cases.
		%
		% One option is to return an appropriate error term, but it is useful
		% only for requests, as oneways send back no result.
		%
		% Another option is to let the faulty process crash: oneways would not
		% send more answers, but linked and monitoring processes could
		% nevertheless be notified.

		% Finally, failed requests result in a log, an error answer being
		% returned, then a crash if the error is due to internal reasons,
		% whereas failed oneways result in a log then a crash, similarly if the
		% error is due to internal reasons.

		% User-defined exceptions are not caught, they will be rethrown and make
		% the process fail.

		% This the counterpart of {'EXIT',ErrorTerm}.
		% What would be the counterpart of {'EXIT',Pid,ErrorTerm}?
		% Reason can be: throw, error or exit.
		Reason:ErrorTerm ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
								"oneway method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
								  length( Parameters ) + 1, Reason, ErrorTerm,
								  erlang:get_stacktrace(), Parameters,
								  wooper:state_to_string( State ) ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),

					% Terminates the process:
					throw( { wooper_oneway_failed, self(), SelectedModule,
						MethodAtom, length( Parameters ) + 1, Parameters,
						ErrorTerm } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash: (error term would
					% often be unreadable with ~p)
					error_logger:error_msg( "~nWOOPER error for PID ~w, request"
								" method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
								  length( Parameters ) + 1, Reason, ErrorTerm,
								  erlang:get_stacktrace(), Parameters,
								  wooper:state_to_string( State ) ] ),

					{ State,
						{ wooper_method_failed, self(), SelectedModule,
							MethodAtom, length( Parameters ) + 1,
							Parameters, ErrorTerm } }

			end

	end.


-endif. % not wooper_debug.





% Section for wooper_handle_oneway_execution/3.



-compile( { inline, [ wooper_handle_oneway_execution/3 ] } ).


-spec wooper_handle_oneway_execution( method_name(), wooper:state(),
							  method_arguments() ) -> wooper:state().


-ifdef(wooper_debug).


% Executes the specified oneway, and performs debug-mode only additional
% checkings.
%
% Returns an updated state.
%
% Note: must not be a macro, as its value would be the one of its first
% statement, not the one of its last. This function is requested to be inlined
% instead.
%
wooper_handle_oneway_execution( MethodAtom, State, ArgumentList ) ->

	case wooper_execute_method( MethodAtom, State, ArgumentList ) of



		{ MacroState, wooper_method_returns_void } ->
			% This is the normal case:
			MacroState;

		{_MacroState, { wooper_result, UnexpectedResult } } ->
			error_logger:error_msg( "Method ~s:~s/~B, which was called with "
				"parameters ~p, returned a result (~p) whereas, according to "
				"its call, it was expected to be a oneway.~n"
				"Either the oneway implementation is incorrect "
				"or it is a request which is incorrectly "
				"called as a oneway.",
				[ State#state_holder.actual_class, MethodAtom,
				  length( ArgumentList ) + 1, ArgumentList,
				  UnexpectedResult ]  ),
			throw( { oneway_request_mismatch, MethodAtom, ArgumentList } )

	end.



-else.


% Executes the specified oneway.
%
% Returns an updated state.
%
% Note: must not be a macro, as its value would be the one of its first
% statement, not the one of its last. This function is requested to be inlined
% instead.
%
wooper_handle_oneway_execution( MethodAtom, State, ArgumentList ) ->

		{ MacroState, _Result } = wooper_execute_method( MethodAtom, State,
												   ArgumentList ),
		MacroState.


-endif.
