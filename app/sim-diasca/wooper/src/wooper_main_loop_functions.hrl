% Modular WOOPER header gathering the main loop of instances
% (wooper_main_loop/1).


% Implies wooper_defines.hrl:
%-include("wooper_execute.hrl").


% For wooper_destruct/1:
%-include("wooper_destruction_functions.hrl").



% No closure nor export needed for wooper_main_loop, as the various spawns are
% done based on the wooper_construct_and_run* functions, which are not exported,
% and are executed thanks to a closure.
%
% Note that, in the definition of this closure, self() should not be used,
% otherwise this will correspond to the PID of the spawned process, and not to
% the one of the creating one (hence CreatorPid = self() which is defined
% outside of these closures).



% Waits for incoming method calls and serves them.
%
-spec wooper_main_loop( wooper:state() ) -> 'deleted'. % no_return().
wooper_main_loop( State ) ->

	%?wooper_log( "wooper_main_loop start.~n" ),

	% Commented-out to avoid the state display prior to each method call:
	% wooper:display_state( State )

	receive

		% Requests (thus with response):

		% Instance PID could be sent back as well to discriminate received
		% answers on the caller side.
		{ MethodAtom, ArgumentList, CallerPid }
				when is_pid( CallerPid ) and is_list( ArgumentList ) ->

			?wooper_log_format( "Main loop (case A) for ~w: "
				"request '~s' with argument list ~w for ~w.~n",
				[ self(), MethodAtom, ArgumentList, CallerPid ] ),

			SenderAwareState = State#state_holder{ request_sender=CallerPid },

			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, ArgumentList ),
			%CallerPid ! { self(), Result }
			CallerPid ! Result,

			% Force a crash if instance-side error detected:
			case element( 1, Result ) of

				wooper_method_failed ->
					throw( Result ) ;

				wooper_method_not_found ->
					throw( Result ) ;

				% wooper_result:
				_ ->
					ok

			end,
			SenderAgnosticState =
				NewState#state_holder{ request_sender=undefined },

			%?wooper_log( "Main loop (case A) ended.~n" ),

			wooper_main_loop( SenderAgnosticState );



		% Auto-wrapping single arguments implies putting lists between
		% double-brackets:
		{ MethodAtom, Argument, CallerPid } when is_pid( CallerPid ) ->

			?wooper_log_format( "Main loop (case B) for ~w: "
				"request '~s' with argument ~w for ~w.~n",
				[ self(), MethodAtom, Argument, CallerPid ] ),

			SenderAwareState = State#state_holder{ request_sender=CallerPid },

			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, [ Argument ] ),

			%CallerPid ! { self(), Result }
			CallerPid ! Result,

			% Forces a crash if instance-side error detected:
			case element( 1, Result ) of

				wooper_method_failed ->
					throw( Result ) ;

				wooper_method_not_found ->
					throw( Result ) ;

				% wooper_result:
				_ ->
					ok

			end,
			SenderAgnosticState =
				NewState#state_holder{ request_sender=undefined },

			%?wooper_log( "Main loop (case B) ended.~n" ),

			wooper_main_loop( SenderAgnosticState );



		% Oneway calls (no caller PID sent, no answer sent back).
		%
		% We check though that indeed no value is returned, by pattern-matching
		% against wooper_method_returns_void, which could be removed if not in
		% debug mode.
		%
		% (if no pattern-matching was done, then either this method would not
		% return anything, or the sender would not be interested in the result)
		{ MethodAtom, ArgumentList } when is_list( ArgumentList ) ->

			?wooper_log_format( "Main loop (case C) for ~w: "
				"oneway '~s' with argument list ~w.~n",
				[ self(), MethodAtom, ArgumentList ] ),

			NewState = wooper_handle_oneway_execution( MethodAtom, State,
												ArgumentList ),

			%?wooper_log( "Main loop (case C) ended.~n" ),

			wooper_main_loop( NewState );



		{ synchronous_delete, CallerPid } ->

			?wooper_log( "Main loop: oneway synchronous delete.~n" ),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			%
			wooper_destruct( State ),
			CallerPid ! { deleted, self() },
			deleted;
			% (do nothing, loop ends here).



		% ping is always available and cannot be overridden:
		{ ping, CallerPid } ->

			?wooper_log_format( "Main loop (case D) for ~w: oneway ping.~n",
				[ self() ] ),

			CallerPid ! { pong, self() },

			%?wooper_log( "Main loop (case D) ended.~n" ),
			wooper_main_loop( State );



		% Oneway with parameters:
		{ MethodAtom, Argument } ->

			?wooper_log_format( "Main loop (case E) for ~w: "
				"oneway '~s' with argument ~w.~n",
				[ self(), MethodAtom, Argument ] ),

			NewState = wooper_handle_oneway_execution( MethodAtom, State,
											[ Argument ] ),

			%?wooper_log( "Main loop (case E) ended.~n" ),

			wooper_main_loop( NewState );



		delete ->

			?wooper_log( "Main loop: oneway delete.~n" ),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			wooper_destruct( State ),
			deleted;
			% (do nothing, loop ends here).



		MethodAtom when is_atom( MethodAtom ) ->

			?wooper_log_format(
				"Main loop (case F) for ~w: oneway from atom ~s.~n",
				[ self(), MethodAtom ] ),

			% Any result should be ignored, only the updated state is kept:
			NewState = wooper_handle_oneway_execution( MethodAtom, State,
												_ArgumentList=[] ),

			%?wooper_log( "Main loop (case F) ended.~n" ),
			wooper_main_loop( NewState );



		{ 'EXIT', Pid, ExitType } when is_pid( Pid ) ->

			?wooper_log_format( "Main loop (case G) for ~w: exit with ~w.~n",
				[ self(), { Pid, ExitType } ] ),

			case ?wooper_hashtable_type:lookupEntry(
					{ _Name=onWOOPERExitReceived, _Arity=3 },
					State#state_holder.virtual_table ) of

				{ value, _Key } ->

					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule, onWOOPERExitReceived,...)':

					% Will thus call 'onWOOPERExitReceived( State, Pid, ExitType
					% )', where ExitType is typically a stack trace:

					{ NewState, _ } = wooper_execute_method(
						onWOOPERExitReceived, State, [ Pid, ExitType ] ),

					%?wooper_log( "Main loop (case G) ended.~n" ),
					wooper_main_loop( NewState );

				% Hashtable key not found:
				_ ->

					% EXIT handler not overridden, using default one:
					%?wooper_log( "Main loop (case G) ended.~n" ),
					NewState = wooper:default_exit_handler( State, Pid,
															ExitType ),
					wooper_main_loop( NewState )

			end;



		Other ->

			% Catch-all:
			?wooper_log_format( "Main loop (case H) for ~w: unmatched ~p.~n",
				[ self(), Other ] ),

			error_logger:error_msg(
			  "WOOPER ignored following message for instance ~w:~n~p.~n",
			  [ self(), Other ] ),

			%?wooper_log( "Main loop (case H) ended.~n" ),
			throw( { erroneous_call, Other } )


	end.

	% Commented out to preserve (presumably) tail-recursion:
	% io:format( "wooper_main_loop exited.~n" ).
