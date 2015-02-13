% Modular WOOPER header gathering the facilities for instance destruction.


% Debug or not:
-spec wooper_destruct( wooper:state() ) -> wooper:state().


-ifdef(wooper_debug).


% Calls recursively the destructors through the inheritance tree.
%
% Each destructor (destruct/1 function) is purely local to the current module.
%
% Initial specified state is always valid (comes from the main loop), but states
% returned by user-defined destructors must be checked in debug mode.
%
wooper_destruct( State ) ->

	% If a class-specific destruct/1 is defined, executes it, otherwise does
	% nothing.
	%
	% Then recurses with higher-level destructors (maybe just storing destruct/1
	% in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_for):

	% We should never rely on 'wooper:get_class_name( State )' here, as it would
	% always return the leaf class. We use ?MODULE (even for embodied
	% instances):
	%
	Exports = module_info( exports ),

	%io:format( "**** Deleting ~w (destructor for class ~w/~w).~n",
	%		   [ self(), ?MODULE, wooper:get_class_name( State ) ] ),

	DestructedState = case lists:member( { destruct, 1 }, Exports ) of

		true ->

			% All destructors, including user-defined ones, must return a
			% (possibly updated) state:

			%io:format( "Deleting ~w (overridden destructor for ~w).~n",
			%		   [ self(), ?MODULE ] ),


			% ?MODULE is always the real class name here:
			%
			%try apply( ActualClassname, destruct, [ State ] ) of
			try ?MODULE:destruct( State ) of

				ReturnedState when is_record( ReturnedState, state_holder ) ->
					ReturnedState;

				Other ->

					error_logger:error_msg(
						"~nWOOPER error for PID ~w of class ~s: "
						"user-defined destructor did not return a state, "
						"but returned '~p' instead.~n",
						[ self(), ?MODULE, Other ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?wooper_error_display_waiting ),
					throw( { invalid_destructor, ?MODULE } )

			catch

				Reason:ErrorTerm ->
					trigger_destruct_error( Reason, ErrorTerm, State )

			end;

		false ->

			% Destructor not overridden, using default one:

			%io:format( "Deleting ~w (default do-nothing destructor "
			%		   "for class ~w).~n", [ self(), ?MODULE ] ),

			% State unchanged here:
			State

	end,

	chain_parent_destructors( DestructedState ).



-else. % if wooper_debug



% Calls recursively the destructors through the inheritance tree.
%
% Each destructor (destruct/1 function) is purely local to the current module.
%
wooper_destruct( State ) ->

	% If a class-specific destruct is defined, executes it, otherwise does
	% nothing.
	%
	% Then recurses with higher-level destructors (maybe just storing destruct/1
	% in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_for):
	%
	DestructedState = case lists:member( { destruct, 1 },
										 module_info( exports ) ) of

		true ->

			% All destructors, included user-defined ones, must return a
			% (possibly updated) state:
			%

			try

				%apply( ?MODULE, destruct, [ State ] )
				?MODULE:destruct( State )

			catch

				Reason:ErrorTerm ->
					trigger_destruct_error( Reason, ErrorTerm, State )

			end;

		false ->

			% Destructor not overridden, using default one:

			%io:format( "Deleting ~w (default do-nothing destructor "
			%   "for class ~w).~n", [ self(), ?MODULE ] )

			% State unchanged:
			State

	end,

	chain_parent_destructors( DestructedState ).


-endif. % wooper_debug




% Triggers a destruction-related error.
%
% (helper)
%
-spec trigger_destruct_error( 'throw' | 'exit' | 'error', term(),
							  wooper:state() ) -> no_return().
trigger_destruct_error( Reason, ErrorTerm, State ) ->

	% Destruction failed:
	% (error term would often be unreadable with ~p)

	ActualClassname = wooper:get_class_name( State ),

	error_logger:error_msg( "~nWOOPER error for PID ~w, "
							"destructor (~s:destruct/1) failed (cause: ~p):~n~n"
							" - with error term:~n~p~n~n"
							" - stack trace was (latest calls first):~n~p~n~n"
							" - instance state was: ~s~n~n",
							[ self(), ActualClassname, Reason, ErrorTerm,
							  erlang:get_stacktrace(),
							  wooper:state_to_string( State )
							] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?wooper_error_display_waiting ),

	% Terminates the process:
	throw( { wooper_destructor_failed, self(), ActualClassname, ErrorTerm } ).



% Calls recursively the destructor of all direct superclasses.
%
% (helper)
%
-spec chain_parent_destructors( wooper:state() ) -> wooper:state().
chain_parent_destructors( State ) ->

	% Then automatically call the direct mother destructors.
	%
	% Using foldr, not foldl: the destructors of mother classes are called in
	% the reverse order compared to the order that was used for construction,
	% for the sake of symmetry.
	%
	% The final state is returned.
	%
	% This would be incorrect, as it would return the superclasses of the
	% current instance, never recursing back in the inheritance graph:
	%
	%{ _State, Superclasses } = executeRequest( State, getSuperclasses ),

	Superclasses = get_superclasses(),

	lists:foldr(

		fun( Class, NewState ) ->

				% More efficient than using apply/3:
				Class:wooper_destruct( NewState )

		end,

		_InitialAcc=State,

		_List=Superclasses ).
