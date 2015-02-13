% Modular WOOPER header gathering the class-related primitives (function
% definitions).


% Method that returns the classname of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
% (const request)
%
-spec getClassName( wooper:state() ) -> request_return( class_name() ).
getClassName( State ) ->
	?wooper_return_state_result( State, State#state_holder.actual_class ).



% "Static method" (only a function) which returns the list of the superclasses
% for that class.
%
% Not to be called by the user, see get_superclasses/1 instead.
%
-spec get_superclasses() -> [ class_name() ].
get_superclasses() ->
	?wooper_superclasses.



% Method that returns the (direct) superclasses of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
% (const request)
%
-spec getSuperclasses( wooper:state() ) -> request_return( [ class_name() ] ).
getSuperclasses( State ) ->
	ActualModule = State#state_holder.actual_class,
	SuperClasses = apply( ActualModule, get_superclasses, [] ),
	%?wooper_return_state_result( State, ?wooper_superclasses ).
	?wooper_return_state_result( State, SuperClasses ).



-ifdef(wooper_debug).


% Returns a full textual description of this instance, including its state and
% virtual table.
%
% This is a method for debug purpose, only activated if wooper_debug is defined.
%
% (const request)
%
wooper_get_instance_description( State ) ->
	?wooper_return_state_result( State, wooper:instance_to_string( State ) ).


-endif. % wooper_debug
