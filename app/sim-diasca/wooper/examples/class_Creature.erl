% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct'):
-define( wooper_construct_parameters, Age, Gender ).


% Construction-related exported operators:
% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
	synchronous_new/2, synchronous_new_link/2,
	synchronous_timed_new/2, synchronous_timed_new_link/2,
	remote_new/3, remote_new_link/3, remote_synchronous_new/3,
	remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
	remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
	construct/3, delete/1 ).



% Declarations of class-specific methods (besides inherited ones).
% isHotBlooded/1 and canEat/2 are abstract here, hence not mentioned:
-define( wooper_method_export, getAge/1, setAge/2, declareBirthday/1,
	getGender/1, getArbitraryNumber/1, testDirectMethodExecution/2,
	testSingleExecution/1 ).


% Non-method exported functions:
-export([ example_fun/0, toString/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Constructs a new Creature.
-spec construct( wooper_state(), age(), gender() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->
	% No mother class.
	setAttributes(State, [ {age,Age}, {gender,Gender} ] ).



% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	State.



% Method implementations.


% Returns the age of this creature.
-spec getAge( wooper_state() ) -> request_return( age() ).
getAge( State ) ->
	?wooper_return_state_result( State, ?getAttr(age) ).



% Sets the age of this creature.
-spec setAge( wooper_state(), age() ) -> oneway_return().
setAge( State, _NewAge ) ->
	% Mother implementation chosen faulty to check override:
	?wooper_return_state_only( setAttribute( State, age, 36 ) ).



% Increments the age of this creature.
-spec declareBirthday( wooper_state() ) -> oneway_return().
declareBirthday( State ) ->
	?wooper_return_state_only(
		setAttribute( State, age, ?getAttr(age)+1 ) ).



% Returns the gender of this creature.
-spec getGender( wooper_state() ) -> request_return( gender() ).
getGender( State ) ->
	?wooper_return_state_result( State, ?getAttr(gender) ).



% Returns a class-specific arbitrary number.
%
% (request)
-spec getArbitraryNumber( wooper_state() ) -> request_return( number() ).
getArbitraryNumber( State ) ->
	?wooper_return_state_result( State, 10 ).



% Tests direct (synchronous) self-invocation of methods (oneway).
%
% To be called only from a Mammal instance, as there is an hardcoded
% pattern-matching that should work only for a Mammal.
%
% Must not be called from the Creature test, otherwise will fail.
%
% (oneway).
-spec testDirectMethodExecution( wooper_state(), age() ) -> oneway_return().
testDirectMethodExecution( State, NewAge ) ->

	io:format( "Testing executeOneway.~n" ),

	% Note: the version of setAge called in the context of a Creature sets in on
	% purpose to a fixed value (36), regardless of the specified age, whereas
	% the Mammal version of setAge behaves as expected:
	NewState = executeOneway( State, setAge, NewAge ),

	% Use this instead to test error management:
	%NewState = executeOneway(test_not_a_state,setAge,NewAge),
	%NewState = executeOneway(State,42,NewAge),

	% NewAge is expected to be 347:
	NewAge = getAttribute( NewState, age ),

	io:format( "Testing executeRequest.~n" ),
	% 15 from Mammal child classes, not 10 from here:

	{OtherState,15} = executeRequest( NewState, getArbitraryNumber ,[] ),
	%{OtherState,15} = executeRequest(test_not_a_state,getArbitraryNumber,[]),
	%{OtherState,15} = executeRequest(NewState,43,[]),

	io:format( "Direct self-invocation success.~n" ),

	?wooper_return_state_only( OtherState ).



% Allows to test that calling an attribute macro with a state parameter returned
% by a function will trigger that function only once.
%
% Indeed a faulty implementation, due to a macro pitfall, used to make a
% statement like 'setAttribute( f(State), attr, value )' call f/1 twice.
%
% The returned value of the setAttribute call was correct, but any side-effect
% triggered by f (sending a message, writing a trace, etc.) happened twice.
%
% (oneway)
-spec testSingleExecution( wooper_state() ) -> oneway_return().
testSingleExecution( State ) ->
	?wooper_return_state_only( setAttribute( side_effect_function(State),
		age, 10 ) ).



-spec side_effect_function( wooper_state() ) -> wooper_state().
side_effect_function( State ) ->
	io:format( "~n### This message must not be displayed more than once.~n" ),
	State.





% Helper function.


% Just to show it can exist:
-spec example_fun() -> 'ok'.
example_fun() ->
	ok.


% This looks like a method, but it is not (returning only a string):
%
% (function)
-spec toString( wooper_state() ) -> string().
toString( State ) ->
	hashtable:toString( State#state_holder.attribute_table ).
