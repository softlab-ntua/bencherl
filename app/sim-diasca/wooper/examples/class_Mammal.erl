% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
%
-module(class_Mammal).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Creature ] ).


% Parameters taken by the constructor ('construct').
% They are here the ones of the mother class (creature) plus fur color:
-define( wooper_construct_parameters, Age, Gender, FurColor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).


% Declarations of class-specific methods (besides inherited ones).
-define(wooper_method_export, setAge/2, isHotBlooded/1, getFurColor/1,
		getArbitraryNumber/1, testExplicitClassSelection/1 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Constructs a new Mammal.
-spec construct( wooper:state(), age(), gender(), fur_color() ) ->
					   wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	CreatureState = class_Creature:construct( State, Age, Gender ),

	{ RequestedState, ActualClass } = executeRequest( CreatureState,
													 getClassName ),

	% Even when constructing a cat, we should see the right class (class_Cat)
	% and not the current class (class_Mammal):
	%
	io:format( "Actual class from constructor: ~s.~n", [ ActualClass ] ),

	setAttribute( RequestedState, fur_color, FurColor ).



% Overriding default destructor: state should be returned, and destructors
% should be called in leaf-to-root order in inheritance tree.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	{ RequestedState, ActualClass } = executeRequest( State,
													 getClassName ),

	% Even when constructing a cat, we should see the right class (class_Cat)
	% and not the current class (class_Mammal):
	io:format( "Actual class from destructor: ~s.~n", [ ActualClass ] ),

	io:format( "Deleting mammal ~w! (overridden destructor)~n", [ self() ] ),

	RequestedState.



% Method implementations.


% Sets correctly the age of this Mammal (not like faulty implementation of the
% Creature mother class).
%
% Overridden from Creature, useful to show the use of executeOneway.
%
% (oneway)
%
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, NewAge ) ->

	% If needing to test the crash of a oneway:
	%A=1,
	%B=2,
	%A=B,

	?wooper_return_state_only( setAttribute( State, age, NewAge ) ).



% All mammals are hot-blooded:
%
% (request)
%
-spec isHotBlooded( wooper:state() ) -> request_return( boolean() ).
isHotBlooded( State ) ->
	?wooper_return_state_result( State, true ).



% Attribute names could be defined in '-define().' header (.hrl) clauses, to
% ensure consistency.
%
% (request)
%
-spec getFurColor( wooper:state() ) -> request_return( fur_color() ).
getFurColor( State ) ->
	?wooper_return_state_result( State, ?getAttr(fur_color) ).



% Returns a class-specific arbitrary number.
% Overridden from Creature, useful to show the use of executeRequest.
%
% (request)
%
-spec getArbitraryNumber( wooper:state() ) -> request_return( number() ).
getArbitraryNumber( State ) ->

	{ RequestedState, ActualClass } = executeRequest( State,
													  getClassName ),

	% Even when constructing a cat, we should see the right class (class_Cat)
	% and not the current class (class_Mammal):
	%
	io:format( "Actual class from non-overridden method: ~s.~n",
			   [ ActualClass ] ),

	% Interesting test for the stack trace, when called from the Mammal test:
	%throw( exception_throw_test_from_request ),
	?wooper_return_state_result( RequestedState, 15 ).



% Allows to test that we can indeed call any version of the implementation of a
% method, not only the latest overridden one.
%
% (oneway)
%
-spec testExplicitClassSelection( wooper:state() ) -> oneway_return().
testExplicitClassSelection( State ) ->

	% Using just executeOneway( State, setAge, 20 ) would call the class_Mammal
	% version, we call the class_Creature version instead, which sets the age to
	% 36 regardless of the specified one:
	%
	NewState = executeOnewayWith( State, class_Creature, setAge, 20 ),

	36 = getAttribute( NewState, age ),

	?wooper_return_state_only( NewState ).
