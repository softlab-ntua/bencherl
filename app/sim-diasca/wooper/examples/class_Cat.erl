% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Cat).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Mammal, class_ViviparousBeing ] ).



% Parameters taken by the constructor ('construct').
%
% They are here the ones of the Mammal mother class (the viviparous being
% constructor does not need any parameter) plus whisker color.
%
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, Age, Gender, FurColor, WhiskerColor ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, delete/1 ).


% Member method declarations.
-define( wooper_method_export, getTeatCount/1, canEat/2, getWhiskerColor/1,
		 toString/1 ).


% Static method declarations.
-define( wooper_static_method_export, get_mean_life_expectancy/0 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").



% Constructs a new Cat.
-spec construct( wooper_state(), age(), gender(), fur_color(), whisker_color() )
			   -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct( MammalState ),

	% Then the class-specific attributes:
	setAttribute( ViviparousMammalState, whisker_color, WhiskerColor ).



-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	io:format( "Deleting cat ~w! (overridden destructor)~n", [self()] ),
	State.



% No guarantee on biological fidelity:
-spec getTeatCount( wooper_state() ) -> request_return( teat_count() ).
getTeatCount( State ) ->
	?wooper_return_state_result( State, 6 ).


% Cats are supposed carnivorous though:
-spec canEat( wooper_state(), food() ) -> request_return( boolean() ).
canEat( State, soup ) ->
	?wooper_return_state_result( State, true );

canEat( State, chocolate ) ->
	?wooper_return_state_result( State, true );

canEat( State, croquette ) ->
	?wooper_return_state_result( State, true );

canEat( State, meat ) ->
	?wooper_return_state_result( State, true );

canEat( State, _OtherFood ) ->
	?wooper_return_state_result( State, false ).


% Const request:
-spec getWhiskerColor( wooper_state() ) -> request_return( color() ).
getWhiskerColor( State )->
	?wooper_return_state_result( State, ?getAttr(whisker_color) ).


-spec toString( wooper_state() ) -> request_return( string() ).
toString( State ) ->

	Description = wooper_instance_toString( State ),

	?wooper_return_state_result( State, Description ).



% Static section.


% Returns the mean life expectancy of a cat, in years.
% (static)
-spec get_mean_life_expectancy() -> age().
get_mean_life_expectancy() ->
	18.
