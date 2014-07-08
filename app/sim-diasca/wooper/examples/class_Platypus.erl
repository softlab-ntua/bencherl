% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Platypus).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[ class_Mammal, class_OvoviviparousBeing ]).


% Parameters taken by the constructor ('construct').
%
% They are here the ones of the Mammal mother class (the ovoviviparous being
% constructor does not need any parameter) plus nozzle color.
%
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, Age, Gender, FurColor, NozzleColor ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, delete/1 ).


% Method declarations.
-define( wooper_method_export, getMeanEggsCount/1, getTeatCount/1, canEat/2,
		 getNozzleColor/1, getAlternateNames/1, popFirstAlternateName/1,
		 testCreationDeletion/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").




% Constructs a new Platypus.
%
-spec construct( wooper_state(), age(), gender(), fur_color(), nozzle_color() )
			   -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),

	OvoviviparousMammalState = class_OvoviviparousBeing:construct(
		MammalState ),

	io:format( "Synchronous time-out is ~p.~n", [ ?synchronous_time_out] ),

	% Then the class-specific attributes:
	setAttributes( OvoviviparousMammalState, [
		  { nozzle_color, NozzleColor },
		  { alternate_names, [ hector, edgar, roger, sean ] },
		  { cat_pid, undefined }

		 ] ).



% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	State.



-spec getMeanEggsCount( wooper_state() ) -> request_return( egg_count() ).
getMeanEggsCount( State ) ->
	?wooper_return_state_result( State, 2 ).



% Returns the number of teats a platypus has.
%
% (request)
%
% It is a mammal, though!
%
-spec getTeatCount( wooper_state() ) -> request_return( teat_count() ).
getTeatCount( State ) ->
	?wooper_return_state_result( State, 0 ).



% Tells whether this platypus can eat specified food.
%
% (request)
%
% Platypuses are supposed carnivorous though:
-spec canEat( wooper_state(), food() ) -> request_return( boolean() ).
canEat( State, leaf ) ->
	?wooper_return_state_result( State, true );

canEat( State,chocolate ) ->
	?wooper_return_state_result( State, true );

canEat( State,weed ) ->
	?wooper_return_state_result( State, true );

canEat( State,fish ) ->
	?wooper_return_state_result( State, true );

canEat( State, _OtherFood ) ->
	?wooper_return_state_result( State, false ).



% Returns the color of the nozzle of this platypus.
%
% (request)
%
-spec getNozzleColor( wooper_state() ) -> request_return( nozzle_color() ).
getNozzleColor( State )->

	% If needing to test the crash of a request:
	%A=1,
	%B=2,
	%A=B,

	?wooper_return_state_result( State, getAttribute( State, nozzle_color ) ).



% Returns the list of alternate names for this platypus.
%
% (request)
%
-spec getAlternateNames( wooper_state() ) -> request_return( [atom()] ).
getAlternateNames( State ) ->
	?wooper_return_state_result( State, ?getAttr(alternate_names) ).



% Returns the first alternate name for this platypus and forget it.
%
% (request)
%
-spec popFirstAlternateName( wooper_state() ) -> request_return( atom() ).
popFirstAlternateName( State ) ->
	{ NewState, Name } = popFromAttribute( State, alternate_names ),
	?wooper_return_state_result( NewState, Name ).



% Allows to test the creation and deletion of other WOOPER instances.
%
% (oneway)
%
testCreationDeletion( State ) ->

	% Initially do-nothing:
	FirstState = delete_synchronously_any_instance_referenced_in( [],
																  State ),

	CatPid = class_Cat:synchronous_new_link( _Age=1, _Gender=male,
								_FurColor=pink, _WhiskerColor=black ),

	io:format( "Cat ~p created from platypus.~n", [ CatPid ] ),

	CatState = setAttribute( FirstState, cat_pid, CatPid ),

	io:format( "Deleting cat ~p created from platypus.~n", [ CatPid ] ),

	DeleteState = delete_synchronously_any_instance_referenced_in( [ cat_pid ],
																   CatState ),

	undefined = getAttribute( DeleteState, cat_pid ),

	?wooper_return_state_only( DeleteState ).
