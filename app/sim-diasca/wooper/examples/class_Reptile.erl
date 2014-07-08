% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_Reptile).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Creature ] ).


% Parameters taken by the constructor ('construct').
% They are here the ones of the mother class (creature):
-define( wooper_construct_parameters, Age, Gender ).


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
-define( wooper_method_export, setAge/2, isHotBlooded/1, canMoult/1 ).


% With this class, we will test serialisation hooks:
-define( wooper_serialisation_hooks,).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Triggered just before serialisation.
%
-spec pre_serialise_hook( wooper_state() ) -> wooper_state().
pre_serialise_hook( State ) ->
	io:format( "Pre-serialising a reptile!~n" ),
	State.



% Triggered just after serialisation.
%
% (using WOOPER default hook implementation augmented of an io:format)
%
-spec post_serialise_hook( class_name(), term_serialisation(), wooper_state() )
						 -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	io:format( "Post-serialising a reptile!~n" ),
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  term_serialisation().
pre_deserialise_hook( _SerialisedEntries={ _Classname, Entries }, _UserData ) ->
	io:format( "Pre-deserialising a reptile!~n" ),
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper_state() ) -> wooper_state().
post_deserialise_hook( State ) ->
	io:format( "Post-deserialising a reptile!~n" ),
	State.




% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Constructs a new Reptile.
-spec construct( wooper_state(), age(), gender() ) -> wooper_state().
construct( State, ?wooper_construct_parameters ) ->
	class_Creature:construct( State, Age, Gender ).
	% To test constructor checking:
	%an_unexpected_initial_state.


% Overridden destructor
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->
	io:format( "Deleting a Reptile." ),
	State.
	% To test destructor checking use instead:
	%an_unexpected_final_state.



% Method implementations.


% Sets correctly the age of this Mammal (not like faulty implementation of the
% Creature mother class).
%
% Overridden from Creature, useful to show the use of executeOneway.
% Note: used to test WOOPER management of error conditions.
%
% (oneway)
%
-spec setAge( wooper_state(), age() ) -> oneway_return().
setAge( State, NewAge ) ->
	%throw( exception_throw_test_from_oneway ),
	%exit( exception_exit_test_from_oneway ),
	?wooper_return_state_only( setAttribute( State, age, NewAge ) ).



% All reptiles are cold-blooded.
%
% Note: used to test WOOPER management of error conditions.
%
% (const request)
%
-spec isHotBlooded( wooper_state() ) -> request_return( boolean() ).
isHotBlooded( State ) ->
	%throw( exception_throw_test_from_request ),
	%exit( exception_exit_test_from_request ),
	?wooper_return_state_result( State, false ).


% All reptiles can moult:
%
% (const request)
%
-spec canMoult( wooper_state() ) -> request_return( boolean() ).
canMoult( State ) ->
	?wooper_return_state_result( State, true ).
