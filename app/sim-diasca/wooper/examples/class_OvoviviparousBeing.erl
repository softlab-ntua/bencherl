% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


-module(class_OvoviviparousBeing).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).



% Declaring all variations of WOOPER standard life-cycle operations:
%
-define( wooper_construct_export, new/0, new_link/0,
		 synchronous_new/0, synchronous_new_link/0,
		 synchronous_timed_new/0, synchronous_timed_new_link/0,
		 remote_new/1, remote_new_link/1, remote_synchronous_new/1,
		 remote_synchronous_new_link/1, remote_synchronisable_new_link/1,
		 remote_synchronous_timed_new/1, remote_synchronous_timed_new_link/1,
		 construct/1, destruct/1 ).



% Declarations of class-specific methods (besides inherited ones).
-define( wooper_method_export, getMeanEggsCount/1, getEggsLaidCount/1,
		layEggs/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Import common types without module prefix:
-include("ecosystem_types.hrl").


% Constructs a new Ovoviviparous being (parameter-less constructor).
-spec construct( wooper:state() ) -> wooper:state() .
construct( State ) ->

	% In order to test the crash of a constructor: non_existing:crash(),

	setAttribute( State, eggs_count, 0 ).



% This useless destructor overriding was made to silence Dialyzer (which is not
% able to determine that this function will never be called, as WOOPER performs
% the appropriate test is made beforehand):
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% In order to test the crash of a destructor: non_existing:crash(),

	State.


% Method implementations.


% Let's say an average means something here:
% (this ought to be a static method, as it does not depend on a state)
-spec getMeanEggsCount( wooper:state() ) -> request_return( egg_count() ).
getMeanEggsCount( State ) ->
	?wooper_return_state_result( State, 1000 ).


% Returns the number of eggs this ovoviviparous laid:
-spec getEggsLaidCount( wooper:state() ) -> request_return( egg_count() ).
getEggsLaidCount( State ) ->
	?wooper_return_state_result( State, ?getAttr(eggs_count) ).


% Increases the number of eggs that this ovoviviparous being laid:
-spec layEggs( wooper:state(), egg_count() ) -> oneway_return().
layEggs( State, NumberOfNewEggs ) ->

	NewEggCount = ?getAttr(eggs_count) + NumberOfNewEggs,

	?wooper_return_state_only(
	   setAttribute( State, eggs_count, NewEggCount ) ).
