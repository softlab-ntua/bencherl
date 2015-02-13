% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Reptile class implementation.
%
% See the class_Reptile.erl tested module.
%
-module(class_Reptile_test).


-include("test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Reptile:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
		[ class_Reptile:is_wooper_debug() ] ),


	MyR = class_Reptile:new_link( 1, male ),

	MyR ! { getClassName, [], self() },
	receive

		{ wooper_result, class_Reptile } ->
			test_facilities:display(
				"After constructor, get_class_name returned 'class_Reptile' "
				"as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyR ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [ class_Creature ] } ->
			test_facilities:display(
				"After constructor, getSuperclasses returned class_Creature "
				"as expected." );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 1 } ->
			test_facilities:display(
				"After constructor, getAge returned 1 as expected." );

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyR ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyR ! { setAge, 2 },

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 2 }->
			test_facilities:display(
				"After setAge, getAge returned 2 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyR ! declareBirthday,

	MyR ! { getAge, [], self() },
	receive

		{ wooper_result, 3 }->
			test_facilities:display(
				"After declareBirthday, getAge returned 3 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyR ! declareBirthday,

	MyR ! { isHotBlooded, [], self() },
	receive

		{ wooper_result, false }->
			test_facilities:display(
				"isHotBlooded returned false as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p", [ UnexpectedBlood ] )

	end,

	MyR ! { canMoult, [], self() },
	receive

		{ wooper_result, true }->
			test_facilities:display(
				"canMoult returned true as expected." );

		{ wooper_result, UnexpectedMoultType } ->
			test_facilities:fail( "wrong moult type: ~p",
				[ UnexpectedMoultType ] )

	end,

	case IsDebug of

		true ->

			MyR ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description:~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,

	% To check the result when using a faulty destructor:
	test_facilities:display( "synchronous deletion of the instance." ),

	wooper:delete_synchronously_instance( MyR ),

	test_facilities:stop().
