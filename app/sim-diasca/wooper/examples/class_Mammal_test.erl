% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Mammal class implementation.
%
% See the class_Mammal.erl tested module.
%
-module(class_Mammal_test).


-include("test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Mammal:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
		[ class_Mammal:is_wooper_debug() ] ),


	MyM = class_Mammal:synchronous_new_link( 30, male, brown ),
	MyM ! { getClassName, [], self() },
	receive

		{ wooper_result, class_Mammal } ->
			test_facilities:display(
				"After constructor, get_class_name returned 'class_Mammal' "
				"as expected.");

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyM ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [ class_Creature ] } ->
			test_facilities:display(
				"After constructor, getSuperclasses returned: class_Creature "
				"as expected.");

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,

	MyM ! { getAge, [], self() },
	receive

		{ wooper_result, 30 } ->
			test_facilities:display(
				"After constructor, getAge returned 30 as expected.");

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyM ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyM ! { setAge, 5 },

	MyM ! { getAge, [], self() },
	receive

		 { wooper_result, 5 }->
			test_facilities:display(
				"After setAge, getAge returned 5 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyM ! declareBirthday,

	MyM ! { getAge, [], self() },
	receive

		 { wooper_result, 6 }->
			test_facilities:display(
				"After declareBirthday, getAge returned 6 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyM ! declareBirthday,

	MyM ! { isHotBlooded, [], self() },
	receive

		 { wooper_result, true }->
			test_facilities:display(
				"isHotBlooded returned true as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p",
				[ UnexpectedBlood ] )

	end,

	% Not too late in the test to have enough time to execute fully:
	test_facilities:display( "Testing direct method invocation." ),

	% Inherited from Creature:
	MyM ! { testDirectMethodExecution, 347 },

	MyM ! testExplicitClassSelection,

	MyM ! { getFurColor, [], self() },
	receive

		 { wooper_result, brown }->
			test_facilities:display(
				"getFurColor returned brown as expected." );

		{ wooper_result, UnexpectedFurColor } ->
			test_facilities:fail( "wrong fur color: ~p",
				[ UnexpectedFurColor ] )

	end,


	case IsDebug of

		true ->

			MyM ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,


	wooper:delete_synchronously_instance( MyM ),

	test_facilities:stop().
