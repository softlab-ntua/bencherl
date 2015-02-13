% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Cat class implementation.
%
% See the class_Cat.erl tested module.
%
-module(class_Cat_test).


% For run/0 export and al:
-include("test_facilities.hrl" ).


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Cat:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
							[ class_Cat:is_wooper_debug() ] ),

	% General tests.

	MyC = class_Cat:new_link( 3, female, sand, white ),

	MyC ! { getClassName, [], self() },
	receive

		{ wooper_result, class_Cat } ->
			test_facilities:display(
				"After constructor, get_class_name returned 'class_Cat' "
				"as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyC ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, Classes=[ class_Mammal, class_ViviparousBeing ] } ->
			test_facilities:display( "After constructor, get_superclasses "
									 "returned ~p as expected.", [ Classes ] );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,


	% Tests related to Mammals and Creatures.

	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 3 } ->
			test_facilities:display(
				"After constructor, getAge returned 3 as expected." );

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyC ! { getGender, [], self() },
	receive

		{ wooper_result, female } ->
			test_facilities:display(
				"After constructor, getGender returned female as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyC ! { setAge, 5 },

	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 5 }->
			test_facilities:display(
				"After setAge, getAge returned 5 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyC ! declareBirthday,

	MyC ! { getAge, [], self() },
	receive

		{ wooper_result, 6 }->
			test_facilities:display(
				"After declareBirthday, getAge returned 6 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyC ! declareBirthday,

	MyC ! { isHotBlooded, [], self() },
	receive

		{ wooper_result, true }->
			test_facilities:display(
				"isHotBlooded returned true as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p", [ UnexpectedBlood ] )

	end,

	MyC ! { getFurColor, [], self() },
	receive

		{ wooper_result, sand }->
			test_facilities:display(
				"getFurColor returned sand as expected." );

		{ wooper_result, UnexpectedFurColor } ->
			test_facilities:fail( "wrong fur color: ~p",
								 [ UnexpectedFurColor ] )

	end,


	% Tests related to ViviparousBeings.

	MyC ! { getMeanChildrenCount, [], self() },
	receive

		{ wooper_result, 4 } ->
			test_facilities:display(
				"After constructor, getMeanChildrenCount returned 4 "
				"as expected." );

		{ wooper_result, UnexpectedMeanCount } ->
			test_facilities:fail( "wrong mean children count: ~p",
				[ UnexpectedMeanCount ] )

	end,

	MyC ! { getBirthGivenCount, [], self() },
	receive

		{ wooper_result, 0 } ->
			test_facilities:display(
				"After constructor, getBirthGivenCount returned 0 "
				"as expected." );

		{ wooper_result, UnexpectedFirstCount } ->
			test_facilities:fail( "wrong first children count: ~p",
				[ UnexpectedFirstCount ] )

	end,

	MyC ! { giveBirth, 5 },

	MyC ! { getBirthGivenCount, [], self() },
	receive

		{ wooper_result, 5 }->
			test_facilities:display(
				"After giveBirth, getBirthGivenCount returned 5 "
				"as expected." );

		{ wooper_result, UnexpectedSecondCount } ->
			test_facilities:fail( "wrong second children count: ~p",
				[ UnexpectedSecondCount ] )

	end,


	% Tests related to Cats.

	MyC ! { getTeatCount, [], self() },
	receive

		{ wooper_result, 6 }->
			test_facilities:display( "getTeatCount returned 6 as expected." );

		{ wooper_result, UnexpectedTeatCount } ->
			test_facilities:fail( "wrong teat count: ~p",
								 [ UnexpectedTeatCount ] )

	end,

	MyC ! { canEat, soup, self() },
	receive

		{ wooper_result, true }->
			test_facilities:display( "This cat can eat soup, as expected." );

		{ wooper_result, UnexpectedFoodPreference } ->
			test_facilities:fail( "wrong food preference: ~p",
				[ UnexpectedFoodPreference ] )

	end,

	MyC ! { canEat, tangerine, self() },
	receive

		{ wooper_result, false }->
			test_facilities:display(
				"This cat cannot eat tangerine, as expected." );

		{ wooper_result, UnexpectedOtherFoodPreference } ->
			test_facilities:fail( "wrong food preference: ~p",
				[ UnexpectedOtherFoodPreference ] )

	end,

	MyC ! { getWhiskerColor, [], self() },
	receive

		{ wooper_result, white }->
			test_facilities:display(
				"This cat has white whiskers, as expected." );

		{ wooper_result, UnexpectedWhiskerColor } ->
			test_facilities:fail( "wrong whisker color: ~p",
				[ UnexpectedWhiskerColor ] );

		UnexpectedReturn ->
			test_facilities:fail( "unexpected method return: ~p",
				[ UnexpectedReturn ] )

	end,

	case IsDebug of

		true ->

			MyC ! { wooper_get_instance_description,[], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,

	% Some waiting could be needed in cases where the interpreter is to stop
	% immediately afterwards, so that the actions performed in the destructor
	% can be performed:
	%
	MyC ! delete,

	MyOtherC = class_Cat:new_link( 3, male, black, white ),

	% No race condition with the end of this test process:
	wooper:delete_synchronously_instance( MyOtherC ),

	test_facilities:display( "This cat could be created and "
							 "be synchronously deleted, as expected." ),

	test_facilities:stop().
