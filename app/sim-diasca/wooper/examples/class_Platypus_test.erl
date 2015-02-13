% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Unit tests for the Platypus class implementation.
%
% See the class_Platypus.erl tested module.
%
-module(class_Platypus_test).


-include("test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_Platypus:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	test_facilities:display("Debug mode: ~s.",
		[ class_Platypus:is_wooper_debug() ] ),


	% General tests.


	MyP = class_Platypus:new_link( 4, male, brown, black ),

	MyP ! { getClassName, [], self() },
	receive

		{ wooper_result, class_Platypus } ->
			test_facilities:display(
				"After constructor, get_class_name returned 'class_Platypus' "
				"as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyP ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [ class_Mammal, class_OvoviviparousBeing ] } ->
			test_facilities:display(
				"After constructor, getSuperclasses returned "
				"class_Creature and class_OvoviviparousBeing as expected." );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,


	% Tests related to Mammals and Creatures.

	MyP ! { getAge, [], self() },
	receive

		{ wooper_result, 4 } ->
			test_facilities:display(
				"After constructor, getAge returned 4 as expected." );

		{ wooper_result, UnexpectedAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedAge ] )

	end,

	MyP ! { getGender, [], self() },
	receive

		{ wooper_result, male } ->
			test_facilities:display(
				"After constructor, getGender returned male as expected." );

		{ wooper_result, UnexpectedGender } ->
			test_facilities:fail( "wrong gender: ~p", [ UnexpectedGender ] )

	end,

	MyP ! { setAge, 5 },

	MyP ! { getAge, [], self() },
	receive

		{ wooper_result, 5 } ->
			test_facilities:display(
							  "After setAge, getAge returned 5 as expected." );

		{ wooper_result, UnexpectedNewAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedNewAge ] )

	end,

	MyP ! declareBirthday,

	MyP ! { getAge, [], self() },
	receive

		{ wooper_result, 6 } ->
			test_facilities:display(
			   "After declareBirthday, getAge returned 6 as expected." );

		{ wooper_result, UnexpectedLastAge } ->
			test_facilities:fail( "wrong age: ~p", [ UnexpectedLastAge ] )

	end,

	MyP ! declareBirthday,

	MyP ! { isHotBlooded, [], self() },
	receive

		{ wooper_result, true } ->
			test_facilities:display(
						"isHotBlooded returned true as expected." );

		{ wooper_result, UnexpectedBlood } ->
			test_facilities:fail( "wrong blood type: ~p", [ UnexpectedBlood ] )

	end,

	MyP ! { getFurColor, [], self() },
	receive

		{ wooper_result, brown } ->
			test_facilities:display(
				"getFurColor returned brown as expected." );

		{ wooper_result, UnexpectedFurColor } ->
			test_facilities:fail( "wrong fur color: ~p",
								 [ UnexpectedFurColor ] )

	end,


	% Tests related to OvoviviparousBeings.

	MyP ! { getMeanEggsCount, [], self() },
	receive

		{ wooper_result, 2 } ->
			test_facilities:display(
				"After constructor, getMeanEggsCount returned 2 as expected." );

		{ wooper_result, UnexpectedMeanCount } ->
			test_facilities:fail( "wrong mean egg count: ~p",
				[ UnexpectedMeanCount ] )


	end,

	MyP ! { getEggsLaidCount, [], self() },
	receive

		{ wooper_result, 0 } ->
			test_facilities:display(
				"After constructor, getEggsLaidCount returned 0 "
				"as expected." );

		{ wooper_result, UnexpectedFirstCount } ->
			test_facilities:fail( "wrong first egg count: ~p",
				[ UnexpectedFirstCount ] )

	end,

	MyP ! { layEggs, 1 },

	MyP ! { getEggsLaidCount, [], self() },
	receive

		{ wooper_result, 1 } ->
			test_facilities:display(
				"After giveBirth, getEggsLaidCount returned 1 "
				"as expected." );

		{ wooper_result, UnexpectedSecondCount } ->
			test_facilities:fail( "wrong second egg count: ~p",
				[ UnexpectedSecondCount ] )

	end,


	% Tests related to Platypuses.

	MyP ! { getTeatCount, [], self() },
	receive

		{ wooper_result, 0 } ->
			test_facilities:display( "getTeatCount returned 0 as expected." );

		{ wooper_result, UnexpectedTeatCount } ->
			test_facilities:fail( "wrong teat count: ~p",
				[ UnexpectedTeatCount ] )

	end,

	MyP ! { canEat, weed, self() },
	receive

		{ wooper_result, true } ->
			test_facilities:display(
				"This Platypus can eat weed, as expected." );

		{ wooper_result, UnexpectedFoodPreference } ->
			test_facilities:fail( "wrong food preference: ~p",
				[ UnexpectedFoodPreference ] )

	end,

	MyP ! { canEat, mammoth, self() },
	receive

		{ wooper_result, false } ->
			test_facilities:display(
				"This Platypus cannot eat mammoth, as expected." );

		{ wooper_result, UnexpectedOtherFoodPreference } ->
			test_facilities:fail( "wrong food preference: ~p",
				[ UnexpectedOtherFoodPreference ] )

	end,

	MyP ! { getNozzleColor, [], self() },
	receive

		 { wooper_result, black } ->
			test_facilities:display(
				"This Platypus has a black nozzle, as expected." );

		{ wooper_result, UnexpectedNozzleColor } ->
			test_facilities:fail( "wrong nozzle color: ~p",
				[ UnexpectedNozzleColor ] )

	end,

	MyP ! { getAlternateNames, [], self() },
	receive

		 { wooper_result,  [hector,edgar,roger,sean]  } ->
			test_facilities:display(
				"This Platypus has the right alternate names: ~w.",
				[ [hector,edgar,roger,sean] ] )

	end,

	MyP ! { popFirstAlternateName, [], self() },
	receive

		 { wooper_result, FirstName } ->
			test_facilities:display(
				"This Platypus forgot its first alternate name: ~w.",
					[ FirstName ] )

	end,

	MyP ! { getAlternateNames, [], self() },
	receive

		 { wooper_result, [edgar,roger,sean] } ->
			test_facilities:display(
				"Finally this Platypus has the right alternate names: ~w.",
					[ [ edgar, roger, sean ] ] )

	end,


	test_facilities:display( "Testing now synchronous operations." ),

	MySyncP = class_Platypus:synchronous_new_link( 3, female, violet, grey ),

	% Oneway tested before a request to ensure synchronicity:
	MySyncP ! testCreationDeletion,

	MySyncP ! { getNozzleColor, [], self() },
	receive

		{ wooper_result, grey } ->
			test_facilities:display(
				"This synchronous Platypus has a grey nozzle, as expected." );

		{ wooper_result, UnexpectedSyncNozzleColor } ->
			test_facilities:fail( "wrong nozzle color: ~p",
				[ UnexpectedSyncNozzleColor ] )

	end,

	case IsDebug of

		true ->

			MyP ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,

	wooper:delete_synchronously_instance( MyP ),

	test_facilities:stop().
