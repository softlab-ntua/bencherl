% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the OvoviviparousBeing class implementation.
% See the class_OvoviviparousBeing.erl tested module.

-module(class_OvoviviparousBeing_test).


-include("test_facilities.hrl").


-export([ run/1 ]).


-spec run() -> no_return().
run() ->
	run( class_OvoviviparousBeing:is_wooper_debug() ).


-spec run( boolean() ) -> no_return().
run( IsDebug ) ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
		[ class_OvoviviparousBeing:is_wooper_debug() ] ),


	MyV = class_OvoviviparousBeing:synchronous_new(),

	MyV ! { getClassName, [], self() },
	receive

		{ wooper_result, class_OvoviviparousBeing } ->
			test_facilities:display(
				"After constructor, get_class_name returned "
				"'class_OvoviviparousBeing' as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyV ! { getSuperclasses, [], self() },
	receive

		{ wooper_result, [] } ->
			test_facilities:display(
				"After constructor, get_superclasses returned [] "
				"as expected." );

		{ wooper_result, UnexpectedSuperclasses } ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,

	MyV ! { getMeanEggsCount, [], self() },
	receive

		{ wooper_result, 1000 } ->
			test_facilities:display(
				"After constructor, getMeanEggsCount returned 1000 "
				"as expected." );

		{ wooper_result, UnexpectedMeanCount } ->
			test_facilities:fail( "wrong mean egg count: ~p",
				[ UnexpectedMeanCount ] )


	end,

	MyV ! { getEggsLaidCount, [], self() },
	receive

		{ wooper_result, 0 } ->
			test_facilities:display(
				"After constructor, getEggsLaidCount returned 0 "
				"as expected." );

		{ wooper_result, UnexpectedFirstCount } ->
			test_facilities:fail( "wrong first egg count: ~p",
				[ UnexpectedFirstCount ] )

	end,

	MyV ! { layEggs, 747 },

	MyV ! { getEggsLaidCount, [], self() },
	receive

		{ wooper_result, 747 }->
			test_facilities:display(
				"After giveBirth, getEggsLaidCount returned 747 "
				"as expected." );

		{ wooper_result, UnexpectedSecondCount } ->
			test_facilities:fail( "wrong second egg count: ~p",
				[ UnexpectedSecondCount ] )

	end,

	case IsDebug of

		true ->
			MyV ! { wooper_get_instance_description, [], self() },
			receive

				{ wooper_result, InspectString } ->
					test_facilities:display( "Instance description: ~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,

	wooper:delete_synchronously_instance( MyV ),

	test_facilities:stop().
