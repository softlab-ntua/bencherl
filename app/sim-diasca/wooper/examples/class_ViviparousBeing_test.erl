% Copyright (C) 2003-2014 Olivier Boudeville
%
% This file is part of the WOOPER examples.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the ViviparousBeing class implementation.
% See the class_ViviparousBeing.erl tested module.

-module(class_ViviparousBeing_test).


-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
		[ class_ViviparousBeing:is_wooper_debug() ] ),

	test_facilities:display(
		"Statically, class name is ~s, superclasses are ~w.",
		[	class_ViviparousBeing:get_class_name(),
			class_ViviparousBeing:get_superclasses() ] ),

	MyV = class_ViviparousBeing:new_link(),
	MyV ! { getClassName, [], self() },
	receive

		{ wooper_result, class_ViviparousBeing } ->
			test_facilities:display(
				"After constructor, get_class_name returned "
				"'class_ViviparousBeing' as expected." );

		{ wooper_result, UnexpectedClass } ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyV ! { get_superclasses, [], self() },
	receive

		{wooper_result, []} ->
			test_facilities:display(
				"After constructor, get_superclasses returned [] "
				"as expected.");

		{wooper_result,UnexpectedSuperclasses} ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,
	MyV ! {getMeanChildrenCount,[],self()},
	receive

		{wooper_result,4} ->
			test_facilities:display(
				"After constructor, getMeanChildrenCount returned 4 "
				"as expected.");

		{wooper_result,UnexpectedMeanCount} ->
			test_facilities:fail( "wrong mean children count: ~p",
				[ UnexpectedMeanCount ] )


	end,
	MyV ! {getBirthGivenCount,[],self()},
	receive

		{wooper_result,0} ->
			test_facilities:display(
				"After constructor, getBirthGivenCount returned 0 "
				"as expected.");

		{wooper_result,UnexpectedFirstCount} ->
			test_facilities:fail( "wrong first children count: ~p",
				[ UnexpectedFirstCount ] )

	end,
	MyV ! {giveBirth,7},
	MyV ! {getBirthGivenCount,[],self()},
	receive

		 {wooper_result,7}->
			test_facilities:display(
				"After giveBirth, getBirthGivenCount returned 7 "
				"as expected." );

		{wooper_result,UnexpectedSecondCount} ->
			test_facilities:fail(  "wrong second children count: ~p",
				[ UnexpectedSecondCount ] )

	end,

	case class_ViviparousBeing:is_wooper_debug() of

		true ->
			MyV ! { wooper_get_instance_description,[], self() },
			receive

				{wooper_result,InspectString} ->
					test_facilities:display( "Instance description: ~s",
											[ InspectString ] )
			end;

		false ->
			ok

	end,
	MyV ! delete,

	test_facilities:stop().
