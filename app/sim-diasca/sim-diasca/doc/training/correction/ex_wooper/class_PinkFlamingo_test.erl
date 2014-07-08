% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Unit tests for the PinkFlamingo class implementation.
% See the class_PinkFlamingo.erl tested module.
%
-module(class_PinkFlamingo_test).


-include("test_facilities.hrl" ).



% This is a WOOPER example, thus we are not supposed to use higher layers like
% traces for tests.


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Debug mode: ~s.",
		[ class_PinkFlamingo:is_wooper_debug() ] ),


	% General tests.

	test_facilities:display(
		"Statically, class name is ~s, superclasses are ~p.",
		[ class_PinkFlamingo:get_class_name(),
		  class_PinkFlamingo:get_superclasses() ] ),

	% Using synchronous_new_link would be better and safer:
	MyFlamingo = class_PinkFlamingo:new( "Syd", 120.0 ),

	MyFlamingo ! {get_class_name,[],self()},
	receive

		{wooper_result,class_PinkFlamingo} ->
			test_facilities:display(
				"After constructor, get_class_name returned "
				"'class_PinkFlamingo' as expected." );

		{wooper_result,UnexpectedClass} ->
			test_facilities:fail( "wrong class: ~p", [ UnexpectedClass ] )

	end,

	MyFlamingo ! {get_superclasses,[],self()},
	receive

		{wooper_result,[class_ViviparousBeing]} ->
			test_facilities:display(
				"After constructor, get_superclasses returned "
				"[class_ViviparousBeing] as expected." );

		{wooper_result,UnexpectedSuperclasses} ->
			test_facilities:fail( "wrong superclasses: ~p",
				[ UnexpectedSuperclasses ] )

	end,

	% This is a static method:
	test_facilities:display( "On average a flamingo has ~f children.",
		[ class_PinkFlamingo:get_mean_children_count() ] ),


	MyFlamingo ! {filterPlankton,camargue},
	MyFlamingo ! {filterPlankton,camargue},
	MyFlamingo ! {filterPlankton,chile},
	MyFlamingo ! {filterPlankton,camargue},

	MyFlamingo ! {getFeatherColor,[],self()},
	receive

		{wooper_result,pink} ->
			test_facilities:display(
				"The flamingo is pink, as expected." );

		{wooper_result,UnexpectedColor} ->
			test_facilities:fail( "wrong flamingo color: ~p",
				[ UnexpectedColor ] )

	end,

	MyFlamingo ! delete,

	% Below, a wrong solution to an example of race condition is shown:
	% synchronous deletion should have been used instead, like in:

	%MyFlamingo ! {synchronous_delete,self()},
	%receive
	%
	%	{deleted,MyFlamingo} ->
	%		ok
	%
	%end,
	timer:sleep(500),

	test_facilities:stop().
