% Copyright (C) 2008-2014 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Class modeling a soda vending machine.
%
-module(class_FaultySodaVendingMachine).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).


% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, MachineName,
		InitialCanCount, CanCost ).


% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		synchronous_new/4, synchronous_new_link/4,
		synchronous_timed_new/4, synchronous_timed_new_link/4,
		remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		construct/5, delete/1 ).



% Method declarations.
-define( wooper_method_export, actSpontaneous/1, getCanCost/2, orderSoda/3,
		getProbe/1 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Actor.SodaVendingMachine").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a faulty soda-vending machine.
%
construct( State, ?wooper_construct_parameters ) when InitialCanCount >= 0 ->

	ActorState = class_Actor:construct( State, ActorSettings, MachineName ),

	?send_info_fmt( ActorState,
		"Creating a new soda vending machine named '~s', "
		"having initially ~B can(s), costing each ~B euro(s).",
		[ MachineName, InitialCanCount, CanCost ] ),

	StockProbe = class_Probe:new(
		io_lib:format( "~s Soda Stock Probe", [ MachineName ] ),
		{ io_lib:format( "~s can stock", [ MachineName ] ) },
		"Monitoring the soda consumption",
		"Simulation tick",
		"Number of cans still available in the machine" ),

	setAttributes( ActorState, [
		{can_count,InitialCanCount},
		{can_cost,CanCost},
		{probe,StockProbe},
		{trace_categorization,?TraceEmitterCategorization} ] ).



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	% Class-specific actions:
	?info_fmt( "Deleting soda vending machine named '~s', "
			  "whose final can stock was ~B.",
			  [ ?getAttr(name), ?getAttr(can_count) ] ),

	% Then allow chaining:
	State.




% Methods section.


% Management section of the actor.


% The core of the soda vending machine behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper_state() ) -> wooper_state().
actSpontaneous( State ) ->

	% Here a machine as no spontaneous behaviour, so it does not do anything
	% special, except collecting some data:

	CurrentTick = class_Actor:get_current_tick( State ),

	?getAttr(probe) ! { setData, [ CurrentTick, { ?getAttr(can_count) } ] },

	?wooper_return_state_only( State ).



% Called by a customer wanting to know the cost of a can for this machine.
%
% (actor oneway)
%
-spec getCanCost( wooper_state(), pid() ) ->
						class_Actor:actor_oneway_return().
getCanCost( State, CustomerPid ) ->

	?info_fmt( "Telling to customer ~w the cost of a can.", [ CustomerPid ] ),

	CustomerPid ! { setCanCost, ?getAttr(can_cost) }.



% Called by a customer wanting to purchase a can.
%
% (actor oneway)
%
-spec orderSoda( wooper_state(), amount(), pid() ) ->
					   class_Actor:actor_oneway_return().
orderSoda( State, CustomerBudget, CustomerPid ) ->

	NewState = case ?getAttr(can_count) of

		CanCount ->

			% We have a can, so where is the cash?
			case ?getAttr(can_cost) of

				CanCost when CanCost > CustomerBudget ->
					?info( "Order failed, as customer is not rich enough." ),
					class_Actor:send_actor_message( CustomerPid,
						onNotEnoughMoney, State );

				_ ->
					SentState = class_Actor:send_actor_message( CustomerPid,
						getCan, State ),
					setAttribute( SentState, can_count, CanCount-1 )

			end,

		0 ->
			info( "Order failed, as no soda can left." ),
			class_Actor:send_actor_message( CustomerPid,
				onNoCanAvailable, State )

	end,

	?wooper_return_state_result( NewState, CustomerBudget ).



% Returns the probe in use by this machine.
%
% Useful for the calling test, so that it can control by itself the probe, as
% depending on whether it is run in batch mode or not, probe displaying is
% wanted or not.
%
% (const request)
%
-spec getProbe( wooper_state() ) -> request_return( pid() ).
getProbe( State ) ->
	?wooper_return_state_result( State, ?getAttr(probe) ).
