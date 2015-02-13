% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Class modeling a soda vending machine.
%
-module(class_SodaVendingMachine).


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
		 construct/5, destruct/1 ).


% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2,
		 getCanCost/2, orderSoda/3 ).


% For common types defined in this Soda-Test example:
-include("soda_test_types.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Soda-test.SodaVendingMachine").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% The class-specific attributes of a soda vending machine are:
%
% - can_count :: can_count() is the (positive integer) number of cans a machine
% currently holds
%
% - can_cost :: amount() is the cost of a can from this machine (as a
% floating-point sum, in euros)
%
% - probe_pid :: class_Probe:probe_pid() is the PID (if any) of the probe
% declared by this vending machine




% Creates a new soda vending machine.
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), can_count(), amount() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) when InitialCanCount >= 0 ->

	ActorState = class_Actor:construct( State, ActorSettings, MachineName ),

	?send_info_fmt( ActorState,
		"Creating a new soda vending machine named '~s', "
		"having initially ~B can(s), costing each ~.2f euro(s).",
		[ MachineName, InitialCanCount, CanCost ] ),

	% Depending on the choice of the result manager, it will be either a PID (if
	% the corresponding result is wanted) or a 'non_wanted_probe' atom:
	StockProbePid = class_Actor:declare_probe(
		_Name=io_lib:format( "~s Soda Stock Probe", [ MachineName ] ),
		_Curves=[ io_lib:format( "~s can stock", [ MachineName ] ) ],
		_Zones=[],
		_Title="Monitoring the soda consumption",
		_XLabel="Simulation tick",
		_YLabel="Number of cans still available in the machine" ),

	setAttributes( ActorState, [

		{ can_count, InitialCanCount },
		{ can_cost, CanCost },
		{ probe_pid, StockProbePid },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

								] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?info_fmt( "Deleting soda vending machine named '~s', whose "
		"final can stock was ~B.", [ ?getAttr(name), ?getAttr(can_count) ] ),

	% Then allow chaining:
	State.




% Methods section.



% Management section of the actor.


% The core of the soda vending machine behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% Here a machine as no spontaneous behaviour, so it does not do anything
	% special and does not set any future action.

	CurrentTick = class_Actor:get_current_tick( State ),

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	class_Probe:send_data( ?getAttr(probe_pid), CurrentTick,
						   { ?getAttr(can_count) } ),

	?wooper_return_state_only( State ).




% We want to synchronise here our probes starting from first diasca.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, SimulationInitialTick }

	end,

	% Pseudo-const method:
	?wooper_return_state_only( State ).



% Called by a customer wanting to know the cost of a can for this machine.
%
% (actor oneway)
%
-spec getCanCost( wooper:state(), pid() ) -> class_Actor:actor_oneway_return().
getCanCost( State, CustomerPid ) ->

	?info_fmt( "Telling to customer ~w the cost of a can.", [ CustomerPid ] ),

	?wooper_return_state_only( class_Actor:send_actor_message( CustomerPid,
		{ setCanCost, ?getAttr(can_cost) }, State ) ).



% Called by a customer wanting to purchase a can.
%
% (actor oneway)
%
-spec orderSoda( wooper:state(), amount(), pid() ) ->
					   class_Actor:actor_oneway_return().
orderSoda( State, CustomerBudget, CustomerPid ) ->

	% To test simulation stalls due to actors (here, thirsty customers) blocking
	% the simulation not because they are busy, but because they are blocked by
	% others (this machine):

	%io:format( "  Soda vending machine ~w sleeping...~n", [ self() ] ),
	%timer:sleep( _Milliseconds=11*1000 ),
	%io:format( "  Soda vending machine ~w slept.~n", [ self() ] ),

	NewState = case ?getAttr(can_count) of

		0 ->
			?info( "Order failed, as no soda can left." ),
			class_Actor:send_actor_message( CustomerPid,
											onNoCanAvailable, State );

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

			end

	end,

	% Manages automatically the fact that the creation of this probe may have
	% been rejected by the result manager:
	class_Probe:send_data( ?getAttr(probe_pid),
						  class_Actor:get_current_tick( State ),
						  { getAttribute( NewState, can_count ) } ),

	?wooper_return_state_only( NewState ).
