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

% Author: Jingxuan Ma (jingxuan.ma@edf.fr)


% This file is part of forest ecosystem test case, which is a Sim-Diasca
% integration test example.


% The objective of this module is to show:
%
% - the significant features of an actor scheduled totally in periodic mode. The
% periodic mode scheduling means all activities are spontaneously scheduled
%
% - how to use data-logger for virtual probe creation and data storage
%
% - how to create actor before simulation starting and in simulation running
%
-module(class_Forest).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).


-define( wooper_construct_parameters, ActorSettings, DomainName, Longevity ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4 ).


% Declarations of class-specific methods (besides inherited ones).
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2,
		 addInPeers/3, deleteFromPeers/2, requiredLocation/2,
		 beginCompetition/2, informedParticipation/4,
		 getProbe/1, getVirtualProbe/1, getDataLoggerPid/1 ).


% Static method declarations (to be directly called from module):
-define( wooper_static_method_export, create_initial_foresters/3 ).


-export([ notify_be_registered/2 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"SSI-Test.Forest").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% The class-specific attributes of a forest are:
%
% - periodic: forest spontaneous activity period
%
% - cycle: the life cycle in the forest; it is defined as 12 ticks which
% presents 12 months a year
%
% - famine_alert_saisons: is a list of period at which the forest sends famine
% alert to squirrels
%
% - savage_alert_saisons: is a list of period at which the forest sends "savage
% attack alert to squirrels
%
% - squirrel_reproduce_saisons: at which the forest sends reproduction message
% to the female squirrels
%
% - fire_alert_triggers: at which the forest sends fire alert to oaks
%
% - oak_reproduce_period: the period of a new oak available
%
% - oak_list:the list of oaks pids
%
% - squirrel_list:the list of suirrels pids
%
% - winner: { WinnerPid, WinnerTailLength }
%
% - target_peers: is all forest habitant pid list
%
% - virtual_probe_pid: the probe created by data-logger
%
% - dataLogger_pid: records the data-logger instance pid
%
% - termination_initiated: when it is true, means the forest instance is ready
% to be terminated. By default, its value is false
%
% - termination_waiting_ticks: the ticks between the actor informs its
% termination and execute its termination. This attribute is defined for
% avoiding termination_but_trigger error
%
% - termination_tick_offset: duration after which this actor should terminate
% Reference to class_Actor.erl for other attributes



% Constructs a new Forest with:
%
% - ActorSettings is the engine settings for this actor
%
% - DomainName is a plain string
%
% - Longevity is expressed in ticks
%
% TODO: represents all ticks in second, for example
%
-spec construct( wooper:state(), class_Actor:actor_settings(), string(),
				 class_TimeManager:tick_offset() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% Firstly, the mother class
	ActorState = class_Actor:construct( State, ActorSettings, DomainName ),

	% The life-cycle of that probe is not managed by this actor:
	{ DataLoggerPid, ForestVirtualProbeID } =
		class_DataLogger:create_virtual_probe(

		   _ProbeName="Forest Ecosystem Probe",

		   _ForestCurveNames=[ "Number of squirrels living in the forest",
							   "Number of oaks in the forest" ],

		   _ForestZones=[],

		   _Title=io_lib:format( "Monitoring Forest ecosystem evolution for ~p",
								  [ self() ] ),

		   _FirstXLabel="Simulation tick",

		   _FirstYLabel="Forester dweller number" ),

	%DataLoggerPid ! { getProbeTable, MyVirtualProbeID, self() },
	%ProbeTable = test_receive(),

	%LongevityInTicks = class_Actor:convert_seconds_to_ticks( Longevity,
	%  ActorState ),

	% Then the class-specific attributes:
	StartingState = setAttributes( ActorState, [

		{ name, DomainName },
		{ periodic, 1 },
		{ cycle, 12 },
		{ famine_alert_seasons, [ 3 ] },
		{ savage_alert_seasons, [ 9 ] },
		{ squirrel_reproduce_seasons, [ 5 ] },
		{ fire_alert_seasons, [ 11 ] },
		{ affected_oaks_count, 0 },
		{ oak_list, [] },
		{ squirrel_list, [] },
		{ male_squirrel_list, [] },
		{ female_squirrel_list, [] },
		{ winner,  { undefined, undefined }  },
		{ nb_reply, 0 },
		{ oak_reproduce_period, 20 },
		{ target_peers, [] },
		{ termination_initiated, false },
		{ longevity, Longevity },
		{ termination_waiting_ticks, 10 },
		{ termination_tick_offset, 1000 },
		{ virtual_probe_pid, ForestVirtualProbeID },
		{ dataLogger_pid, DataLoggerPid },
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

	] ),

	?send_info( StartingState, "Creating a new forest." ),

	StartingState.




% Methods implementation section.


% The spontaneous behaviour of a forest instance.
% (actor oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTick = ?getAttr(current_tick_offset),
	WaitTicks = ?getAttr(termination_waiting_ticks),

	NewState = case ?getAttr(termination_initiated) of


		true when WaitTicks > 0 ->

				NewWaitTick = ?getAttr(termination_waiting_ticks) -1,

				TermState = setAttribute( State, termination_waiting_ticks,
										  NewWaitTick ),

				executeOneway( TermState, scheduleNextSpontaneousTick );


		true ->
				?info_fmt( "The forest is terminating at tick #~B.",
						   [ CurrentTick ] ),

				executeOneway( State, declareTermination );


		false ->

			UpdatedState = case CurrentTick of

				0 ->
					?info( "The forest is waiting for its dwellers." ),
					State;

				_Others ->

					case test_trigger_termination_conditions( State ) of

						true ->
							notify_termination( State );

						false ->
							perform_spontaneous_action( State )

					end

			end,

			NewTick = CurrentTick + ?getAttr(periodic),
			executeOneway( UpdatedState, addSpontaneousTick, NewTick )

	end,

	OakList = ?getAttr(oak_list),
	NbOak = length(OakList),

	SquList = ?getAttr(squirrel_list),
	NbSquirrel = length(SquList),

	% Sends data to the virtual probe:
	?getAttr(dataLogger_pid) ! { setData,
		[ ?getAttr(virtual_probe_pid), CurrentTick, { NbSquirrel, NbOak } ] },

	?wooper_return_state_only( NewState ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% Spontaneous activities section.


% Called for creating initial Forest dwellers.

% It must be called before the simulation starts.
%
% (actor oneway)
%
%initialForesterCreation( State, NbOak, NbSquirrel ) ->

	% The initial oak actors are created as initial placed actor
%	initial_placed_inhabitant_creation(State, NbOak, "class_Oak", []),
	% The initial squirrel actors are created as normal initial actor
%	NbFSquirrel = round(NbSquirrel/2),
%	initial_inhabitant_creation(State,
%					   NbFSquirrel, "class_FemaleRedSquirrel", []),
%	initial_inhabitant_creation(State,
%					   (NbSquirrel-NbFSquirrel), "class_MaleRedSquirrel", []),

%	?wooper_return_state_only( State).




% Static method.
%
% Called for creating initial Forest dwellers.
% It must be called before the simulation start
%
-spec create_initial_foresters( basic_utils:count(), basic_utils:count(),
							   pid() ) -> basic_utils:void().
create_initial_foresters( NbOak, NbSquirrel, ForestPid ) ->

	% The initial oak actors are created as initial placed actors:
	initial_placed_inhabitant_creation( ForestPid, NbOak, "class_Oak", [] ),

	% The initial squirrel actors are created as normal initial actors:
	NbFSquirrel = round( NbSquirrel / 2 ),

	initial_inhabitant_creation( ForestPid, NbFSquirrel,
								 "class_FemaleRedSquirrel", [] ),

	initial_inhabitant_creation( ForestPid, NbSquirrel-NbFSquirrel,
								 "class_MaleRedSquirrel", [] ).



% Called to delete a specified peer from known target peers.
%
% An updated state is returned.
%
% (actor oneway)
%
-spec deleteFromPeers( wooper:state(), pid() ) ->
							 class_Actor:actor_oneway_return().
deleteFromPeers( State, PeerPid ) ->

	?info_fmt( "~w is removed from forest peers.", [PeerPid] ),

	OakList       = ?getAttr(oak_list),
	SquirrelList  = ?getAttr(squirrel_list),
	MSquirrelList = ?getAttr(male_squirrel_list),
	FSquirrelList = ?getAttr(female_squirrel_list),

	UpdatedOakList       = lists:delete( PeerPid, OakList ),
	UpdatedSquirrelList  = lists:delete( PeerPid,SquirrelList ),
	UpdatedMSquirrelList = lists:delete( PeerPid, MSquirrelList ),
	UpdatedFSquirrelList = lists:delete( PeerPid, FSquirrelList ),

	UpdatedState = updated_peers( State, UpdatedOakList, UpdatedSquirrelList,
								  UpdatedMSquirrelList, UpdatedFSquirrelList ),

	?wooper_return_state_only( UpdatedState ).



% Called to add a specified peer to known peers.
%
% (actor oneway)
%
-spec addInPeers( wooper:state(), class_name(), pid() ) ->
						class_Actor:actor_oneway_return().
addInPeers( State, ClassName, SenderPid ) ->

	?info_fmt( "~s with ~w is added in forest peers.",
			  [ ClassName, SenderPid ] ),

	UpdatedState = case ?getAttr(termination_initiated) of

		false ->
			?info_fmt( "~s with ~w is added in forest peers.",
					   [ ClassName, SenderPid ] ),
			add_pid_in_peers( State, ClassName, SenderPid );

		true ->
			?info_fmt( "~s with ~w cannot be added in forest peers.",
					   [ ClassName, SenderPid ] ),
			class_Actor:send_actor_message( SenderPid, forestDestroyed, State )

	end,

	?wooper_return_state_only( UpdatedState ).



% Called by a squirrel for be allocated to a new oak
%
% (actor oneway)
%
-spec requiredLocation( wooper:state(), pid() ) ->
							  class_Actor:actor_oneway_return().
requiredLocation( State, SquirrelPid ) ->

	OakList = ?getAttr(oak_list),
	Length = length( OakList ),

	{ UpdatedState, OakPid } = case ?getAttr(affected_oaks_count) of

		AffectedOaksCount when AffectedOaksCount < Length ->

			Pid = lists:nth( AffectedOaksCount + 1, OakList ),
			NewState = setAttribute( State, affected_oaks_count,
									 AffectedOaksCount + 1 ),
			{ NewState, Pid };


		_Others ->

			NewState = setAttribute( State, affected_oaks_count, 1 ),
			Pid = hd( OakList ),
			{ NewState, Pid }

		end,

	% Informing the squirrel of its new oak PID:
	StateAfterInformSquirrel = class_Actor:send_actor_message( SquirrelPid,
						{ beAllocated, OakPid }, UpdatedState ),

	% Informing Oak its new lodger
	NewUpdatedState = class_Actor:send_actor_message( OakPid,
				{ beAffected, SquirrelPid }, StateAfterInformSquirrel ),

	?wooper_return_state_only( NewUpdatedState ).



% Called by a female squirrel.
%
% When the forest receives this message, it sends a "beInvited" message to all
% male squirrels for launching a competition.
%
% (actor oneway)
%
-spec beginCompetition( wooper:state(), pid() ) ->
							  class_Actor:actor_oneway_return().
beginCompetition( State, LauncherPid ) ->

	?info_fmt( "Initiated by ~w: a competition invitation is sent to "
			   "all male squirrels, ~p.",
			   [ LauncherPid, ?getAttr(male_squirrel_list) ] ),

	MSquirrelList = ?getAttr(male_squirrel_list),

	NState = case MSquirrelList of

		[] ->
			class_Actor:send_actor_message( LauncherPid,
					{ theWinner, undefined }, State );

		MSquirrelList ->

			SendFun = fun( InhabitantPid, FunState ) ->
					class_Actor:send_actor_message( InhabitantPid,
							{ beInvited, LauncherPid }, FunState )
			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, MSquirrelList )

	end,
	setAttributes( NState, [
			{ winner, { _winnerPid=undefined, _winnerTail=undefined } },
			{ nb_reply, 0 } ] ).



% Called by the competition participant (male squirrel).
%
% When the forest receives this message, it will compare the tail lengths and
% update winner attribute with male squirrel pid and max tail length.
%
% (actor oneway)
%
-spec informedParticipation( wooper:state(), pid(), number(), pid() ) ->
					class_Actor:actor_oneway_return().
informedParticipation( State, LauncherPid, TailLength, SenderPid ) ->

	?info_fmt( "The participation of ~w is received, its tail length is ~p.",
			   [ SenderPid, TailLength ] ),

	ReceivedReply = ?getAttr(nb_reply) + 1,

	{ Winner, Length } = case ?getAttr(winner) of

		{ undefined, undefined } when TailLength =:= refused ->
					{ undefined, undefined };

		{ undefined, undefined } ->
					{ SenderPid, TailLength };

		{ _WinnerPid, WinnerTail } when WinnerTail < TailLength ->
					{ SenderPid, TailLength };

		{ WinnerPid, WinnerTail } when WinnerTail >= TailLength ->
					{ WinnerPid, WinnerTail }

	end,

	NbMSquirrel = length( ?getAttr(male_squirrel_list) ),

	NState = case NbMSquirrel of

		N when N =:= ReceivedReply ->
			class_Actor:send_actor_message( LauncherPid,
						{ theWinner, Winner }, State );

		_Others ->
			State

	end,

	setAttributes( NState, [ { nb_reply, ReceivedReply },
							 { winner, {Winner,Length} } ] ).




% Probe-related section.


% Returns the PID of the forest probe.
%
% (const request)
%
-spec getProbe( wooper:state() ) -> request_return( pid() ).
getProbe( State ) ->
	?wooper_return_state_result( State, ?getAttr(probe_pid) ).




% Returns the PID of the forest virtual probe.
%
% The virtual probe is created by data logger.
%
% Useful for the calling test, so that it can control by itself the probe, as,
% depending on whether it is run in batch mode or not, probe displaying is
% wanted or not.
%
% (const request)
%
-spec getVirtualProbe( wooper:state() ) -> request_return( pid() ).
getVirtualProbe( State ) ->
	?wooper_return_state_result( State, ?getAttr(virtual_probe_pid) ).



% Called for returning the singleton data-logger PID.
%
% (const request)
%
-spec getDataLoggerPid( wooper:state() ) -> request_return( pid() ).
getDataLoggerPid( State ) ->
	?wooper_return_state_result( State, ?getAttr(dataLogger_pid) ).



% Helper functions


% Initial forest dweller creation session, these functions can only be called
% before the simulation starts.



% Called to create synchronously a number of initial placed actors:
%
% - Number is the number of inhabitants to be created
% - ClassName is the class name
% - CreatedList is the created pid list
%
% A CreatedPidList is returned.
%
% NB: in this test, all trees must be created in form of placed actor with
% PlacementHint = forest pid

initial_placed_inhabitant_creation( _ForestPid, 0, _ClassName, CreatedList ) ->
	CreatedList;


initial_placed_inhabitant_creation( ForestPid, Number, ClassName,
								   CreatedList ) ->

	ActualCreatedNb = length( CreatedList ) + 1,

	Name = ClassName ++ integer_to_list( ActualCreatedNb ),

	NewPid = class_Actor:create_initial_placed_actor(
			list_to_atom( ClassName ),
			[ _Name=Name, _DefaultAge=ActualCreatedNb, _ForestPid=ForestPid ],
			_PlacementHint=forest_inhabitant ),

	UpdatedCreatedList = [ NewPid | CreatedList ],

	initial_placed_inhabitant_creation( ForestPid, Number-1, ClassName,
										UpdatedCreatedList ).



% Called to create synchronously a number of initial actors.
%
% Method parameters are:
%
% - Number is the number of habitant to be created
%
% - ClassName is the class name
%
% - CreatedList is the created PID list
%
% A CreatedPidList is returned.
%
initial_inhabitant_creation( _ForestPid, 0, _ClassName, CreatedList ) ->
	CreatedList;


initial_inhabitant_creation( ForestPid, Number, ClassName, CreatedList ) ->

	ActualCreatedNb = length(CreatedList) + 1,

	Name = lists:subtract( ClassName, "class_" ) ++
		integer_to_list( ActualCreatedNb ),

	NewPid = class_Actor:create_initial_actor(
		list_to_atom( ClassName ),
		[ _Name=Name, _defaultAge=ActualCreatedNb, _Forest=ForestPid ] ),

	UpdatedCreatedList = [ NewPid | CreatedList ],

	initial_inhabitant_creation( ForestPid, Number-1, ClassName,
								 UpdatedCreatedList ).



% Run-time forest dweller creation section.


% Called to create a new oak actor (placed actor) in simulation time
%
new_placed_oak_creation( State ) ->

	OakList = ?getAttr(oak_list),

	ActualCreatedNb = length( OakList ) + 1,

	Name = "class_Oak" ++ integer_to_list( ActualCreatedNb ),

	class_Actor:create_placed_actor( class_Oak,
			[ _Name=Name, _defaultAge=0, _Forest=self() ],
			  _PlacementHint=self(), State ).




% Spontaneous activities verification section.


% Called to verify if any simulation termination condition is satisfied.
%
% The normal simuilation termination conditions are:
% -  the termination offset is reached
% - there is no oak in the forest
% - there is no squirrel in the forest
%
test_trigger_termination_conditions( State ) ->

	TerminationOffset = ?getAttr(termination_tick_offset),

	Nb_Oak = length( ?getAttr(oak_list) ),

	Nb_Squirrel = length( ?getAttr(squirrel_list) ),

	case ?getAttr(current_tick_offset) of

		PastOffset when PastOffset >= TerminationOffset ->
			?info( "Forest is ruined because its longevity is reached." ),
			true;

		_Others ->

			case Nb_Oak of

				0 ->
					?info( "Forest is ruined because of no any tree." ),
					true;

				_OtherOak ->

					case Nb_Squirrel of

						0 ->
							?info( "Forest is dying, as there is no "
								   "living being in it." ),
							true;

						_AtLeastOne ->
							false

					end

			end

	end.



% Forest specific periodic spontaneous behaviors.
%
% (helper)
%
perform_spontaneous_action( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	ActionTrigger= CurrentOffset rem ?getAttr(cycle),

	% Get trigger period lists:
	FamineTriggers = ?getAttr(famine_alert_seasons),
	SavageTriggers = ?getAttr(savage_alert_seasons),
	FireTriggers   = ?getAttr(fire_alert_seasons),

	ReproductionTriggers = ?getAttr(squirrel_reproduce_seasons),

	NewState = case lists:member( ActionTrigger, FamineTriggers ) of

		true ->
				notify_alert( State, famine );

		false ->
				case lists:member( ActionTrigger, SavageTriggers) of

					true ->
							notify_alert( State, savage );

					false ->
							case lists:member( ActionTrigger, FireTriggers ) of

								 true ->
									   notify_alert( State, fire );

								false ->
									case lists:member( ActionTrigger,
													   ReproductionTriggers ) of

										true ->
											notify_alert( State, reproduction );

										false ->
											State

									end
							end
				end
	end,

	UpdatedState = case CurrentOffset rem ?getAttr(oak_reproduce_period) of

		0 ->
			?info_fmt( "A new oak is availabe at tick #~p.",
					  [ CurrentOffset ] ),
			new_placed_oak_creation( NewState );

		_Others ->
			NewState

	end,

	?wooper_return_state_only( UpdatedState ).




% Forest spontaneous activity execution section.


% Notify the forest dwellers that they are registered to th forist
%
% (helper)
%
-spec notify_be_registered( wooper:state(), [pid()] ) -> wooper:state().
notify_be_registered( State, InHabitantList ) ->

	SendFun = fun( InhabitantPid, FunState ) ->

		% Returns an updated state:
		class_Actor:send_actor_message( InhabitantPid, beRegistered, FunState )

	end,
	% Returns an updated state:
	lists:foldl( SendFun, State, InHabitantList ).



% Sending alert to relative forest dwellers
%
% (helper)
%
notify_alert( State, Alert ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),

	RelativePidList = case Alert of

		fire ->
			[ hd( ?getAttr(oak_list) ) ];

		famine ->
			?getAttr(squirrel_list);

		savage ->
			[ hd( ?getAttr(squirrel_list) ) ];

		reproduction ->
			?getAttr(female_squirrel_list)

		% Completly covered:
		%_Others ->
		%	?getAttr(target_peers)

		end,

	?info_fmt( "A ~p alert is broadcast to relative forest dwellers ~p "
			  "at tick offset #~p.",
			  [ Alert, RelativePidList, CurrentTickOffset ] ),

	SendFun = fun( InhabitantPid, FunState ) ->

				% Returns an updated state:
				class_Actor:send_actor_message( InhabitantPid,
												{ beAlert, Alert }, FunState )

	end,

	%basic_utils:debug( "foldl with ~p", [ RelativePidList ] ),

	% Returns an updated state:
	lists:foldl( SendFun, State, RelativePidList ).



% The forest informs its dwellers of its termination.
%
% (helper)
%
notify_termination( State ) ->

	?info( "A forest destroyed message is sent to the forest dwellers." ),

	NewState = case ?getAttr(target_peers) of

		[] ->
			State;

		TargetPeers ->
			SendFun = fun( InhabitantPid, FunState ) ->

				%Returns an updated state:
				class_Actor:send_actor_message( InhabitantPid, forestDestroyed,
												FunState )

			end,

			% Returns an updated state:
			lists:foldl( SendFun, State, TargetPeers )

		end,

	% Prepares itself for termination:
	prepare_termination( NewState ).




% An updated state is returned.
%
% (helper)
%
prepare_termination( State ) ->

	CurrentOffset = ?getAttr(current_tick_offset),

	setAttributes( State, [

		 { termination_initiated, true },
		 { termination_tick_offset, CurrentOffset }

		] ).



add_pid_in_peers( State, ClassName, PeerPid ) ->

	OakList       = ?getAttr(oak_list),
	SquirrelList  = ?getAttr(squirrel_list),
	MSquirrelList = ?getAttr(male_squirrel_list),
	FSquirrelList = ?getAttr(female_squirrel_list),

	{ NOakList, NSquirrelList, NMSquirrelList, NFSquirrelList } =

		case ClassName of

			class_Oak ->
				NewOakList = [ PeerPid | OakList ],
				{ NewOakList, SquirrelList, MSquirrelList, FSquirrelList };

			class_MaleRedSquirrel ->
				NewSquirrelList = [ PeerPid | SquirrelList ],
				NewMSquirrelList = [ PeerPid | MSquirrelList ],
				{ OakList, NewSquirrelList, NewMSquirrelList, FSquirrelList };

			class_FemaleRedSquirrel ->
				NewSquirrelList = [ PeerPid | SquirrelList ],
				NewFSquirrelList = [ PeerPid | FSquirrelList ],
				{ OakList, NewSquirrelList, MSquirrelList, NewFSquirrelList }

		end,

	updated_peers( State, NOakList, NSquirrelList, NMSquirrelList,
				   NFSquirrelList ).



% Called to return a updated state with the updated forest dweller list.
%
% (helper)
%
updated_peers( State, OakList, SquirrelList, MSquirrelList, FSquirrelList ) ->

	UpdatedOakNumber = length( OakList ),
	UpdatedSquirreNumber = length( SquirrelList ),
	UpdatedTargetPeers = OakList ++ SquirrelList,

	setAttributes( State, [

		{ nb_oak, UpdatedOakNumber },
		{ oak_list, OakList },
		{ nb_squirrel, UpdatedSquirreNumber },
		{ squirrel_list, SquirrelList },
		{ male_squirrel_list, MSquirrelList },
		{ female_squirrel_list, FSquirrelList },
		{ target_peers, UpdatedTargetPeers }

	] ).
