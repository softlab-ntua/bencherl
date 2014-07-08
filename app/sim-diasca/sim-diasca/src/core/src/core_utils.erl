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


% This module gathers some facilities for all core classes and for all
% test/simulation cases.
%
-module(core_utils).




-export([ get_title/0, wait_ready/0, suspend_simulation_until_enter_pressed/1,
		  draw_item_from/2, draw_items_from/3 ]).


% For notify_debug_fmt and al:
-include("traces.hrl").




% Returns a textual description of the version of Sim-Diasca being used.
%
-spec get_title() -> string().
get_title() ->

	case init:get_argument( '-sim-diasca-version' ) of

		{ ok, [ VersionString ] } ->
			"Sim-Diasca distributed branch (v" ++ VersionString ++ ")";

		_ ->
			"Sim-Diasca distributed branch"

	end.



% Waits until an actor is ready, and acknowledges its notification.
%
-spec wait_ready() -> basic_utils:void().
wait_ready() ->

	receive

		{ actorMessage, [ _ATick, notifyReady, ActorPid ] } ->
			
			?notify_debug_fmt( "Actor ~w ready.", [ ActorPid ] ),
			
			% Acknowledges the actor message, otherwise the actor will be
			% frozen:
			%
			ActorPid ! { acknowledgeMessage, self() }

	end.



% Suspends the simulation until the Enter key is pressed.
%
-spec suspend_simulation_until_enter_pressed( pid() ) -> basic_utils:void().
suspend_simulation_until_enter_pressed( TimeManagerPid ) ->

	case executable_utils:is_batch() of

		true ->
			nothing_done;

		false ->

			%io:format("Requesting the simulation to be suspended.~n"),
			TimeManagerPid ! suspend,

			io:get_line( "Simulation requested to be suspended, "
				"press Enter to resume it." ),

			TimeManagerPid ! resume,

			io:format( "Simulation requested to be resumed.~n" )

	end.



% Draws one item from specified list using a random law and the specified random
% manager, and returns a pair made of the drawn item and of the resulting list,
% which is the specified one with the first instance of that drawn item removed:
% { DrawnItem, RemainingList }.
%
% Expects the specified list to be non-empty.
%
% Note: this function is mostly deprecated, as now stochastic values can
% generally (ex: for actors) be obtained without direct exchange with a random
% manager).
%
-spec draw_item_from( [ T ], pid() ) -> { T, [ T ] }.
draw_item_from( DrawableList, RandomManagerPid ) when DrawableList =/= [] ->

	% getUniformValue returns a number in 1..N:
	RandomManagerPid ! { getUniformValue, length( DrawableList ), self() },
	
	DrawnPosition = receive

		{ wooper_result, { uniform_value, Value } } ->
			Value

	end,

	% No item should be drawn twice:
	DrawnItem = lists:nth( DrawnPosition, DrawableList ),

	{ DrawnItem, lists:delete( DrawnItem, DrawableList ) }.



% Draws ItemCount items from specified list using a random law and the specified
% random manager, and returns either the 'too_many_drawn_items' atom is the
% specified list is too short, or a pair made of the list of drawn items and of
% the resulting list, which is the specified one with the first instance of all
% drawn items removed: { DrawnItemList, RemainingList }.
%
% Note: this function is mostly deprecated, as now stochastic values can
% generally (ex: for actors) be obtained without direct exchange with a random
% manager).
%
-spec draw_items_from( [ T ], basic_utils:count(), pid() ) ->
							 'too_many_drawn_items' | { [ T ], [ T ] }.
draw_items_from( DrawableList, ItemCount, RandomManagerPid ) ->
	draw_items_from( DrawableList, ItemCount, RandomManagerPid, _Acc=[] ).


draw_items_from( DrawableList, 0, _RandomManagerPid, Acc ) ->
	{ Acc, DrawableList };

draw_items_from( _DrawableList=[], _ItemCount, _RandomManagerPid, _Acc ) ->
	too_many_drawn_items;

draw_items_from( DrawableList, ItemCount, RandomManagerPid, Acc ) ->

	{ DrawnItem, RemainingList } = draw_item_from( DrawableList,
											   RandomManagerPid ),

	draw_items_from( RemainingList, ItemCount - 1, RandomManagerPid,
		[ DrawnItem | Acc ] ).
