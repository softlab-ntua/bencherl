% Copyright (C) 2014 EDF R&D

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


% This is a typical plugin example, to be re-used as a guide to develop actual
% Sim-Diasca plugins.
%
-module(my_plugin_example).


-behaviour(sim_diasca_plugin).


-export([

		 on_simulator_start/2,
		 on_deployment_start/1,
		 on_deployment_stop/1,
		 on_technical_settings_available/2,
		 on_case_initialisation_start/1,
		 on_case_initialisation_stop/1,
		 on_simulation_start/1,
		 on_simulation_bootstrap_start/1,
		 on_simulation_bootstrap_stop/1,
		 on_simulation_wallclock_milestone_met/2,
		 on_simulation_tick_milestone_met/2,
		 on_simulation_stop/1,
		 on_result_gathering_start/1,
		 on_result_gathering_stop/1,
		 on_simulator_stop/1,
		 on_case_specific_event/3

		 ]).


% For the notify/1 macro:
-include("traces.hrl").


% For the technical_settings record:
-include("sim_diasca_plugin.hrl").


% Shorthand:
-type plug_data() :: sim_diasca_plugin:plugin_data().





% Implementation notes.
%
% This plugin is stateless: its state, as recorded by the plugin manager
% (specified as the PluginData parameter), will be first 'undefined' (the
% default), then 'ok', and kept to this value.



% Callcack section, as requested by the 'sim_diasca_plugin' behaviour.


% Callback triggered as soon as the simulator is started (or almost, as basic
% services, including the trace one, are already up).
%
% This plugin may update these requested configuration changes, which may come
% from other plugins and may be in turn be changed by others.

% The on_technical_settings_available/2 callback could allow to check the
% effectiveness of this request (ex: if plugins requested incompatible changes).
%
-spec on_simulator_start( sim_diasca_plugin:configuration_changes(),
						  sim_diasca_plugin:plugin_data() ) ->
			{ sim_diasca_plugin:configuration_changes(),
			  sim_diasca_plugin:plugin_data() }.
on_simulator_start( ConfigurationChanges, _PluginData ) ->

	% One may look at the traces sent by the deployment agent(s) to check the
	% actual number of sequencers:

	notify( io_lib:format( "simulator started; keeping as are following "
						   "input configuration changes: ~p.",
						   [ ConfigurationChanges ] ) ),

	{ ConfigurationChanges, ok }.

	% As an example, one may use this code instead:

	%SchedulerCount = 2,

	%notify( io_lib:format( "simulator started; changing configuration, "
	%					   "requesting ~B schedulers.",
	%					   [ SchedulerCount ] ) ),

	%NewConfigurationChanges = ConfigurationChanges#configuration_changes{
	%						compute_scheduler_count=SchedulerCount },

	%{ NewConfigurationChanges, ok }.



% Callback triggered when the deployment phase starts.
%
-spec on_deployment_start( plug_data() ) -> plug_data().
on_deployment_start( _PluginData ) ->
	notify( "deployment started" ),
	ok.



% Callback triggered when the deployment phase stops.
%
-spec on_deployment_stop( plug_data() ) -> plug_data().
on_deployment_stop( _PluginData ) ->
	notify( "deployment stopped" ),
	ok.



% Callback triggered when the simulation technical settings are available,
% notably once the deployment phase is over.
%
-spec on_technical_settings_available( sim_diasca_plugin:technical_settings(),
									   plug_data() ) -> plug_data().
on_technical_settings_available(
  #technical_settings{ computing_nodes=ComputingNodes,
					   cookie=Cookie },
  _PluginData ) ->

	NodeString = io_lib:format(
				   "cookie '~s' used for the ~B computing node(s):~s",
				   [ Cookie,
					 length( ComputingNodes ),
					 text_utils:atom_list_to_string( ComputingNodes ) ] ),

	notify( "technical details available: " ++ NodeString ),
	ok.



% Callback triggered when the creation of the initial state of the simulation
% starts.
%
-spec on_case_initialisation_start( plug_data() ) -> plug_data().
on_case_initialisation_start( _PluginData ) ->
	notify( "case initialisation started" ),
	ok.


% Callback triggered when the creation of the initial state of the simulation
% just finished.
%
-spec on_case_initialisation_stop( plug_data() ) -> plug_data().
on_case_initialisation_stop( _PluginData ) ->
	notify( "case initialisation stopped" ),
	ok.



% Callback triggered when the simulation is just started and must evaluate the
% first diasca of all initial actors.
%
-spec on_simulation_bootstrap_start( plug_data() ) -> plug_data().
on_simulation_bootstrap_start( _PluginData ) ->
	notify( "simulation bootstrap started" ),
	ok.


% Callback triggered when the evaluation of the first diasca of all initial
% actors is over.
%
-spec on_simulation_bootstrap_stop( plug_data() ) -> plug_data().
on_simulation_bootstrap_stop( _PluginData ) ->
	notify( "simulation bootstrap stopped" ),
	ok.



% Callback triggered when a simulation milestone is met in wallclock time,
% i.e. after some elapsed duration.
%
-spec on_simulation_wallclock_milestone_met( unit_utils:milliseconds(),
						  plug_data() ) -> plug_data().
on_simulation_wallclock_milestone_met( CurrentMillisecond, _PluginData ) ->
	notify( io_lib:format( "simulation wall-clock milestone met, "
						   "after ~s; current wallclock time is ~s.",
				[ text_utils:duration_to_string( CurrentMillisecond ),
				  basic_utils:get_textual_timestamp() ] ) ),
	ok.



% Callback triggered when a simulation milestone is met in virtual time,
% i.e. when enough ticks have been evaluated.
%
-spec on_simulation_tick_milestone_met( class_TimeManager:tick_offset(),
						  plug_data() ) -> plug_data().
on_simulation_tick_milestone_met( TickOffset, _PluginData ) ->
	notify( io_lib:format( "simulation tick milestone met at "
						   "tick offset #~B, while current "
						   "wall-clock time is ~s.",
						   [ TickOffset,
							 basic_utils:get_textual_timestamp() ] ) ),
	ok.



% Callback triggered when the simulation is started (first tick, first diasca).
%
-spec on_simulation_start( plug_data() ) -> plug_data().
on_simulation_start( _PluginData ) ->
	notify( "simulation started" ),
	ok.



% Callback triggered when the simulation is stopped (an ending criterion was
% just met).
%
-spec on_simulation_stop( plug_data() ) -> plug_data().
on_simulation_stop( _PluginData ) ->
	notify( "simulation stopped" ),
	ok.



% Callback triggered when the results start being gathered, after simulation
% termination.
%
-spec on_result_gathering_start( plug_data() ) -> plug_data().
on_result_gathering_start( _PluginData ) ->
	notify( "result gathering started" ),
	ok.


% Callback triggered when the results have been gathered.
%
-spec on_result_gathering_stop( plug_data() ) -> plug_data().
on_result_gathering_stop( _PluginData ) ->
	notify( "result gathering stopped" ),
	ok.


% Callback triggered when the simulator execution stopped under normal
% circumstances (i.e. not crashing).
%
-spec on_simulator_stop( plug_data() ) -> plug_data().
on_simulator_stop( _PluginData ) ->
	notify( "simulator stopped" ),
	ok.



% Callback triggered when the simulator execution stopped under normal
% circumstances (i.e. not crashing).
%
-spec on_case_specific_event( sim_diasca_plugin:case_specific_event(),
							  sim_diasca_plugin:event_data(), plug_data() ) ->
									plug_data().
on_case_specific_event( _CaseSpecificEvent, _EventData,  _PluginData ) ->

	% Currently disabled, as too verbose, and duplicating traces already sent
	% from the simulation case:
	%
	%notify( io_lib:format( "[~s] ~s", [ CaseSpecificEvent, EventData ] ) ),

	ok.




% Helper section.


% Helper.
notify( _Message ) ->

	% We can even use our dedicated trace sub-channel:

	% Here we both output the message on the console and in our dedicated trace
	% sub-channel:
	%
	% (no mute variable here: not wanting spurious matchings)
	%
	% Parameters: Message, EmitterName, EmitterCategorization,
	% MessageCategorization
	%

	%?notify_em( Message, "my_plugin_example", "Core.PluginManagement",
	%			"Uncategorized" ).

	% Here we just send a (maskable) trace, no console output:
	%?notify_info_em( Message, "my_plugin_example", "Core.PluginManagement",
	%			"Uncategorized" ),

	ok.
