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


% Definition of the behaviour that shall be implemented by all Sim-Diasca
% plugins.
%
-module(sim_diasca_plugin).



% Callbacks are enumerated in the typical chronological order at which they
% shall be triggered on a usual, successful simulation setting.


% A plugin may wish to have a state kept by the engine.
%
% The initial state of all plugins is the atom 'undefined'.
%
% Note that we expect that, more often, stateful plugins will create processes
% to keep their state, these processes being registered in a naming service.
%
-type plugin_data() :: any().



% Events the plugins will be notified of (corresponds to the functions they
% shall all implement):
%
% (note: they are listed in their usual expected nominal order of appearance,
% although this order is not guaranteed, for example 'on_result_gathering_start'
% may happen before 'on_simulation_stop': race conditions are tolerated here)
%
-type plugin_event() ::  'on_simulator_start'
					   | 'on_deployment_start'
					   | 'on_deployment_stop'
					   | 'on_technical_settings_available'
					   | 'on_case_initialisation_start'
					   | 'on_case_initialisation_stop'
					   | 'on_simulation_start'
					   | 'on_simulation_bootstrap_start'
					   | 'on_simulation_bootstrap_stop'
					   | 'on_simulation_wallclock_milestone_met'
					   | 'on_simulation_tick_milestone_met'
					   | 'on_simulation_stop'
					   | 'on_result_gathering_start'
					   | 'on_result_gathering_stop'
					   | 'on_simulator_stop'
					   | 'on_case_specific_event'.


% Case-specific events can be used additionally:
%
-type case_specific_event() :: atom().


% For the configuration_changes, technical_settings records:
-include("sim_diasca_plugin.hrl").



-type configuration_changes() :: #configuration_changes{}.

-type technical_settings() :: #technical_settings{}.



% Any data sent with events allowing it:
%
-type event_data() :: term().


-export_type([ plugin_data/0, plugin_event/0, case_specific_event/0,
			   configuration_changes/0, technical_settings/0, event_data/0 ]).






% Callback triggered as soon as the simulator is started (or almost, as basic
% services, including the trace one, are already up).
%
% This is also the chance for a given plugin to request configuration changes.
%
% Configuration changes will be propagated amongst plugins, which will be
% notified in turn of these changes and will be given the opportunity to updated
% them.

% The on_technical_settings_available/2 callback could allow to check the
% effectiveness of this request (ex: if plugins requested incompatible changes).
%
-callback on_simulator_start( configuration_changes(), plugin_data() ) ->
	{ configuration_changes(), plugin_data() }.



% Callback triggered when the deployment phase starts.
%
-callback on_deployment_start( plugin_data() ) -> plugin_data().


% Callback triggered when the deployment phase stops.
%
-callback on_deployment_stop( plugin_data() ) -> plugin_data().


% Callback triggered when the simulation technical settings are available,
% notably once the deployment phase is over.
%
-callback on_technical_settings_available( technical_settings(),
										   plugin_data() ) -> plugin_data().



% Callback triggered when the simulation case starts the creation of the initial
% state of the simulation.
%
-callback on_case_initialisation_start( plugin_data() ) -> plugin_data().


% Callback triggered when the simulation case finished the creation of the
% initial state of the simulation.
%
-callback on_case_initialisation_stop( plugin_data() ) -> plugin_data().



% Callback triggered when the simulation is started (first tick, first diasca).
%
-callback on_simulation_start( plugin_data() ) -> plugin_data().


% Callback triggered when the simulation is just started and must evaluate the
% first diasca of all initial actors.
%
-callback on_simulation_bootstrap_start( plugin_data() ) -> plugin_data().


% Callback triggered when the evaluation of the first diasca of all initial
% actors is over.
%
-callback on_simulation_bootstrap_stop( plugin_data() ) -> plugin_data().



% Callback triggered when a wallclock milestone is met (i.e. when a given
% duration in real time elapsed).
%
-callback on_simulation_wallclock_milestone_met( unit_utils:milliseconds(),
						  plugin_data() ) -> plugin_data().


% Callback triggered when a tick milestone is met (i.e. when a given
% number of ticks have been evaluated).
%
-callback on_simulation_tick_milestone_met( class_TimeManager:tick_offset(),
						plugin_data() ) -> plugin_data().



% Callback triggered when the simulation is stopped (an ending criterion was
% just met; only called on successful ending).
%
-callback on_simulation_stop( plugin_data() ) -> plugin_data().



% Callback triggered when the results start being gathered, after simulation
% termination.
%
-callback on_result_gathering_start( plugin_data() ) -> plugin_data().


% Callback triggered when the results have been gathered.
%
-callback on_result_gathering_stop( plugin_data() ) -> plugin_data().



% Callback triggered when the simulator execution stopped under normal
% circumstances (i.e. not crashing).
%
-callback on_simulator_stop( plugin_data() ) -> plugin_data().



% Callback triggered iff a case decided to notify the plugins of a specific
% event.
%
-callback on_case_specific_event( case_specific_event(), event_data(),
								  plugin_data() ) -> plugin_data().
