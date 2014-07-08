% Copyright (C) 2012-2014 EDF R&D

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


% This is the root Sim-Diasca module, to be called from most simulation cases,
% hiding the details of the simulation services being used underneath, and
% offering opportunities for single-place updates with no impact on existing
% cases.
%
-module(sim_diasca).


-export([ init/3, run_simulation/2, run_simulation_and_browse_results/2,
		  shutdown/0 ]).


% For notify_warning_fmt and al:
-include("traces.hrl").


% For simulation_settings:
-include("class_TimeManager.hrl").


% For deployment_settings:
-include("class_DeploymentManager.hrl").


% For load_balancing_settings:
-include("class_LoadBalancer.hrl").


% For user_process_name:
-include("sim_diasca.hrl").




% Initialises the engine according to specified settings.
%
% Returns the PID of the deployment manager.
%
% (function)
%
-spec init( simulation_settings(), deployment_settings(),
		   load_balancing_settings() ) -> pid().
init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ) ->

	% Detailed checking of this field done later, by the deployment manager:
	%
	case DeploymentSettings#deployment_settings.crash_resilience of

		K when is_integer( K ) andalso K > 0 ->
			% Here, an actual resilience is wanted. As a result, this current
			% process (i.e. the one of the simulation case) shall resist to any
			% node loss, thus must trap exits (ex: for initial - linked - actors
			% that were running on a crashed node).  However, process crashes
			% should not remain silent, thus EXIT messages will be searched for
			% later.
			process_flag( trap_exit, _ResistExitMsg=true );

		_ ->
			ok

	end,

	% We register this process (the one of the simulation case), so that I can
	% be found by others, like the resilience manager:
	%
	basic_utils:register_as( ?user_process_name, global_only ),


	% Simply returns this PID, for later use:
	%
	% (we kept the link with the user process corresponding to the simulation
	% case, as if no resilience had been requested we want to stop whenever a
	% node crashed, and with resilience enabled we trap exits, and are thus able
	% to detect crashes nevertheless)
	%
	class_DeploymentManager:synchronous_new_link( SimulationSettings,
			DeploymentSettings, LoadBalancingSettings, deploy_from_scratch ).



% Runs the actual simulation, until reaching the stop tick.
%
% (function)
%
run_simulation( StopTick, DeploymentManagerPid ) ->

	% As some processes (ex: the time manager) have the PID of this simulation
	% case process in their state, it must be declared too to the corresponding
	% instance tracker (now that it has been deployed):
	%
	class_InstanceTracker:register_agent( ?user_process_name ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },

	RootTimeManagerPid = traces:receive_applicative_message(),

	?notify_info_fmt( "Starting simulation, "
					  "for a stop at tick offset ~B.", [ StopTick ] ),

	RootTimeManagerPid ! { start, [ StopTick, self() ] },

	?notify_info( "Waiting for the simulation to end, "
				  "since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->
			?notify_info( "Simulation stopped spontaneously, "
						  "specified stop tick must have been reached." )

	end.



% Runs the actual simulation, until reaching the stop tick, and allows the user
% to browse the corresponding results, if it succeeded.
%
% (function)
%
run_simulation_and_browse_results( StopTick, DeploymentManagerPid ) ->

	run_simulation( StopTick, DeploymentManagerPid ),

	?notify_info( "Browsing the report results, if in batch mode." ),

	class_ResultManager:browse_reports().



% Shutdowns the engine.
%
% (function)
%
-spec shutdown() -> basic_utils:void().
shutdown() ->

	% Stateless, hence resilience-friendly.

	case basic_utils:is_registered( ?deployment_manager_name, global ) of

		not_registered ->
			ok;

		DeployPid ->
			class_DeploymentManager:shutdown( DeployPid )

	end,

	basic_utils:unregister( ?user_process_name, global_only ),

	check_exit_messages().



% Lists any EXIT messages that would linger in mailbox.
%
check_exit_messages() ->

	receive

		{ 'EXIT', _From, _Reason=normal } ->
			% Ignored:
			check_exit_messages();

		{ 'EXIT', From, Reason } ->
			?notify_warning_fmt( "process whose PID was ~w had exited "
								 "with reason '~p'.~n", [ From, Reason ] ),
			check_exit_messages()

	after 0 ->

			% Stop recursing:
			ok

	end.
