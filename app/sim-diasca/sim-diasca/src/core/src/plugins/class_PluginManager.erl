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



% Manager of the Sim-Diasca plugins, that allows third-party tools to interface
% to the engine.
%
-module(class_PluginManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, PluginDirectories ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, delete/1 ).


% Member method declarations.
-define( wooper_method_export, notifyEvent/2, notifySimulatorStart/1,
		 notifyParametrisedEvent/3, notifyCaseSpecificEvent/3 ).


% Static method declarations.
-define( wooper_static_method_export, notify/1, notify_simulator_start/0,
		 notify/2, notify_case_specific/2 ).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Core.PluginManagement").


-define( hashtable_type, lazy_hashtable ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For plugin_manager_name:
-include("class_PluginManager.hrl").


% For the configuration_changes record:
-include("sim_diasca_plugin.hrl").



% Where the plugin manager should be registered.
% Could be local_and_global or global_only as well:
%-define( registration_type, local_only ).
-define( registration_type, local_and_global ).




% Implementation notes.



% All state attributes of the plugin manager are explained here:
%
% - plugin_table :: ?hashtable_type:?hashtable_type( basic_utils:module_name(),
% term() ] is an associative table whose keys are the module name of each plugin
% (as an atom) and whose values are any state information returned by a given
% plugin


% Constructs a new plugin manager, from following parameters:
%
% - PluginDirectories :: [ file_utils:directory_name() ] is a list of the paths
% that should be looked into, in order to detect plugins
%
-spec construct( wooper_state(), [ file_utils:directory_name() ] ) ->
					   wooper_state().
construct( State, ?wooper_construct_parameters ) ->

	TraceState = class_TraceEmitter:construct( State, "Plugin Manager" ),

	% Then the class-specific actions:

	% Ensures also it is a singleton indeed:
	basic_utils:register_as( ?plugin_manager_name, ?registration_type ),

	InitState = setAttributes( TraceState, [

		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }

											   ] ),

	?send_info_fmt( InitState, "Initialising plugin manager, "
					"with following ~B plugin directories: ~s",
					[ length( PluginDirectories ),
					  text_utils:string_list_to_string( PluginDirectories ) ]
				  ),


	% List of absolute paths, extension-less BEAMs:
	Plugins = get_plugins_from( PluginDirectories, InitState ),

	% Keys are the plugin module names (as atoms), associated values start at
	% 'undefined':
	%
	PluginTable = create_initial_plugin_table( Plugins, InitState ),

	ModuleState = setAttribute( InitState, plugin_table, PluginTable ),

	load_plugins( Plugins, ModuleState ),

	ModuleState.



% Overridden destructor.
%
-spec delete( wooper_state() ) -> wooper_state().
delete( State ) ->

	%io:format( "Deleting load balancer ~w.~n", [ self() ] ),

	% Class-specific actions:
	?trace( "Deleting plugin manager." ),

	class_InstanceTracker:unregister_agent(),

	basic_utils:unregister( ?plugin_manager_name, ?registration_type ),

	?debug( "Plugin manager deleted." ),

	% Then allow chaining:
	State.





% Methods section.



% Requests all plugins to be specified of following standard event.
%
% (request, for synchronicity)
%
-spec notifyEvent( wooper_state(), sim_diasca_plugin:plugin_event() ) ->
						 request_return( 'event_notified' ).
notifyEvent( State, Event ) ->

	NewState = notify_event( Event, State ),

	?wooper_return_state_result( NewState, event_notified ).



% Requests all plugins to be specified of the start of the simulator, and give
% them a chance of updating the requested configuration changes.
%
% (request, for synchronicity)
%
-spec notifySimulatorStart( wooper_state() ) ->
		request_return( sim_diasca_plugin:configuration_changes() ).
notifySimulatorStart( State ) ->

	?info( "Notifying all plugins that the simulator starts, "
		   "giving them a chance of updating the requested "
		   "configuration changes." ),

	BlankConfChanges = #configuration_changes{},

	InitialPluginTable = ?getAttr(plugin_table),

	{ FinalChanges, FinalTable } = lists:foldl(
		fun( { Mod, PlugState }, { Changes, Table } ) ->

				{ NewChanges, NewPlugState } = apply( Mod,
							_Function=on_simulator_start,
							_Arg=[ Changes, PlugState ] ),

				% Update state:
				NewTable = ?hashtable_type:addEntry( _K=Mod,
												_V=NewPlugState, Table ),

				{ NewChanges, NewTable }

		end,
		_Acc0={ BlankConfChanges, InitialPluginTable },
		_List=?hashtable_type:enumerate( InitialPluginTable )

						  ),

	?info_fmt( "Final requested configuration changes: ~p.",
			   [ FinalChanges ] ),

	FinalState = setAttribute( State, plugin_table, FinalTable ),

	?wooper_return_state_result( FinalState, FinalChanges ).



% Requests all plugins to be specified of following parametrised event.
%
% (request, for synchronicity)
%
-spec notifyParametrisedEvent( wooper_state(),
							   sim_diasca_plugin:plugin_event(),
							   sim_diasca_plugin:event_data() ) ->
				 request_return( 'parametrised_event_notified' ).
notifyParametrisedEvent( State, Event, Parameters ) ->

	NewState = notify_parametrised_event( Event, Parameters, State ),

	?wooper_return_state_result( NewState, parametrised_event_notified ).



% Requests all plugins to be specified of following case-specific event, with
% its associated parameter.
%
% (request, for synchronicity)
%
-spec notifyCaseSpecificEvent( wooper_state(),
							   sim_diasca_plugin:case_specific_event(),
							   sim_diasca_plugin:event_data() ) ->
				 request_return( 'case_specific_event_notified' ).
notifyCaseSpecificEvent( State, CaseSpecificEvent, EventParameter ) ->

	NewState = notify_case_specific_event( CaseSpecificEvent, EventParameter,
										   State ),

	?wooper_return_state_result( NewState, case_specific_event_notified ).




% Helper functions.


% Returns a list of plugins found, as a list of the extension-less absolute
% paths of the plugin modules.
%
-spec get_plugins_from( [ file_utils:directory_name() ], wooper_state() ) ->
							  [ string() ].
get_plugins_from( PluginDirectories, State ) ->
	get_plugins_from( PluginDirectories, State, _AccPlugins=[] ).



% Helper:
%
get_plugins_from( _PluginDirectories=[], _State, AccPlugins ) ->
	AccPlugins;

get_plugins_from( _PluginDirectories=[ Dir | T ], State, AccPlugins ) ->

	AbsDir = file_utils:ensure_path_is_absolute( Dir ),

	case file_utils:is_existing_directory( AbsDir ) of

		true ->
			NewPlugins = case get_plugins_from_dir( AbsDir ) of

				[] ->
					?debug_fmt( "No plugin found in directory '~s'.",
								[ AbsDir ] ),
					[];

				Plugins ->
					?debug_fmt( "~B plugin(s) found in directory '~s': ~p.",
								[ length( Plugins ), AbsDir, Plugins ] ),
					Plugins

			end,

			get_plugins_from( T, State, NewPlugins ++ AccPlugins );

		false ->
			?debug_fmt( "Plugin directory '~s' does not exist.", [ AbsDir ] ),
			get_plugins_from( T, State, AccPlugins )

	end.



% Returns a list of the BEAM files (absolute paths, but with their extension
% removed) found in the specified directory.
%
% (helper)
%
-spec get_plugins_from_dir( file_utils:directory_name() ) ->
								  [ file_utils:file_name() ].
get_plugins_from_dir( DirectoryName ) ->

	% First, select all BEAM regular files:
	{ Files, _Dirs, _Others, _Devs } = file_utils:list_dir_elements(
										 DirectoryName ),

	Beams = file_utils:filter_by_extension( Files, ".beam" ),

	% Then remove their extension (to specify moduels) and make them absolute
	% paths:
	%
	Modules = [ file_utils:replace_extension( _Filename=B,
		   _SourceExtension=".beam", _TargetExtension="" ) || B <- Beams ],

	% Full paths needed:
	[ file_utils:join( DirectoryName, M ) || M <- Modules ].



% Initialises and returns the plugin table.
%
% (helper)
%
create_initial_plugin_table( Plugins, State ) ->

	case length( Plugins ) of

		0 ->
			?info( "Plugin manager started, but no plugin found." ),
			?hashtable_type:new();

		L ->

			StringModules = [ filename:basename( P ) || P <- Plugins ],

			?info_fmt( "Plugin manager started, with ~B plugin(s):~s",
				[ L, text_utils:string_list_to_string( StringModules )  ] ),

			Modules = [ text_utils:string_to_atom( S ) || S <- StringModules ],

			EmptyTable = ?hashtable_type:new(),

			lists:foldl( fun( Mod, Table ) ->
							% Initial plugin state is undefined:
							?hashtable_type:addEntry( _K=Mod, _V='undefined',
													  Table )
						 end,
						 _Acc0=EmptyTable,
						 _List=Modules )

	end.



% Loads specified plugins.
%
load_plugins( Plugins, State ) ->
	[ load_plugin( P, State ) || P <- Plugins ].


% Loads specified plugin.
%
load_plugin( Plugin, State ) ->

	% No need to tweak the code paths:
	case code:load_abs( Plugin ) of

		{ error, Reason } ->
			?error_fmt( "Loading of plugin '~s' failed: ~s.",
						[ Plugin, Reason ] ),
			throw( { plugin_loading_failed, Plugin, Reason } );

		{ module, Module } ->
			?debug_fmt( "Plugin '~s' successfully loaded from '~s'.",
					[ Module, filename:dirname( Plugin ) ] )

	end.



% Notifies known plugins of specified event, returns an updated state.
%
% (helper)
%
notify_event( Event, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of event '~s'.", [ Event ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

					NewPlugState = apply( Mod, _Function=Event,
										  _Arg=[ PlugState ] ),

					% Update state:
					?hashtable_type:addEntry( _K=Mod, _V=NewPlugState, Table )

					end,
					_Acc0=PluginTable,
					_List=?hashtable_type:enumerate( PluginTable )

						  ),

	setAttribute( State, plugin_table, NewTable ).



% Notifies known plugins of specified parametrised event, returns an updated
% state.
%
% (helper)
%
notify_parametrised_event( Event, Parameters, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of event '~s' "
			   "parametrised with '~p'.", [ Event, Parameters ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

					NewPlugState = apply( Mod,
						_Function=Event,
						_Arg=[ Parameters, PlugState ] ),

					% Update state:
					?hashtable_type:addEntry( _K=Mod, _V=NewPlugState, Table )

					end,
					_Acc0=PluginTable,
					_List=?hashtable_type:enumerate( PluginTable )

						  ),

	setAttribute( State, plugin_table, NewTable ).



% Notifies known plugins of specified case-specific event, returns an updated
% state.
%
% (helper)
%
notify_case_specific_event( CaseSpecificEvent, EventParameter, State ) ->

	PluginTable = ?getAttr(plugin_table),

	?info_fmt( "Notifying all plugins of case-specific event '~s' "
			   "with parameter '~p'.", [ CaseSpecificEvent, EventParameter ] ),

	NewTable = lists:foldl( fun( { Mod, PlugState }, Table ) ->

					NewPlugState = apply( Mod,
						_Function=on_case_specific_event,
						_Arg=[ CaseSpecificEvent, EventParameter, PlugState ] ),

					% Update state:
					?hashtable_type:addEntry( _K=Mod, _V=NewPlugState, Table )

					end,
					_Acc0=PluginTable,
					_List=?hashtable_type:enumerate( PluginTable )

						  ),

	setAttribute( State, plugin_table, NewTable ).



% Static section.


% To notify from any place the plugin manager of an event.
%
% (static)
%
-spec notify( sim_diasca_plugin:plugin_event() ) -> basic_utils:void().
notify( Event ) ->

	PluginManagerPid = basic_utils:get_registered_pid_for(
						 ?plugin_manager_name, _Scope=global ),


	PluginManagerPid ! { notifyEvent, [ Event ], self() },

	receive

		{ wooper_result, event_notified } ->
			ok

	end.



% Notifies that the simulator started, in order to allow plugins to return
% requests for configuration changes.
%
-spec notify_simulator_start() -> sim_diasca_plugin:configuration_changes().
notify_simulator_start() ->

	PluginManagerPid = basic_utils:get_registered_pid_for(
						 ?plugin_manager_name, _Scope=global ),

	% Starts with blank changes:
	PluginManagerPid ! { notifySimulatorStart, [], self() },

	receive

		{ wooper_result, ConfigurationChanges } ->
			ConfigurationChanges

	end.



% To notify from any place the plugin manager of a parametrised event.
%
% (static)
%
-spec notify( sim_diasca_plugin:plugin_event(),
			  sim_diasca_plugin:event_data() ) -> basic_utils:void().
notify( Event, Parameters ) ->

	PluginManagerPid = basic_utils:get_registered_pid_for(
						 ?plugin_manager_name, _Scope=global ),


	PluginManagerPid ! { notifyParametrisedEvent, [ Event, Parameters ],
						 self() },

	receive

		{ wooper_result, parametrised_event_notified } ->
			ok

	end.



% To notify from any place the plugin manager of a case-specific event.
%
% (static)
%
-spec notify_case_specific( sim_diasca_plugin:case_specific_event(),
		sim_diasca_plugin:event_data() ) -> basic_utils:void().
notify_case_specific( Event, EventParameter ) ->
		PluginManagerPid = basic_utils:get_registered_pid_for(
						 ?plugin_manager_name, _Scope=global ),

	PluginManagerPid ! { notifyCaseSpecificEvent, [ Event, EventParameter ],
						 self() },

	receive

		{ wooper_result, case_specific_event_notified } ->
			ok

	end.
