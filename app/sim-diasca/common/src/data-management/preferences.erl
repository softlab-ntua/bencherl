% Copyright (C) 2013-2014 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Creation date: Thursday, October 31, 2013
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Service dedicated to the management of user-defined preferences.
%
% A preferences element is designated by a key (an atom), associated to a value
% (that can be any term).
%
% Preferences can be stored in file(s).
%
% This is typically a way of storing durable information in one's user account
% in a transverse way compared to programs and versions thereof, and of sharing
% them conveniently.
%
-module(preferences).


-export([ init/0, get/1, set/2, to_string/0 ]).


-type key() :: atom().


% Can be 'undefined' (no difference between a non-registered key and a key
% registered to 'undefined':
%
-type value() :: hashtable:value().


-type entry() :: hashtable:entry().
-type entries() :: hashtable:entries().


-export_type([ key/0, value/0, entry/0, entries/0 ]).



% Implementation notes:
%
% Preferences are managed through a singleton, globally registered process,
% maintaining an associative table whose content can be defined programmatically
% and/or thanks to data files.

% There is a potential race condition for the starting of this service: a
% process could trigger its creation while its creation is in progress, due to
% an earlier trigger.



% Name for global registration:
-define( preferences_server_name, ceylan_preferences_server ).


% Name of the preferences file (searched at the root of the user account):
-define( preferences_filename, ".ceylan-erlang-preferences.txt" ).


-define( hashtable_type, lazy_hashtable ).



% Ensures that, if not done already, the preferences service is initialised
% immediately, if wanting an explicit start rather than one implied by the use
% of an operation onto it.
%
% Returns in any case the PID of the corresponding preferences server.
%
-spec init() -> pid().
init() ->

	case basic_utils:is_registered( ?preferences_server_name, global ) of

		not_registered ->

			% A goal is to acquire the "lock" (the global name) ASAP, deferring
			% all possible other operations:
			%
			CallerPid = self(),

			% No sensible link to be created here, so we must beware of a silent
			% crash of this server:
			%
			spawn( fun() -> server_main_run( CallerPid ) end ),

			receive

				{ preference_server_pid, Pid } ->
					Pid

			end;

		Pid ->
			Pid

	end.



% Returns the value associated to specified key in the preferences (if any),
% otherwise 'undefined'.
%
-spec get( key() ) -> value().
get( Key ) ->

	ServerPid = init(),

	ServerPid ! { get_preference, Key, self() },

	receive

		{ notify_preference, V } ->
			V

	end.



% Associates, in current preferences, specified value to specified key (possibly
% overwriting any previous value).
%
-spec set( key(), value() ) -> basic_utils:void().
set( Key, Value ) ->

	ServerPid = init(),

	ServerPid ! { set_preference, Key, Value }.




%  Returns a textual description of the preferences server (if any).
%
-spec to_string() -> string().
to_string() ->

	case basic_utils:is_registered( ?preferences_server_name, global ) of

		not_registered ->
			"no preferences server is running";

		ServerPid ->
			ServerPid ! { to_string, self() },

			receive

				{ notify_preferences_status, PrefString } ->
					PrefString

			end

	end.



% Section for the preferences server itself.


% Launcher of the preference server.
%
server_main_run( SpawnerPid ) ->

	case basic_utils:register_or_return_registered( ?preferences_server_name,
												  global_only ) of

		registered ->

			% We gain the shared name, we are the one and only server:
			EmptyTable = ?hashtable_type:new(),

			PrefFilename = file_utils:join(
							 system_utils:get_user_home_directory(),
							 ?preferences_filename ),

			FinalTable = case file_utils:is_existing_file( PrefFilename ) of

				true ->
					add_preferences_from( PrefFilename, EmptyTable );

				false ->
					io:format( "No preferences file found "
							   "(searched for '~s').~n", [ PrefFilename ] ),
					EmptyTable

			end,

			% Spawner could already know that PID in this case:
			SpawnerPid ! { preference_server_pid, self() },

			% Never returns:
			server_main_loop( FinalTable );


		ServerPid ->
			% Notifies and terminates:
			SpawnerPid ! { preference_server_pid, ServerPid }

	end.



% Main loop of the preferences server.
%
server_main_loop( Hashtable ) ->

	%io:format( "Waiting for preferences-related request, "
	%		   "having ~B recorded preferences.~n",
	%		   [ ?hashtable_type:getEntryCount( Hashtable ) ] ),

	receive

		{ get_preference, Key, SenderPid } ->

			Answer = case ?hashtable_type:lookupEntry( Key, Hashtable ) of

				hashtable_key_not_found ->
							 undefined;

				{ value, V } ->
							 V

			end,

			SenderPid ! { notify_preference, Answer },

			server_main_loop( Hashtable );


		{ set_preference, Key, Value } ->

			NewTable = ?hashtable_type:addEntry( Key, Value, Hashtable ),

			server_main_loop( NewTable );


		{ to_string, SenderPid } ->

			Res = case ?hashtable_type:enumerate( Hashtable ) of

				[] ->
					"no preferences recorded";

				L ->

					% Enforces a consistent order:
					Strings = [ io_lib:format( "~p: ~p", [ K, V ] )
					   || { K, V } <- lists:sort( L ) ],

					io_lib:format( "~B preferences recorded:~s~n",
								 [ length( L ),
								 text_utils:string_list_to_string( Strings ) ] )

			end,

			SenderPid ! { notify_preferences_status, Res },

			server_main_loop( Hashtable )


	end.



% Helper functions.


% Adds preferences found in specified file into specified table, and returns it.
%
% (helper)
%
add_preferences_from( Filename, Table ) ->

	case file:consult( Filename ) of

		{ ok, Entries } ->

			case check_entries( Entries ) of

				ok ->
					NewTable = ?hashtable_type:addEntries( Entries, Table ),
					%io:format( "Loaded from preferences file '~s' "
					%           "following entries:~s",
					% [ PrefFilename, ?hashtable_type:toString( NewTable ) ] ),
				   io:format( "Preferences file '~s' loaded.~n", [ Filename ] ),
				   NewTable;

				ErrorString ->
					io:format( "Error when reading preferences file '~s' (~s), "
					   "no preferences read.~n", [ Filename, ErrorString ] ),
					Table

			end;


		{ error, { Line, _Mod, Term } } ->
			FlattenError = io_lib:format( "~p", [ Term ] ),
			io:format( "Error in preferences file '~s' at line ~B (~s), "
					   "no preferences read.~n",
					   [ Filename, Line, FlattenError ] ),
			Table;


		{ error, Reason } ->
			io:format( "Error when reading preferences file '~s' (~p), "
					   "no preferences read.~n", [ Filename, Reason ] ),
			Table

	end.


% Checks specified entries.
%
check_entries( _Entries=[] ) ->
	ok;

check_entries( _Entries=[ { K, _V } | T ] ) when is_atom( K ) ->
	check_entries( T );

check_entries( _Entries=[ { K, _V } | _T ] ) ->
	io_lib:format( "key '~p' is not an atom", [ K ] );

check_entries( _Entries=[ E | _T ] ) ->
	io_lib:format( "entry '~p' is not a key/value pair", [ E ] ).
