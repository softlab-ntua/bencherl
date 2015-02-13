% Copyright (C) 2007-2014 Olivier Boudeville
%
% This file is part of the WOOPER library.
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
% Creation date: Friday, July 12, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Module of the WOOPER class manager.
%
% The purpose of this process is, on a per-node basis, to create and notably to
% serve to instances the virtual table corresponding to the actual class they
% are corresponding to.

% This way each virtual table is computed only once per node, and no significant
% per-instance memory is used for the virtual table: all the instances of a
% given class just refer to a common virtual table stored by this manager, and
% each virtual table and the table of virtual tables itself are optimised, with
% regard to their respective load factor.
%
-module(wooper_class_manager).



% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/



-export([ start/1, ping/1 ]).


% For wooper_class_manager_name:
-include("wooper_class_manager.hrl").


% For wooper_hashtable_type:
-include("wooper_defines_exports.hrl").



% Approximate average method count for a given class, including inherited ones.
%
% (ideally should be slightly above the maximum number of actual methods)
%
-define( wooper_method_count_upper_bound, 32 ).



% Approximate average class count for the program.
%
% (ideally should be slightly above the maximum number of actual classes being
% instanciated)
%
-define( wooper_class_count_upper_bound, 128 ).


% Comment/uncomment to respectively disable and enable debug mode:
%-define(debug,).


-define(log_prefix, "[WOOPER Class manager] ").


% To avoid warnings (note: display/1 is apparently a BIF, renamed to
% display_msg/1):
%
-export([ display_state/1, display_table_creation/1, display_msg/1 ]).




% Uncomment to activate debug mode:
%-define(wooper_debug_class_manager,).

-spec display_state( ?wooper_hashtable_type:?wooper_hashtable_type() ) ->
						   basic_utils:void().
-spec display_table_creation( basic_utils:module_name() ) -> basic_utils:void().
-spec display_msg( string() ) -> basic_utils:void().


-ifdef(wooper_debug_class_manager).


display_state( Tables ) ->
	error_logger:info_msg( ?log_prefix "Storing now ~B table(s).~n",
					[ ?wooper_hashtable_type:getEntryCount( Tables ) ] ).


display_table_creation( Module ) ->
	error_logger:info_msg( ?log_prefix "Creating a virtual table "
							"for module ~s.~n", [ Module ] ).


display_msg( String ) ->

	Message = io_lib:format( ?log_prefix "~s.~n", [ String ] ),
	error_logger:info_msg( Message ).


-else.


display_state( _Tables ) ->
	ok.

display_table_creation( _Module ) ->
	ok.

display_msg( _String ) ->
	ok.


-endif.



% Starts a new blank class manager.
%
-spec start( pid() ) -> basic_utils:void().
start( ClientPID ) ->

	display_msg( io_lib:format( "Starting WOOPER class manager "
						"on node ~s (PID: ~w)", [ node(), self() ] ) ),

	% Two first instances being created nearly at the same time might trigger
	% the creation of two class managers, if the second instance detects no
	% manager is registered while the first manager is created but not
	% registered yet. That would result in superflous class managers. Up to one
	% should exist.
	%
	% Note: as the register call cannot be performed at once (not an atomic
	% operation), there must remain a tiny window for a race condition to
	% happen).
	%
	% Local registration only, as we want the instances to benefit from a local
	% direct reference to the same method table, rather waste memory with one
	% copy of the table per instance.
	%
	% In a distributed context, there should be exactly one class manager per
	% node.
	%
	case catch register( ?wooper_class_manager_name, self() ) of

		true ->
			ClientPID ! class_manager_registered,

			EmptyClassTable = ?wooper_hashtable_type:new(
								 ?wooper_class_count_upper_bound ),

			loop( EmptyClassTable );


		% A manager is already registered, let it be the only one and stop:
		{ 'EXIT', { badarg,_ } } ->

			display_msg( ?log_prefix
						"Already a manager available, terminating" ),

			% Let's notify the client nevertheless:
			ClientPID ! class_manager_registered
			% The instances should use the first manager only.
			% (no looping performed, terminating this second manager).

	end.



% Manager main loop, serves virtual tables on request (mostly on instances
% creation).
%
-spec loop( ?wooper_hashtable_type:?wooper_hashtable_type() ) ->
				  no_return() | 'ok' .
loop( Tables ) ->

	display_state( Tables ),

	receive

		{ get_table, Module, Pid } ->
			{ NewTables, TargetTable } = get_virtual_table_for( Module,
															   Tables ),
			Pid ! { virtual_table, TargetTable },
			loop( NewTables );

		display ->
			error_logger:info_msg( ?log_prefix "Internal state is: ~s~n",
				[ ?wooper_hashtable_type:toString( Tables ) ] ),
			loop( Tables );

		stop ->
			unregister( ?wooper_class_manager_name ),
			display_msg( "Stopped on request" )

	end.



% Look-up specified table.
%
% If found, returns it immediately, otherwise constructs it, stores the result
% and returns it as well.
%
% Virtual tables are stored in a ?wooper_hashtable_type.
%
% Returns a pair formed of the new set of virtual tables and of the requested
% table.
%
-spec get_virtual_table_for( basic_utils:module_name(),
							?wooper_hashtable_type:?wooper_hashtable_type() )
		-> { ?wooper_hashtable_type:?wooper_hashtable_type(),
			 ?wooper_hashtable_type:?wooper_hashtable_type() }.
get_virtual_table_for( Module, Tables ) ->

	case ?wooper_hashtable_type:lookupEntry( Module, Tables ) of

		{ value, Table } ->

			% Cache hit, no change in internal data:
			{ Tables, Table };

		hashtable_key_not_found ->

			% Time to create this virtual table and to store it:
			display_table_creation( Module ),
			ModuleTable = create_method_table_for( Module ),

			% Each class has its virtual table optimised:
			OptimisedModuleTable = ?wooper_hashtable_type:optimise(
															   ModuleTable ),

			% Here the table could be patched with destruct/1, if defined.
			ClassTable = ?wooper_hashtable_type:addEntry( Module,
											 OptimisedModuleTable, Tables ),

			% And the table of virtual tables is itself optimised each time a
			% new class is introduced:
			{ ?wooper_hashtable_type:optimise( ClassTable ),
			 OptimisedModuleTable }

	end.




% Creates recursively (indirectly thanks to 'update_method_table_with') the
% virtual table corresponding to specified module.
%
-spec create_method_table_for( basic_utils:module_name() ) ->
						?wooper_hashtable_type:?wooper_hashtable_type().
create_method_table_for( TargetModule ) ->

	lists:foldl(

		fun( Module, Hashtable ) ->
			update_method_table_with( Module, Hashtable )
		end,

		create_local_method_table_for( TargetModule ),

		apply( TargetModule, get_superclasses, [] ) ).



% Updates specified virtual table with the method of specified module
% (i.e. precomputes the virtual table for the related class).
%
% In case of key collision, the values specified in ?Wooper_Hashtable_Type have
% priority over the ones relative to Module. Hence methods redefined in child
% classes are selected, rather than the ones of the mother class.
%
-spec update_method_table_with( basic_utils:module_name(),
		  ?wooper_hashtable_type:?wooper_hashtable_type() ) ->
						  ?wooper_hashtable_type:?wooper_hashtable_type().
update_method_table_with( Module, Hashtable ) ->
	?wooper_hashtable_type:merge( Hashtable,
								 create_method_table_for( Module ) ).



% Tells whether the function Name/Arity should be registered into the method
% virtual table.
%
select_function( _,0 )                                                -> false ;
select_function( new,_ )                                              -> false ;
select_function( new_link,_ )                                         -> false ;
select_function( synchronous_new,_ )                                  -> false ;
select_function( synchronous_new_link,_ )                             -> false ;
select_function( synchronous_timed_new,_ )                            -> false ;
select_function( synchronous_timed_new_link,_ )                       -> false ;
select_function( remote_new,_ )                                       -> false ;
select_function( remote_new_link,_ )                                  -> false ;
select_function( remote_synchronous_new,_ )                           -> false ;
select_function( remote_synchronous_new_link,_ )                      -> false ;
select_function( remote_synchronous_timed_new,_ )                     -> false ;
select_function( remote_synchronous_timed_new_link,_ )                -> false ;
select_function( construct,_ )                                        -> false ;
select_function( destruct,1 )                                         -> false ;
select_function( delete_any_instance_referenced_in,_ )                -> false ;
select_function( delete_synchronously_any_instance_referenced_in,_ )  -> false ;
select_function( delete_synchronously_instances,_ )                   -> false ;
select_function( wooper_check_undefined,_ )                           -> false ;
select_function( wooper_construct_and_run,_ )                         -> false ;
select_function( wooper_construct_and_run_synchronous,_ )             -> false ;
select_function( wooper_debug_listen,_ )                              -> false ;
select_function( wooper_destruct,_ )                                  -> false ;
select_function( wooper_display_instance,_ )                          -> false ;
select_function( wooper_display_loop_state,_ )                        -> false ;
select_function( wooper_display_state,_ )                             -> false ;
select_function( wooper_display_virtual_table,_ )                     -> false ;
select_function( wooper_get_all_attributes,_ )                        -> false ;
select_function( wooper_get_state_description,_ )                     -> false ;
select_function( wooper_get_virtual_table_description,_ )             -> false ;
select_function( wooper_pop_from_attribute,_ )                        -> false ;
select_function( executeOneway,_ )                                    -> false ;
select_function( executeRequest,_ )                                   -> false ;
select_function( executeOnewayWith,_ )                                -> false ;
select_function( executeRequestWith,_ )                               -> false ;
select_function( module_info,1)                                       -> false ;
% Includes 'wooper_get_instance_description/1', which could be useful to debug:
select_function( _, _ )                                               -> true.




% Returns an hashtable appropriate for method look-up, for the specified module.
-spec create_local_method_table_for( basic_utils:module_name() ) ->
							   ?wooper_hashtable_type:?wooper_hashtable_type().
create_local_method_table_for( Module ) ->

	% Filter-out functions that should not be callable via RMI:
	lists:foldl(

		% Filter-out functions that should not be callable via RMI:
		fun( { Name, Arity }, Hashtable ) ->
			case select_function(  Name, Arity ) of

				true ->
					?wooper_hashtable_type:addEntry( { Name, Arity }, Module,
													Hashtable );

				false ->
					Hashtable

			end
		end,

		?wooper_hashtable_type:new( ?wooper_method_count_upper_bound ),

		Module:module_info( exports ) ).



% ping specified WOOPER instance, returns pong if it could be successfully
% pinged, otherwise returns pang.
%
-spec ping( atom() | pid() ) -> 'pang' | 'pong'.
ping( Target ) when is_pid( Target ) ->

	Target ! { ping,  self() },
	receive

		{ pong, Target } ->
			pong

		after 500 ->
			pang

	end;

ping( Target ) when is_atom( Target ) ->

	case global:whereis_name( Target ) of

		undefined ->
			case whereis( Target ) of
				undefined ->
					pang;

				Pid ->
					ping( Pid )
			end;

		Pid ->
			ping( Pid )

	end.
