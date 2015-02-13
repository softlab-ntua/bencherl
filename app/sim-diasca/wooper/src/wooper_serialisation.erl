% Copyright (C) 2012-2014 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Module containing facilities for the serialisation and deserialisation of
% WOOPER instances.
%
% Note: offseting as much as possible code from the counterpart class header in
% this module allows for smaller, cache-friendly BEAMs, and short compilation
% times.
%
-module(wooper_serialisation).




% First, all kinds of exports:


% For getAttr/1, and for setAttributes/2 and all being unused:
-include("wooper_state_exports.hrl").

% For synchronous_time_out:
-include("wooper_defines_exports.hrl").

% For attribute_name/0:
-include("wooper_types_exports.hrl").

% To silence getClassName/1, get_superclasses/0 not being used:
-include("wooper_classes_exports.hrl").

% Otherwise executeRequest/3 and all reported as unused:
-include("wooper_execute_exports.hrl").

% To silence wooper_execute_method_with/4 being unused:
-include("wooper_execute_internal_exports.hrl").

% To silence wooper_execute_method_with/4 and all being unused:
-include("wooper_serialisation_exports.hrl").



% Instance loading:
%
-export([

		  load/1, load/3, load_link/1, load_link/3,

		  synchronous_load/1, synchronous_load/3, synchronous_load_link/1,
		  synchronous_load_link/3,

		  remote_synchronisable_load_link/2, remote_synchronisable_load_link/4,
		  remote_synchronous_timed_load_link/2,
		  remote_synchronous_timed_load_link/4

		]).



% Instance deserialisation:
%
-export([ deserialise/4 ]).



% Serialisation helpers:
%
-export([ handle_private_processes/2, mute_attributes/2,
		  check_attributes_equal/3, replace_attribute/3, replace_attributes/3,
		  merge_list_for/3, merge_lists_for/3,
		  list_restoration_markers/0
		]).



% For list_impl:
-include("data_types.hrl").



% Any function that is able to transform the state of an instance.
%
% Note: see basic_utils:traverse_term/4, which may be useful in that context.
%
-type entry_transformer() :: 'undefined' |
			fun( ( attribute_entry(), basic_utils:user_data() ) ->
					 { attribute_entry(), basic_utils:user_data() } ).



% The serialisation form of an instance, as an Erlang term:
%
-type term_serialisation() :: [ attribute_entry() ].


% The serialisation form of an instance, as an ext_binary, i.e. a binary data
% object, structured according to the Erlang external term format:
%
% However erlang:ext_binary/0 is not exported:
%-type bin_serialisation() :: erlang:ext_binary().
%
-type bin_serialisation() :: binary().


-export_type([ entry_transformer/0, term_serialisation/0,
			   bin_serialisation/0 ]).


-type restoration_marker() :: ?process_restoration_marker
							| ?file_restoration_marker
							| ?term_restoration_marker.


% Dummy entry for this special case:
-define( wooper_superclasses, [] ).




% Now, function definitions:


% For executeRequest/2:
-include("wooper_execute_functions.hrl").

% For getAttribute/2, setAttribute/3, etc.:
-include("wooper_state_functions.hrl").


% For wooper_deserialise/4:
-include("wooper_serialisation_functions.hrl").


% For wooper_execute_method/3:
-include("wooper_execute_internal_functions.hrl").


% For get_superclasses/1:
-include("wooper_classes_functions.hrl").





% In this section, we define for the current class load counterparts to new
% operators, i.e. the various ways of creating an instance of it not through a
% normal construction process, but from a deserialisation one (loading).
%
% For example, synchronous_new_link becomes synchronous_load_link.


% Transformers are expected not to depend on the order of their calls, as
% loadings can happen in parallel.

% No updated user data is ever sent back, for the sake of API uniformity.




% Spawns a new instance of this class, based on the specified serialisation
% information.
%
% Returns the PID of the created instance for this loading.
%
% Creation is asynchronous: this function returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec load( bin_serialisation() ) -> pid().
load( BinSerialisation ) ->
	load( BinSerialisation, _EntryTransformer=undefined, _UserData=undefined ).



% Spawns a new instance of this class, based on the specified serialisation
% information, entry transformer and user data.
%
% Returns the PID of the created instance for this loading.
%
% Creation is asynchronous: this function returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec load( bin_serialisation(), entry_transformer(), basic_utils:user_data() )
		  -> pid().
load( BinSerialisation, EntryTransformer, UserData ) ->

	spawn( fun() -> deserialise( BinSerialisation, EntryTransformer,
										UserData, _ListenerPid=undefined )
		   end ).



% Spawns a new instance of this class, based on the specified serialisation
% information, and links it to the current process.
%
% Returns the PID of the created instance for this loading.
%
% Creation is asynchronous: this function returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec load_link( bin_serialisation() ) -> pid().
load_link( BinSerialisation ) ->
	load_link( BinSerialisation, _EntryTransformer=undefined,
			   _UserData=undefined ).



% Spawns a new instance of this class, based on the specified serialisation
% information, entry transformer and user data, and links it to the current
% process.
%
% Returns the PID of the created instance for this loading.
%
% Creation is asynchronous: this function returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec load_link( bin_serialisation(), entry_transformer(),
				 basic_utils:user_data() ) -> pid().
load_link( BinSerialisation, EntryTransformer, UserData ) ->
	spawn_link( fun() -> deserialise( BinSerialisation, EntryTransformer,
										UserData, _ListenerPid=undefined )
				end ).



% Spawns a new instance of this class, based on the specified serialisation
% information.
%
% Returns the PID of the created instance for this loading.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec synchronous_load( bin_serialisation() ) -> pid().
synchronous_load( BinSerialisation ) ->
	synchronous_load( BinSerialisation, _EntryTransformer=undefined,
					  _UserData=undefined ).



% Spawns a new instance of this class, based on the specified serialisation
% information, entry transformer and user data.
%
% Returns the PID of the created instance for this loading.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec synchronous_load( bin_serialisation(), entry_transformer(),
						basic_utils:user_data() ) -> pid().
synchronous_load( BinSerialisation, EntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> deserialise( BinSerialisation,
						  EntryTransformer, UserData, _ListenerPid=CreatorPid )
						end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.




% Spawns a new instance of this class, based on the specified serialisation
% information, and links it to the current process.
%
% Returns the PID of the created instance for this loading.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec synchronous_load_link( bin_serialisation() ) -> pid().
synchronous_load_link( BinSerialisation ) ->
	synchronous_load_link( BinSerialisation, _EntryTransformer=undefined,
			   _UserData=undefined ).



% Spawns a new instance of this class, based on the specified serialisation
% information, entry transformer and user data, and links it to the current
% process.
%
% Returns the PID of the created instance for this loading.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec synchronous_load_link( bin_serialisation(), entry_transformer(),
				 basic_utils:user_data() ) -> pid().
synchronous_load_link( BinSerialisation, EntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> deserialise( BinSerialisation,
					   EntryTransformer, UserData, _ListenerPid=CreatorPid )
							end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% We did not feel the specific need to define:
%
% - synchronous_timed_load
% - synchronous_timed_load_link
% - remote_load
% - remote_load_link
% - remote_synchronous_load
% - remote_synchronous_load_link
% - remote_synchronous_timed_load
%
% (but they can be added if wanted)




% Spawns on specified node a new instance of this class, based on the specified
% serialisation information and links it to the current process.
%
% Returns the PID of the created instance.
%
% Creation is asynchronous (the PID is directly returned), however a {
% spawn_successful, SpawnedPid } message will be received once (if ever) the
% instance is up and running. This allows to perform the actual instance
% creations in parallel, by waiting bulks of creations.
%
-spec remote_synchronisable_load_link( net_utils:node_name(),
									   bin_serialisation() ) -> pid().
remote_synchronisable_load_link( Node, BinSerialisation ) ->
	remote_synchronisable_load_link( Node, BinSerialisation,
					_EntryTransformer=undefined, _UserData=undefined ).


% Spawns on specified node a new instance of this class, based on the specified
% serialisation information, entry transformer and user data, and links it to
% the current process.
%
% Returns the PID of the created instance.
%
% Creation is asynchronous (the PID is directly returned), however a {
% spawn_successful, SpawnedPid } message will be received once (if ever) the
% instance is up and running. This allows to perform the actual instance
% creations in parallel, by waiting bulks of creations.
%
-spec remote_synchronisable_load_link( net_utils:node_name(),
   bin_serialisation(), entry_transformer(), basic_utils:user_data() ) -> pid().
remote_synchronisable_load_link( Node, BinSerialisation, EntryTransformer,
								 UserData ) ->

	CreatorPid = self(),

	spawn_link( Node,
			fun() -> deserialise( BinSerialisation, EntryTransformer,
										 UserData, _ListenerPid=CreatorPid )
			end ).



% Spawns on specified node a new instance of this class, based on the specified
% serialisation information, entry transformer and user data, and links it to
% the current process.
%
% Returns the PID of the created instance for this loading, or the time_out
% atom.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec remote_synchronous_timed_load_link( net_utils:node_name(),
		   bin_serialisation() ) -> pid().
remote_synchronous_timed_load_link( Node, BinSerialisation ) ->
	remote_synchronous_timed_load_link( Node, BinSerialisation,
			_EntryTransformer=undefined, _UserData=undefined ).



% Spawns on specified node a new instance of this class, based on the specified
% serialisation information, entry transformer and user data, and links it to
% the current process.
%
% Returns the PID of the created instance for this loading, or the time_out
% atom.
%
% Creation is synchronous: the call will return only when the created process
% reports that it is up and running.
%
-spec remote_synchronous_timed_load_link( net_utils:node_name(),
   bin_serialisation(), entry_transformer(), basic_utils:user_data() ) -> pid().
remote_synchronous_timed_load_link( Node, BinSerialisation, EntryTransformer,
									UserData ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node,
			fun() -> deserialise( BinSerialisation, EntryTransformer,
										 UserData, _ListenerPid=CreatorPid )
			end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		io:format( "(remote_synchronous_timed_load_link: throwing time-out "
				   "on node ~p for module ~p after ~p milliseconds)~n",
				   [ Node, ?MODULE, ?synchronous_time_out ] ),

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.




% Deserialises the specified instance from its serialised form (as a term, not
% as a binary), to obtain its corresponding state, using specified entry
% transformer and user data, then having the current executing process embody
% this instance from then on.
%
% Does not return, as the WOOPER main loop will manage from then this just
% deserialised instance.
%
% (helper, as the receiver process may not even be already a WOOPER instance)
%
% Note: the hosting process is not created here, as, for an increased
% parallelism, we expect deserialisations to happen directly from the final
% instance processes; we consider here that the process executing this helper is
% the final host.
%
-spec deserialise( bin_serialisation(), entry_transformer(),
				   basic_utils:user_data(), ListenerPid ) -> any() % no_return()
						   when ListenerPid :: basic_utils:maybe( pid() ).
deserialise( BinSerialisation, EntryTransformer, UserData, ListenerPid ) ->

	{ Classname, SerialisedEntries } = binary_to_term( BinSerialisation ),

	% First we extract the WOOPER extra information:
	{ RandomState, OtherEntries } = option_list:extract( wooper_random_state,
													SerialisedEntries ),

	HookedEntries = pre_deserialise_hook( { Classname, OtherEntries },
											  UserData ),

	% Slight optimisation compared to using wooper:retrieve_virtual_table/0
	% upfront later:
	%
	wooper:get_class_manager() ! { get_table, Classname, self() },

	{ TransformedEntries, FinalUserData } = case EntryTransformer of

			undefined ->
				{ HookedEntries, UserData };

			_ ->
				lists:foldl( EntryTransformer,
							 _Acc0={ _ResultingEntries=[], UserData },
							 _List=HookedEntries )

	end,

	% Sent as soon as available, rather than at the end:
	case ListenerPid of

		undefined ->
			ok;

		_->
			ListenerPid ! { onDeserialisation, [ self(), FinalUserData ] }

	end,

	% Now we have the right attributes enumerated.

	% We need to bypass any constructor here.

	AttributeTable = ?wooper_hashtable_type:addEntries( TransformedEntries,
										?wooper_hashtable_type:new() ),

	OptimisedAttributeTable = ?wooper_hashtable_type:optimise( AttributeTable ),

	% Must be restored as well:
	case RandomState of

		undefined ->
			ok;

		_ ->
			random_utils:set_random_state( RandomState )

	end,


	% Deferred get_table answer:
	VirtualTable = receive

		{ virtual_table, Table } ->
			Table

	end,

	ForgedState = #state_holder{

		virtual_table   = VirtualTable,

		attribute_table = OptimisedAttributeTable,

		actual_class    = Classname,

		request_sender  = undefined

	 },


	% We could check here that no serialisation marker remains, with a specific
	% entry transformer and list_restoration_markers/0.

	FinalState = post_deserialise_hook( ForgedState ),


	% That's as simple as that!

	apply( Classname, wooper_main_loop, [ FinalState ] ).





% Handles private processes (through the name of the corresponding attributes),
% i.e. processes that are internal to an instance that is to be serialised, so
% that any next serialisation will see instead of their (former) PID a
% serialisation marker.
%
% Returns an updated state.
%
% (helper, typically used in pre_serialise_hook/1, to avoid trying to serialise
% internal processes)
%
-spec handle_private_processes( [ attribute_name() ], wooper:state() ) ->
									  wooper:state().
handle_private_processes( PrivateAttributeNames, State ) ->

	lists:foldl( fun( PrivateAttrName, AccState ) ->

			NewValue = case getAttribute( AccState, PrivateAttrName ) of

				undefined ->
					undefined;

				Pid when is_pid( Pid ) ->
					% We just hide these PIDs on the serialised form: after
					% serialisation the live state will still reference them.
					?process_restoration_marker

			end,

			setAttribute( AccState, PrivateAttrName, NewValue )


				end,

				_Acc0=State,

				_List=PrivateAttributeNames ).




% Mutes specified attributes (i.e. replaces any attribute value not equal to
% 'undefined' by a term restoration marker), for example so that they can escape
% the serialisation process.
%
% Typically used in pre_serialise_hook/2, to store only relevant, useful
% information.
%
% (helper)
%
-spec mute_attributes( [ attribute_name() ], wooper:state() ) -> wooper:state().
mute_attributes( AttributeNameList, State ) ->

	lists:foldl( fun( AttrName, AccState ) ->

					case hasAttribute( AccState, AttrName ) of

						true ->

							case getAttribute( AccState, AttrName ) of

								undefined ->
									% Let it as is:
									AccState;

								_ ->
									setAttribute( AccState, AttrName,
												 ?term_restoration_marker )

							end;

						false ->
							throw( { unknown_attribute, AttrName, AccState } )

					end

				 end,

				 _Acc0=State,

				_List=AttributeNameList ).



% Checks that the specified attributes have the same value in the specified
% state and in the specified entries, otherwise throws an exception.
%
% (helper)
%
-spec check_attributes_equal( [ attribute_name() ],
		 [ attribute_entry() ], wooper:state() ) -> basic_utils:void().
check_attributes_equal( _AttributeNames=[], _AttributeEntries,
							  _State ) ->
	ok;

check_attributes_equal( _AttributeNames=[ AttributeName | T ],
							  AttributeEntries, State ) ->

	{ AttributeValue, RemainingEntries } = option_list:extract(
								 _K=AttributeName, AttributeEntries ),

	case ?getAttr(AttributeName) of

		AttributeValue ->
			check_attributes_equal( T, RemainingEntries, State );

		OtherValue ->
			throw( { attribute_value_mismatch, AttributeName,
					{ OtherValue, AttributeValue } } )

	end.



% Replaces the value held in the specified state by the one of the specified
% attribute found in specified entry.
%
% (helper)
%
-spec replace_attribute( attribute_name(), [ attribute_entry() ],
			 wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attribute( AttributeName, AttributeEntries, State ) ->

	case hasAttribute( State, AttributeName ) of

		true ->

			{ ToSetValue, RemainingEntries } = option_list:extract(
								 _K=AttributeName, AttributeEntries ),

			NewState = setAttribute( State, AttributeName, ToSetValue ),

			{ RemainingEntries, NewState };


		false ->
			throw( { unknown_attribute, AttributeName, State } )

	end.



% Replaces the values held in the specified state by the ones of the specified
% attributes found in specified entries.
%
% (helper)
%
-spec replace_attributes( [ attribute_name() ], [ attribute_entry() ],
				wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attributes( AttributeNames, AttributeEntries, State ) ->

	lists:foldl( fun( AttrName, { AccEntries, AccState } ) ->
					replace_attribute( AttrName, AccEntries, AccState )
			end,

			_Acc0={ AttributeEntries, State },

			_List=AttributeNames ).



% Extracts the value (supposedly, any type of list) of specified attribute from
% specified entries, and append that list to the corresponding one found in the
% specified state, stored under the same attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_list_for( attribute_name(), [ attribute_entry() ],
			wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_list_for( AttributeName, AttributeEntries, State ) ->

	{ ToMergeValue, RemainingEntries } = option_list:extract( _K=AttributeName,
										AttributeEntries ),

	InitialValue = ?getAttr(AttributeName),

	MergedValue = case ToMergeValue of

		PlainList when is_list( PlainList ) ->
			InitialValue ++ PlainList;

		% We suppose it is a ?list_impl:
		%
		% (note that their behaviour differs from the one of plain lists - there
		% are no duplicates in list_impl)
		ListImpl ->
			?list_impl:union( InitialValue, ListImpl )

	end,

	MergedState = setAttribute( State, AttributeName, MergedValue ),

	{ RemainingEntries, MergedState }.



% Extracts the value (supposedly, any type of list) of each of the specified
% attributes from specified entries, and append that list to the corresponding
% one found in the specified state, stored under the same attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_lists_for( [ attribute_name() ], [ attribute_entry() ],
	wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_lists_for( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(

			fun( AttrName, { AccEntries, AccState } ) ->
					merge_list_for( AttrName, AccEntries, AccState )
			end,

			_Acc0={ AttributeEntries, State },

			_List=AttributeNames ).



% Returns a list of the known restoration markers.
%
% (helper)
%
-spec list_restoration_markers() -> [ restoration_marker() ].
list_restoration_markers() ->
	[ ?process_restoration_marker, ?file_restoration_marker,
	  ?term_restoration_marker ].
