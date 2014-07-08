% Copyright (C) 2003-2014 Olivier Boudeville
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



% WOOPER: Wrapper for OOP in ERlang.

% See documentation at:
% http://ceylan.sourceforge.net/main/documentation/wooper/


% Creation date: Friday, July 6, 2007.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com).

% Licensed under a disjunctive tri-license: MPL/GPL/LGPL, see:
% http://ceylan.sourceforge.net/main/documentation/wooper/index.html#license


% Provides most classical constructs: new/delete operators, remote method
% invocation (RMI), polymorphism and multiple inheritance, all with state
% management and in a quite efficient way (i.e. no significantly faster approach
% in Erlang could be imagined by the author - before he became aware of the
% existence of parse transforms).

% Instances are created thanks to the new operator, which calls automatically
% the relevant constructor ('construct' function).

% A class C is mapped to an Erlang module, preferably named 'class_C'.
%
% An active object is mapped to an Erlang process.
%
% Methods support Remote Invocation Calls, mapped to Erlang messages.
%
% Inheritance is implemented thanks to a per-class method virtual table,
% including the locally-defined ones and all the inherited ones.
%
% This table is shared among all the instances of a given class, thanks to a
% singleton-like class manager process that keeps references to the virtual
% table of each class.
%
% Instance state is maintained thanks to a per-instance attribute table, storing
% all its attributes, including all the inherited ones.
%
% The hashtable type, defined in hashtable.erl, is used at all levels:
% per-instance (for the attribute table), per-class (for the so-called virtual
% table), per-node (for the class manager).
%
% The proplist module could be used instead.

% When an exported function is called as a method (i.e. it is listed in the
% wooper_method_export variable, see below) the list of parameters being
% received is prefixed with the instance state (a bit like 'self' in Python):
% A ! { aMethod, [1,2] } results in the calling of the 'aMethod' function
% defined in the class module of A (exported thanks to wooper_method_export)
% with parameters automatically given to that function being: 'CurrentStateOfA,
% 1, 2' instead of '1, 2', with CurrentStateOfA being the A state variable
% automatically kept in the instance WOOPER main loop.
%
% Hence 'aMethod' must have been defined as aMethod/3 instead of aMethod/2 (it
% is indeed 'aMethod(State,X,Y) -> [..]'), whereas from the outside it is called
% with only two parameters specified (state not being included).


% The usual content of the '-export([XXX]).' clause in a class module should be
% dispatched in:
%
%    '-define( wooper_method_export, YYY ).', to declare methods, ex:
% '-define( wooper_method_export, getAge/1, setAge/2, declareBirthday/1 ).'
% Zero arity is not possible since there is at least the 'State' first
% parameter. So one just increments the number of intended real
% function-specific parameters in this export.
% Ex: a function 'setAge' taking in input only one logical parameter, NewAge,
% should actually be defined as 'setAge(State,NewAge) -> [..]' and therefore
% declared as: '-define( wooper_method_export, a/1, setAge/2, b/2 ).'
% Note: one should not forget, when overloading a method F/A, to specify it in
% wooper_method_export, otherwise its closest ancestor method will be called
% instead. In this case a warning is issued at compilation of the child class:
% 'Warning: function F/A is unused.'; static methods can be declared also here.
%
% '-define( wooper_construct_export, new/p, new_link/p, construct/p+1, ...).'
% Ex:
% '-define( wooper_construct_export, new/2, new_link/2, construct/3, ...).'
% to declare the appropriate construction-related functions (the 'new'
% variations and the 'construct' operator), p being the number of
% parameters defined in the wooper_construct_parameters variable.
% Only the relevant 'construct' function has to be actually defined by the
% developer: all new variations are automatically defined appropriately
% (see in this file).
% Declaring and implementing a toString/1 method is optional, but may be
% convenient for the debugging of method implementations.
%
%   '-export([ZZZ]).', ex: '-export([example_fun/0, f/2]).' for usual exported
% functions, that are not methods.
%
% Note that the dispatching of functions into wooper_method_export,
% wooper_construct_export and classical exports is done mainly for
% self-documenting purpose (they are all just translated into the usual
% export declarations).




% Shared code.
%
% All WOOPER classes should mention their superclasses and their WOOPER exports
% before the WOOPER header is included.

% Example:
% -module(class_Cat).
% -define( wooper_superclasses, [class_Mammal,class_ViviparousBeing] ).
% -define( wooper_method_export, hasWhiskers/1, canEat/2 ).
% -define( wooper_construct_parameters, Age, Gender, FurColor ).
% -define( wooper_construct_export, new/3, new_link/3, construct/4, ... ).
% -include("wooper.hrl").
% [...]
% See also: class_Template.erl



% Note:
%
% - the hashtable type used by WOOPER (not the one exposed as a potential
% attribute) should be a preprocessor define
%
% - this define should match the one in wooper_class_manager.erl
%
% The 'hashtable' choice should be the best one here, as we precisely know when
% to optimise the table (once for all).
%
-define( wooper_hashtable_type, hashtable ).



% Allows to define WOOPER base variables and methods for that class.



% Records the state of an instance.
% Module is the Erlang module the class is mapped to.
%
% This is the class-specific object state, each instance of this class will have
% its own state_holder, quite similar to the 'C++' this pointer.
%
% Constant data (ex: the virtual table) are referenced by each class instance,
% they are not duplicated (pointer to a virtual table shared by all class
% instances rather than deep copy).
%
% The virtual table holds the method name to module mapping for a given class.
% The attribute table (a hashtable) records all the data members of a given
% instance, including all the inherited ones.
%
% The request sender member is used internally by WOOPER so that a request
% method have a way of retrieving the corresponding caller PID. This avoids the
% caller to specify its PID twice, one for WOOPER, one for the method, as a
% method parameter, in the case the method itself needs the caller PID, for
% example to register it in a list in its own state. Thus a caller does not have
% to specify: 'MyInstance ! {my_request,[self()],self()}', specifying
% 'MyInstance ! {my_request,[],self()}' is enough: the method will be able to
% retrieve the caller PID thanks to the request_sender member, automatically set
% by WOOPER. For non-request methods (oneways), WOOPER will set request_sender
% to the atom 'undefined', to ensure the oneway crashes whenever trying to use
% this request-specific information to send a message.
%
% Therefore when you see the first parameter of a method, 'State', it is
% actually just an instance of the following record:
%
-record( state_holder, {

		virtual_table    :: 'undefined' |
							?wooper_hashtable_type:?wooper_hashtable_type(),

		attribute_table  :: 'undefined' |
							?wooper_hashtable_type:?wooper_hashtable_type(),

		% Only means we know to access the actual class name:
		%
		% (otherwise we could not, for example, report in an intermediate child
		% class the actual class name of a deleted instance)
		%
		actual_class     :: basic_utils:module_name(),

		request_sender   :: pid() | 'undefined'

} ).


%-opaque wooper_state() :: #state_holder{}.
-type wooper_state() :: #state_holder{}.


% Not module(), which can be also a tuple():
-type class_name() :: wooper:class_name().

-type method_name() :: wooper:method_name().

-type request_name() :: wooper:request_name().
-type oneway_name()  :: wooper:oneway_name().


% List of arguments, or non-list standalone one:
-type method_argument() :: wooper:method_argument().

% Standalone (non-list) arguments may be specified:
-type method_arguments() :: wooper:method_arguments().


-type request_call() :: { request_name(), method_arguments(), pid() }.
-type oneway_call()  :: { oneway_name(), method_arguments() } | oneway_name().


% The actual value of interest returned by a request:
%-type request_result() :: any().
-type request_result( T ) :: T.


-type request_return( T ) :: { wooper_state(), request_result( T ) }.
-type oneway_return() :: wooper_state().


-type attribute_name() :: atom().
-type attribute_value() :: any().

-type attribute_entry() :: { attribute_name(), attribute_value() }.



% A request is typically:
%
% -spec my_request :: fun( wooper_state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> request_return( T ).


% A oneway is typically:
%
% -spec my_oneway :: fun( wooper_state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> oneway_return().



-export_type([ wooper_state/0,
			   request_result/1, request_return/1, oneway_return/0,
			   attribute_name/0, attribute_value/0, attribute_entry/0 ]).



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



% The conventional atom to mark internal, local processes that must escape the
% serialisation/deserialisation processes:
%
-define( process_restoration_marker, 'wooper_restore_local_process' ).


% The conventional atom to mark internal, local open files (akin to file
% descriptor)s) that must escape the serialisation/deserialisation processes:
%
-define( file_restoration_marker, 'wooper_restore_local_file' ).


% The conventional atom to mark internal, local terms that must escape the
% serialisation process (ex: they may be recreated afterwards):
%
-define( term_restoration_marker, 'wooper_restore_local_term' ).


-type restoration_marker() :: ?process_restoration_marker
							| ?file_restoration_marker
							| ?term_restoration_marker.



% A list could be managed that would allow to discriminate the methods from the
% other exported functions. As macros cannot be substitued in strings it would
% probably force the developer to list them twice.

% The class name, as mapped to a module.
-define( className, ?MODULE ).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of actual
% attributes for a given class)
%
-define( WooperAttributeCountUpperBound, 12 ).



% For the name of the registered process that keeps the per-class method
% hashtables:
%
-include("wooper_class_manager.hrl").



% For list_impl:
-include("data_types.hrl").


% WOOPER internal functions.


% Declaration of functions for state management:
%
-export([ setAttribute/3, setAttributes/2, hasAttribute/2, getAttribute/2,
		  removeAttribute/2, addToAttribute/3, subtractFromAttribute/3,
		  incrementAttribute/2, decrementAttribute/2,
		  toggleAttribute/2, appendToAttribute/3, deleteFromAttribute/3,
		  addKeyValueToAttribute/4, popFromAttribute/2 ]).



% These frequent operations must be as fast as possible:
%
-compile( { inline, [ setAttribute/3, setAttributes/2, hasAttribute/2,
					  getAttribute/2, removeAttribute/2,
					  addToAttribute/3, subtractFromAttribute/3,
					  incrementAttribute/2, decrementAttribute/2,
					  toggleAttribute/2,
					  appendToAttribute/3, deleteFromAttribute/3,
					  addKeyValueToAttribute/4, popFromAttribute/2,
					  handle_oneway_execution/3 ] } ).



% These methods/functions are defined for all classes:
%
-define( WooperBaseMethods, get_class_name/0, getClassName/1,
		 get_superclasses/0, get_superclasses/1,
		 executeRequest/2, executeRequest/3,
		 executeOneway/2, executeOneway/3,
		 executeRequestWith/3, executeRequestWith/4,
		 executeOnewayWith/3, executeOnewayWith/4,
		 obtain_results_for_request/3,
		 delete_any_instance_referenced_in/2,
		 delete_synchronously_any_instance_referenced_in/2,
		 delete_synchronously_instance/1, delete_synchronously_instances/1,

		 serialise/3, handle_private_processes/2, wooper_mute_attributes/2,
		 wooper_check_attributes_equal/3, wooper_replace_attributes/3,
		 wooper_merge_list_for/3, wooper_merge_lists_for/3,
		 list_restoration_markers/0,

		 pre_serialise_hook/1, post_serialise_hook/3, pre_deserialise_hook/2,
		 post_deserialise_hook/1,

		 wooper_deserialise/4,

		 load/1, load/3, load_link/1, load_link/3,

		 synchronous_load/1, synchronous_load/3, synchronous_load_link/1,
		 synchronous_load_link/3,

		 remote_synchronisable_load_link/2, remote_synchronisable_load_link/4,
		 remote_synchronous_timed_load_link/2,
		 remote_synchronous_timed_load_link/4,

		 wooper_destruct/1, wooper_check_undefined/2,
		 wooper_get_all_attributes/1, wooper_receive/0,
		 is_wooper_debug/0, wooper_debug_listen/3,
		 wooper_display_state/1, wooper_display_virtual_table/1,
		 wooper_display_instance/1, wooper_display_loop_state/1 ).


-export([ ?WooperBaseMethods ]).



% No closure nor export needed for wooper_main_loop, as the various spawns are
% done based on the wooper_construct_and_run* functions, which are not exported,
% and are executed thanks to a closure.
%
% Note that, in the definition of this closure, self() should not be used,
% otherwise this will correspond to the PID of the spawned process, and not to
% the one of the creating one (hence CreatorPid = self() which is defined
% outside of these closures).



% wooper_method_export is deprecated in favor of:
%
% - wooper_public_method_export
% - wooper_protected_method_export
% - wooper_private_method_export
% - wooper_static_method_export
%
% Could have been wooper_member_method_export


-ifdef(wooper_method_export).
-export([?wooper_method_export]).
-endif.

-ifdef(wooper_member_method_export).
-export([?wooper_member_method_export]).
-endif.

-ifdef(wooper_public_method_export).
-export([?wooper_public_method_export]).
-endif.


-ifdef(wooper_protected_method_export).
-export([?wooper_protected_method_export]).
-endif.


-ifdef(wooper_private_method_export).
-export([?wooper_private_method_export]).
-endif.

-ifdef(wooper_static_method_export).
-export([?wooper_static_method_export]).
-endif.



% Must be defined, but an error message at their call should be clearer:
-ifdef(wooper_construct_export).
-export([?wooper_construct_export]).
-endif.


% Voluntary underspecification, to be able to toggle:
-spec is_wooper_debug() -> boolean().


% Specifications common to debug/non-debug sections:

-spec wooper_display_loop_state( wooper_state() ) ->
				basic_utils:void().


% On debug mode, methods will have to return an atom to ensure they respect the
% right format:
-ifdef(wooper_debug).


% Only defined in debug mode:

-spec wooper_get_state_description( wooper_state() ) ->
				{ wooper_state(), string() }.

-spec wooper_get_virtual_table_description( wooper_state() ) ->
				{ wooper_state(), string() }.

-spec wooper_get_instance_description( wooper_state() ) ->
				{ wooper_state(), string() }.


	% These methods/functions are specific to the debug mode:
	-export([ wooper_get_state_description/1,
			  wooper_get_virtual_table_description/1,
			  wooper_get_instance_description/1 ]).


	is_wooper_debug() ->
		true.


	wooper_display_loop_state( State ) ->
		wooper_display_state( State ).


	% Uncomment to have all WOOPER recompiled classes output verbosely their
	% information:
	% (useful when everything is compiled without this flag and then
	% uncommenting the flag to recompile only the class(es) to debug)
	%-define(wooper_log_wanted,).

	-ifdef(wooper_log_wanted).
		-define(wooper_log(Msg),io:format(Msg)).
		-define(wooper_log_format(Msg,Format),io:format(Msg,Format)).
	-else.
		-define(wooper_log(Msg),no_wooper_log).
		-define(wooper_log_format(Msg,Format),no_wooper_log).
	-endif.


-else.

	% Not in debug mode here:

	is_wooper_debug() ->
		false.


	wooper_display_loop_state(_) ->
		debug_no_activated.

	-define(wooper_log(Msg),no_wooper_log).
	-define(wooper_log_format(Msg,Format),no_wooper_log).


-endif.



% Uncomment to activate synchronous new with time-out:
% (generally to be left uncommented)
-define(use_synchronous_timed_new,).



% Define the synchronous time-out if not already set:
-ifdef(use_synchronous_timed_new).


% A reasonable duration (in milliseconds) before a time-out is triggered after a
% created instance does not seem to answer properly:
% (we could block forever but for most cases it would make the debugging harder)

-ifndef(synchronous_time_out).


-ifdef(wooper_debug).

% Suitable for most applications (5 seconds):
-define(synchronous_time_out,5000).

-else. % wooper_debug


% Better for simulations (30 minutes):
-define(synchronous_time_out, (30*60*1000) ).

% Also possible:
%-define(synchronous_time_out,infinity).

-endif. % wooper_debug


-endif. % ifndef(synchronous_time_out)


-endif. % ifdef(use_synchronous_timed_new).






% Number of milliseconds to wait for, in order to be sure that the error message
% could be written to the console, knowing that the operation is asynchronous
% and thus may not be performed should the VM halt immediately:
% (otherwise you will not see any stacktrace)
-define(error_display_waiting,400).


% Now that type-checking on the state record is performed in debug mode, in both
% modes method results are sent directly:
%
% (no wooper_result atom added any more in debug mode)
%
-define(wooper_return_state_result(State,Result),{State,Result}).
-define(wooper_return_state_only(State), State).



% Returns all the attributes of this instance, as a list of { attribute_name,
% attribute_value } pairs.
%
-spec wooper_get_all_attributes( wooper_state() ) ->
									   ?wooper_hashtable_type:entries().
wooper_get_all_attributes( State ) ->
	?wooper_hashtable_type:enumerate( State#state_holder.attribute_table ).




% Returns the result corresponding to the first pending WOOPER request
% (generally the latest sent one).
%
% (helper)
%
-spec wooper_receive() -> request_result( any() ).
wooper_receive() ->

	receive

		{ wooper_result, R } ->
			R

	end.



% Helper function to test requests.
%
% Allows to test from the shell an instance by sending it requests (hence
% needing a receive whereas in the shell).
%
% Available even when debug mode is off.
%
-spec wooper_debug_listen( pid(), request_name(), method_argument() )
						 -> request_result( 'ok' ).
wooper_debug_listen( Pid, RequestName, Arguments ) ->

	Pid ! { RequestName, Arguments, self() },

	receive

		% A list is assumed to be a string here:
		{ wooper_result, Result } when is_list(Result) ->
			io:format( "String result of call to '~w' "
				"with arguments '~w': ~s~n",
				[ RequestName, Arguments, Result ] );

		{ wooper_result, Result } ->
			io:format( "Result of call to '~w' with arguments '~w': ~s~n",
				[ RequestName, Arguments,
				 text_utils:term_to_string( Result ) ] );

		Anything ->
			io:format( "Answer to call to '~w' with arguments '~w': ~s~n",
				[ RequestName, Arguments,
				 text_utils:term_to_string( Anything ) ] )

	end.



% "Static method" (only a function) which returns the name of the class.
%
% Note: this function might return the name of a super-class if called from a
% destructor or an inherited, non-overridden method.
%
% See also: getClassName/1 for a more reliable counterpart.
%
-spec get_class_name() -> class_name().
get_class_name() ->
	?className.



% "Static method" (only a function) which returns the list of the
% superclasses for that class.
%
-spec get_superclasses() -> [ class_name() ].
get_superclasses() ->
	?wooper_superclasses.



% Method that returns the classname of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
% (const request)
%
-spec getClassName( wooper_state() ) -> request_return( class_name() ).
getClassName( State ) ->
	?wooper_return_state_result( State, State#state_holder.actual_class ).



% Method that returns the (direct) superclasses of the instance.
-spec get_superclasses( wooper_state() ) -> request_return( [class_name()] ).
get_superclasses( State ) ->
	?wooper_return_state_result( State, ?wooper_superclasses ).




% The creation of an instance of a WOOPER class can be:
%
% - either non-blocking or synchronous (and, if synchronous, with or without a
% time-out)
%
% - either not linked or linked to the current process
%
% - either local (on the current node) or remote (on another node)



% Following construction variations are always declared, for a constructor
% expected to take N base parameters:
% new/N, new_link/N, synchronous_new/N, synchronous_new_link/N

% If use_synchronous_timed_new is defined, WOOPER adds:
% synchronous_timed_new/N, synchronous_timed_new_link/N

% If use_remote_new is defined, WOOPER adds:
% remote_new/N+1, remote_new_link/N+1, remote_synchronous_new/N+1,
% remote_synchronous_new_link/N+1, remote_synchronisable_new_link/N+1.

% Finally, if use_synchronous_timed_new *and* use_remote_new are defined,
% WOOPER adds these timed operations:
% remote_synchronous_timed_new/N+1, remote_synchronous_timed_new_link/N+1.

% Therefore for a template for a full-blown declaration would be:
% (in  the next block, just search and replace with your text editor A with N,
% and B with N+1)

% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
%-define( wooper_construct_export, new/A, new_link/A,
%	synchronous_new/A, synchronous_new_link/A,
%	synchronous_timed_new/A, synchronous_timed_new_link/A,
%	remote_new/B, remote_new_link/B, remote_synchronous_new/B,
%	remote_synchronous_new_link/B, remote_synchronisable_new_link/B,
%   remote_synchronous_timed_new/B, remote_synchronous_timed_new_link/B,
%   construct/B, delete/1 ).

% Note: delete/1 can be removed from the export list above if no special
% destructor is to be defined.


% There are construction operators that just take the construction parameters,
% ?wooper_construct_parameters (like new/N), and other operators that take
% an additional parameter, the target node (like remote_new/N+1).
%
% As wooper_construct_parameters can be void (i.e. declared as
% '-define(wooper_construct_parameters,).'), the declaration 'new(
% ?wooper_construct_parameters )' would be correct, whereas 'remote_new(
% Node, ?wooper_construct_parameters )' would result in the incorrect syntax
% 'remote_new( Node, )'.
%
% A solution could be, depending on wooper_construct_parameters being void or
% not, to define either 'remote_new( Node )' (if void) or 'remote_new(
% Node,?wooper_construct_parameters )' (if not void).
%
% However the Erlang macros do not seem to support tests based on their value
% (we can only test whether they are defined or not), thus the following
% convention has been used:

% wooper_construct_parameters should be defined if and only if there is at least
% one parameter to be declared.
%
% Therefore, in the case of a constructor taking two parameters, X and Y, we
% will have:
%
% -define( wooper_construct_parameters, X, Y ).
% ...
% construct( State, ?wooper_construct_parameters ) ->
% ...
%
% whereas in the case of a constructor taking no parameter, we will have:
%
% ...
% construct( State ) ->
% ...
%

% i.e. there will be in this case no: '-define(wooper_construct_parameters,)'.


% No specification can be provided for new operators, due to their
% class-specific arities.


% First case: wooper_construct_parameters is defined:

-ifdef(wooper_construct_parameters).



% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
%
new( ?wooper_construct_parameters ) ->

	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),

	spawn( fun() -> wooper_construct_and_run( [ ?wooper_construct_parameters ] )
						end ).



% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
%
% Returns the PID of the newly created and linked instance.
%
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
%
new_link( ?wooper_construct_parameters ) ->

	spawn_link( fun() -> wooper_construct_and_run(
					  [ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
%
synchronous_new( ?wooper_construct_parameters ) ->

	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class and links it to the current process,
% using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
%
synchronous_new_link( ?wooper_construct_parameters ) ->

	%io:format( "synchronous_new_link for ~s with parameters:~n~p.~n",
	%		  [?MODULE, [ ?wooper_construct_parameters ] ] ),

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	receive

		{ spawn_successful, SpawnedPid } ->
			%io:format( "synchronous_new_link: spawned ~w.~n", [SpawnedPid] ),
			SpawnedPid

	end.





-ifdef(use_synchronous_timed_new).




% Spawns a new instance for this class, using specified parameters to construct
% it.
%
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new( ?wooper_construct_parameters ) ->

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_time_out, ?MODULE } )

	end.



% Spawns a new instance for this class, and links it to the current process,
% using specified parameters to construct it.
% Returns the PID of the newly created instance, or the time_out atom.
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
synchronous_timed_new_link( ?wooper_construct_parameters ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_linked_time_out, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new




% If use_remote_new is defined, following construction variations will be
% automatically defined (class implementor will have to declare them):
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - remote_synchronisable_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then having remote_new/N+1.




% Uncomment to activate remote new constructions:
% (now we always enable them)
-define(use_remote_new,).


-ifdef(use_remote_new).


% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new( Node, ?wooper_construct_parameters ) ->

	spawn( Node, fun() -> wooper_construct_and_run(
						  [ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process, using specified parameters to construct it.
%
% If Node does not exist, a useless pid is returned.
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
remote_new_link( Node, ?wooper_construct_parameters ) ->

	spawn_link( Node, fun() -> wooper_construct_and_run(
		[ ?wooper_construct_parameters ] ) end ).



% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new will return only when the
% created process reports it is up and running.
%
remote_synchronous_new( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_new operator: "
	%	"spawning ~w:wooper_construct_and_run_synchronous "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new_link will return only when the
% created process reports it is up and running.
%
remote_synchronous_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_new_link operator: "
	%	"spawning ~w:wooper_construct_and_run_synchronous "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
												CreatorPid ) end ),

	% Blocks until the spawned process answers:
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous (the PID is directly returned), however a
% {spawn_successful,SpawnedPid} message will be received once (if ever) the
% instance is up and running. This allows to perform the actual instance
% creations in parallel, by waiting bulks of creations.
%
remote_synchronisable_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronisable_new_link operator: "
	%	"spawning ~w:wooper_construct_and_run_synchronous "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),
	%timer:sleep(200),

	CreatorPid = self(),

	spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
												CreatorPid ) end ).



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node, using
% specified parameters to construct it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new will return
% only when the created process reports it is up and running, or when
% a time-out occurs.
%
remote_synchronous_timed_new( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_timed_new operator: "
	%	"spawning ~w:wooper_construct_and_run_synchronous "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),
	%timer:sleep(200),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[ ?wooper_construct_parameters ], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_time_out, Node, ?MODULE } )

	end.



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new_link will return only
% when the created process reports it is up and running, or when a time-out
% occurs.
%
remote_synchronous_timed_new_link( Node, ?wooper_construct_parameters ) ->

	%io:format( "remote_synchronous_timed_new_link operator: "
	%		  "spawning ~w:wooper_construct_and_run_synchronous "
	%		  "with parameters ~w on node ~w from node ~w.~n",
	%		  [ ?MODULE, [ ?wooper_construct_parameters ] , Node, node() ] ),

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [ ?wooper_construct_parameters ],
											CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			%io:format( "remote_synchronous_timed_new_link: returning ~w.~n",
			%		  [ SpawnedPid ] ),
			SpawnedPid

	after ?synchronous_time_out ->

		io:format( "(remote_synchronous_timed_new_link: throwing time-out "
				   "on node ~p for module ~p after ~p milliseconds)~n",
				   [ Node, ?MODULE, ?synchronous_time_out ] ),

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new







-else. % -ifdef(wooper_construct_parameters).



% Second case: wooper_construct_parameters is *not* defined:
% No argument, thus specs can be defined.


% Spawns a new instance for this class.
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: new returns as soon as the creation is triggered,
% without waiting for it to complete.
%
-spec new() -> pid().
new() ->
	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]),

	spawn( fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class and links it to the current process.
%
% Returns the PID of the newly created and linked instance.
%
% Creation is asynchronous: new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
-spec new_link() -> pid().
new_link() ->

	spawn_link( fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new will return only when the created
% process reports it is up and running.
%
-spec synchronous_new() -> pid().
synchronous_new() ->

	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]),

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class and links it to the current process.
% Returns the PID of the newly created instance.
%
% Creation is synchronous: synchronous_new_link will return only when the
% created process reports it is up and running.
%
-spec synchronous_new_link() -> pid().
synchronous_new_link() ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



-ifdef(use_synchronous_timed_new).




% Spawns a new instance for this class.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
%
-spec synchronous_timed_new() -> pid().
synchronous_timed_new() ->

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper_construct_and_run_synchronous(
									   [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_time_out, ?MODULE } )

	end.



% Spawns a new instance for this class, and links it to the current process.
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: synchronous_timed_new will return only when the
% created process reports it is up and running, or when a time-out occurs.
-spec synchronous_timed_new_link() -> pid().
synchronous_timed_new_link() ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( fun() -> wooper_construct_and_run_synchronous(
		 [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { synchronous_linked_time_out, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new



% If use_remote_new is defined, following construction variations will be
% automatically defined (class implementor will have to declare them):
%  - remote_new
%  - remote_new_link
%  - remote_synchronous_new
%  - remote_synchronous_new_link
%  - synchronous_timed_new
%
% The arity of these remote operators is equal to the one of their local
% counterparts plus one: if having new/N, then having remote_new/N+1.




% Uncomment to activate remote new constructions:
-define(use_remote_new,).


-ifdef(use_remote_new).


% Spawns a new instance for this class on specified interconnected node.
% If Node does not exist, a useless pid is returned.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new returns as soon as the creation is
% triggered, without waiting for it to complete.
%
-spec remote_new( net_utils:node_name() ) -> pid().
remote_new( Node ) ->
	spawn( Node, fun() -> wooper_construct_and_run( [] ) end ).


% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process.
%
% If Node does not exist, a useless pid is returned.
% Returns the PID of the newly created instance.
%
% Creation is asynchronous: remote_new_link returns as soon as the creation is
% triggered, without waiting for it to complete.
-spec remote_new_link( net_utils:node_name() ) -> pid().
remote_new_link( Node ) ->
	spawn_link( Node, fun() -> wooper_construct_and_run( [] ) end ).



% Spawns a new instance for this class on specified interconnected node.
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new will return only when the
% created process reports it is up and running.
%
-spec remote_synchronous_new( net_utils:node_name() ) -> pid().
remote_synchronous_new( Node ) ->

	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	%	"with no parameter.~n", [?MODULE]),

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process.
%
% Returns the PID of the newly created instance.
%
% Creation is synchronous: remote_synchronous_new_link will return only when the
% created process reports it is up and running.
%
-spec remote_synchronous_new_link( net_utils:node_name() ) -> pid().
remote_synchronous_new_link( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% Spawns a new instance for this class on specified interconnected node and
% links it to the current process, using specified parameters to construct it.
%
% Returns the PID of the newly created instance.
%
% Creation is asynchronous (the PID is directly returned), however a
% {spawn_successful,SpawnedPid} message will be received once (if ever) the
% instance is up and running. This allows to perform the actual instance
% creations in parallel, by waiting bulks of creations.
%
remote_synchronisable_new_link( Node ) ->

	%io:format( "remote_synchronisable_new_link operator: "
	%	"spawning ~w:wooper_construct_and_run_synchronous "
	%	"with parameters ~w.~n", [?MODULE,[?wooper_construct_parameters]]),
	%timer:sleep(200),

	CreatorPid = self(),

	spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ).



-ifdef(use_synchronous_timed_new).


% Spawns a new instance for this class on specified interconnected node.
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new will return only when
% the created process reports it is up and running, or when a time-out occurs.
%
-spec remote_synchronous_timed_new( net_utils:node_name() ) -> pid().
remote_synchronous_timed_new( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn( Node, fun() -> wooper_construct_and_run_synchronous(
		[], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_time_out, Node, ?MODULE } )

	end.



% Spawns a new instance for this class on specified interconnected node, and
% links it to the current process.
%
% Returns the PID of the newly created instance, or the time_out atom.
%
% Creation is synchronous: remote_synchronous_timed_new_link will return only
% when the created process reports it is up and running, or when a time-out
% occurs.
%
-spec remote_synchronous_timed_new_link( net_utils:node_name() ) -> pid().
remote_synchronous_timed_new_link( Node ) ->

	CreatorPid = self(),

	SpawnedPid = spawn_link( Node, fun() ->
		wooper_construct_and_run_synchronous( [], CreatorPid ) end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after ?synchronous_time_out ->

		throw( { remote_synchronous_linked_time_out, Node, ?MODULE } )

	end.



-endif. % use_synchronous_timed_new

-endif. % use_remote_new


-endif. % -ifdef(wooper_construct_parameters).



% Type specifications must not depend on debug mode:

-spec wooper_construct_and_run( list() ) -> no_return().
-spec wooper_construct_and_run_synchronous( list(), pid() ) -> no_return().


-ifdef(wooper_debug).


% Extensive testings in this mode.

% Indirection level to allow constructors to be chained.
%
% Allows to obtain the virtual table from the instance, not from its parent.
%
wooper_construct_and_run( ParameterList ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n", [ ParameterList, length(ParameterList) ] ),

	BlankTable = #state_holder{

		virtual_table   = wooper_retrieve_virtual_table(),

		attribute_table = ?wooper_hashtable_type:new(
									?WooperAttributeCountUpperBound ),

		actual_class    = ?MODULE,

		request_sender  = undefined

	},

	try apply( ?MODULE, construct, [ BlankTable | ParameterList ] ) of

		ConstructState when is_record( ConstructState, state_holder ) ->

			% Enforce a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			wooper_main_loop( ConstructState#state_holder{
									 attribute_table=TunedTable } );

		Other ->
			error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned '~p' instead."
				" Construction parameters were:~n~p.~n",
				[ self(), ?MODULE, Other, ParameterList ] ),
			% Wait a bit as error_msg seems asynchronous:
			timer:sleep( ?error_display_waiting ),
			throw( { invalid_constructor, ?MODULE } )

	catch

		Reason:ErrorTerm ->

			% Construction failed:
			% (error term would often be unreadable with ~p)

			error_logger:error_msg( "~nWOOPER error for PID ~w, "
					"constructor (~s:construct/~B) failed (cause: ~p):~n~n"
					" - with error term:~n~p~n~n"
					" - for parameters:~n~p~n~n"
					" - stack trace was (latest calls first):"
					"~n~p~n~n",
					[ self(), ?MODULE, length(ParameterList)+1, Reason,
					 ErrorTerm, ParameterList, erlang:get_stacktrace() ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_constructor_failed, self(), ?MODULE,
						length(ParameterList)+1, ParameterList, ErrorTerm } )

	end.



% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent.
%
wooper_construct_and_run_synchronous( ParameterList, SpawnerPid ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),
	BlankTable = #state_holder{

		virtual_table   = wooper_retrieve_virtual_table(),

		attribute_table = ?wooper_hashtable_type:new(
										 ?WooperAttributeCountUpperBound ),

		actual_class    = ?MODULE,

		request_sender  = undefined

	},

	try apply( ?MODULE, construct, [ BlankTable | ParameterList ] ) of

		ConstructState when is_record(ConstructState,state_holder) ->

			% Notify early:
			SpawnerPid ! { spawn_successful, self() },

			% Enforce a closer-to-ideal load factor of the hashtable if needed,
			% as by convention no attribute should be introduced outside of the
			% constructor:
			TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

			wooper_main_loop( ConstructState#state_holder{
									 attribute_table=TunedTable } );

		Other ->
			error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
				"constructor did not return a state, but returned '~p' instead."
				" Construction parameters were:~n~p.~n",
				[ self(), ?MODULE, Other, ParameterList ] ),
			% Wait a bit as error_msg seems asynchronous:
			timer:sleep( ?error_display_waiting ),
			throw( { invalid_constructor, ?MODULE } )

	catch

		Reason:ErrorTerm ->

			% Construction failed:
			% (error term would often be unreadable with ~p)

			error_logger:error_msg( "~nWOOPER error for PID ~w, "
					"constructor (~s:construct/~B) failed (cause: ~p):~n~n"
					" - with error term:~n~p~n~n"
					" - for parameters:~n~p~n~n"
					" - stack trace was (latest calls first):"
					"~n~p~n~n",
					[ self(), ?MODULE, length(ParameterList)+1, Reason,
					 ErrorTerm, ParameterList, erlang:get_stacktrace() ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_constructor_failed, self(), ?MODULE,
						length(ParameterList)+1, ParameterList, ErrorTerm } )

	end.



-else.



% Less testings in this mode.

% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent.
%
wooper_construct_and_run( ParameterList ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),

	BlankTable = #state_holder{

		virtual_table   = wooper_retrieve_virtual_table(),

		attribute_table = ?wooper_hashtable_type:new(
										?WooperAttributeCountUpperBound ),

		actual_class    = ?MODULE,

		request_sender  = undefined

	},

	ConstructState = try

		apply( ?MODULE, construct, [ BlankTable | ParameterList ] )

	catch

		Reason:ErrorTerm ->

			% Construction failed:
			% (error term would often be unreadable with ~p)

			error_logger:error_msg( "~nWOOPER error for PID ~w, "
					"constructor (~s:construct/~B) failed (cause: ~p):~n~n"
					" - with error term:~n~p~n~n"
					" - for parameters:~n~p~n~n"
					" - stack trace was (latest calls first):"
					"~n~p~n~n",
					[ self(), ?MODULE, length(ParameterList)+1, Reason,
					 ErrorTerm, ParameterList, erlang:get_stacktrace() ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_constructor_failed, self(), ?MODULE,
						length(ParameterList)+1, ParameterList, ErrorTerm } )

	end,

	% Enforce a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

	wooper_main_loop( ConstructState#state_holder{
									attribute_table=TunedTable } ).



% Indirection level to allow constructors to be chained.
% Allows to obtain the virtual table from the instance, not from its parent.
%
wooper_construct_and_run_synchronous( ParameterList, SpawnerPid ) ->

	%io:format("wooper_construct_and_run called with parameters ~w, "
	%	"whose length is ~B.~n",[ParameterList,length(ParameterList)]),

	BlankTable = #state_holder{

		virtual_table   = wooper_retrieve_virtual_table(),

		attribute_table = ?wooper_hashtable_type:new(
										?WooperAttributeCountUpperBound ),

		actual_class    = ?MODULE,

		request_sender  = undefined

	},

	ConstructState = try

			 apply( ?MODULE, construct, [ BlankTable | ParameterList ] )

	catch

		Reason:ErrorTerm ->

			% Construction failed:
			% (error term would often be unreadable with ~p)

			error_logger:error_msg( "~nWOOPER error for PID ~w, "
					"constructor (~s:construct/~B) failed (cause: ~p):~n~n"
					" - with error term:~n~p~n~n"
					" - for parameters:~n~p~n~n"
					" - stack trace was (latest calls first):"
					"~n~p~n~n",
					[ self(), ?MODULE, length(ParameterList)+1, Reason,
					 ErrorTerm, ParameterList, erlang:get_stacktrace() ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_constructor_failed, self(), ?MODULE,
						length(ParameterList)+1, ParameterList, ErrorTerm } )

	end,

	% Notify early:
	SpawnerPid ! { spawn_successful, self() },

	% Enforce a closer-to-ideal load factor of the hashtable if needed, as by
	% convention no attribute should be introduced outside of the constructor:
	TunedTable = ?wooper_hashtable_type:optimise(
							ConstructState#state_holder.attribute_table ),

	wooper_main_loop( ConstructState#state_holder{
									attribute_table=TunedTable } ).


-endif.





% State management section.


% Note that these macros should be one-liners, otherwise their value would be
% the first evaluated in the macro - not the last.
%
% Indeed 'NewState = ?setAttribute( State, ...)' *and* ' my_function(
% ?setAttribute( State, ...)' have to be supported.
%
% In case of a macro with multiple instructions, in the first example NewState
% would be bound to the result of the first instruction of the macro, as shown
% in:

% 1> A=a,b,c.
% c
% 2> A.
% a

% instead of the expected final macro result.
%
% Whereas for the second example the result sent to my_function would be (as
% expected) the updated state (c, in the previous shell output).

% Another problem that must be avoided is that if calling for example
% '?setAttribute( f(State), name, value )', then we do not want any side-effect
% caused by f/1 to be triggered more than once.
%
% For example in the setAttribute/3 macros defined below (and now
% commented-out), multiple references to (State) would have resulted in as many
% calls to f/1, which is not correct.

% Therefore we came to the conclusion that macros could not fulfill our needs,
% and defined full-blown functions instead, even if we incur the performance
% penalty of an additional function call.
%
% So finally '?setAttribute( AState, name, value )' should be transformed into
% '?setAttribute( AState, name, value )'.

% Newer macros are defined for backward compatibility, resulting in the same
% function call. They should not be used anymore, their purpose is just to
% support unchanged legacy code.

% Finally, with an upcoming version of WOOPER making use of parse transforms and
% of class-specific records, attributes will be used thanks to the same function
% calls, while fully removing their performance penalty.



% Faulty macro (risk of side-effects being executed more than once):

% Sets specified attribute of the instance to the specified value, based from
% specified state.
%
% Returns an updated state.
% Always succeeds.
% See also: the setAttributes macro to set more than one attribute at a time.
%-define(setAttribute(State,AttributeName,AttributeValue),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntry(
%			(AttributeName),
%			(AttributeValue),
%			(State)#state_holder.attribute_table )
%	}
%).


% Correct function-based version:

% Sets specified attribute of the instance to the specified value, based from
% specified state.
%
% Returns an updated state.
% Always succeeds.
%
% See also: setAttributes/3, to set more than one attribute at a time.
-spec setAttribute( wooper_state(), attribute_name(), attribute_value() ) ->
						wooper_state().
setAttribute( State, AttributeName, AttributeValue ) ->
   State#state_holder{
	   attribute_table = ?wooper_hashtable_type:addEntry(
		   AttributeName,
		   AttributeValue,
		   State#state_holder.attribute_table )
   }.



% Uncomment if old-style attribute management macros are to be enabled:
% (by default we do not want to support them anymore)
%-define(use_legacy_macros,).

-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( setAttribute( State, AttributeName, AttributeValue ),
	setAttribute( (State), (AttributeName), (AttributeValue) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Sets a list of attribute/value pairs in specified state.
%
% The expected parameter is a list of pairs (2-element tuples), each pair
% containing in first position the attribute name and in second one the
% attribute value.
%
% Returns an updated state.
% Always succeeds.
% See also: setAttribute/3.
%-define(setAttributes(State,ListOfAttributePairs),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntries(
%			(ListOfAttributePairs),
%			(State)#state_holder.attribute_table )
%	}
%).


% Correct function-based version (to be inlined):


% Sets a list of attribute/value pairs in specified state.
%
% The expected parameter is a list of pairs (2-element tuples), each pair
% containing in first position the attribute name and in second one the
% attribute value.
%
% Returns an updated state.
% Always succeeds.
% See also: the setAttribute function.
%
-spec setAttributes( wooper_state(), [ attribute_entry() ] ) ->
						wooper_state().
setAttributes( State, ListOfAttributePairs ) ->

   State#state_holder{
	   attribute_table = ?wooper_hashtable_type:addEntries(
		   ListOfAttributePairs,
		   State#state_holder.attribute_table )
   }.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define(setAttributes( State, ListOfAttributePairs ),
	setAttributes( (State), (ListOfAttributePairs) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Tells whether specified attribute exists, returns true or false.
%
% Note: usually the best practise is to set all possible attributes from the
% constructor, either to an appropriate value or to 'undefined', instead of
% having instances with or without a given attribute.
%
% Note: not expected to be ever used, as all attributes should be defined
% directly in the constructor, hence no attribute could appear later, if this
% good practise is respected.
%
%-define(hasAttribute(State,AttributeName),
%	?wooper_hashtable_type:hasEntry( (AttributeName),
%		(State)#state_holder.attribute_table ) ).



% Correct function-based version (to be inlined):

% Tells whether specified attribute exists, returns true or false.
%
% Note: usually the best practise is to set all possible attributes from the
% constructor, either to an appropriate value or to 'undefined', instead of
% having instances with or without a given attribute.
%
% Note: not expected to be ever used, as all attributes should be defined
% directly in the constructor, hence no attribute could appear later, if this
% good practise is respected.
%
-spec hasAttribute( wooper_state(), attribute_name() ) -> boolean().
hasAttribute( State, AttributeName ) ->
	?wooper_hashtable_type:hasEntry( AttributeName,
									State#state_holder.attribute_table ).



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( hasAttribute( State, AttributeName ),
	hasAttribute( (State), (AttributeName) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause crash.
%
% Note: almost never used, as either the attribute can be obtained with
% getAttr/1 (as externally defined) or it is already bound to a variable.
%
% See also: the getAttr/1 shorthand.
%
%-define( getAttribute( State, AttributeName ),
%	?wooper_hashtable_type:getEntry( (AttributeName),
%		(State)#state_holder.attribute_table ) ).



% Correct function-based version (to be inlined):

% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause crash.
%
% Note: not used very frequently, as either the attribute can be obtained with
% getAttr/1 (as externally defined) or the value is already bound in an
% available variable.
%
% See also: the getAttr/1 shorthand.
%
-spec getAttribute( wooper_state(), attribute_name() ) -> attribute_value().
getAttribute( State, AttributeName ) ->
	?wooper_hashtable_type:getEntry( AttributeName,
									State#state_holder.attribute_table ).



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( getAttribute( State, AttributeName ),
	getAttribute( (State), (AttributeName) ) ).

-endif.



% Returns the value associated to specified named-designated attribute, if
% found, otherwise triggers a case clause crash.
%
% This macro is usually more useful than the getAttribute function, as one
% generally wants to retrieve an attribute already available in the 'State'
% parameter of a method (otherwise that value is available through a bound
% variable in the method body).
%
% Therefore the use of a variable named 'State' can often be implied.
%
% Beware to the implicit use of the 'State' variable: in some cases other
% states should be used; due to this variable, getAttr must remain a macro.
%
% See the longer getAttribute/2 function.
-define( getAttr(AttributeName),
	getAttribute( State, (AttributeName) )
).





% Faulty macro (risk of side-effects being executed more than once):

% Returns an updated state not having anymore specified attribute.
% No error is triggered if the specified attribute was not existing.
%-define(removeAttribute(State,AttributeName),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:removeEntry( (AttributeName),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Returns an updated state not having anymore specified attribute.
%
% No error is triggered if the specified attribute was not existing.
%
% Note: this operation is not recommended, as attributes should always be
% defined. Better keep it defined, but set it to 'undefined'.
%
removeAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:removeEntry( AttributeName,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( removeAttribute( State, AttributeName ),
	removeAttribute( (State), (AttributeName) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Adds specified value to specified attribute, supposed to be a number.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%-define(addToAttribute(State,AttributeName,Value),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addToEntry(
%			(AttributeName),
%			(Value),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Adds specified value to specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec addToAttribute( wooper_state(), attribute_name(), attribute_value() ) ->
		wooper_state().
addToAttribute( State, AttributeName, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( addToAttribute( State, AttributeName, Value ),
	addToAttribute( (State), (AttributeName), (Value) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Subtracts specified value from specified attribute, supposed to be a number.
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no subtraction can be performed on the attribute value.
%-define(subtractFromAttribute(State,AttributeName,Value),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:subtractFromEntry(
%			(AttributeName),
%			(Value),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Subtracts specified value from specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no subtraction can be performed on the attribute value.
%
-spec subtractFromAttribute( wooper_state(), attribute_name(),
					attribute_value() ) -> wooper_state().
subtractFromAttribute( State, AttributeName, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:subtractFromEntry(
			AttributeName,
			Value,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( subtractFromAttribute( State, AttributeName, Value ),
	subtractFromAttribute( (State), (AttributeName), (Value) )
).

-endif.




% Correct function-based version (to be inlined):

% Increments specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec incrementAttribute( wooper_state(), attribute_name() ) ->
		wooper_state().
incrementAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			_Value=1,
			State#state_holder.attribute_table )
	}.



% Correct function-based version (to be inlined):

% Decrements specified attribute, supposed to be a number.
%
% Returns an updated state.
%
% A case clause is triggered if the attribute did not exist, a bad arithm is
% triggered if no addition can be performed on the attribute value.
%
-spec decrementAttribute( wooper_state(), attribute_name() ) ->
		wooper_state().
decrementAttribute( State, AttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addToEntry(
			AttributeName,
			_Value=-1,
			State#state_holder.attribute_table )
	}.





% Faulty macro (risk of side-effects being executed more than once):

% Returns an updated state in which specified boolean attribute is toggled:
% if true will be false, if false will be true.
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
%-define( toggleAttribute(State,BooleanAttributeName),
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:toggleEntry(
%			(BooleanAttributeName),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Returns an updated state in which specified boolean attribute is toggled: if
% true will be false, if false will be true.
%
% A case clause is triggered if the attribute does not exist or it is not a
% boolean value.
%
-spec toggleAttribute( wooper_state(), attribute_name() ) ->  wooper_state().
toggleAttribute( State, BooleanAttributeName ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:toggleEntry(
			BooleanAttributeName,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( toggleAttribute( State, BooleanAttributeName ),
	toggleAttribute( (State), (BooleanAttributeName) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
%
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
%
%-define(appendToAttribute(State,AttributeName,Element),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:appendToEntry(
%			(AttributeName),
%			(Element),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Appends specified element to specified attribute, supposed to be a list.
% A case clause is triggered if the attribute did not exist.
%
% Returns an updated state.
%
% Note: no check is performed to ensure the attribute is a list indeed, and the
% operation will not complain if not.
%
-spec appendToAttribute( wooper_state(), attribute_name(),
						attribute_value() ) -> wooper_state().
appendToAttribute( State, AttributeName, Element ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:appendToEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define(appendToAttribute( State, AttributeName, Element ),
	appendToAttribute( (State), (AttributeName), (Element) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
%
% A case clause is triggered if the attribute did not exist.
% If the element is not in the specified list, the list will not be modified.
%-define(deleteFromAttribute(State,AttributeName,Element),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:deleteFromEntry(
%			(AttributeName),
%			(Element),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):

% Deletes the first match of specified element from specified attribute,
% supposed to be a list.
%
% A case clause is triggered if the attribute did not exist.
% If the element is not in the specified list, the list will not be modified.
%
% Returns an updated state.
%
-spec deleteFromAttribute( wooper_state(), attribute_name(),
		  attribute_value() ) -> wooper_state().
deleteFromAttribute( State, AttributeName, Element ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:deleteFromEntry(
			AttributeName,
			Element,
			State#state_holder.attribute_table )
	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( deleteFromAttribute( State, AttributeName, Element ),
	deleteFromAttribute( (State), (AttributeName), (Element) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Assumes the specified attribute is a hashtable and adds the specified
% key/value pair to it.
%
% Several lines compacted into a bit impressive one-liner.
%-define(addKeyValueToAttribute(State,AttributeName,Key,Value),
%
%	(State)#state_holder{
%		attribute_table = ?wooper_hashtable_type:addEntry(
%			(AttributeName),
%			?wooper_hashtable_type:addEntry( (Key), (Value),
%				?wooper_hashtable_type:getEntry( (AttributeName),
%					(State)#state_holder.attribute_table )),
%			(State)#state_holder.attribute_table )
%	}
%).



% Correct function-based version (to be inlined):


% Assumes the specified attribute is a hashtable and adds the specified
% key/value pair to it.
%
% Returns an updated state.
%
% Several lines compacted into a bit impressive one-liner.
%
% Note: to be used with much caution, as a class may use a type of hashtable
% unrelated to the one used by WOOPER (on the other hand we do not want to force
% all classes to define 'hashtable_type').
%
-spec addKeyValueToAttribute( wooper_state(), attribute_name(),
			?wooper_hashtable_type:key(), ?wooper_hashtable_type:value() ) ->
									wooper_state().
addKeyValueToAttribute( State, AttributeName, Key, Value ) ->

	State#state_holder{
		attribute_table = ?wooper_hashtable_type:addEntry(

			AttributeName,

			?wooper_hashtable_type:addEntry( Key, Value,
				?wooper_hashtable_type:getEntry( AttributeName,
					State#state_holder.attribute_table ) ),

			State#state_holder.attribute_table )

	}.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( addKeyValueToAttribute( State, AttributeName, Key, Value ),
	addKeyValueToAttribute( (State), (AttributeName), (Key), (Value) )
).

-endif.



% Faulty macro (risk of side-effects being executed more than once):

% Removes the head from specified attribute, supposed to be a list, and returns
% a tuple {NewState,PoppedHead}.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{PoppedState,Head} = ?popFromAttribute(State,my_list)'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
% Note: This cannot be a one-line macro, it has to be a function.
%
%-define(popFromAttribute(State,AttributeName),
%	wooper_pop_from_attribute( (State), (AttributeName) )
%).



% Correct function-based version (to be inlined):

% Removes the head from specified attribute, supposed to be a list, and returns
% a tuple {NewState,PoppedHead}.
%
% For example, if the attribute 'my_list' contains [5,8,3], executing:
% '{PoppedState,Head} = ?popFromAttribute(State,my_list)'
% returns a state whose my_list attribute is [8,3] and a value Head = 5.
%
% A case clause is triggered if the attribute did not exist.
%
-spec popFromAttribute( wooper_state(), attribute_name() ) ->
							{ wooper_state(), attribute_value() }.
popFromAttribute( State, AttributeName ) ->

	{ Head, PoppedAttributeTable } = ?wooper_hashtable_type:popFromEntry(
				  AttributeName, State#state_holder.attribute_table ),

	{ State#state_holder{ attribute_table = PoppedAttributeTable },
		Head }.



-ifdef( use_legacy_macros ).

% Macro defined for backward compatibility only:

-define( popFromAttribute( State, AttributeName ),
	popFromAttribute( (State), (AttributeName) )
).

-endif.



% Checks that the value of specified attribute is 'undefined'.
% Triggers an exception otherwise.
%
% Note: operates on a state called 'State', thus must be a macro.
% The check could be disabled in debug mode.
%
% This results in function call, as a pure macro, if used more than once in a
% function, would trigger warnings about unused variables or, more probably,
% would attempt to perform unintended pattern matches.
%
-define( checkUndefined( Attribute ),
	wooper_check_undefined( State, (Attribute) )
).



% Helper function for the checkUndefined macro.
%
-spec wooper_check_undefined( wooper_state(), attribute_name() ) ->
							basic_utils:void().
wooper_check_undefined( State, Attribute ) ->

	try

		undefined = ?getAttr(Attribute)

	catch

		exit:{ {badmatch,UnexpectedValue}, Stack } ->

			% Attribute value was not equal to 'undefined':
			throw( { attribute_was_not_undefined, {Attribute,UnexpectedValue},
				Stack } );

		exit:Error ->
			% Other error (ex: unknown attribute):
			throw( { attribute_error, Attribute, Error } );

		OtherError ->
			throw( { unexpected_attribute_error, Attribute, OtherError } )

	end.



% Returns the sender of the request.
%
% Must be a macro, due to the implied state parameter.
%
-define( getSender(), State#state_holder.request_sender ).




% Returns the Wooper Class Manager.
%
% If if it is already running, find it and returns its atom, otherwise launch it
% and returns that same atom as well.
%
-spec wooper_get_class_manager() -> basic_utils:registration_name().
wooper_get_class_manager() ->

	case lists:member( ?WooperClassManagerName, registered() ) of

		true ->
			?WooperClassManagerName;

		_ ->
			spawn( ?WooperClassManagerName, start, [self()] ),
			% Only dealing with registered managers (instead of using directly
			% their PID) allows to be sure only one instance (singleton) is
			% being used, to avoid the case of two managers being launched at
			% the same time (the second will then terminate immediately).
			receive

				class_manager_registered ->
					?WooperClassManagerName

			% 10-second time-out:
			after 10000 ->

				error_logger:error_msg( "wooper_get_class_manager: "
					"unable to find WOOPER class manager after 10 seconds.~n"
					"Please check that WOOPER has been compiled beforehand.~n"
				),
				undefined

			end

	end.




% Most functions below could be encapsulated in a WOOPER-dedicated module
% (in a .erl instead of a .hrl), but in this case calls may be less efficient.



% Methods for getting informations about an instance.


% Returns a textual representation of the attributes of the specified state.
%
-spec wooper_state_toString( wooper_state() ) -> string().
wooper_state_toString( State ) ->

	Attributes = ?wooper_hashtable_type:enumerate(
								   State#state_holder.attribute_table ),

	% We prefer having the attributes sorted by their name, in alphabetical
	% order:
	%
	SortedAttributes = lists:keysort( _Index=1, Attributes ),

	lists:foldl(

		fun( { AttName, AttrValue }, Acc ) ->
			Acc ++ io_lib:format(
				"     * ~s = ~s~n",
				[
					text_utils:term_to_string( AttName ),
					text_utils:term_to_string( AttrValue, _MaxDepth=16,
											_MaxLength=100 )
				] )

		end,

		io_lib:format( "State of ~w:~nInstance of ~s with ~B attribute(s):~n",
			[ self(), get_class_name(), length(Attributes) ] ),

		SortedAttributes ).



% Returns a textual representation of the virtual table corresponding to
% specified state.
%
% (helper)
%
-spec wooper_virtual_table_toString( wooper_state() ) -> string().
wooper_virtual_table_toString( State ) ->

	lists:foldl(

		fun( { {Name,Arity}, Module }, String ) ->
			String ++ io_lib:format( "     * ~s/~B -> ~s~n",
				[ Name, Arity, Module ] )
		end,

		io_lib:format( "Virtual table of ~w:~n"
					  "(method name/arity -> module defining that method)~n",
					  [ self() ] ),

		?wooper_hashtable_type:enumerate( State#state_holder.virtual_table ) ).



% Returns a textual representation of this instance, including its state and
% virtual table.
%
% (helper)
%
-spec wooper_instance_toString( wooper_state() ) -> string().
wooper_instance_toString( State ) ->
	io_lib:format( "Inspection of instance ~w:~n~n  + ~s~n  + ~s",
		[ self(), wooper_state_toString( State ),
			wooper_virtual_table_toString( State ) ] ).



% Displays the inner state of this instance.
% This is not a method.
%
-spec wooper_display_state( wooper_state() ) -> basic_utils:void().
wooper_display_state( State ) ->
	error_logger:info_msg( "~s~n", [ wooper_state_toString(State) ] ).



% Displays the inner state of this instance.
% This is not a method.
%
-spec wooper_display_virtual_table( wooper_state() ) -> basic_utils:void().
wooper_display_virtual_table( State ) ->
	error_logger:info_msg( "~s~n", [ wooper_virtual_table_toString(State) ] ).


% Displays the inner state of this instance.
% This is not a method.
%
-spec wooper_display_instance( wooper_state() ) -> basic_utils:void().
wooper_display_instance( State ) ->
	error_logger:info_msg( "~s~n", [ wooper_instance_toString(State) ] ).



% Common to all debug cases:
-spec handle_oneway_execution( method_name(), wooper_state(),
							  method_arguments() ) -> wooper_state().



-ifdef(wooper_debug).


% Returns a textual description of the inner state of this instance.
%
% This is a method for debug-purpose, only activated if wooper_debug is defined.
%
% (const request)
%
wooper_get_state_description( State ) ->
	?wooper_return_state_result( State, wooper_state_toString( State ) ).



% Returns a textual description of the virtual table used by this instance.
%
% This is a method for debug-purpose, only activated if wooper_debug is defined.
%
% (const request)
%
wooper_get_virtual_table_description( State ) ->
	?wooper_return_state_result( State, wooper_virtual_table_toString( State ) ).



% Returns a full textual description of this instance, including its state and
% virtual table.
%
% This is a method for debug-purpose, only activated if wooper_debug is defined.
%
% (const request)
%
wooper_get_instance_description( State ) ->
	?wooper_return_state_result( State, wooper_instance_toString( State ) ).



% Executes the specified oneway, and performs debug-mode only additional
% checkings.
%
% Returns an updated state.
%
% Note: must not be a macro, as its value would be the one of its first
% statement, not the one of its last. This function is requested to be inlined
% instead.
%
handle_oneway_execution( MethodAtom, State, ArgumentList ) ->

	case wooper_execute_method( MethodAtom, State, ArgumentList ) of

		{ MacroState, wooper_method_returns_void } ->
			% This is the normal case:
			MacroState;

		{_MacroState, { wooper_result, UnexpectedResult } } ->
			error_logger:error_msg( "Method ~s:~s/~B, which was called with "
				"parameters ~p, returned a result (~p) whereas, according to "
				"its call, it was expected to be a oneway.~n"
				"Either the oneway implementation is incorrect "
				"or it is a request which is incorrectly "
				"called as a oneway.",
				[ State#state_holder.actual_class, MethodAtom,
				 length(ArgumentList)+1, ArgumentList, UnexpectedResult ]  ),
			throw( { oneway_request_mismatch, MethodAtom, ArgumentList } )

	end.



-else.


% Executes the specified oneway, with no specific checking.
%
% Returns an updated state.
%
% Note: must not be a macro, as its value would be the one of its first
% statement, not the one of its last. This function is requested to be inlined
% instead.
%
handle_oneway_execution( MethodAtom, State, ArgumentList ) ->

		{ MacroState, _Result } = wooper_execute_method( MethodAtom, State,
												   ArgumentList ),
		MacroState.



-endif.



% WOOPER default EXIT handler.
%
% Returns an updated state.
%
% Can be overridden by defining or inheriting the onWooperExitReceived/3 method.
%
-spec wooper_default_exit_handler( wooper_state(), pid(), any() ) ->
										 wooper_state().
wooper_default_exit_handler( State, Pid, ExitType ) ->

	error_logger:warning_msg( "WOOPER default EXIT handler of the ~w "
	  "instance ~w ignored following EXIT message from ~w:~n~p.~n~n",
	  [ State#state_holder.actual_class, self(), Pid, ExitType ] ),

	State.



% Waits for incoming method calls and serves them.
-spec wooper_main_loop( wooper_state() ) -> no_return().
wooper_main_loop( State ) ->

	%?wooper_log( "wooper_main_loop start.~n" ),

	% Comment-out to avoid the state display prior to each method call:
	%wooper_display_loop_state(State),

	receive

		% Requests (thus with response):

		% Instance PID could be sent back as well to discriminate received
		% answers on the caller side.
		{ MethodAtom, ArgumentList, CallerPid }
				when is_pid(CallerPid) and is_list(ArgumentList) ->

			?wooper_log_format( "Main loop (case A) for ~w: "
				"request '~s' with argument list ~w for ~w.~n",
				[ self(), MethodAtom, ArgumentList, CallerPid ] ),

			SenderAwareState = State#state_holder{ request_sender=CallerPid },

			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, ArgumentList ),
			%CallerPid ! { self(), Result }
			CallerPid ! Result,

			% Force a crash if instance-side error detected:
			case element( 1, Result ) of

				wooper_method_failed ->
					throw( Result ) ;

				wooper_method_not_found ->
					throw( Result ) ;

				% wooper_result:
				_ ->
					ok

			end,
			SenderAgnosticState =
				NewState#state_holder{ request_sender=undefined },

			%?wooper_log( "Main loop (case A) ended.~n" ),

			wooper_main_loop( SenderAgnosticState );


		% Auto-wrapping single arguments implies putting lists between
		% double-brackets:
		{ MethodAtom, Argument, CallerPid } when is_pid(CallerPid) ->

			?wooper_log_format( "Main loop (case B) for ~w: "
				"request '~s' with argument ~w for ~w.~n",
				[ self(), MethodAtom, Argument, CallerPid ] ),

			SenderAwareState = State#state_holder{ request_sender=CallerPid },

			{ NewState, Result } = wooper_execute_method( MethodAtom,
				SenderAwareState, [ Argument ] ),

			%CallerPid ! { self(), Result }
			CallerPid ! Result,

			% Forces a crash if instance-side error detected:
			case element( 1, Result ) of

				wooper_method_failed ->
					throw( Result ) ;

				wooper_method_not_found ->
					throw( Result ) ;

				% wooper_result:
				_ ->
					ok

			end,
			SenderAgnosticState =
				NewState#state_holder{ request_sender=undefined },

			%?wooper_log( "Main loop (case B) ended.~n" ),

			wooper_main_loop( SenderAgnosticState );


		% Oneway calls (no caller PID sent, no answer sent back).
		%
		% We check though that indeed no value is returned, by pattern-matching
		% against wooper_method_returns_void, which could be removed if not in
		% debug mode.
		%
		% (if no pattern-matching was done, then either this method would not
		% return anything, or the sender would not be interested in the result)
		{ MethodAtom, ArgumentList } when is_list(ArgumentList) ->

			?wooper_log_format( "Main loop (case C) for ~w: "
				"oneway '~s' with argument list ~w.~n",
				[ self(), MethodAtom, ArgumentList ] ),

			NewState = handle_oneway_execution( MethodAtom, State,
												ArgumentList ),

			%?wooper_log( "Main loop (case C) ended.~n" ),

			wooper_main_loop( NewState );

		{ synchronous_delete, CallerPid } ->

			?wooper_log( "Main loop: oneway synchronous delete.~n" ),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			wooper_destruct( State ),
			CallerPid ! { deleted, self() },
			deleted;
			% (do nothing, loop ends here).


		% ping is always available and cannot be overridden:
		{ ping, CallerPid } ->

			?wooper_log_format( "Main loop (case D) for ~w: oneway ping.~n",
				[ self() ]),

			CallerPid ! { pong, self() },

			%?wooper_log( "Main loop (case D) ended.~n" ),
			wooper_main_loop( State );


		% Oneway with parameters:
		{ MethodAtom, Argument } ->

			?wooper_log_format( "Main loop (case E) for ~w: "
				"oneway '~s' with argument ~w.~n",
				[ self(), MethodAtom, Argument ] ),

			NewState = handle_oneway_execution( MethodAtom, State,
											[ Argument ] ),

			%?wooper_log( "Main loop (case E) ended.~n" ),

			wooper_main_loop( NewState );


		delete ->

			?wooper_log("Main loop: oneway delete.~n"),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			wooper_destruct( State ),
			deleted;
			% (do nothing, loop ends here).


		MethodAtom when is_atom(MethodAtom) ->

			?wooper_log_format(
				"Main loop (case F) for ~w: oneway from atom ~s.~n",
				[ self(), MethodAtom ] ),

			% Any result should be ignored, only the updated state is kept:
			NewState = handle_oneway_execution( MethodAtom, State,
												_ArgumentList=[] ),

			%?wooper_log( "Main loop (case F) ended.~n" ),
			wooper_main_loop( NewState );


		{ 'EXIT', Pid, ExitType } when is_pid(Pid) ->

			?wooper_log_format( "Main loop (case G) for ~w: exit with ~w.~n",
				[ self(), { Pid, ExitType } ] ),

			case ?wooper_hashtable_type:lookupEntry(
		 { onWooperExitReceived, 33707 }, State#state_holder.virtual_table ) of

				{ value, _Key } ->
					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule,onWooperExitReceived,...)':

					{ NewState, _ } = wooper_execute_method(
						onWooperExitReceived, State, [ Pid, ExitType ] ),

					%?wooper_log( "Main loop (case G) ended.~n" ),
					wooper_main_loop( NewState );

				% Hashtable key not found:
				_ ->

					% EXIT handler not overridden, using default one:
					%?wooper_log( "Main loop (case G) ended.~n" ),
					wooper_main_loop( wooper_default_exit_handler( State,
						Pid, ExitType ) )

			end;


		Other ->

			% Catch-all:
			?wooper_log_format( "Main loop (case H) for ~w: unmatched ~p.~n",
				[ self(), Other ] ),

			error_logger:error_msg(
			  "WOOPER ignored following message for instance ~w:~n~p.~n",
			  [ self(), Other ] ),

			%?wooper_log( "Main loop (case H) ended.~n" ),
			throw( { erroneous_call, Other } )


	end.
	% Commented out to preserve (presumably) tail-recursion:
	% io:format( "wooper_main_loop exited.~n" ).



% Returns the virtual table corresponding to this class.
-spec wooper_retrieve_virtual_table() ->
				   ?wooper_hashtable_type:?wooper_hashtable_type().
wooper_retrieve_virtual_table() ->

	% For per-instance virtual table: wooper_create_method_table_for(?MODULE).
	wooper_get_class_manager() ! { get_table, ?MODULE, self() },
	receive

		{ virtual_table, ?MODULE, Table } ->
			%?wooper_hashtable_type:display(Table),
			Table

	end.



% Debug or not:
-spec wooper_destruct( wooper_state() ) -> wooper_state().


-ifdef(wooper_debug).


% Calls recursively the destructors through the inheritance tree.
%
% Each wooper_destruct function is purely local to the current module.
%
% Initial specified is always valid (comes from the main loop, but states
% returned by user-defined destructors must be checked in debug mode.
%
wooper_destruct( State ) ->

	% If a class-specific delete is defined, execute it, otherwise do
	% nothing. Then recurse with higher-level destructors (maybe just storing
	% delete/1 in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_for):
	DeletedState = case lists:member( {delete,1}, module_info(exports) ) of

		true ->
			% All destructors, including user-defined ones, must return a
			% (possibly updated) state:
			try ?MODULE:delete( State ) of

				ReturnedState when is_record( ReturnedState, state_holder ) ->
					ReturnedState;

				Other ->

					error_logger:error_msg(
						"~nWOOPER error for PID ~w of class ~s: "
						"user-defined destructor did not return a state, "
						"but returned '~p' instead.~n",
						[ self(), ?MODULE, Other ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),
					throw( { invalid_destructor, ?MODULE } )

			catch

				Reason:ErrorTerm ->

					% Destruction failed:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"destructor (~s:delete/1) failed (cause: ~p):~n~n"
						" - with error term:~n~p~n~n"
						" - stack trace was (latest calls first):"
						"~n~p~n~n"
						" - instance state was: ~s~n~n",
						[ self(), ?MODULE, Reason, ErrorTerm,
						 erlang:get_stacktrace(), wooper_state_toString(State)
						 ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_destructor_failed, self(), ?MODULE,
						ErrorTerm } )
			end;

		false ->
			% Destructor not overridden, using default one:
			%io:format( "Deleting ~w (default do-nothing destructor "
			%  "for class ~w).~n", [ self(), ?MODULE ] )
			% State unchanged:
			State

	end,

	% Then automatically call the direct mother destructors.
	%
	% Using foldr, not foldl: the destructors of mother classes are called in
	% the reverse order compared to the order that was used for construction,
	% for the sake of symmetry.
	%
	% Final state is dropped.
	lists:foldr(
		fun(Class,NewState) -> apply( Class, wooper_destruct, [NewState] ) end,
		DeletedState, get_superclasses() ).



-else.


% Calls recursively the destructors through the inheritance tree.
% Each wooper_destruct function is purely local to the current module.
%
wooper_destruct( State ) ->
	% If a class-specific delete is defined, execute it, otherwise do
	% nothing. Then recurse with higher-level destructors (maybe just storing
	% delete/1 in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_for):
	DeletedState = case lists:member( {delete,1}, module_info(exports) ) of

		true ->
			% All destructors, included user-defined ones, must return a
			% (possibly updated) state:
			try

				?MODULE:delete( State )

			catch

				Reason:ErrorTerm ->

					% Destruction failed:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"destructor (~s:delete/1) failed (cause: ~p):~n~n"
						" - with error term:~n~p~n~n"
						" - stack trace was (latest calls first):"
						"~n~p~n~n"
						" - instance state was: ~s~n~n",
						[ self(), ?MODULE, Reason, ErrorTerm,
						 erlang:get_stacktrace(), wooper_state_toString(State)
						 ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_destructor_failed, self(), ?MODULE,
						ErrorTerm } )
			end;

		false ->
			% Destructor not overridden, using default one:
			%io:format( "Deleting ~w (default do-nothing destructor "
			%   "for class ~w).~n", [ self(), ?MODULE ] )
			% State unchanged:
			State

	end,
	% Then automatically call the direct mother destructors.
	%
	% Using foldr, not foldl: the destructors of mother classes are called in
	% the reverse order compared to the order that was used for construction,
	% for the sake of symmetry.
	%
	% Final state is dropped.
	lists:foldr(
		fun( Class, NewState ) -> apply( Class, wooper_destruct,
										[ NewState ] ) end,
		DeletedState, get_superclasses() ).

-endif.



% Looks-up specified method (Method/Arity, ex: toString/0) to be found in
% heritance tree and returns either { 'value', Module } with Module
% corresponding to the class that implements that method, or
% 'hashtable_key_not_found'.
%
% Note: uses the pre-built virtual table for this class.
%
-spec wooper_lookup_method( wooper_state(), method_name(), arity() ) ->
		   { 'value', class_name() } | 'hashtable_key_not_found'.
wooper_lookup_method( State, MethodAtom, Arity ) ->
	?wooper_hashtable_type:lookupEntry( { MethodAtom, Arity },
		State#state_holder.virtual_table ).



% To be specified more closely maybe:
-type method_internal_result() :: any().



% Common to debug and non-debug modes:

-spec wooper_execute_method( method_name(), wooper_state(),
							method_arguments() ) ->
	{ wooper_state(), method_internal_result() }.


-spec wooper_execute_method_with( class_name(), method_name(), wooper_state(),
			method_arguments() ) ->
	{ wooper_state(), method_internal_result() }.


-spec wooper_effective_method_execution( module(), method_name(),
			wooper_state(), method_arguments() ) ->
	{ wooper_state(), method_internal_result() }.



% Request specs.

-spec executeRequest( wooper_state(), request_name() ) ->
	{ wooper_state(), method_internal_result() }.


-spec executeRequest( wooper_state(), request_name(), method_arguments() ) ->
	{ wooper_state(), method_internal_result() }.


-spec executeRequestWith( wooper_state(), class_name(), request_name() ) ->
	{ wooper_state(), method_internal_result() }.


-spec executeRequestWith( wooper_state(), class_name(), request_name(),
			method_arguments() ) ->
	{ wooper_state(), method_internal_result() }.



% Oneway specs.

-spec executeOneway( wooper_state(), oneway_name() ) -> wooper_state().


-spec executeOneway( wooper_state(), oneway_name(), method_arguments() ) ->
						   wooper_state().



-spec executeOnewayWith( wooper_state(), class_name(), oneway_name() ) ->
							   wooper_state().


-spec executeOnewayWith( wooper_state(), class_name(), oneway_name(),
				method_arguments() ) -> wooper_state().




% Following code is duplicated because no '-ifdef' clause can be defined in case
% clauses:


-ifdef(wooper_debug).


% Executes the specified method, designated by its atom, with specified instance
% state and list of parameters.
%
% If the method is not found (either in the class module or in its ancester
% trees), an error tuple beginning with the atom 'wooper_method_not_found' is
% returned with an unchanged state.
%
% If the method is found, but if its execution fails, an error tuple beginning
% with the atom 'wooper_method_failed' is returned with an unchanged state.
%
% If it does not fail but returns an unexpected result (i.e. not a tuple
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
%
% If its execution succeeds, then {wooper_result,Result} is returned, with
% Result being the actual result of the method call, with an updated state.
%
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returned, which allows a caller that sent his
% PID to be warned it is useless, as no answer should be expected.
%
% The error logs have been added, as debugging faulty oneways is more difficult:
% they cannot return any error to the caller, they can just crash and notify any
% linked or monitoring process.
%
% Note: atom and state checking in guards should be superfluous.
%
wooper_execute_method( MethodAtom, State, Parameters ) when is_atom(MethodAtom)
		andalso is_record(State,state_holder) andalso is_list(Parameters) ->

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]),

	% +1: take into account the State additional parameter:
	case wooper_lookup_method( State, MethodAtom, length(Parameters)+1 ) of

		{ value, LocatedModule } ->

			%io:format("wooper_execute_method: executing ~s:~s(~w) from ~s.~n",
			%   [ ?MODULE, MethodAtom, Parameters, LocatedModule ]),

			% Returns { NewState, PossibleResult }:
			wooper_effective_method_execution( LocatedModule, MethodAtom,
				State, Parameters );

		hashtable_key_not_found ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple
					% elements, as if in a single string ("M/A"), the result
					% is displayed as a list:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"oneway method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length(Parameters)+1, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_not_found, self(),
							State#state_holder.actual_class,
							MethodAtom, length(Parameters)+1, Parameters} );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"request method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length(Parameters)+1, Parameters ] ),

					{ State, { wooper_method_not_found, self(),
							 State#state_holder.actual_class,
							 MethodAtom, length(Parameters)+1, Parameters } }


			% No other term can be returned.

			end

		% Removed, as determined by Dialyzer as impossible to occur:

		%% Other ->
		%%	error_logger:warning_msg(
		%%		"WOOPER ignored following message:~n~p.~n", [ Other ] ),
		%%	wooper_main_loop( State )

	end.



% Exactly as wooper_execute_method, except that the target module (class) is
% directly specified, instead of being determined from the instance virtual
% table.
%
wooper_execute_method_with( ClassName, MethodAtom, State, Parameters )
		when is_atom(ClassName) andalso is_atom(MethodAtom)
		andalso is_record(State,state_holder) andalso is_list(Parameters) ->

	% One check should be added: ClassName must be a super-class
	% (direct or not) of the actual class.
	wooper_effective_method_execution( ClassName, MethodAtom, State,
		Parameters ).



% Triggers the actual method execution.
%
wooper_effective_method_execution( SelectedModule, MethodAtom, State,
		Parameters ) ->

	%io:format( "WOOPER: effective execution of ~p:~p.~n",
	%		  [ SelectedModule, MethodAtom ] ),

	try apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Void method (no result returned, only a state):
		% ?wooper_return_state_only:
		NewState when is_record(NewState,state_holder) ->
			{ NewState, wooper_method_returns_void };

		% Method returning a result (and a state of course):
		% ?wooper_return_state_result:
		{ NewState, Result } when is_record(NewState,state_holder) ->
			{ NewState, {wooper_result,Result} };

		% Neither a wooper result nor an exception: faulty return.
		Other ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"oneway method ~s:~s/~B made a faulty return "
						"'~p', parameters were:~n~p~n",
						[ self(), SelectedModule, MethodAtom,
							length(Parameters)+1, Other, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_faulty_return, self(),
						SelectedModule, MethodAtom,
						length(Parameters)+1, Parameters, Other } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "~nWOOPER error for PID ~w, "
						"request method ~s:~s/~B made a faulty return "
						"'~p', parameters were:~n~p~n",
						[ self(), SelectedModule, MethodAtom,
							length(Parameters)+1, Other, Parameters ] ),

					{ State,
						{ wooper_method_faulty_return, self(),
							SelectedModule, MethodAtom,
							length(Parameters)+1, Parameters, Other } }

			end

	catch

		% All next cases are error cases.
		%
		% One option is to return an appropriate error term, but it is useful
		% only for requests, as oneways send back no result.
		%
		% Another option is to let the faulty process crash: oneways would not
		% send more answers, but linked and monitoring processes could
		% nevertheless be notified.

		% Finally, failed requests result in a log, an error answer being
		% returned, then a crash if the error is due to internal reasons,
		% whereas failed oneways result in a log then a crash, similarly if the
		% error is due to internal reasons.

		% User-defined exceptions are not caught, they will be rethrown and make
		% the process fail.

		% This the counterpart of {'EXIT',ErrorTerm}.
		% What would be the counterpart of {'EXIT',Pid,ErrorTerm}?
		% Reason can be: throw, error or exit.
		Reason:ErrorTerm ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
								"oneway method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters, erlang:get_stacktrace(),
									wooper_state_toString(State) ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_oneway_failed, self(), SelectedModule,
						MethodAtom, length(Parameters)+1, Parameters,
						ErrorTerm } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash: (error term would
					% often be unreadable with ~p)
					error_logger:error_msg( "~nWOOPER error for PID ~w, request"
								" method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters, erlang:get_stacktrace(),
									wooper_state_toString(State)
								 ] ),

					% Wait a bit (yes, even for requests, as often the VM will
					% be halted just before) as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					{ State,
						{ wooper_method_failed, self(), SelectedModule,
							MethodAtom, length(Parameters)+1,
							Parameters, ErrorTerm } }

			end

	end.



% Parameter-less request, calling implicitly any overridden version of the
% method.
%
% Returns an updated state.
%
executeRequest( State, RequestAtom ) when is_record( State, state_holder )
		andalso is_atom(RequestAtom) ->

	%io:format("executeRequest/2: executing ~s() from ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{request_sender=self()},

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


executeRequest( State, RequestAtomError )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: '~p' is not an atom.~n",
		[ self(), State#state_holder.actual_class, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtomError } );


executeRequest( StateError, RequestAtom ) when is_atom( RequestAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequest( StateError, RequestAtomError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
						   "when executing request: '~p' is not a state and "
						   "'~p' is not an atom.~n",
						   [ self(), ?MODULE, StateError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, StateError, RequestAtomError } ).





% Allows to call synchronously from the code of a given class its actual
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeRequest should be used: 'MyVehicle ! {startEngine..' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequest( State, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )
		andalso is_list(ArgumentList) ->

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Here the third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom( RequestAtom )->

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [StandaloneArgument] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Catches all errors:
executeRequest( StateError, RequestAtom, _LastArg )
		when is_atom(RequestAtom) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequest( _State, RequestAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: '~p' is not an atom.~n",
		[ self(), ?MODULE, RequestAtomError] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtomError } ).





% Parameter-less request, calling the version of the method as defined in the
% specified class.
%
executeRequestWith( State, ClassName, RequestAtom )
	when is_record(State,state_holder) andalso is_atom(ClassName)
		andalso is_atom(RequestAtom) ->

	%io:format("executeRequestWith/3: executing ~s() from ~s with ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


executeRequestWith( StateError, ClassName, RequestAtom )
		when is_atom(ClassName) andalso is_atom(RequestAtom) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p in the context of class ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


executeRequestWith( _State, ClassNameError, RequestAtomError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request in a class context: "
		"'~p' and '~p' should both be atoms.~n",
		[ self(), ?MODULE, ClassNameError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, ClassNameError, RequestAtomError } ).





% Allows to call synchronously from the code of a given class overridden methods
% (requests, here) as defined in specified classes.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequestWith( State, ClassName, RequestAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom(ClassName)
		andalso is_atom(RequestAtom) andalso is_list(ArgumentList) ->

	%io:format("executeRequestWith/4 with list: executing ~s(~w) from ~s "
	%  "with ~s.~n", [ RequestAtom, ArgumentList,
	% State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result};


% Here the third parameter is not a list:
executeRequestWith( State, ClassName, RequestAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom(ClassName)
		andalso is_atom(RequestAtom) ->

	%io:format("executeRequestWith/3 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [StandaloneArgument] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Error cases below:
executeRequestWith( StateError, ClassName, RequestAtom, _LastArg )
		when is_atom(ClassName) andalso is_atom(RequestAtom) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, RequestAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, RequestAtom } );


% Catches all remaining errors:
executeRequestWith( _State, ClassNameError, RequestAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing request: both '~p' (classn name) and "
		"'~p' (request name) should be atoms.~n",
		[ self(), ?MODULE, ClassNameError, RequestAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_request_call, ClassNameError, RequestAtomError } ).




% Parameter-less oneway.
executeOneway( State, OnewayAtom ) when is_record( State, state_holder )
		andalso is_atom( OnewayAtom ) ->

	%io:format("executeOneway/2: executing ~s() from ~s.~n",
	%   [ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [] ),

	% Returns:
	NewState;


executeOneway( State, OnewayError ) when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s: "
		"the oneway name should be an atom, not '~p'.~n",
		[ self(), State#state_holder.actual_class, OnewayError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayError } );


executeOneway( StateError, OnewayAtom ) when is_atom( OnewayAtom ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOneway( StateError, OnewayError ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~s' is not a state and '~p' is not an atom.~n",
		[ self(), ?MODULE, StateError, OnewayError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayError } ).





% Allows to call synchronously from the code of a given class its actual
% overridden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeOneway should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOneway( State, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom(OnewayAtom)
		andalso is_list(ArgumentList) ->

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom(OnewayAtom) ->

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method( OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState;


% All errors caught below:
executeOneway( StateError, OnewayAtom, _LastArg ) when is_atom(OnewayAtom) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOneway( State, OnewayAtomError, _LastArg )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~p' is not an atom.~n",
		[ self(), State#state_holder.actual_class, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } );

executeOneway( _State, OnewayAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: '~p' is not an atom.~n",
		[ self(), ?MODULE, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } ).






% Parameter-less oneway, relying on the specified actual class to be used,
% instead of determining it from the instance virtual table.
executeOnewayWith( State, ClassName, OnewayAtom )
		when is_record(State,state_holder) andalso is_atom(ClassName)
			andalso is_atom(OnewayAtom) ->

	%io:format("executeOnewayWith/3: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } =
		wooper_execute_method_with( ClassName, OnewayAtom, State, [] ),

	% Returns:
	NewState;


executeOnewayWith( State, ClassName, OnewayAtom )
  when is_record( State, state_holder ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway: "
		"'~p' (oneway name) and '~p' (class name) should be both atoms.~n",
		[ self(), State#state_holder.actual_class, OnewayAtom, ClassName ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


executeOnewayWith( StateError, ClassName, OnewayAtom ) ->

	% Catches all:

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p with ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } ).





% Allows to call synchronously from the code of a given class the oneway defined
% in specified class, instead of determining it from the instance virtual table.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOnewayWith( State, ClassName, OnewayAtom, ArgumentList ) when
		is_record( State, state_holder ) andalso is_atom(ClassName)
		andalso is_atom(OnewayAtom) andalso is_list(ArgumentList) ->

	%io:format("executeOneway/4 with list: executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOnewayWith( State, ClassName, OnewayAtom, StandaloneArgument ) when
		is_record( State, state_holder ) andalso is_atom(ClassName)
		andalso is_atom(OnewayAtom) ->

	%io:format("executeOneway/4 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, wooper_method_returns_void } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState;


executeOnewayWith( StateError, ClassName, OnewayAtom, _LastArg )
		when is_atom(ClassName) andalso is_atom(OnewayAtom) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway ~p with ~s: "
		"first parameter should be a state, not '~p'.~n",
		[ self(), ?MODULE, OnewayAtom, ClassName, StateError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtom } );


% Catches all remaining errors:
executeOnewayWith( _State, ClassName, OnewayAtomError, _LastArg ) ->

	error_logger:error_msg( "~nWOOPER error for PID ~w of class ~s "
		"when executing oneway with ~s: both '~p' and '~p' should be atoms.~n",
		[ self(), ?MODULE, ClassName, OnewayAtomError ] ),

	% Wait a bit as error_msg seems asynchronous:
	timer:sleep( ?error_display_waiting ),
	throw( { invalid_oneway_call, OnewayAtomError } ).



-else.


% Not in debug mode here (wooper_debug not set):




% Executes the specified method, designated by its atom, with specified instance
% state and parameters.
%
% If the method is not found (either in the class module or in its ancester
% trees), an error tuple beginning with the atom 'wooper_method_not_found' is
% returned with an unchanged state.
%
% If the method is found, if its execution fails, an error tuple beginning with
% the atom 'wooper_method_failed' is returned with an unchanged state.
%
% If it does not fail but returns an unexpected result (i.e. not a tuple
% beginning with the atom 'return'), an error tuple beginning with the atom
% 'wooper_method_faulty_return' is returned with an unchanged state.
%
% If its execution succeeds, then {wooper_result,Result} is returned (with
% Result being the actual result of the method call) with an updated state.
%
% Finally, if the method does not return any result, the atom
% 'wooper_method_returns_void' is returns, which allows a caller that sent his
% PID to be warned it is useless, as no answer should be expected.
%
wooper_execute_method( MethodAtom, State, Parameters ) ->

	%io:format("wooper_execute_method: looking up ~s(~w) from ~s.~n",
	%	[ MethodAtom, Parameters, ?MODULE ]),

	% +1: take into account the State additional parameter:
	case wooper_lookup_method( State, MethodAtom, length(Parameters)+1 ) of


		{ value, LocatedModule } ->

			%io:format("wooper_execute_method: executing ~s:~s(~w) from ~s.~n",
			%   [ ?MODULE, MethodAtom, Parameters, LocatedModule ]),

			wooper_effective_method_execution( LocatedModule, MethodAtom,
				State, Parameters );


		hashtable_key_not_found ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% Method name and arity returned as separate tuple elements,
					% as if in a single string ("M/A"), the result is displayed
					% as a list:
					error_logger:error_msg( "WOOPER error for PID ~w, "
						"oneway method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length(Parameters)+1, Parameters ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_method_not_found, self(),
							State#state_holder.actual_class,
							MethodAtom, length(Parameters)+1, Parameters } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash:
					error_logger:error_msg( "WOOPER error for PID ~w, "
						"request method ~s:~s/~B not found, "
						"parameters were:~n~p~n",
						[ self(), State#state_holder.actual_class, MethodAtom,
							length(Parameters)+1, Parameters ] ),

					{ State, { wooper_method_not_found, self(),
							  State#state_holder.actual_class,
							  MethodAtom, length(Parameters)+1, Parameters } }

			end

		% No other term can be returned.

	end.



% Exactly as wooper_execute_method, except that the target module (class) is
% directly specified, instead of being determined from the instance virtual
% table.
%
wooper_execute_method_with( ClassName, MethodAtom, State, Parameters ) ->

	% FIXME One check should be added: ClassName must be a super-class
	% (direct or not) of the actual class.

	wooper_effective_method_execution( ClassName, MethodAtom, State,
		Parameters ).



% Triggers the actual method execution.
%
wooper_effective_method_execution( SelectedModule, MethodAtom, State,
		Parameters ) ->

	%io:format( "WOOPER: effective execution of ~p:~p.~n",
	%		  [ SelectedModule, MethodAtom ] ),

	try apply( SelectedModule, MethodAtom, [ State | Parameters ] ) of

		% Method returning a result (and a state of course):
		% ?wooper_return_state_result:
		{ NewState, Result }  ->
			{ NewState, {wooper_result,Result} };

		% Void method (no result returned, only a state):
		% ?wooper_return_state_only:
		NewState ->
			{ NewState, wooper_method_returns_void }

	catch

		% All next cases are error cases.
		%
		% One option is to return an appropriate error term, but it is useful
		% only for requests, as oneways send back no result.
		%
		% Another option is to let the faulty process crash: oneways would not
		% send more answers, but linked and monitoring processes could
		% nevertheless be notified.

		% Finally, failed requests result in a log, an error answer being
		% returned, then a crash if the error is due to internal reasons,
		% whereas failed oneways result in a log then a crash, similarly if the
		% error is due to internal reasons.

		% User-defined exceptions are not caught, they will be rethrown and make
		% the process fail.

		% This the counterpart of {'EXIT',ErrorTerm}.
		% What would be the counterpart of {'EXIT',Pid,ErrorTerm}?
		% Reason can be: throw, error or exit.
		Reason:ErrorTerm ->

			case State#state_holder.request_sender of

				undefined ->

					% This is a oneway, so log and crash:
					% (error term would often be unreadable with ~p)

					error_logger:error_msg( "~nWOOPER error for PID ~w, "
								"oneway method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters, erlang:get_stacktrace(),
									wooper_state_toString(State) ] ),

					% Wait a bit as error_msg seems asynchronous:
					timer:sleep( ?error_display_waiting ),

					% Terminates the process:
					throw( { wooper_oneway_failed, self(), SelectedModule,
						MethodAtom, length(Parameters)+1, Parameters,
						ErrorTerm } );

				_ ->

					% This is a request, send error term and rely on the calling
					% function (wooper_main_loop) to crash: (error term would
					% often be unreadable with ~p)
					error_logger:error_msg( "~nWOOPER error for PID ~w, request"
								" method ~s:~s/~B failed (cause: ~p):~n~n"
								" - with error term:~n~p~n~n"
								" - for parameters:~n~p~n~n"
								" - stack trace was (latest calls first):"
								"~n~p~n~n"
								" - instance state was: ~s~n~n",
								[ self(), SelectedModule, MethodAtom,
									length(Parameters)+1, Reason, ErrorTerm,
									Parameters, erlang:get_stacktrace(),
									wooper_state_toString(State) ] ),

					{ State,
						{ wooper_method_failed, self(), SelectedModule,
							MethodAtom, length(Parameters)+1,
							Parameters, ErrorTerm } }

			end

	end.



% Parameter-less request.
executeRequest( State, RequestAtom ) ->

	%io:format("executeRequest/2: executing ~s() from ~s.~n",
	%	[ RequestAtom, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Allows to call synchronously from the code of a given class its actual
% overridden methods (requests, here), including from child classes.
%
% Example: If in a start method of an EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeRequest should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequest( State, RequestAtom, ArgumentList )
  when is_list(ArgumentList) ->

	%io:format("executeRequest/3 with list: executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, ArgumentList, State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };



% Here third parameter is not a list:
executeRequest( State, RequestAtom, StandaloneArgument ) ->

	%io:format("executeRequest/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ RequestAtom, StandaloneArgument,
	% State#state_holder.actual_class ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{request_sender=self()},

	% No special checking performed in release mode:
	{ NewState, {wooper_result,Result} } = wooper_execute_method( RequestAtom,
		SenderAwareState, [ StandaloneArgument ] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Parameter-less request, calling the version of the method as defined in the
% specified class.
executeRequestWith( State, ClassName, RequestAtom ) ->

	%io:format("executeRequestWith/3: executing ~s() from ~s with ~s.~n",
	%	[ RequestAtom , State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result}.




% Allows to call synchronously from the code of a given class overridden methods
% (requests, here) as defined in specified classes.
%
% If no failure occurs, returns {wooper_result,NewState,Result}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeRequestWith( State, ClassName, RequestAtom, ArgumentList )
  when is_list(ArgumentList) ->

	%io:format("executeRequestWith/4 with list: executing ~s(~w) from ~s "
	%  "with ~s.~n", [ RequestAtom, ArgumentList,
	% State#state_holder.actual_class, ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, ArgumentList ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result };


% Here the third parameter is not a list:
executeRequestWith( State, ClassName, RequestAtom, StandaloneArgument ) ->

	%io:format("executeRequestWith/3 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ RequestAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% Auto-calling method:
	SenderAwareState = State#state_holder{ request_sender=self() },

	% Correction checking by pattern-matching:
	{ NewState, {wooper_result,Result} } = wooper_execute_method_with(
		ClassName, RequestAtom, SenderAwareState, [ StandaloneArgument ] ),

	% Returns:
	{ NewState#state_holder{ request_sender=undefined }, Result }.



% Parameter-less oneway.
executeOneway( State, OnewayAtom ) ->

	%io:format("executeOneway/2: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom, State, [] ),

	% Returns:
	NewState.



% Allows to call synchronously from the code of a given class its actual
% overridden methods (oneways, here), including from child classes.
%
% Example: If in a start method of a EngineVehicle class one wants to call the
% (possibly overridden by, say, a class Car) startEngine method, then
% executeOneway should be used: 'MyVehicle ! startEngine' would not be
% synchronous, startEngine() would call EngineVehicle:startEngine instead of
% Car:startEngine when called from a Car instance, and of course EngineVehicle
% should know nothing from its Car child class.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOneway( State, OnewayAtom, ArgumentList ) when is_list(ArgumentList) ->

	%io:format("executeOneway/3 with list: executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom,
										  State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOneway( State, OnewayAtom, StandaloneArgument ) ->

	%io:format("executeOneway/3 with standalone argument: "
	%	"executing ~s(~w) from ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Less checking performed in release mode:
	{ NewState, _VoidResult } = wooper_execute_method( OnewayAtom,
										  State, [ StandaloneArgument ] ),

	% Returns:
	NewState.




% Parameter-less oneway, relying on the specified actual class to be used,
% instead of determining it from the instance virtual table.
%
executeOnewayWith( State, ClassName, OnewayAtom ) ->

	%io:format("executeOnewayWith/3: executing ~s() from ~s.~n",
	%	[ OnewayAtom, State#state_holder.actual_class ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with( ClassName,
										  OnewayAtom, State, [] ),

	% Returns:
	NewState.



% Allows to call synchronously from the code of a given class the oneway defined
% in specified class, instead of determining it from the instance virtual table.
%
% If no failure occurs, returns {wooper_result,NewState}.
%
% Note: Stripped-down version of wooper_main_loop.
%
executeOnewayWith( State, ClassName, OnewayAtom, ArgumentList )
  when is_list(ArgumentList) ->

	%io:format("executeOneway/4 with list: executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, ArgumentList, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, ArgumentList ),

	% Returns:
	NewState;


% Here third parameter is not a list:
executeOnewayWith( State, ClassName, OnewayAtom, StandaloneArgument ) ->

	% io:format("executeOneway/4 with standalone argument: "
	%	"executing ~s(~w) from ~s with ~s.~n",
	%	[ OnewayAtom, StandaloneArgument, State#state_holder.actual_class,
	% ClassName ]),

	% No request_sender to change with oneways.

	% Correction checking by pattern-matching:
	{ NewState, _VoidResult } = wooper_execute_method_with(
		ClassName, OnewayAtom, State, [ StandaloneArgument ] ),

	% Returns:
	NewState.


-endif.




% Section for functions whose definitions do not change depending on the debug
% mode.



% Deletes (asynchronously: "fire and forget") the WOOPER instance(s) potentially
% stored in the specified attribute list.
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: DeleteState = delete_any_instance_referenced_in( [
% first_pid_attr, second_pid_attr ], State ) or
% delete_any_instance_referenced_in( my_pid_attr, State ).
%
% (helper)
%
-spec delete_any_instance_referenced_in( [ attribute_name() ], wooper_state() )
									   -> wooper_state().
delete_any_instance_referenced_in( _Attributes=[], State ) ->
	State;

delete_any_instance_referenced_in( [ PidAttribute | T ], State ) ->

	NewState = case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid(Pid) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end,
	delete_any_instance_referenced_in( T, NewState );

delete_any_instance_referenced_in( PidAttribute, State ) ->

	case ?getAttr(PidAttribute) of

		undefined ->
			State;

		Pid when is_pid(Pid) ->
			Pid ! delete,
			setAttribute( State, PidAttribute, undefined )

	end.





% Deletes (synchronously, in a parallel yet blocking manner) the WOOPER
% instance(s) potentially stored in specified attribute list (a standalone
% attribute may be specified as well).
%
% Sets the corresponding attribute(s) to 'undefined', returns an updated state.
%
% Ex: in a destructor: NewState =
% delete_synchronously_any_instance_referenced_in( [ first_pid_attr,
% second_pid_attr ], State ) or
% delete_synchronously_any_instance_referenced_in( my_pid_attr, State ).
%
-spec delete_synchronously_any_instance_referenced_in(
	[ attribute_name() ] | attribute_name(), wooper_state() ) -> wooper_state().
delete_synchronously_any_instance_referenced_in( _Attributes=[], State ) ->
	State;

delete_synchronously_any_instance_referenced_in( Attributes, State )
  when is_list( Attributes ) ->

	% Triggers the deletion of selected instances:
	{ TargetAttributes, TargetPids } = delete_pid_from( Attributes, State ),

	%io:format( "delete_synchronously_any_instance_referenced_in:~n"
	%			" - attributes are: ~p~n"
	%			" - PIDs are: ~p~n", [ TargetAttributes, TargetPids ] ),

	% Waits for their completion:
	wait_for_deletion_ack( TargetPids ),

	%io:format( "(all deletion acks received for ~p)~n", [ TargetAttributes ] ),

	% Erases deleted PIDs:
	UndefinedAttributes = [ {AttrName,undefined} ||
							  AttrName <- TargetAttributes ],

	setAttributes( State, UndefinedAttributes );


delete_synchronously_any_instance_referenced_in( Attribute, State ) ->

	case ?getAttr(Attribute) of

		undefined ->
			State;

		Pid when is_pid(Pid) ->
			Pid ! { synchronous_delete, self() },

			receive

				{ deleted, Pid } ->
					setAttribute( State, Attribute, undefined )

			end

	end.




% Helper, which sends delete messages to all PID found in the list of
% attributes, and returns a list of the attributes and a list of the PÏDs.
%
delete_pid_from( Attributes, State ) ->

	DeleteMessage = { synchronous_delete, self() },

	delete_pid_from( Attributes, DeleteMessage, State, _AccAttr=[],
					_AccPid=[] ).


delete_pid_from( _Attributes=[], _DeleteMessage, _State, AccAttr, AccPid ) ->
	{ AccAttr, AccPid };

delete_pid_from( [ Attr | T ], DeleteMessage, State, AccAttr, AccPid ) ->

	case ?getAttr( Attr ) of

		undefined ->
			delete_pid_from( T, DeleteMessage, State, AccAttr, AccPid ) ;

		Pid when is_pid(Pid) ->

			%io:format( "Deleting now ~s (PID: ~w).~n", [ Attr, Pid ] ),

			Pid ! DeleteMessage,
			delete_pid_from( T, DeleteMessage, State, [ Attr | AccAttr ],
					[ Pid | AccPid ] )

	end.



% Deletes specified instance synchronously.
%
% Will wait forever the effective termination of specified instance.
%
-spec delete_synchronously_instance( pid() ) -> basic_utils:void().
delete_synchronously_instance( InstancePid ) ->

	%io:format( "delete_synchronously_instance for ~p.~n", [ InstancePid ] ),

	InstancePid ! { synchronous_delete, self() },

	receive

		{ deleted, InstancePid } ->
			ok

	end.



% Deletes specified instances synchronously (yet in parallel).
%
% Will wait forever the effective termination of all instances (and will
% regularly write a message on the console if waiting for too long) .
%
% (exported helper)
%
-spec delete_synchronously_instances( [ pid() ] ) -> basic_utils:void().
delete_synchronously_instances( InstanceList ) ->

	%io:format( "delete_synchronously_instances for ~p.~n", [ InstanceList ] ),

	DeleteMessage = { synchronous_delete, self() },

	[ I ! DeleteMessage || I <- InstanceList ],

	wait_for_deletion_ack( InstanceList ).



% Helper used to wait for the receiving of all deletion acknowledgements:
%
% Could almost use basic_utils:wait_for_acks/3.
%
wait_for_deletion_ack( _WaitedPids=[] ) ->
	ok;

wait_for_deletion_ack( WaitedPids ) ->

	receive

		{ deleted, Pid } ->

			case lists:member( Pid, WaitedPids ) of

				false ->
					throw( { unexpected_pid_deletion, Pid } );

				true ->
					NewWaitedPids = lists:delete( Pid, WaitedPids ),
					wait_for_deletion_ack( NewWaitedPids )

			end

	% Note that this time-out is reset at each ack:
	after ?synchronous_time_out ->

			NewWaitedPids = examine_waited_deletions( WaitedPids, _Acc=[] ),

			io:format( "(still waiting for the synchronous deletion of "
					   "following live WOOPER instance(s): ~p)~n",
					   [ NewWaitedPids ] ),

			% Warns, but does not trigger failures:
			wait_for_deletion_ack( NewWaitedPids )

	end.



examine_waited_deletions( _WaitedPids=[], Acc ) ->
	Acc;

examine_waited_deletions( _WaitedPids=[ Pid | T ], Acc ) ->

	case erlang:is_process_alive(Pid) of

		true ->
			examine_waited_deletions( T, [ Pid | Acc ] );

		false ->
			io:format( "Stopped waiting for the deletion of instance "
					   "whose PID is ~p: not found alive.~n", [ Pid ] ),

			examine_waited_deletions( T, Acc )

	end.





% Serialisation/deserialisation section.


% For some classes, the implementor may want to define pre/post hooks for
% serialisation/deserialisation.
%
% In this case he should define 'wooper_serialisation_hooks' and specify the
% four corresponding hooks.
%
-ifndef(wooper_serialisation_hooks).



% Default do-nothing hooks:


% Triggered just before serialisation.
%
% We are here to return a state directly suitable for serialisation, for example
% with no transient technical identifiers (like PID, open files, etc.) - unless
% a later entry transformer is able to manage them.
%
-spec pre_serialise_hook( wooper_state() ) -> wooper_state().
pre_serialise_hook( State ) ->
	State.



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, that
% will be written.
%
% (we do not want to return a state, as we do not want that a state modified by
% the serialisation be mistakenly used afterwards)
%
-spec post_serialise_hook( class_name(), term_serialisation(), wooper_state() )
						 -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
% Default version corresponding to post_serialise_hook/3.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								 term_serialisation().
pre_deserialise_hook( _SerialisationTerm={ _Classname, Entries }, _UserData ) ->
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper_state() ) -> wooper_state().
post_deserialise_hook( State ) ->
	State.


-endif.



% Serialises the specified instance (i.e. the state thereof), using specified
% entry transformer and user data.
%
% Returns a binary corresponding to a { InnerPair, UpdatedUserData } pair made
% of:
%
% 1. an inner pair { Classname, Entries }, converted into a binary and
% containing the corresponding class name (as an atom) and the associated
% serialisation (as a list of transformed terms)
%
% 2. the resulting user-data (possibly modified by the successive operations
% done by the entry transformer)
%
% This is a method, but as it is defined unconditionally in the WOOPER header
% (in the future, a WOOPER object superclass will be available, defining a
% default, overridable serialise/3 method), it cannot be overridden on a
% per-class basis. Hence the needs for serialisation hooks. We still need to
% specify the classname however, as at deserialisation time this information
% will be necessary.
%
% (const request)
%
% Notes:
%
% - we do not take the 'request_sender' field into account for serialisation,
% as, by design, there is indeed such a caller (since serialise/3 is a request),
% but it is of no interest here
%
% - as we do not have a typed (class-specific) state (record-like)
% data-structures, we have to store not only the values, but also the keys; a
% major margin of compactness still lies there
%
-spec serialise( wooper_state(), entry_transformer(), basic_utils:user_data() )
%		  -> request_result( { bin_serialisation(), basic_utils:user_data() } ).
		  -> request_result( { any(), basic_utils:user_data() } ).
% Note: the first element of the returned pair should be bin_serialisation(),
% i.e. binary() (both manual checking and Dialyzer displayed success typing
% agree), however unless it is replaced by any(), Dialyzer will complain. Very
% strange!
%
serialise( State, _EntryTransformer=undefined, UserData ) ->

	% Here no entry transformer is to be used, raw serialisation.

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{ attribute_table=AttributeTable,
					actual_class=Classname } = pre_serialise_hook( State ),

	io:format( " - serialising, with no transformer, instance ~p of class ~s~n",
			  [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc.
	%
	% The WOOPER serialisation mechanisms do not account for them currently (ex:
	% links may be dictated by the application logic and thus may not have to be
	% stored), except one: the current random state of the serialised process,
	% which is transparently managed by WOOPER so that the deserialisation will
	% lead to restoring the right random state.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },


	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort( [ RandomAttribute |
					   ?wooper_hashtable_type:enumerate( AttributeTable )  ] ),

	% By default returns { Classname, Entries }:
	FullContent = post_serialise_hook( Classname, Entries, PreState ),

	SerialisedContent = term_to_binary( FullContent,
										_Opts=[ { compressed, 9 } ] ),

	Res = { SerialisedContent, UserData },

	% Yes, it is 'State', as we do not want to continue with any state forged
	% for the serialisation (ex: with transformed local processes), we want to
	% continue as we were!
	%
	?wooper_return_state_result( State, Res );



serialise( State, EntryTransformer, UserData ) ->

	% Here an entry transformer is to be used, for a smarter serialisation (ex:
	% PID-aware).

	% Hooks may be defined on a per-class basis:

	PreState = #state_holder{ attribute_table=AttributeTable,
					actual_class=Classname } = pre_serialise_hook( State ),

	io:format( " - serialising, with transformer, instance ~p of class ~s~n",
			  [ self(), Classname ] ),

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc.
	%
	% The WOOPER serialisation mechanisms do not account for them currently (ex:
	% links may be dictated by the application logic and thus may not have to be
	% stored), except one: the current random state of the serialised process,
	% which is transparently managed by WOOPER so that the deserialisation will
	% lead to restoring the right random state.

	% So, let's add the WOOPER extra information:
	CurrentRandomState = random_utils:get_random_state(),

	RandomAttribute = { wooper_random_state, CurrentRandomState },

	% Retrieving all attribute key/value pairs (sorting is probably a bit
	% cleaner):
	%
	Entries = lists:sort( [ RandomAttribute |
					   ?wooper_hashtable_type:enumerate( AttributeTable )  ] ),

	%io:format( "Original entries:~n~p~n", [ Entries ] ),

	% Applying the entry transformer on each of them:
	{ TransformedEntries, FinalUserData } = lists:foldl( EntryTransformer,
					  _Acc0={ _ResultingEntries=[], UserData }, _List=Entries ),

	%io:format( "Transformed entries:~n~p~n", [ TransformedEntries ] ),

	% No need to reverse the transformed list.

	% By default returns { Classname, TransformedEntries }:
	FullContent = post_serialise_hook( Classname, TransformedEntries,
									   PreState ),

	SerialisedContent = term_to_binary( FullContent,
										_Opts=[ { compressed, 9 } ] ),

	Res = { SerialisedContent, FinalUserData },

	% Yes, it is 'State', as we do not want to continue with any state forged
	% for the serialisation (ex: with transformed local processes), we want to
	% continue as we were!
	%
	?wooper_return_state_result( State, Res ).



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
-spec handle_private_processes( [ attribute_name() ], wooper_state() ) ->
									  wooper_state().
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
% 'undefined' by a term restoration marker) , for example so that they can
% escape the serialisation process.
%
% Typically used in pre_serialise_hook/2, to store only relevant, useful
% information.
%
% (helper)
%
-spec wooper_mute_attributes( [ attribute_name() ], wooper_state() ) ->
	wooper_state().
wooper_mute_attributes( AttributeNameList, State ) ->

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
-spec wooper_check_attributes_equal( [ attribute_name() ],
		 [ attribute_entry() ], wooper_state() ) -> basic_utils:void().
wooper_check_attributes_equal( _AttributeNames=[], _AttributeEntries,
							  _State ) ->
	ok;

wooper_check_attributes_equal( _AttributeNames=[ AttributeName | T ],
							  AttributeEntries, State ) ->

	{ AttributeValue, RemainingEntries } = option_list:extract(
								 _K=AttributeName, AttributeEntries ),

	case ?getAttr(AttributeName) of

		AttributeValue ->
			wooper_check_attributes_equal( T, RemainingEntries, State );

		OtherValue ->
			throw( { attribute_value_mismatch, AttributeName,
					{ OtherValue, AttributeValue } } )

	end.



% Replaces the value held in the specified state by the one of the specified
% attribute found in specified entry.
%
% (helper)
%
-spec wooper_replace_attribute( attribute_name(), [ attribute_entry() ],
				wooper_state() ) -> { [ attribute_entry() ], wooper_state() }.
wooper_replace_attribute( AttributeName, AttributeEntries, State ) ->

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
-spec wooper_replace_attributes( [ attribute_name() ], [ attribute_entry() ],
				wooper_state() ) -> { [ attribute_entry() ], wooper_state() }.
wooper_replace_attributes( AttributeNames, AttributeEntries, State ) ->

	lists:foldl( fun( AttrName, { AccEntries, AccState } ) ->
					wooper_replace_attribute( AttrName, AccEntries, AccState )
			end,

			_Acc0={ AttributeEntries, State },

			_List=AttributeNames ).



% Extracts the value (supposedly, any type of list) of specified attribute from
% specified entries, and append that list to the corresponding one found in the
% specified state, stored under the same attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec wooper_merge_list_for( attribute_name(), [ attribute_entry() ],
			wooper_state() ) -> { [ attribute_entry() ], wooper_state() }.
wooper_merge_list_for( AttributeName, AttributeEntries, State ) ->

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
-spec wooper_merge_lists_for( [ attribute_name() ], [ attribute_entry() ],
	wooper_state() ) -> { [ attribute_entry() ], wooper_state() }.
wooper_merge_lists_for( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(

			fun( AttrName, { AccEntries, AccState } ) ->
					wooper_merge_list_for( AttrName, AccEntries, AccState )
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


% Deserialises the specified instance from its serialised form (as a term, not
% as a binary), to obtain the corresponding state of an instance, using
% specified entry transformer and user data, then having the executing process
% embody the corresponding instance from then.
%
% Does not return, as the WOOPER main loop will manage from then this just
% deserialised instance.
%
% (helper, as the receiver process may not even be already a WOOPER instance)
%
% Note: the hosting process is not created here, as for an increased parallelism
% we expect deserialisations to happen directly from the final instance
% processes: we consider here that the process executing this helper is the
% final host.
%
-spec wooper_deserialise( term_serialisation(), entry_transformer(),
				   basic_utils:user_data(), ListenerPid ) -> no_return()
						   when ListenerPid :: basic_utils:maybe( pid() ).
wooper_deserialise( SerialisedEntries, EntryTransformer, UserData,
					ListenerPid ) ->

	% First we extract the WOOPER extra information:
	{ RandomState, OtherEntries } = option_list:extract( wooper_random_state,
													SerialisedEntries ),

	HookedEntries = pre_deserialise_hook( { ?MODULE, OtherEntries },
											  UserData ),

	% The class name is implicit, based on the choice of the module on which
	% this helper is called:
	%
	ActualClassName = ?MODULE,

	% Slight optimisation compared to using wooper_retrieve_virtual_table/0
	% upfront later:
	%
	wooper_get_class_manager() ! { get_table, ActualClassName, self() },

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


	% Deferred wooper_retrieve_virtual_table/0 answer:
	VirtualTable = receive

		{ virtual_table, ?MODULE, Table } ->
			Table

	end,

	ForgedState = #state_holder{

		virtual_table   = VirtualTable,

		attribute_table = OptimisedAttributeTable,

		actual_class    = ActualClassName,

		request_sender  = undefined

	 },


	% We could check here that no serialisation marker remains, with a specific
	% entry transformer and list_restoration_markers/0.

	FinalState = post_deserialise_hook( ForgedState ),


	% That's as simple as that!

	wooper_main_loop( FinalState ).




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

	spawn( fun() -> wooper_deserialise( BinSerialisation, EntryTransformer,
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
	spawn_link( fun() -> wooper_deserialise( BinSerialisation, EntryTransformer,
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

	SpawnedPid = spawn( fun() -> wooper_deserialise( BinSerialisation,
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

	SpawnedPid = spawn_link( fun() -> wooper_deserialise( BinSerialisation,
					   EntryTransformer, UserData, _ListenerPid=CreatorPid )
							end ),

	% Blocks until the spawned process answers:
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	end.



% We did not felt the specific need to define:
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
			fun() -> wooper_deserialise( BinSerialisation, EntryTransformer,
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
			fun() -> wooper_deserialise( BinSerialisation, EntryTransformer,
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




% Sends the specified request to all specified processes for execution, in
% parallel, and returns the corresponding results.
%
% Note: no specified order is enforced in the result list; hence this helper is
% meant to be used when we can collect each result irregardless of its specific
% sender.
%
% (exported helper)
%
-spec obtain_results_for_request( request_name(), method_arguments(),
						  [ pid() ] ) -> [ any() ].
obtain_results_for_request( RequestName, Parameters, TargetPidList ) ->
	CommonMessage = { RequestName, Parameters, self() },
	% Sending done in parallel:
	[ Pid ! CommonMessage || Pid <- TargetPidList ],

	% Of course we expect that no previously received WOOPER message is
	% remaining in the queue.

	collect_wooper_messages( _Count=length( TargetPidList ), _Acc=[] ).



% Collects specified number of WOOPER messages, and returns a list of the
% corresponding results.
%
% (helper)
%
collect_wooper_messages( _Count=0, Acc ) ->
	Acc;

collect_wooper_messages( Count, Acc ) ->

	receive

		{ wooper_result, Res } ->
			collect_wooper_messages( Count-1, [ Res | Acc ] )

	end.
