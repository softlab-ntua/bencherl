% Modular WOOPER header gathering all general-purposes defines.


% Note: types are defined here but exported in wooper.erl.



% Note: the hashtable type used by WOOPER (not the one exposed as a potential
% attribute) should be a preprocessor define
%
% The 'hashtable' choice should be the best one here, as we precisely know when
% to optimise the table (once for all).
%
-define( wooper_hashtable_type, hashtable ).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of actual
% attributes for a given class)
%
-define( wooper_attribute_count_upper_bound, 16 ).


% Number of milliseconds to wait for, in order to be sure that the error message
% could be written to the console, knowing that the operation is asynchronous
% and thus may not be performed should the VM halt immediately:
%
% (otherwise you will not see any stacktrace)
%
-define( wooper_error_display_waiting, 400 ).




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
		% To be used, instead of ?MODULE or alike.
		%
		actual_class     :: basic_utils:module_name(),

		request_sender   :: pid() | 'undefined'

} ).



% We should now prefer using wooper:state() to wooper:state():
%-type wooper:state() :: wooper:state().



% Now that type-checking on the state record is performed in debug mode, in both
% modes method results are sent directly:
%
% (no wooper_result atom added any more in debug mode)
%
-define( wooper_return_state_result( State, Result ), { State, Result } ).
-define( wooper_return_state_only( State ), State ).



-ifdef(wooper_debug).


	% Uncomment to have all WOOPER recompiled classes output verbosely their
	% information:
	% (useful when everything is compiled without this flag and then
	% uncommenting the flag to recompile only the class(es) to debug)
	%-define(wooper_log_wanted,).

-ifdef(wooper_log_wanted).

	-define( wooper_log( Msg ), io:format( Msg ) ).
	-define( wooper_log_format( Msg, Format ), io:format( Msg, Format ) ).

-else. % wooper_log_wanted

	-define( wooper_log( Msg ), no_wooper_log ).
	-define( wooper_log_format( Msg, Format ), no_wooper_log ).

-endif. % wooper_log_wanted


-else. % wooper_debug


	-define( wooper_log( Msg ), no_wooper_log ).
	-define( wooper_log_format( Msg, Format ), no_wooper_log ).


-endif. % wooper_debug





% A reasonable duration (in milliseconds) before a time-out is triggered after a
% instance (notably, a created one) does not seem to answer properly.
%
% (we could block forever but for at least some cases it would make the
% debugging harder)


-ifndef(synchronous_time_out).


-ifdef(wooper_debug).


% Suitable for most applications (5 seconds):
-define(synchronous_time_out,5000).


-else. % wooper_debug


% Better for applications in production (30 minutes):
-define(synchronous_time_out, (30*60*1000) ).

% Also possible:
%-define(synchronous_time_out,infinity).

-endif. % wooper_debug


-endif. % ifndef(synchronous_time_out)
