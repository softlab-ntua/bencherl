% Modular WOOPER header gathering the main types of interest.


% Allows to define WOOPER base variables and methods for that class.


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


% To be specified more closely maybe:
-type method_internal_result() :: any().


% The actual value of interest returned by a request:
%-type request_result() :: any().
-type request_result( T ) :: T.


% Describes the outcome of a set of requests: either all succeeded, or some
% failed (that are specified).
%
-type requests_outcome() :: 'success' | { 'failure', [ pid() ] }.


-type request_return( T ) :: { wooper:state(), request_result( T ) }.
-type oneway_return() :: wooper:state().


-type attribute_name() :: atom().
-type attribute_value() :: any().

-type attribute_entry() :: { attribute_name(), attribute_value() }.



% A request is typically:
%
% -spec my_request :: fun( wooper:state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> request_return( T ).


% A oneway is typically:
%
% -spec my_oneway :: fun( wooper:state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> oneway_return().


% We prefer defining these types into an header file (this one) rather than in
% the wooper module, to lighten the syntax:
%
-export_type([ class_name/0, method_name/0, request_name/0, oneway_name/0,
			   method_argument/0, method_arguments/0, requests_outcome/0 ]).

-export_type([ request_result/1, request_return/1, oneway_return/0,
			   attribute_name/0, attribute_value/0, attribute_entry/0 ]).
