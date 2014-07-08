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



% Module containing some facilities for WOOPER users.
%
-module(wooper).


-export([ send_request/3, wait_for_request_answers/2,
		  wait_for_request_answers/3,
		  send_and_wait_request/4, send_and_wait_request/5
		]).


% Not module(), which can be also a tuple():
-type class_name() :: atom().

-type method_name() :: atom().

-type request_name() :: method_name().
-type oneway_name()  :: method_name().



% List of arguments, or non-list standalone one:
-type method_argument() :: any().

% Standalone (non-list) arguments may be specified:
-type method_arguments() :: [ method_argument() ] | method_argument().


% Describes the outcome of a set of requests: either all succeeded, or some
% failed (that are specified).
%
-type requests_outcome() :: 'success' | { 'failure', [ pid() ] }.



-export_type([ class_name/0, method_name/0, request_name/0, oneway_name/0,
			method_argument/0, method_arguments/0, requests_outcome/0 ]).



% Sends specified request (based on its names and arguments) to each of the
% specified target instances.
%
% (helper)
%
-spec send_request( request_name(), method_arguments(), [ pid() ] ) ->
						  basic_utils:void().
send_request( RequestName, RequestArgs, TargetInstancePIDs ) ->

	Request = { RequestName, RequestArgs, self() },

	[ InstancePid ! Request || InstancePid <- TargetInstancePIDs ].



% Waits for an answer (acknowledgement, based on specified atom) from the
% specified requested instances.
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ pid() ], atom() ) -> requests_outcome().
wait_for_request_answers( RequestedPidList, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ).



% Waits for an answer (acknowledgement, based on specified atom) from the
% specified requested instances, unless the specified time-out is exceeded
% (specified as integer milliseconds or the 'infinity' atom).
%
% Allows to trigger requests in parallel yet being able to wait synchronously
% for them.
%
% (helper)
%
-spec wait_for_request_answers( [ pid() ], basic_utils:time_out(), atom() ) ->
					  requests_outcome().
wait_for_request_answers( RequestedPidList, _Timeout=infinity, AckAtom ) ->
	wait_indefinitively_for_request_answers( RequestedPidList, AckAtom );

wait_for_request_answers( RequestedPidList, Timeout, AckAtom ) ->

	InitialTimestamp = basic_utils:get_timestamp(),

	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  AckAtom ).





% Waits until end of time if necessary.
%
% (helper)
%
wait_indefinitively_for_request_answers( _RequestedPidList=[], _AckAtom ) ->
	success;

wait_indefinitively_for_request_answers( RequestedPidList, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_indefinitively_for_request_answers( NewPidList, AckAtom )

	end.




% Wait until specified time-out is reached.
%
% (helper)
%
wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  AckAtom ) ->
	wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
							  _DefaultPollDuration=1000, AckAtom ).


wait_for_request_answers( _RequestedPidList=[], _InitialTimestamp, _Timeout,
						  _PollDuration, _AckAtom ) ->
	success;

wait_for_request_answers( RequestedPidList, InitialTimestamp, Timeout,
						  PollDuration, AckAtom ) ->

	receive

		{ wooper_result, { AckAtom, SenderPid } } ->

			NewPidList = list_utils:delete_existing( SenderPid,
													 RequestedPidList ),

			wait_for_request_answers( NewPidList, InitialTimestamp, Timeout,
									  PollDuration, AckAtom )

	after PollDuration ->

			NewDuration = basic_utils:get_duration( InitialTimestamp,
											  basic_utils:get_timestamp() ),

			case NewDuration > Timeout of

				true ->
					{ failure, RequestedPidList };

				false ->
					% Still waiting then:
					wait_for_request_answers( RequestedPidList,
						   InitialTimestamp, Timeout, PollDuration, AckAtom )

			end

	end.



% Sends specified request (based on its names and arguments) to each of the
% specified target instances, and waits (indefinitively) for their
% acknowledgement.
%
-spec send_and_wait_request( request_name(), method_arguments(), [ pid() ],
						   atom() ) -> basic_utils:void().
send_and_wait_request( RequestName, RequestArgs, TargetInstancePIDs,
					   AckAtom ) ->

	send_request( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_indefinitively_for_request_answers( TargetInstancePIDs, AckAtom ).



% Sends specified request (based on its names and arguments) to each of the
% specified target instances, and waits for their acknowledgement ; returns
% whether it succeeded or if some instances triggered a time-out.
%
-spec send_and_wait_request( request_name(), method_arguments(), [ pid() ],
					   basic_utils:time_out(), atom() ) -> requests_outcome().
send_and_wait_request( RequestName, RequestArgs, TargetInstancePIDs,
					   Timeout, AckAtom ) ->

	send_request( RequestName, RequestArgs, TargetInstancePIDs ),

	wait_for_request_answers( TargetInstancePIDs, Timeout, AckAtom ).
