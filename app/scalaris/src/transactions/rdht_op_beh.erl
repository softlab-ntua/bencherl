%% @copyright 2009 onScale solutions GmbH

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

%% @author Florian Schintke <schintke@onscale.de>
%% @doc Part of generic transactions implementation.
%%      The behaviour of an operation in a transaction.
%% @version $Id: rdht_op_beh.erl 2639 2012-01-03 15:00:45Z kruber@zib.de $
-module(rdht_op_beh).
-author('schintke@onscale.de').
-vsn('$Id: rdht_op_beh.erl 2639 2012-01-03 15:00:45Z kruber@zib.de $').

%-define(TRACE(X,Y), io:format(X,Y)).
-define(TRACE(X,Y), ok).
% for behaviour
-ifndef(have_callback_support).
-export([behaviour_info/1]).
-endif.

-ifdef(have_callback_support).
-callback tlogentry_get_status(tx_tlog:tlog_entry()) -> tx_tlog:tx_status().
-callback tlogentry_get_value(tx_tlog:tlog_entry()) -> any().
-callback tlogentry_get_version(tx_tlog:tlog_entry()) -> integer().
-else.
-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [
     {tlogentry_get_status, 1},
     {tlogentry_get_value, 1},
     {tlogentry_get_version, 1}
    ];
behaviour_info(_Other) ->
    undefined.
-endif.
