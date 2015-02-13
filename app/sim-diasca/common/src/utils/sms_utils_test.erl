%% -*- coding: utf-8 -*-

% Note that, without the UTF line above and with pre-R17 Erlang versions, sent
% text is garbled due to a wrong encoding.


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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the sms_utils toolbox.
%
% See the sms_utils.erl tested module.
%
-module(sms_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-export([ run/1 ]).



-spec run() -> no_return().
run() ->
	run( _EnableSMSSending=false ).



-spec run( boolean() ) -> no_return().
run( EnableSMSSending ) ->


	test_facilities:start( ?MODULE ),

	SMSAccount = case preferences:get( sms_account ) of

		undefined ->

			test_facilities:display( "(no SMS account found in preferences, "
									 "not trying to send any SMS)" ),

			test_facilities:stop();

		Account ->
			Account

	end,

	test_facilities:display( "Using account: ~s.",
							 [ sms_utils:account_to_string( SMSAccount ) ] ),

	Recipient = case preferences:get( mobile_number ) of

		undefined ->

			test_facilities:display( "(no target mobile number found "
									 "in preferences, not trying to "
									 "send any SMS)" ),

			test_facilities:stop();

		Number ->
			Number

	end,

	{ CreditRes, UpdatedSMSAccount } = sms_utils:update_credits( SMSAccount ),

	test_facilities:display( "Updated account (credit request answered: '~s'):"
							 " ~s.", [ CreditRes, sms_utils:account_to_string(
												   UpdatedSMSAccount ) ] ),

	% â becomes a, ~ disappears, {} and [] become (), etc.
	Message = "Le château d'Hélène est un bien beau château ! "
		"Test: &~\"#{([-|`_\ç^à@)])=+}*/,?;.§",

	%ServiceClass = eco
	ServiceClass = pro,

	FirstSMS = sms_utils:create_sms( Message, Recipient,
									 _SenderDescription="33616833333",
									 ServiceClass ),

	% Not wanting using all credits because of testings:
	SendRes = case EnableSMSSending of

		true ->
			sms_utils:send( FirstSMS, UpdatedSMSAccount );

		false ->
			test_facilities:display( "(test not allowed to send SMS, "
									 "stopping here.)" ),

			test_facilities:stop()

	end,

	test_facilities:display( "Result of the sending of ~s:~p.",
							 [ sms_utils:sms_to_string( FirstSMS ), SendRes ] ),

	test_facilities:stop().
