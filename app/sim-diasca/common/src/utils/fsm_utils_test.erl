% Copyright (C) 2003-2014 Olivier Boudeville
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


% Unit tests for the fsm_utils toolbox.
%
% See the fsm_utils.erl tested module.
%
-module(fsm_utils_test).



% For run/0 export and al:
%
-include("test_facilities.hrl").



% For FSM macro defines:
%
-include("fsm_utils.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	StartFsmState = fsm_utils:create_blank_fsm_state(),
	FsmState = ?setFsmAttribute( StartFsmState, test_question, 42 ),
	{ value, 42 } = ?getFsmAttr( test_question ),

	FsmStateTwo = fsm_utils:setFsmAttribute( FsmState, other_test_question,
											43 ),

	{ value, 43 } = fsm_utils:getFsmAttribute( FsmStateTwo,
											  other_test_question ),

	test_facilities:stop().
