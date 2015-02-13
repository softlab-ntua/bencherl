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
% Creation date: August 30, 2007.


% Gathering of various convenient facilities.
% See fsm_utils_test.erl for the corresponding test.
-module(fsm_utils).


-export([ create_blank_fsm_state/0, setFsmAttribute/3, getFsmAttribute/2 ]).




% Approximate average attribute count for a given FSM state instance.
-define( FSMAttributeCountUpperBound, 5 ).




% Creates an attribute table appropriate to store a FSM state.
%
% setFsmAttribute, getFsmAttribute and getFsmAttr are to be used with these
% variables too.
%
-spec create_blank_fsm_state() -> hashtable:hashtable().
create_blank_fsm_state() ->
	hashtable:new( ?FSMAttributeCountUpperBound ).




% Sets specified FSM state attribute.
%
-spec setFsmAttribute( hashtable:hashtable(), hashtable:key(),
					  hashtable:value() ) -> hashtable:hashtable().
setFsmAttribute( FsmState, AttributeName, AttributeValue ) ->
	hashtable:addEntry( AttributeName, AttributeValue, FsmState ).




% Retrieves specified FSM state attribute.
%
% Returns either a {value,Value} pair, if the attribute is found, or
% 'attribute_not_found'.
%
-spec getFsmAttribute(hashtable:hashtable(),hashtable:key())
		-> hashtable:value() | 'attribute_not_found'.
getFsmAttribute( FsmState, AttributeName ) ->

	case hashtable:lookupEntry( AttributeName, FsmState) of

		hashtable_key_not_found->
			attribute_not_found ;

		Other ->
			Other

	end.
