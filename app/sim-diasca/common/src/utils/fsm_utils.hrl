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



% Sets specified attribute of the instance to the specified value, thanks to
% specified FSM state.
%
% Returns an updated state.
%
-define( setFsmAttribute( FsmState, AttributeName, AttributeValue ),
	hashtable:addEntry( AttributeName, AttributeValue, FsmState )
).



% Returns a {value,Value} pair, with Value being the value associated to
% specified named-designated attribute, if found, otherwise returns
% 'attribute_not_found'.
%
% See also: getAttr/1.
%
-define( getFsmAttribute( FsmState, AttributeName ),
	case hashtable:lookupEntry( AttributeName, FsmState ) of

		hashtable_key_not_found ->
			attribute_not_found ;

		Other ->
			Other

	end
).



% Returns a {value,Value} pair, with Value being the value associated to
% specified named-designated attribute, if found, otherwise returns
% 'attribute_not_found'.
%
% Beware to the implicit use of the 'FsmState' variable: in some cases other
% states should be used. See the getAttribute/2 macro.
%
-define( getFsmAttr(AttributeName), ?getFsmAttribute(FsmState,AttributeName) ).
