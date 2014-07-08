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
% Creation date: Saturday, February 20, 2010.


% Unit tests for the option list implementation.
%
% See the option_list.erl tested module.
%
-module(option_list_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	SingleOptionList = option_list:new( [ {blue,2 } ] ),
	2 = option_list:get( blue, SingleOptionList ),

	% Pattern-match:
	SingleOptionList = option_list:set( {blue,2}, option_list:new() ),

	InitialOptionList = [ {yellow,1}, {blue,1}, {red,1}, {green,1},
						 {purple,1} ],

	test_facilities:display( "Initial option list: ~w.",
							[InitialOptionList] ),

	BlackOptionList = option_list:set( {black,1}, InitialOptionList ),
	test_facilities:display( "Option list with black entry added: ~w.",
			   [BlackOptionList] ),

	RedOptionList = option_list:set( {red,2}, BlackOptionList ),
	test_facilities:display( "Option list with red entry incremented: ~w.",
			   [RedOptionList] ),

	EndpointOptionList = option_list:set( {black,2},
		option_list:set( {purple,2}, RedOptionList ) ),

	test_facilities:display( "Option list with endpoints updated: ~w.",
			   [EndpointOptionList] ),

	SecondOptionList = option_list:set( {magenta,1}, SingleOptionList ),
	UpdatingOptionList = option_list:set( {black,3}, SecondOptionList ),

	UpdatedOptionList = option_list:update_with(
						EndpointOptionList, UpdatingOptionList),

	test_facilities:display( "Update of previous option list "
			"with option_list ~w is: ~w.",
			[ UpdatingOptionList, UpdatedOptionList ] ),

	3 = option_list:get( black ,UpdatedOptionList ),

	InitialOptionList = option_list:enumerate( InitialOptionList ),

	test_facilities:stop().
