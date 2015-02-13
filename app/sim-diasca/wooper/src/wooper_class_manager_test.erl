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


% Unit tests for the WOOPER class manager implementation.
% See the wooper_class_manager.erl tested module.

-module(wooper_class_manager_test).


-include("test_facilities.hrl").


% For wooper_class_manager_name:
-include("wooper_class_manager.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	TestedModule = wooper_class_manager,

	test_facilities:display( "Spawning module ~s.", [ TestedModule ] ),

	spawn( TestedModule, start, [self()] ),
	receive

		class_manager_registered ->
			test_facilities:display( "Requesting its state display." ),
			?wooper_class_manager_name ! display

	% 10-second time-out:
	after 10000 ->
		test_facilities:fail( "#### wooper_get_class_manager: unable to find "
			"class manager after 10s, test failed." )

	end,

	test_facilities:display( "Requesting it to stop." ),

	?wooper_class_manager_name ! stop,

	% Probably, if ExitAfterTest is set, the test will stop before the manager
	% itself.
	test_facilities:stop().
