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


% Unit tests for the random utils toolbox.
%
% See the random_utils.erl tested module.
%
-module(random_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	random_utils:start_random_source( default_seed ),

	RandomList = [ random_utils:get_random_value( X )
				   || X <- lists:seq( 1, 15 ) ]
			  ++ [ random_utils:get_random_value( X, Y )
				   || X <- lists:seq( 1, 5 ), Y <- lists:seq( 6, 15 )  ],

	Min = 5,
	Max = 12,
	Count = 500,

	AnotherList = random_utils:get_random_values( Min, Max, Count ),

	case length(AnotherList) == Count of

		true ->
			% All values (including bounds) must have been drawn at least one:
			[ true = lists:member( X, AnotherList )
			  || X <- lists:seq( Min, Max ) ];

		false ->
			throw( probable_faulty_batched_ranged_generation )

	end,

	random_utils:stop_random_source(),


	test_facilities:display(
		"Current module being used as random source: ~w.",
		[ random_utils:get_random_module_name() ] ),

	test_facilities:display( "A list of integer random values between 1 and 5 "
		"(both included): ~w.", [ RandomList ] ),

	test_facilities:stop().
