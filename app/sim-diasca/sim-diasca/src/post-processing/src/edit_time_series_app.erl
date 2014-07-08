% Copyright (C) 2010-2012 Olivier Boudeville
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
% Creation date: Tuesday, January 11, 2011


% This is an Erlang script preferably to be called by the edit-time-series.sh
% wrapper script, for convenience.
%
-module(edit_time_series_app).


-include("app_facilities.hrl").


% No export needed for exec/0 here.

-spec exec() -> no_return().
exec() ->

	app_facilities:start( ?MODULE ),

	io:format( "Parameters received: ~p~n.", [ init:get_plain_arguments() ] ),

	app_facilities:stop().
