% Copyright (C) 2011-2014 Olivier Boudeville
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


% This header tends to be included in a lot of places:
-ifndef(data_types_hrl_guard).
-define(data_types_hrl_guard,).


% When needing random access to a potentially long list (ex: removing a specific
% element), using a plain list is less efficient than using more advanced
% containers.



% The list_impl macro allows to select once for all the list datatype that
% should be used internally:
%
% (gb_sets seems indeed the most suitable choice, but some measurements should
% be made to be sure of it)
%
% Note: this list_impl type should respect the gb_sets API and implicit
% contracts (ex: adding an element more than once must not change the data
% structure).
%
% Note: there are semantic differences between plain lists and list_impl
% ones. For example, a plain list can contain the same element more than once,
% while a list_impl cannot (i.e. union/2 will not result in duplicates).
%
%-define(list_impl,ordsets).
-define(list_impl,gb_sets).


% For homogeneous lists (at least to declare them as such):
% -type ?list_impl( T ) :: ?list_impl().


% The type of list_impl, for specifications:
%
-define(list_impl_type,gb_sets:set()).

% No compliant with success typings:
%-define(list_impl_type,set()).


% All kind of lists:
%
-type any_list() :: list() | ?list_impl_type.


-endif. % data_types_hrl_guard
