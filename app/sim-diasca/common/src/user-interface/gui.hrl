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
% Creation date: Tuesday, January 29, 2013


% Header to export gui-related defines.
%
% See gui.erl for the corresponding implementation.



% For WxWindows defines:
-include_lib("wx/include/wx.hrl").


-define( any_id, ?wxID_ANY ).

-define( no_parent, wx:null() ).


% The special color that means "transparent" (i.e. no filling):
-define( transparent_color, ?wxTRANSPARENT_BRUSH ).



% Rewriting of '-record(wx,' could have been, with maybe a better naming:
%
%-record( gui_event, {
%
%		   id :: id(),
%		   event_source :: gui_object(),
%		   user_data :: user_data(),
%		   event_type :: event_type()
%
%}).
%
%-type gui_event() :: #gui_event{}.
%
% Anyway we receive messages as wx records, and cannot change that easily.
