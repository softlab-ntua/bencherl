% Copyright (C) 2008-2014 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% This header centralizes all simulation-wide settings.



% Separating mute/by speak allows to use different notifications (ex: shorter if
% spoken, for example ticks are far too long to be spoken).

% As notify_mute needs an implicit 'State' variable, it cannot be a function, it
% is therefore a macro, and notify_by_speak too for homogeneity.

-define( notify_mute( Message ),
	?info( Message )
).


-define( notify_mute_fmt( Message, Values ),
	?info_fmt( Message, Values )
).


% Tells whether the simulation system should be talkative.
% (uncomment to mute)
%
%-define(talkative,).

-ifdef(talkative).

-define( notify_by_speak( Message ),
	basic_utils:speak( Message )
).

-else.

-define( notify_by_speak( Message ),
	speak_muted
).

-endif.


-define( notify_user( Message ),
	?notify_mute( Message ),
	?notify_by_speak( Message )
).
