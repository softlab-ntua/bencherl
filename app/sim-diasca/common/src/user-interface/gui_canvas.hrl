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


% Header to export canvas-related defines.
%
% See canvas.erl for the corresponding implementation.



% wxCanvas does not exist, we emulate it.
%
% Canvas are back-buffered: drawing operations on it will not be visible until
% the blit/1 function is called on it.
%
-record( canvas, {

		   panel       :: gui:panel(),
		   bitmap      :: gui:bitmap(),
		   back_buffer :: gui:back_buffer()

}).


% The actual canvas type we are to use:
-type canvas() :: #canvas{}.


% An OpenGL-based canvas:
-type gl_canvas():: wxGLCanvas:wxGLCanvas().
