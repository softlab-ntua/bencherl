% Copyright (C) 2012-2014 Olivier Boudeville
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
% Creation date: Tuesday, March 5, 2013


% Unit tests for the graph_utils toolbox.
%
% See the graph_utils.erl tested module.
%
-module(graph_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Let's create a very simple (yet cyclic) type of graph, described by a list
	% of:
	%
	% { Vertex, Children } :: { vertex_name(), [ vertex_name() ] }

	% a ->
	%     b ->
	%          b (0-length cycle created!)
	%          e
	%     c ->
	%          f ->
	%               g ->
	%                    a (cycle created!)
	%               h
	%     d
	%
	%
	%
	LetterGraph = [ { a, [ b, c, d ] },
					{ b, [ e ] },
					{ c, [ f ] },
					{ d, [] },
					{ e, [] },
					{ f, [ g, h ] },
					{ g, [ a ] },
					{ h, [] } ],

	test_facilities:display( "Using following cyclic test graph: ~p.",
							[ LetterGraph ] ),

	% Predicate to find h:
	HPredicate = fun( h, _Graph ) -> true ;
					( _OtherLetter, _Graph ) -> false
				 end,

	% Returns the children of a given vertex (letter):
	Feeder = fun( Letter, Graph ) ->
				{ _Letter, Children } = lists:keyfind( _K=Letter, _N=1, Graph ),
				Children
			 end,

	HRes = graph_utils:find_breadth_first( _InitialVertex=a, HPredicate, Feeder,
					_UserData=LetterGraph ),

	test_facilities:display( "Path from a to h: ~p.", [ HRes ] ),

	%[ a, c, f, h ] = HRes,

	ZPredicate = fun( z, _Graph ) -> true ;
					( _OtherLetter, _Graph ) -> false
				 end,

	ZRes = graph_utils:find_breadth_first( _InitialVertex=a, ZPredicate, Feeder,
					_UserData=LetterGraph ),

	test_facilities:display( "Path from a to z: ~p.", [ ZRes ] ),

	no_path_found = ZRes,

	test_facilities:stop().
