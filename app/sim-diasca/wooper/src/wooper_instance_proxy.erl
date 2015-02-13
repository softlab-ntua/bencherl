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


% Module to create WOOPER instance proxies.
%
-module(wooper_instance_proxy).


% The purpose of a proxy P is to be a process that acts as a man-in-the-middle
% for a WOOPER target instance T: all processes interacting with P will actually
% interact transparently with T.
%
% This can be useful when, for example, a local process (P) is needed (ex: must
% be locally registered), whereas the actual service is implemented by a remote
% instance (T).
%
% Note that this proxy is only as transparent as reasonably achievable and,
% that, anyway, proxies are seldom satisfactory solutions.
%
-export([ start/1, start_link/1 ]).


-spec start( pid() ) -> pid().
start( TargetInstancePid ) ->

	io:format( "Starting proxy for WOOPER instance ~w.~n",
			   [ TargetInstancePid ] ),

	spawn( fun() -> proxy_main_loop( TargetInstancePid ) end ).



-spec start_link( pid() ) -> pid().
start_link( TargetInstancePid ) ->

	io:format( "Starting linked proxy for WOOPER instance ~w.~n",
			   [ TargetInstancePid ] ),

	spawn_link( fun() -> proxy_main_loop( TargetInstancePid ) end ).




% Starts a proxy for specified WOOPER instance, designated by specified PID.
proxy_main_loop( TargetInstancePid ) ->

	io:format( "Proxy ~w waiting for a call to WOOPER target instance ~w.~n",
			   [ self(), TargetInstancePid ] ),


	% This proxy is expected to receive either requests or oneways:
	%
	receive

		{ RequestName, Args, SenderPid } ->

			io:format( "Proxy ~w processing request ~p.~n",
					   [ self(), { RequestName, Args, SenderPid } ] ),

			TargetInstancePid ! { RequestName, Args, self() },
			receive

				R ->

					io:format( "Proxy ~w returning ~p to caller ~w.~n",
					  [ self(), R, SenderPid ] ),

					SenderPid ! R

			end,
			proxy_main_loop( TargetInstancePid ) ;


		{ OnewayName, Args } ->

			io:format( "Proxy ~w processing oneway ~p.~n",
					   [ self(), { OnewayName, Args } ] ),

			TargetInstancePid ! { OnewayName, Args },

			proxy_main_loop( TargetInstancePid );


		delete ->

			io:format( "Deleting proxy ~w for WOOPER target instance ~w.~n",
					   [ self(), TargetInstancePid ] ),

			% No looping here:
			TargetInstancePid ! delete;


		OnewayName when is_atom( OnewayName ) ->

			io:format( "Proxy ~w processing oneway ~p.~n",
					   [ self(), OnewayName ] ),

			TargetInstancePid ! OnewayName,
			proxy_main_loop( TargetInstancePid );


		Other ->

			io:format( "Warning: WOOPER instance proxy (~w) for ~w ignored "
					   "following message: ~p.~n",
					   [ self(), TargetInstancePid, Other ] ),

			proxy_main_loop( TargetInstancePid )

	end.
