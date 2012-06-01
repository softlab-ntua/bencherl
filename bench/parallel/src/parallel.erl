%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

%% Author  : Björn-Egil Dahlberg
%% Created : 16 April 2010

-module(parallel).

-export([bench_args/1, run/3]).

bench_args(short) ->
	[[N,M] || N <- [20000], M <- [256]];
bench_args(intermediate) ->
    [[N,M] || N <- [30000], M <- [512]];
bench_args(long) ->
    [[N,M] || N <- [70000], M <- [600]].

run([N,M|_], _, _) ->
	Me   = self(),
	Base = [ok || _ <- lists:seq(1, M)],
	Pids = [spawn_link(fun() -> loop(Me, N, []) end) || _ <- lists:seq(1, M)],
	Res  = [receive {Pid, What} -> What end || Pid <- Pids],
	Base = Res,
	ok.

loop(Pid, 0, Out) -> Pid ! {self(), check_now(Out)};
loop(Pid, N, Out) -> loop(Pid, N - 1, [now()|Out]).

check_now([_,_]) -> ok;
check_now([_|Ts]) -> check_now(Ts).

