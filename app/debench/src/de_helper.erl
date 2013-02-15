%% DEbench: A benchmarking suite for distributed Erlang
%% author: Amir Ghaffari
%% @RELEASE project (http://www.release-project.eu/)

-module(de_helper).

-export([ping_nodes/2, getRandomm/1, id/0, get_timestamp/0, file_exist/1 ]).

-include("de_bench.hrl").

getRandomm(Max) ->              
        {_A, B, C} = erlang:now(),           
        random:seed(C, B, C),
        random:uniform(Max).

%% Ping [Nodes]
ping_nodes([],Pangs) ->
	Pangs;
ping_nodes([Node|RemNodes],Pangs) ->
	Ping = net_adm:ping(Node),
	case Ping of
		pong ->
			ping_nodes(RemNodes,Pangs);
		pang ->
			ping_nodes(RemNodes,[Node|Pangs])
	end.

%%=============================================================

%%
%% Construct a string suitable for use as a unique ID 
%%
id() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).

get_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    {_,_,Micro} = erlang:now(),
    R=getRandomm(10000000),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w2~w~w", [Y, M, D, H, Min, S, Micro,R]).

file_exist(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         -> io:format("~s is found~n", [Filename]);
        {error, enoent} -> io:format("~s is missing~n", [Filename]);
        {error, Reason} -> io:format("~s is ~s~n", [Filename, Reason])
    end.


