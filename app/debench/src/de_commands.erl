%% DEbench: A benchmarking suite for distributed Erlang
%% author: Amir Ghaffari
%% @RELEASE project (http://www.release-project.eu/)

-module(de_commands).

-export([run/3, calculate/2]).

-include("de_bench.hrl").

run(TargetNode, Ope, Data) ->
	case Ope of
		spawning ->
			RanNum=de_helper:getRandomm(100),
			Pid = spawn_link(TargetNode,?MODULE, calculate, [self(), RanNum]),
			receive
				{result, Pid, Result} ->
					exit(Pid, kill),
					{ok, Result}
			after timer:seconds(60) ->
				    {error, spawn_timeout_error}
			end;

		global_register ->
			ProcessName=de_helper:get_timestamp(),
			global:register_name(ProcessName, self()),
			{ok, global_register, ProcessName};

		global_unregister ->
			global:unregister_name(Data),
			{ok, global_unregister};

		global_whereis ->
			Pid=global:whereis_name(Data),
			case is_pid(Pid) of
			true ->
				{ok, global_whereis, Data};
			false ->
				{error, whereis_error, Data}
			end;

		_ ->
			{error, no_operation}
	end.

calculate(Sender, Num) ->
Sender ! {result, self(), Num*2}.

