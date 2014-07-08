#!/usr/bin/env escript

% Copyright (C) 2010-2012 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.


% See benchmark-command.sh which is simpler and probably more reliable
% (execution seems to behave differently with os:cmd).

get_time_format() ->
	"[{wall_clock,'%E'}, {system,'%S'}, {user,'%U'}, {total_memory,'%K'}].".


get_benchmark_command( CommandToBenchmark ) ->
	"/usr/bin/time --format \"" ++ get_time_format() ++ "\" "
		++ CommandToBenchmark ++ " 1>/dev/null".



parse_result( StringRes ) ->
	%io:format( "Parsing result '~s'.~n", [StringRes] ),
	{ok,Tokens,_} = erl_scan:string( StringRes ),
	{ok,Term} = erl_parse:parse_term( Tokens ),
	%io:format( "Parse result: '~p'.~n", [Term] ),
	Term.



% Parses a time atom like: '0:01.00', returns a number of milliseconds:
parse_time( TimeAtom ) ->

	%io:format( "Parsing time '~s'.~n", [TimeAtom] ),

	case string:tokens( atom_to_list(TimeAtom), ":.") of

		[Minutes,Seconds,Hundredth] ->
			{M,[]} = string:to_integer(Minutes),
			{S,[]} = string:to_integer(Seconds),
			{H,[]} = string:to_integer(Hundredth),
			( M*60 + S ) * 1000 + 10*H;

		[Seconds,Hundredth] ->
			{S,[]} = string:to_integer(Seconds),
			{H,[]} = string:to_integer(Hundredth),
			S*1000 + 10*H

	end.


%% parse_float( FloatAtom ) ->
%%	{F,[]} = string:to_float( atom_to_list(FloatAtom) ),
%%	F.


parse_integer( IntAtom ) ->
	{I,[]} = string:to_integer( atom_to_list(IntAtom) ),
	I.


interpret_result( [{wall_clock,W}, {system,S}, {user,U}, {total_memory,M}] ) ->
	{parse_time(W), parse_time(S), parse_time(U), parse_integer(M)}.


run_command( Command, Count ) ->
	run_command( Command, Count, Count, {0,0,0,0} ).


run_command( _Command, _Count=0, Count, {WTotal,STotal,MTotal,UTotal} ) ->
	{WTotal/Count,STotal/Count,MTotal/Count,UTotal/Count};

run_command( Command, CurrentCount, Count, {WTotal,STotal,MTotal,UTotal} ) ->
	RawRes = os:cmd( Command ),
	%io:format( "Raw intermediary result: ~p.~n", [ RawRes ] ),
	{W,S,M,U} = interpret_result( parse_result( RawRes ) ),
	%o:format( "Intermediary result: ~p.~n", [ {W,S,M,U} ] ),
	run_command( Command, CurrentCount-1, Count,
				{WTotal+W,STotal+S,MTotal+M,UTotal+U} ).


display_result( {W,S,U,M}, CommandToBenchmark, MeasureCount ) ->
	io:format( "The execution of command '~s' led, after ~B measures, to:~n"
			   " - a mean wall-clock time of ~.1f milliseconds~n"
			   " - a mean system time of ~.1f milliseconds~n"
			   " - a mean user time of ~.1f milliseconds~n"
			   " - a mean total memory of ~f KB~n",
			  [CommandToBenchmark, MeasureCount, W, S, U, M] ).



main([CommandToBenchmark]) ->

	%io:format( "Benchmarked command is: '~p'.~n", [CommandToBenchmark] ),

	ActualCommand = get_benchmark_command(CommandToBenchmark),
	%io:format( "Actual benchmarking command is: '~p'.~n", [ActualCommand] ),

	MeasureCount = 5,
	Res = run_command( ActualCommand, MeasureCount ),
	display_result( Res, CommandToBenchmark, MeasureCount ),

	ok;

main(_) ->
	usage().


usage() ->
	io:format( "Usage: benchmark-command.escript <COMMAND>: "
			  "returns a mean resource consumption for the specified command.\n"
			  "Example: benchmark-command.escript \"my_script.sh 1 2\"" ),
	halt(5).
