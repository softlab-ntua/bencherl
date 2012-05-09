%%% File        : dialyzer_benchmark.erl
%%% Author      : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Functions for benchmarking Dialyzer as part of the RELEASE
%%%               project.
%%% Created     : 27 Apr 2012 by Stavros Aronis

-module(dialyzer_benchmark).

-export([plt/1, otp/1]).

-spec plt(string()) -> no_return().

plt(PltFilename) ->
    try dialyzer:run([{analysis_type, plt_build},
		      {report_mode, normal},
		      {files_rec, [filename:absname("input/plt")]},
		      {timing, true},
		      {output_plt, PltFilename}]) of
	[] ->
	    halt(0)
    catch
	Class:Reason ->
	    io:format("Failed. The error was: ~w\n~w",[Class, Reason]),
	    halt(1)
    end.

-define(reference_file, "input/reference").
-define(tmp_result_file, "tmp/result").
    
otp(PltFilename) ->
    case file:read_file_info(PltFilename) of
	{ok, _} -> ok;
	{error, _ } ->
	    io:format("Plt ~s does not exist.\n",[PltFilename]),
	    halt(1)
    end,
    try dialyzer:run([{files_rec,
		       [filename:absname(F) ||
			   F <- ["input/plt", "input/otp"]]},
		      {report_mode, normal},
		      {init_plt, PltFilename},
		      {timing, true}]) of
	RawWarns ->
	    Warns = lists:sort([dialyzer:format_warning(W) || W <- RawWarns]),
	    case Warns of
		[] -> 
		    io:format("No warnings returned"),
		    halt(1);
		_  ->
		    case file:open(?tmp_result_file,[write]) of
			{ok, OutFile} ->
			    io:format(OutFile,"\n~s",[Warns]),
			    file:close(OutFile);
			Other ->
			    io:format("Error writing output:\n~w\n",[Other]),
			    halt(1)
		    end
	    end,
	    case diff(?reference_file, ?tmp_result_file) of
		'same' -> file:delete(?tmp_result_file),
			  halt(0);
		Any ->
		    io:format("Different result:\n~p\n",[Any]),
		    halt(1)
	    end
    catch
	Class:Reason ->
	    io:format("Failed. The error was: ~w\n~p",[Class, Reason]),
	    halt(1)
    end.
    
-type diff_result()::'same' | {'differ', diff_list()} |
		     {error, {file:filename(), term()}}.
-type diff_list()::[{id(), line(), string()}].
-type id()::'new'|'old'.
-type line()::non_neg_integer().

-spec diff(file:filename(), file:filename()) -> diff_result().

diff(Filename1, Filename2) ->
    File1 =
	case file:open(Filename1, [read]) of
	    {ok, F1} -> {file, F1};
	    _        -> empty
	end,
    File2 =
	case file:open(Filename2, [read]) of
	    {ok, F2} -> {file, F2};
	    _        -> empty
	end,
    case diff1(File1, File2) of
	{error, {N, Error}} ->
	    case N of
		1 -> {error, {Filename1, Error}};
		2 -> {error, {Filename2, Error}}
	    end;
	[]       -> 'same';
	DiffList -> {'differ', DiffList}
    end.

diff1(File1, File2) ->
    case file_to_lines(File1) of
	{error, Error} -> {error, {1, Error}};
	Lines1 ->
	    case file_to_lines(File2) of
		{error, Error} -> {error, {2, Error}};
		Lines2 ->
		    Common = lcs_fast(Lines1, Lines2),
		    diff2(Lines1, 1, Lines2, 1, Common, [])
	    end
    end.

diff2([], _, [], _, [], Acc) -> lists:keysort(2,Acc);
diff2([H1|T1], N1, [], N2, [], Acc) ->
    diff2(T1, N1+1, [], N2, [], [{new, N1, H1}|Acc]);
diff2([], N1, [H2|T2], N2, [], Acc) ->
    diff2([], N1, T2, N2+1, [], [{old, N2, H2}|Acc]);
diff2([H1|T1], N1, [H2|T2], N2, [], Acc) ->
    diff2(T1, N1+1, T2, N2+1, [], [{new, N1, H1}, {old, N2, H2}|Acc]);
diff2([H1|T1]=L1, N1, [H2|T2]=L2, N2, [HC|TC]=LC, Acc) ->
    case H1 =:= H2 of
	true  -> diff2(T1, N1+1, T2, N2+1, TC, Acc);
	false ->
	    case H1 =:= HC of
		true  -> diff2(L1, N1, T2, N2+1, LC, [{old, N2, H2}|Acc]);
		false -> diff2(T1, N1+1, L2, N2, LC, [{new, N1, H1}|Acc])
	    end
    end.

-spec lcs_fast([string()], [string()]) -> [string()].

lcs_fast(S1, S2) ->
  M = length(S1),
  N = length(S2),
  Acc = array:new(M*N, {default, 0}),
  {L, _} = lcs_fast(S1, S2, 1, 1, N, Acc), 
  L.

-spec lcs_fast([string()], [string()],
	       pos_integer(), pos_integer(),
	       non_neg_integer(), array()) -> {[string()], array()}.

lcs_fast([], _, _, _, _, Acc) ->
  {[], Acc};
lcs_fast(_, [], _, _, _, Acc) ->
  {[], Acc};
lcs_fast([H1|T1] = S1, [H2|T2] = S2, N1, N2, N, Acc) ->
  I = (N1-1) * N + N2 - 1,
  case array:get(I, Acc) of
    0 ->
      case string:equal(H1, H2) of
	true ->
	  {T, NAcc} = lcs_fast(T1, T2, N1+1, N2+1, N, Acc),
	  L = [H1|T],
	  {L, array:set(I, L, NAcc)};
	false ->
	  {L1, NAcc1} = lcs_fast(S1, T2, N1, N2+1, N, Acc), 
	  {L2, NAcc2} = lcs_fast(T1, S2, N1+1, N2, N, NAcc1),
	  L = longest(L1, L2), 
	  {L, array:set(I, L, NAcc2)}
      end;
    L -> 
      {L, Acc}
  end.

-spec longest([string()], [string()]) -> [string()].

longest(S1, S2) ->
  case length(S1) > length(S2) of
    true -> S1;
    false -> S2
  end.

file_to_lines(empty) ->
    [];
file_to_lines({file, File}) ->
    case file_to_lines(File, []) of
	{error, _} = Error -> Error;
	Lines              -> lists:reverse(Lines)
    end.

file_to_lines(File, Acc) ->
    case io:get_line(File, "") of
	{error, _}=Error -> Error;
	eof              -> Acc;
	A                -> file_to_lines(File, [A|Acc])
    end.
