-module(bencherlui_results_controller, [Req]).
-compile(export_all).

index('GET', []) ->
    {ok, [{benchmark_runs, benchmark_run_list_json_text()}]}.

benchmark_runs_json('GET', []) ->
    {json, [{benchmark_runs, benchmark_run_list()}]}.

list_of_files_in_dir(DirPath) ->
    {ok, FileNameList} = 
        file:list_dir(DirPath),
    Files =lists:filter(
             fun(Name) -> Name /= ".gitignore" end,
             FileNameList),
    [list_to_binary(Name) || Name <- Files].

benchmark_run_list() ->
    list_of_files_in_dir("../../../results").

benchmark_run_list_json_text() ->
    BenchmarkRuns = benchmark_run_list(),
    rfc4627:encode(BenchmarkRuns).

benchmark_list_for_name(RunName) ->
    list_of_files_in_dir(lists:concat(["../../../results/", RunName])).

benchmark_list_for_name_json_text(RunName) ->
    BenchmarkList = benchmark_list_for_name(RunName),
    rfc4627:encode(BenchmarkList).

benchmarks_for_run('GET', []) ->
    RunName = Req:query_param("run"),
    {output, benchmark_list_for_name_json_text(RunName)}.

find_first_parentheses_help([], [], _) ->
    no_match;
find_first_parentheses_help([],[$(|Rest], 0) ->
    find_first_parentheses_help([$(], Rest, 1);
find_first_parentheses_help([],[_|Rest], 0) ->
    find_first_parentheses_help([], Rest, 0);
find_first_parentheses_help(SoFar, Remaining, 0) ->
    {lists:reverse(SoFar), Remaining};
find_first_parentheses_help(SoFar, [$(|Rest], N) ->
    find_first_parentheses_help([$(|SoFar], Rest, N +1);
find_first_parentheses_help(SoFar, [$)|Rest], N) ->
    find_first_parentheses_help([$)|SoFar], Rest, N -1);
find_first_parentheses_help(SoFar, [Char|Rest], N) ->
    find_first_parentheses_help([Char|SoFar], Rest, N);
find_first_parentheses_help(_, [], _) ->
    no_match.

find_first_parentheses(String) ->
    find_first_parentheses_help([], String, 0).

add_data_points_to_dict(NrOfSched, Line, Dict) ->
    case find_first_parentheses(Line) of
        {Label, RestOfLine1} ->
            case re:run(RestOfLine1,"\\d+") of
                {match, [{Start, _}|_]} ->
                    {Value, RestOfLine2} = 
                        string:to_float(lists:nthtail(Start, RestOfLine1)),
                    NewDict = dict:append(Label, [NrOfSched, Value], Dict),
                    add_data_points_to_dict(
                      NrOfSched, 
                      RestOfLine2, 
                      NewDict);
                _ ->
                    Dict
            end;
        _ ->
            Dict
    end.

add_line_to_dict(Line, Dict) ->
    case re:run(Line,"\\d+") of
        {match, [{Start, _}|_]} ->
            {NrOfSched, RestOfLine} = 
                string:to_integer(lists:nthtail(Start, Line)),
            add_data_points_to_dict(NrOfSched, RestOfLine, Dict);
        _ ->
            Dict
    end.


result_file_to_dict(FileName) ->
    Lines = readlines(FileName),    
    lists:foldl(
      fun(Line, Dict) -> 
              io:format("Line: ~p ~n", [Line]),
              add_line_to_dict(Line, Dict)
      end, 
      dict:new(), 
      Lines).

result_file_to_json_text(FileName) ->
    Dict = result_file_to_dict(FileName),
    List = lists:map(
             fun({Label, Data}) ->
                     dict:append_list(
                       data, Data, 
                       dict:append_list(
                         label, list_to_binary(Label), 
                         dict:new()))
             end,
             dict:to_list(Dict)),
    rfc4627:encode(List).

benchmark_results('GET', []) ->
    RunName = Req:query_param("run"),
    BenchmarkName = Req:query_param("benchmark"),
    FileName = lists:concat(
                 ["../../../results/", 
                  RunName, "/", 
                  BenchmarkName, 
                  "/measurements/DEFOTP.DEFARGS.sched.time"]),
    {output, result_file_to_json_text(FileName)}.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
    after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> [Line|get_all_lines(Device)]
    end.

