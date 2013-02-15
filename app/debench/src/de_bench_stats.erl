%% DEbench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_stats.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_stats).

-behaviour(gen_server).


%% API
-export([start_link/0, run/0, op_complete/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-include("de_bench.hrl").

-record(state, { ops,
				 start_time = now(),
                 last_write_time = now(),
                 report_interval,
                 errors_since_last_report = false,
                 summary_file,
                 errors_file,
                 max_num_operarions,
                 operation_counter}).  


%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run() ->
	Sleep_time=de_bench_config:get(sleep_time_after_ping, 0),
    gen_server:call(?MODULE, run, timer:seconds(Sleep_time+5)).

op_complete(Op, ok, ElapsedUs) ->
    op_complete(Op, {ok, 1}, ElapsedUs);
op_complete(Op, {ok, Units}, ElapsedUs) ->  
    %% Update the histogram and units counter for the op in question
    folsom_metrics:notify({latencies, Op}, ElapsedUs),
    folsom_metrics:notify({units, Op}, {inc, Units}),
    gen_server:call(?MODULE, {increase_counter});
op_complete(Op, Result, ElapsedUs) ->  
    gen_server:call(?MODULE, {op, Op, Result, ElapsedUs}),
    gen_server:call(?MODULE, {increase_counter}).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
    %% Trap exits so we have a chance to flush data
    process_flag(trap_exit, true),

    %% Spin up folsom
    folsom:start(),

    %% Initialize an ETS table to track error and crash counters during
    %% reporting interval
    ets:new(de_bench_errors, [protected, named_table]),

    %% Initialize an ETS table to track error and crash counters since
    %% the start of the run
    ets:new(de_bench_total_errors, [protected, named_table]),

    %% Initialize an ETS table to count all done operations
    ets:new(de_bench_total_operations, [protected, named_table]),

    %% Get the list of operations we'll be using for this test
    F1 =
        fun({OpTag, _Count}) -> {OpTag, OpTag};
           ({Label, OpTag, _Count}) -> {Label, OpTag}
        end,
    Ops = [F1(X) || X <- de_bench_config:get(operations, [])],
    

    %% Setup a histogram and counter for each operation -- we only track latencies on
    %% successful operations
    [begin
         folsom_metrics:new_histogram({latencies, Op}, slide, de_bench_config:get(report_interval)),
         folsom_metrics:new_counter({units, Op})
     end || Op <- Ops ],

    %% Setup output file handles for dumping periodic CSV of histogram results.
    [erlang:put({csv_file, X}, op_csv_file(X)) || X <- Ops],

    %% Setup output file w/ counters for total requests, errors, etc.
    {ok, SummaryFile} = file:open(set_test_dir("summary.csv"), [raw, binary, write]),
    file:write(SummaryFile, <<"elapsed, window, total, successful, failed\n">>),

    %% Setup errors file w/counters for each error.  Embedded commas likely
    %% in the error messages so quote the columns.
    {ok, ErrorsFile} = file:open(set_test_dir("errors.csv"), [raw, binary, write]),
    file:write(ErrorsFile, <<"\"error\",\"count\"\n">>),

    %% Schedule next write/reset of data
    ReportInterval = timer:seconds(de_bench_config:get(report_interval)),
	Max_num_operarions=de_bench_config:get(max_num_operarions, 0),
    {ok, #state{ ops = Ops,
                 report_interval = ReportInterval,
                 summary_file = SummaryFile,
                 errors_file = ErrorsFile,
                 max_num_operarions = Max_num_operarions,
                 operation_counter=0
                 }}.


handle_call(run, _From, State) ->
    %% Schedule next report
    io:format("stats process starts at ~p ~n",[erlang:time()]),
    Now = now(),
    erlang:send_after(State#state.report_interval, self(), report),
    {reply, ok, State#state { start_time = Now, last_write_time = Now}};

handle_call({op, Op, {error, Reason}, _ElapsedUs}, _From, State) ->
    increment_error_counter(Op),
    increment_error_counter({Op, Reason}),
    {reply, ok, State#state { errors_since_last_report = true  }};

handle_call({increase_counter}, _From, State) ->
	if 
		State#state.operation_counter> State#state.max_num_operarions -> 
			{reply, finish, State#state { operation_counter = State#state.operation_counter+1  }};

		State#state.max_num_operarions > 0 -> 
			{reply, ok, State#state { operation_counter = State#state.operation_counter+1  }};

		true ->
			{reply, ok, State}
	end.


handle_cast(_, State) ->
    {noreply, State}.

handle_info(report, State) ->
    Now = now(),
    process_stats(Now, State),
    %% Schedule next report
    erlang:send_after(State#state.report_interval, self(), report),
    {noreply, State#state { last_write_time = Now, errors_since_last_report = false}}.

terminate(_Reason, State) ->
    %% Do the final stats report and write the errors file
    process_stats(now(), State),
    report_total_errors(State),

    [ok = file:close(F) || {{csv_file, _}, F} <- erlang:get()],
    ok = file:close(State#state.summary_file),
    ok = file:close(State#state.errors_file),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ====================================================================
%% Internal functions
%% ====================================================================

set_test_dir(FileName) ->
	TestDir=de_bench_config:get(test_dir_path),
	filename:join([TestDir, FileName]).


op_csv_file({Label, _Op}) ->
    Fname = normalize_label(Label) ++ "_latencies.csv",
    {ok, F} = file:open(set_test_dir(Fname), [raw, binary, write]),
    ok = file:write(F, <<"elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n">>),
    F.

normalize_label(Label) when is_list(Label) ->
    replace_special_chars(Label);
normalize_label(Label) when is_binary(Label) ->
    normalize_label(binary_to_list(Label));
normalize_label(Label) when is_integer(Label) ->
    normalize_label(integer_to_list(Label));
normalize_label(Label) when is_atom(Label) ->
    normalize_label(atom_to_list(Label));
normalize_label(Label) when is_tuple(Label) ->
    Parts = [normalize_label(X) || X <- tuple_to_list(Label)],
    string:join(Parts, "-").

replace_special_chars([H|T]) when
      (H >= $0 andalso H =< $9) orelse
      (H >= $A andalso H =< $Z) orelse
      (H >= $a andalso H =< $z) ->
    [H|replace_special_chars(T)];
replace_special_chars([_|T]) ->
    [$-|replace_special_chars(T)];
replace_special_chars([]) ->
    [].


increment_error_counter(Key) ->
    ets_increment(de_bench_errors, Key, 1).

ets_increment(Tab, Key, Incr) when is_integer(Incr) ->
    %% Increment the counter for this specific key. We have to deal with
    %% missing keys, so catch the update if it fails and init as necessary
    case catch(ets:update_counter(Tab, Key, Incr)) of
        Value when is_integer(Value) ->
            ok;
        {'EXIT', _} ->
            case ets:insert_new(Tab, {Key, Incr}) of
                true ->
                    ok;
                _ ->
                    %% Race with another load gen proc, so retry
                    ets_increment(Tab, Key, Incr)
            end
    end;
ets_increment(Tab, Key, Incr) when is_float(Incr) ->
    Old = case ets:lookup(Tab, Key) of
              [{_, Val}] -> Val;
              []         -> 0
          end,
    true = ets:insert(Tab, {Key, Old + Incr}).

error_counter(Key) ->
    lookup_or_zero(de_bench_errors, Key).

lookup_or_zero(Tab, Key) ->
    case catch(ets:lookup_element(Tab, Key, 2)) of
        {'EXIT', _} ->
            0;
        Value ->
            Value
    end.


process_stats(Now, State) ->
    %% Determine how much time has elapsed (seconds) since our last report
    %% If zero seconds, round up to one to avoid divide-by-zeros in reporting
    %% tools.
    Elapsed = erlang:max(trunc(timer:now_diff(Now, State#state.start_time) / 1000000), 1),
    Window  = erlang:max(trunc(timer:now_diff(Now, State#state.last_write_time) / 1000000), 1),

    %% Time to report latency data to our CSV files
    {Oks, Errors} = lists:foldl(fun(Op, {TotalOks, TotalErrors}) ->
                                        {Oks, Errors} = report_latency(Elapsed, Window, Op),
                                        {TotalOks + Oks, TotalErrors + Errors}
                                end, {0,0}, State#state.ops),

    %% Reset units
    [folsom_metrics_counter:clear({units, Op}) || Op <- State#state.ops],

    %% Write summary
    file:write(State#state.summary_file,
               io_lib:format("~w, ~w, ~w, ~w, ~w\n",
                             [Elapsed,
                              Window,
                              Oks + Errors,
                              Oks,
                              Errors])),

    %% Dump current error counts to console
    case (State#state.errors_since_last_report) of
        true ->
            ErrCounts = ets:tab2list(de_bench_errors),
            true = ets:delete_all_objects(de_bench_errors),
            ?INFO("Errors:~p\n", [lists:sort(ErrCounts)]),
            [ets_increment(de_bench_total_errors, Err, Count) || 
                              {Err, Count} <- ErrCounts],
            ok;
        false ->
            ok
    end.

%%
%% Write latency info for a given op to the appropriate CSV. Returns the
%% number of successful and failed ops in this window of time.
%%
report_latency(Elapsed, Window, Op) ->
    Stats = folsom_metrics:get_histogram_statistics({latencies, Op}),
    Errors = error_counter(Op),
    Units = folsom_metrics:get_metric_value({units, Op}),
    case proplists:get_value(n, Stats) > 0 of
        true ->
            P = proplists:get_value(percentile, Stats),
            Line = io_lib:format("~w, ~w, ~w, ~w, ~.1f, ~w, ~w, ~w, ~w, ~w, ~w\n",
                                 [Elapsed,
                                  Window,
                                  Units,
                                  proplists:get_value(min, Stats),
                                  proplists:get_value(arithmetic_mean, Stats),
                                  proplists:get_value(median, Stats),
                                  proplists:get_value(95, P),
                                  proplists:get_value(99, P),
                                  proplists:get_value(999, P),
                                  proplists:get_value(max, Stats),
                                  Errors]);
        false ->
            ?WARN("No data for op: ~p\n", [Op]),
            Line = io_lib:format("~w, ~w, 0, 0, 0, 0, 0, 0, 0, 0, ~w\n",
                                 [Elapsed,
                                  Window,
                                  Errors])
    end,
    ok = file:write(erlang:get({csv_file, Op}), Line),
    {Units, Errors}.

report_total_errors(State) ->                          
    case ets:tab2list(de_bench_total_errors) of
        [] ->
            ?INFO("No Errors.\n", []);
        UnsortedErrCounts ->
            ErrCounts = lists:sort(UnsortedErrCounts),
            ?INFO("Total Errors:\n", []),
            F = fun({Key, Count}) ->
                        case lists:member(Key, State#state.ops) of
                            true ->
                                ok; % per op total
                            false ->
                                ?INFO("  ~p: ~p\n", [Key, Count]),
                                file:write(State#state.errors_file, 
                                           io_lib:format("\"~w\",\"~w\"\n",
                                                         [Key, Count]))
                        end
                end,
            lists:foreach(F, ErrCounts)
    end.




