%% DEbench: A benchmarking suite for Distributed Erlang
%% This file is a modified version of basho_bench_worker.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench
%% ==========================================
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, run/1]).

%% gen_server callbacks
-export([init/1,terminate/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3,idle_process/0,initialize_global_size/2]).

-record(state, { id, sup_id, worker_pid, parent_pid, ops, ops_len, next_op, data, erlang_nodes, continue}).

-include("de_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

run(Pids) ->
    [ok = gen_server:call(Pid, run) || Pid <- Pids],
    ok.

start_link(SupChild, Id) ->
    gen_server:start_link(?MODULE, [SupChild, Id], []).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================


init([SupChild, Id]) ->
	Erlang_nodes=de_bench_config:get(erlange_nodes, []),
    case Erlang_nodes of
        [] ->
			?FAIL_MSG("~s requires erlange_nodes to be defined in config file.\n", [?MODULE]);
        _ ->
            ok
    end,
	Ops     = ops_tuple(),
	State = #state { id = Id, sup_id = SupChild, parent_pid=self(),
	ops = Ops, ops_len = size(Ops), next_op='', data='', erlang_nodes=Erlang_nodes, continue=true},
	Pangs=de_helper:ping_nodes(Erlang_nodes,[]),
	case Pangs of
	[] ->
		?CONSOLE("ping: All nodes (~p) are available \n", [length(Erlang_nodes)]);
	_->
		?ERROR("ping: ~p nodes from total ~p nodes are not accessible: ~p~n", [length(Pangs),length(Erlang_nodes), Pangs])
	end,

	%% NOTE: If the worker process dies, this obviously introduces some entroy
	%% into the equation since you'd be restarting the RNG all over.
	process_flag(trap_exit, true),
   %% Link the worker and the sub-process to ensure that if either exits, the
    %% other goes with it.
    WorkerPid = spawn_link(fun() -> worker_init(State) end),
    WorkerPid ! {init_driver, self()},
    receive
        driver_ready ->
        ok
    end,

    %% If the system is marked as running this is a restart; queue up the run
    %% message for this worker
    case de_bench_app:is_running() of
        true ->
            ?WARN("Restarting crashed worker.\n", []),
            gen_server:cast(self(), run);
        false ->
            ok
    end,
	{ok, State#state { worker_pid = WorkerPid}}.

terminate(_Reason, _State) ->
    ok.

handle_call(run, _From, State) ->
	State#state.worker_pid ! run,
	{reply, ok, State}. 

handle_cast(run, State) ->
	State#state.worker_pid ! run,
	{noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        normal ->
            %% Clean shutdown of the worker; spawn a process to terminate this
            %% process via the supervisor API and make sure it doesn't restart.
            spawn(fun() -> stop_worker(State#state.sup_id) end),
            {noreply, State};

        _ ->
            ?ERROR("Worker ~p exited with reason: ~p~n", [Pid, Reason]),
            %% Worker process exited for some other reason; stop this process
            %% as well so that everything gets restarted by the sup
            {stop, normal, State}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


worker_init(State) ->
    %% Trap exits from linked parent process; use this to ensure the driver
    %% gets a chance to cleanup
    process_flag(trap_exit, true),
    Initial_global_size=de_bench_config:get(initial_global_size, 0),
    ?CONSOLE("Initial global size = ~p ~n", [Initial_global_size]),
    initialize_global_size(Initial_global_size,State),
    worker_idle_loop(State).

%% Initially register a number of processes globally 
initialize_global_size(Initial_global_size, State) ->
case Initial_global_size of
	0 ->
		ok;
	_->
		ProcessName=de_helper:get_timestamp(),
		Pid=global:whereis_name(ProcessName),
		case is_pid(Pid) of
		true ->
			timer:sleep(1),
			initialize_global_size(Initial_global_size, State);
		false ->
			Erlang_nodes  = State#state.erlang_nodes,
			NodeIndex = de_helper:getRandomm(length(Erlang_nodes)),
			case is_list(lists:nth(NodeIndex,Erlang_nodes)) of
			true ->
				Selected_node = list_to_atom(lists:nth(NodeIndex,Erlang_nodes));
			false ->
				Selected_node = lists:nth(NodeIndex,Erlang_nodes)
			end,
			ProcessID = spawn_link(Selected_node,?MODULE, idle_process, []),
			%ProcessID=spawn(fun() -> idle_process() end),
			PName=pid_to_list(ProcessID),
			global:register_name(list_to_atom(lists:append(PName, ProcessName)),ProcessID),
			initialize_global_size(Initial_global_size-1, State)
		end
end.

idle_process() ->
    receive
        never_happen ->
			ok
    end.

worker_idle_loop(State) ->
    receive
        {init_driver, Caller} ->
			Caller ! driver_ready,
            worker_idle_loop(State);
        run ->
			io:format("Worker process ~p starts at ~p ~n",[State#state.id, erlang:time()]),
			worker_active_loop(State)
    end.

needs_shutdown(State) ->
    Parent = State#state.parent_pid,
    receive
        {'EXIT', Parent, _Reason} ->
            true
    after 0 ->
		case State#state.continue of
			true ->
				false;
			false ->
				true
		end
    end.

worker_active_loop(State) ->
	case State#state.next_op of
	global_unregister ->
		Next = {global_unregister, global_unregister};
	global_whereis ->
		Next = {global_whereis, global_whereis};
	_ ->
		Next = element(de_helper:getRandomm(State#state.ops_len), State#state.ops)
	end,
    {_Label, OpTag} = Next,
	Erlang_nodes  = State#state.erlang_nodes,
	NodeIndex = de_helper:getRandomm(length(Erlang_nodes)),
	case is_list(lists:nth(NodeIndex,Erlang_nodes)) of
	true ->
		Selected_node = list_to_atom(lists:nth(NodeIndex,Erlang_nodes));
	false ->
		Selected_node = lists:nth(NodeIndex,Erlang_nodes)
	end,

	case OpTag of
	global_unregister when State#state.next_op /= global_unregister ->
		State2=State;
	global_whereis when State#state.next_op /= global_whereis ->
		State2=State;
	_ ->
		Start = now(),
		Result = (catch de_commands:run(Selected_node, OpTag, State#state.data) ),
		ElapsedUs = timer:now_diff(now(), Start),
		case Result of 
			{ok, _} ->
				case de_bench_stats:op_complete(Next, ok, ElapsedUs) of
					ok ->
						State2=State#state {next_op='', data='', continue=true};
					finish ->
						State2=State#state {next_op='', data='', continue=false}
				end;

			{ok, global_register, Res} ->
				case de_bench_stats:op_complete(Next, ok, ElapsedUs) of
					ok ->
						State2=State#state {next_op=global_whereis, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=global_whereis, data=Res, continue=false}
				end;

			{ok, global_whereis, Res} ->
				case de_bench_stats:op_complete(Next, ok, ElapsedUs) of
					ok ->
						State2=State#state {next_op=global_unregister, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=global_unregister, data=Res, continue=false}
				end;

			{error, _} ->
				case de_bench_stats:op_complete(Next, Result, ElapsedUs) of
					ok ->
						State2=State;
					finish ->
						State2=State#state {continue=false}
				end;

			{error, whereis_error, Data} ->
				case de_bench_stats:op_complete(Next, {error, whereis_error}, ElapsedUs) of
					ok -> 
						State2=State#state {next_op=global_unregister, data=Data, continue=true};
					finish ->
						State2=State#state {next_op=global_unregister, data=Data, continue=false}
				end;

			{'EXIT', Reason} ->
				%% Operation failed, generate a crash error and terminate.
				case de_bench_stats:op_complete(Next, {error, Reason}, ElapsedUs) of
					ok ->
						State2=State;
					finish ->
						State2=State#state {continue=false}
				end
		end
	end,
	case needs_shutdown(State2) of
		true ->
			ok;
		false ->
				worker_active_loop(State2)
	end.

%%
%% Stop a worker process via the supervisor and terminate the app
%% if there are no workers remaining
%%
%% WARNING: Must run from a process other than the worker!
%%

stop_worker(SupChild) ->
    ok = de_bench_sup:stop_child(SupChild),
    case de_bench_sup:workers() of
        [] ->
            %% No more workers -- stop the system
            de_bench_app:stop();
        _ ->
            ok
    end.

%%
%% Expand operations list into tuple suitable for weighted, random draw
%%
ops_tuple() ->
    F =
        fun({OpTag, Count}) ->
                lists:duplicate(Count, {OpTag, OpTag});
           ({Label, OpTag, Count}) ->
                lists:duplicate(Count, {Label, OpTag})
        end,
    Ops = [F(X) || X <- de_bench_config:get(operations, [])],
    list_to_tuple(lists:flatten(Ops)).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
