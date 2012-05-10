%% orbit-int master (controlling orbit computation)
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%

-module(master).

-export([orbit/3,
         get_gens/1, get_master/1, get_workers/1, get_spawn_img_comp/1,
         get_global_table_size/1, get_idle_timeout/1,
         set_idle_timeout/2, clear_spawn_img_comp/1,
         now/0]).

-compile({no_auto_import, [now/0]}).

%% DATA
%%   Static Machine Configuration:
%%     {Gs,               %list of generators
%%      Master,           %pid of master process
%%      Workers,          %list of Worker
%%      GlobalTableSize,  %size of global hash table
%%      IdleTimeout,      %milliseconds this worker idles before sending 'done'
%%      SpawnImgComp}     %true iff this worker spawns image computations
%%
%%   Worker:
%%     {Pid,              %pid of worker process
%%      TableOffset,      %offset (= index 0) of local table into global table
%%      TableSize}        %size of local hash table
%%
%%   Host:
%%     {Node,             %atom naming Erlang node
%%      Procs,            %number of processors
%%      TableSize,        %size of hash table per processor
%%      IdleTimeout}      %milliseconds a processor idles before sending 'done'
%%
%%   Statistics:
%%     List of pairs where the first component is an atom, the second
%%     some data. Part of the data is the fill frequency of the table
%%     (a list whose ith element indicates frequency of filling degree i).


%% MESSAGES
%%   Master -> Worker:        {init, StaticMachConf}
%%
%%   Master/Worker -> Worker: {vertex, X, Slot, K}
%%                               %X is vertex
%%                               %Slot is slot of X on target worker
%%                               %K is atomic credit shipped with vertex
%%
%%   Worker -> Master:        {done, Cs}
%%                               %Cs is non-zero credit (rep as list of ints)
%%
%%   Master -> Worker:        {dump}
%%
%%   Worker -> Master:        {result, Xs, Stats}
%%                               %Xs is list of found orbit vertices
%%                               %Stats is statistics about worker's table


%% compute orbit of elements in list Xs under list of generators Gs;
%% the argument Hosts is either an integer N, a triple {P, N, T}, or
%% a non-empty list [{H, P, N, T} | ...] of quadruples:
%% * N:                       run the sequential algorithm with table size N
%% * {P, N, T, S}:            run the parallel algorithm on P processors
%%                            each with table size N, idle timeout T and
%%                            spawn image computation flag S;
%% * [{H, P, N, T, S} | ...]: run the distributed algorithm on the list of
%%                            hosts, where each quintuple {H, P, N, T, S}
%%                            specifies
%%                            * host name H (ie. name of Erlang node),
%%                            * number of processors P on H,
%%                            * table size N (per processor),
%%                            * idle timeout T, and
%%                            * spawn image computation flag S.
%% The function returns a pair consisting of the computed orbit and
%% a list of statistics, the first element of which reports overall statistics,
%% and all remaining elements report statistics of some worker.
orbit(Gs, Xs, Hosts) ->
  if
    is_integer(Hosts) ->
      TableSize = Hosts,
      sequential:orbit(Gs, Xs, TableSize);
    true ->
      par_orbit(Gs, Xs, Hosts)
  end.

par_orbit(Gs, Xs, Hosts) ->
  % spawn workers on Hosts
  {Workers, GlobTabSize} = start_workers(Hosts),

  % assemble StaticMachConf and distribute to Workers
  StaticMachConf = mk_static_mach_conf(Gs, self(), Workers, GlobTabSize),
  lists:foreach(fun({Pid, _, _}) -> Pid ! {init, StaticMachConf} end, Workers),

  % start wall clock timer
  StartTime = now(),

  % distribute initial vertices to workers
  Credit = worker:distribute_vertices(StaticMachConf, credit:one(), Xs),

  % collect credit handed back by idle workers
  collect_credit(Credit),

  % measure elapsed time (in milliseconds)
  ElapsedTime = now() - StartTime,

  % tell all Workers to dump their tables
  lists:foreach(fun({Pid, _, _}) -> Pid ! {dump} end, Workers),

  % collect results from all workers and return them
  collect_orbit(ElapsedTime, length(Workers)).


%% start_workers starts worker processes depending on the input Hosts:
%% * if Hosts is a quadruple {P, _, _, _} then P processes are forked on the
%%   executing Erlang node;
%% * if Hosts is a non-empty list {H1, P1, _, _, _}, {H2, P2, _, _, _}, ...
%%   then P1 processes are forked on Erlang node H1, P2 processes on node H2,
%%   and so on.
%% The function returns a pair {Workers, GlobalTableSize}, where
%% * GlobalTableSize is the total number of slots of the global hash table, and
%% * Workers is a list of Worker, sorted wrt. TableOffset in ascending order.
start_workers({Procs, TabSize, TmOut, SpawnImgComp}) ->
  {Workers, GlobalTableSize} = do_start_shm({Procs, TabSize, TmOut, SpawnImgComp}, {[], 0}),
  {lists:reverse(Workers), GlobalTableSize};
start_workers([Host | Hosts]) ->
  {Workers, GlobalTableSize} = do_start_dist([Host | Hosts], {[], 0}),
  {lists:reverse(Workers), GlobalTableSize}.

do_start_shm({0, _, _, _}, Acc) ->
  Acc;
do_start_shm({M, TabSize, TmOut, SpawnImgComp}, {Workers, GTabSize}) ->
  Pid = spawn_link(worker, init, [TabSize, TmOut, SpawnImgComp]),
  NewWorkers = [{Pid, GTabSize, TabSize} | Workers],
  NewGTabSize = GTabSize + TabSize,
  Acc = {NewWorkers, NewGTabSize},
  do_start_shm({M - 1, TabSize, TmOut, SpawnImgComp}, Acc).

do_start_dist([], Acc) ->
  Acc;
do_start_dist([{_, 0, _, _, _} | Hosts], Acc) ->
  do_start_dist(Hosts, Acc);
do_start_dist([{Node, M, TabSize, TmOut, SpawnImgComp} | Hosts], {Workers, GTabSize}) ->
  Pid = spawn_link(Node, worker, init, [TabSize, TmOut, SpawnImgComp]),
  NewWorkers = [{Pid, GTabSize, TabSize} | Workers],
  NewGTabSize = GTabSize + TabSize,
  Acc = {NewWorkers, NewGTabSize},
  do_start_dist([{Node, M - 1, TabSize, TmOut, SpawnImgComp} | Hosts], Acc).


%% collect_credit collects leftover credit from idle workers until
%% the credit adds up to 1.
collect_credit(Credit) ->
  case credit:is_one(Credit) of
    true  -> ok;  %% break loop and return dummy atom
    _Else ->
      receive
        {done, WorkersCredit} ->
          CollectedCredit = credit:credit(WorkersCredit, Credit),
          collect_credit(CollectedCredit)
      end
  end.


%% collect_orbit collects partial orbits and stats from N workers.
collect_orbit(ElapsedTime, N) ->
  {PartOrbits, WorkerStats} = do_collect_orbit(N, [], []),
  Orbit = lists:flatten(PartOrbits),
  Stats = [master_stats(ElapsedTime, WorkerStats) | WorkerStats],
  {Orbit, Stats}.

do_collect_orbit(0, PartOrbits, WorkerStats) -> {PartOrbits, WorkerStats};
do_collect_orbit(N, PartOrbits, WorkerStats) ->
  receive
    {result, PartOrbit, WorkerStat} ->
      do_collect_orbit(N - 1, [PartOrbit|PartOrbits], [WorkerStat|WorkerStats])
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions

%% functions operating on the StaticMachConf
mk_static_mach_conf(Gs, Master, Workers, GlobalTableSize) ->
   {Gs, Master, Workers, GlobalTableSize, 0, true}.
get_gens(StaticMachConf)              -> element(1, StaticMachConf).
get_master(StaticMachConf)            -> element(2, StaticMachConf).
get_workers(StaticMachConf)           -> element(3, StaticMachConf).
get_global_table_size(StaticMachConf) -> element(4, StaticMachConf).
get_idle_timeout(StaticMachConf)      -> element(5, StaticMachConf).
get_spawn_img_comp(StaticMachConf)    -> element(6, StaticMachConf).
set_idle_timeout(StaticMachConf, X)  -> setelement(5, StaticMachConf, X).
clear_spawn_img_comp(StaticMachConf) -> setelement(6, StaticMachConf, false).


%% produce readable statistics
master_stats(ElapsedTime, WorkerStats) ->
  Freq = table:sum_freqs([table:freq_from_stat(W) || W <- WorkerStats]),
  VertsRecvd = lists:sum([worker:verts_recvd_from_stat(W) || W <- WorkerStats]),
  CreditRetd = lists:sum([worker:credit_retd_from_stat(W) || W <- WorkerStats]),
  MinAtomicCredit = lists:max([worker:min_atomic_credit_from_stat(W) || W <- WorkerStats]),
  MaxInitIdle = lists:max([worker:init_idle_from_stat(W) || W <- WorkerStats]),
  MaxIdle = lists:max([worker:max_idle_from_stat(W) || W <- WorkerStats]),
  MaxTailIdle = lists:max([worker:tail_idle_from_stat(W) || W <- WorkerStats]),
  [{wall_time, ElapsedTime},
   {vertices_recvd, VertsRecvd},
   {credit_retd, CreditRetd},
   {min_atomic_credit, MinAtomicCredit},
   {max_init_idle_time, MaxInitIdle},
   {max_idle_time, MaxIdle},
   {max_tail_idle_time, MaxTailIdle} | table:freq_to_stat(Freq)].


%% current wall clock time (in milliseconds since start of RTS)
now() -> element(1, statistics(wall_clock)).
