%% orbit-int worker (computing vertices and holding part of hash table)
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%

-module(worker).

-export([init/3,
         distribute_vertices/3,
         send_image/4,
         verts_recvd_from_stat/1,
         credit_retd_from_stat/1,
         min_atomic_credit_from_stat/1,
         init_idle_from_stat/1,
         tail_idle_from_stat/1,
         max_idle_from_stat/1]).

%% DATA (see module master)


%% MESSAGES (see module master)


%% counters/timers record
-record(ct,
  {verts_recvd = 0,       %% #vertices received by this server so far
   credit_retd = 0,       %% #times server has returned credit to master
   min_atomic_credit = 0, %% minimal atomic credit received so far
   last_event = master:now(), %% time stamp [ms] of most recent event
   init_idle = -1,        %% idle time [ms] between init recv first vertex
   tail_idle = -1,        %% idle time [ms] between send last vertex and dump
   max_idle = -1}).       %% max idle [ms] time between vertices


%% initialise worker
init(LocalTableSize, IdleTimeout, SpawnImgComp) ->
  Table = table:new(LocalTableSize),
  receive
    {init, StaticMachConf0} ->
      StatData = #ct{},
      StaticMachConf1 = master:set_idle_timeout(StaticMachConf0, IdleTimeout),
      StaticMachConf = case SpawnImgComp of
			   true  -> StaticMachConf1;
			   _Else -> master:clear_spawn_img_comp(StaticMachConf1)
		       end,
      Credit = credit:zero(),
      vertex_server(StaticMachConf, Credit, Table, StatData)
  end.


%% main worker loop: server handling vertex messages;
%% StaticMachConf -- info about machine configuration
%% Credit         -- credit currently held by the server,
%% Table          -- hash table holding vertices
%% StatData       -- various counters and timers for gathering statistics
vertex_server(StaticMachConf, Credit, Table, StatData) ->
  IdleTimeout = master:get_idle_timeout(StaticMachConf),
  receive
    {vertex, X, Slot, K} ->
      Credit_plus_K = credit:credit_atomic(K, Credit),
      Now = master:now(),
      {NewCredit, NewTable} = handle_vertex(StaticMachConf, X, Slot, Credit_plus_K, Table),
      VertsRecvd = StatData#ct.verts_recvd,
      MinAtomicCredit = StatData#ct.min_atomic_credit,
      LastEvent = StatData#ct.last_event,
      InitIdle = StatData#ct.init_idle,
      MaxIdle = StatData#ct.max_idle,
      NewStatData0 = StatData#ct{verts_recvd = VertsRecvd + 1,
                                 min_atomic_credit = max(MinAtomicCredit, K)},
      NewStatData1 = if
                       InitIdle < 0 -> NewStatData0#ct{init_idle = Now - LastEvent};
                       true         -> NewStatData0#ct{max_idle = max(MaxIdle, Now - LastEvent)}
                     end,
      NewStatData = NewStatData1#ct{last_event = master:now()},
      vertex_server(StaticMachConf, NewCredit, NewTable, NewStatData);

    {dump} ->
      Now = master:now(),
      LastEvent = StatData#ct.last_event,
      NewStatData = StatData#ct{tail_idle = Now - LastEvent,
                                last_event = master:now()},
      dump_table(StaticMachConf, Table, NewStatData)

  after
    IdleTimeout ->
      CreditRetd = StatData#ct.credit_retd,
      NewCreditRetd = return_credit(StaticMachConf, Credit, CreditRetd),
      NewStatData = StatData#ct{credit_retd = NewCreditRetd},
      vertex_server(StaticMachConf, credit:zero(), Table, NewStatData)
  end.


%% handle_vertex checks whether vertex X is stored in Slot of Table;
%% if not, it is in inserted there and the images of the generators
%% are distributed among the workers.
%% Precondition: Credit is non-zero.
handle_vertex(StaticMachConf, X, Slot, Credit, Table) ->
  % check whether X is already in Table
  case table:is_member(X, Slot, Table) of
    true  -> {Credit, Table};  % X already in table; do nothing

    _Else ->                   % X not in table
      % insert X at Slot
      NewTable = table:insert(X, Slot, Table),

      % distribute images of X under generators to their respective workers
      NewCredit = distribute_images(StaticMachConf, X, Credit),

      % return remaining credit and updated table
      {NewCredit, NewTable}
  end.


%% return_credit sends non-zero Credit back to the master;
%% returns number of times credit has been returned so far
return_credit(StaticMachConf, Credit, CreditRetd) ->
  case credit:is_zero(Credit) of
    true  -> CreditRetd;
    false -> MasterPid = master:get_master(StaticMachConf),
             MasterPid ! {done, Credit},
             CreditRetd + 1
  end.


%% dump_table sends a list containing the local partial orbit to the master,
%% together with some statistics on the distribution of vertices in the table.
dump_table(StaticMachConf, Table, StatData) ->
  MasterPid = master:get_master(StaticMachConf),
  Stat = worker_stats(node(self()), table:get_freq(Table), StatData),
  PartialOrbit = table:to_list(Table),
  MasterPid ! {result, PartialOrbit, Stat}.


%% distribute_images distributes the images of vertex X under the generators
%% to the workers determined by the hash; some ore all of of the Credit is
%% used to send the messages, the remaining credit is returned;
%% computation and sending of vertices is actually done asynchronously.
%% Precondition: Credit is non-zero.
distribute_images(StaticMachConf, X, Credit) ->
  Gs = master:get_gens(StaticMachConf),
  do_distribute_images(StaticMachConf, X, Credit, Gs).

do_distribute_images(_StaticMachConf, _X, Credit, []) ->
  Credit;
do_distribute_images(StaticMachConf, X, Credit, [G]) ->
  {K, RemainingCredit} = credit:debit_atomic(Credit),
  case master:get_spawn_img_comp(StaticMachConf) of
    true  -> spawn(worker, send_image, [StaticMachConf, X, G, K]), ok;
    _Else -> send_image(StaticMachConf, X, G, K)
  end,
  RemainingCredit;
do_distribute_images(StaticMachConf, X, Credit, [G|Gs]) ->
  {K, NonZeroRemainingCredit} = credit:debit_atomic_nz(Credit),
  case master:get_spawn_img_comp(StaticMachConf) of
    true  -> spawn(worker, send_image, [StaticMachConf, X, G, K]), ok;
    _Else -> send_image(StaticMachConf, X, G, K)
  end,
  do_distribute_images(StaticMachConf, X, NonZeroRemainingCredit, Gs).


%% distribute_vertices distributes the list of vertices Xs to the workers
%% determined by the hash; some ore all of of the Credit is used to send
%% the messages, the remaining credit is returned.
%% Precondition: If Xs is non-empty then Credit must be non-zero.
distribute_vertices(_StaticMachConf, Credit, []) ->
  Credit;
distribute_vertices(StaticMachConf, Credit, [X]) ->
  {K, RemainingCredit} = credit:debit_atomic(Credit),
  send_vertex(StaticMachConf, X, K),
  RemainingCredit;
distribute_vertices(StaticMachConf, Credit, [X|Xs]) ->
  {K, NonZeroRemainingCredit} = credit:debit_atomic_nz(Credit),
  send_vertex(StaticMachConf, X, K),
  distribute_vertices(StaticMachConf, NonZeroRemainingCredit, Xs).


%% send_image sends image of X under G to the worker determined by
%% the hash of G(X); the message is tagged with atomic credit K.
send_image(StaticMachConf, X, G, K) ->
  Y = G(X),
  send_vertex(StaticMachConf, Y, K).


%% send_vertex hashes vertex X and sends it to the worker determined by
%% the hash; the message is tagged with atomic credit K.
send_vertex(StaticMachConf, X, K) ->
  {Pid, Slot} = hash_vertex(StaticMachConf, X),
  Pid ! {vertex, X, Slot, K},
  ok.


%% hash_vertex computes the two-dimensional hash table slot of vertex X where
%% the first dim is a worker pid and the second a slot in that worker's table.
hash_vertex(StaticMachConf, X) ->
  % get static info
  GlobalTableSize = master:get_global_table_size(StaticMachConf),
  Workers = master:get_workers(StaticMachConf),

  % compute raw hash and slot in global table
  Hash = erlang:phash2(X),
  GlobalSlot = Hash rem GlobalTableSize,

  % translate global slot into worker pid and local slot
  global_to_local_slot(Workers, GlobalSlot).


%% global_to_local_slot traverses the list Workers sequentially to translate
%% slot GlobSlot in the global hash table into a two-dimensional local slot
%% {pid, slot}, where 'pid' is the PID of a worker and 'slot' a the slot
%% in that worker's local hash table.
%% Precondition: GlobSlot < sum of TableSize in Workers.
%% Note: This procedure is horribly inefficient (linear in size of Workers);
%%       it should be log (size of Workers) at most.
global_to_local_slot([{Pid, _, TabSize} | Workers], GlobSlot) ->
  if
    GlobSlot < TabSize  -> {Pid, GlobSlot};
    true                -> global_to_local_slot(Workers, GlobSlot - TabSize)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions

%% produce readable statistics
worker_stats(Node, Frequency, StatData) ->
  [{node, Node},
   {vertices_recvd, StatData#ct.verts_recvd},
   {credit_retd, StatData#ct.credit_retd},
   {min_atomic_credit, StatData#ct.min_atomic_credit},
   {init_idle_time, StatData#ct.init_idle},
   {max_idle_time, StatData#ct.max_idle},
   {tail_idle_time, StatData#ct.tail_idle} | table:freq_to_stat(Frequency)].

verts_recvd_from_stat(Stat) ->
  case lists:keyfind(vertices_recvd, 1, Stat) of
    {_, VertsRecvd} -> VertsRecvd;
    _Else           -> false
  end.

credit_retd_from_stat(Stat) ->
  case lists:keyfind(credit_retd, 1, Stat) of
    {_, CreditRetd} -> CreditRetd;
    _Else           -> false
  end.

min_atomic_credit_from_stat(Stat) ->
  case lists:keyfind(min_atomic_credit, 1, Stat) of
    {_, MinAtomicCredit} -> MinAtomicCredit;
    _Else                -> false
  end.

init_idle_from_stat(Stat) ->
  case lists:keyfind(init_idle_time, 1, Stat) of
    {_, InitIdle} -> InitIdle;
    _Else         -> false
  end.

tail_idle_from_stat(Stat) ->
  case lists:keyfind(tail_idle_time, 1, Stat) of
    {_, TailIdle} -> TailIdle;
    _Else         -> false
  end.

max_idle_from_stat(Stat) ->
  case lists:keyfind(max_idle_time, 1, Stat) of
    {_, MaxIdle} -> MaxIdle;
    _Else         -> false
  end.
