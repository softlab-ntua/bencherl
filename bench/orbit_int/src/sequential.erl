%% orbit-int sequential implementation
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%

-module(sequential).

-export([orbit/3]).

%% DATA
%%   Static Machine Configuration:
%%     {Gs,               %list of generators
%%      TableSize}        %hash table size (ie. #slots)
%%
%%   Statistics:
%%     List of pairs where the first component is an atom, the second
%%     some data. Part of the data is the fill frequency of the table
%%     (a list whose ith element indicates frequency of filling degree i).


%% compute orbit of elements in list Xs under list of generators Gs,
%% where the hash table is of size TableSize.
%% The function returns a pair consisting of the computed orbit and a singleton
%% list of statistics (mainly runtime and fill degree of the table).
orbit(Gs, Xs, TableSize) ->
  % assemble static configuration
  StaticMachConf = mk_static_mach_conf(Gs, TableSize),

  % initialise hash table and work queue
  Table = table:new(TableSize),
  Queue = queue:from_list(Xs),

  % start wall clock timer
  StartTime = master:now(),
  % start vertex server and compute orbit
  {FinalTable, VertsRecvd} = vertex_server(StaticMachConf, Table, Queue, 0),

  % measure elapsed time (in milliseconds)
  ElapsedTime = master:now() - StartTime,

  % return result
  Orbit = table:to_list(FinalTable),
  Stat = seq_stats(ElapsedTime, table:get_freq(FinalTable), VertsRecvd),
  {Orbit, [Stat]}.


%% main loop working off work Queue;
%% StaticMachConf -- static data
%% Table          -- hash table holding vertices
%% Queue          -- work queue
%% VertsRecvd     -- number of vertices removed from the Queue so far
vertex_server(StaticMachConf, Table, Queue, VertsRecvd) ->
  case queue:out(Queue) of
    {empty, _}           -> {Table, VertsRecvd};
    {{value, X}, Queue1} ->
      {NewTable, NewQueue} = handle_vertex(StaticMachConf, X, Table, Queue1),
      vertex_server(StaticMachConf, NewTable, NewQueue, VertsRecvd + 1)
  end.


%% handle_vertex checks whether vertex X is stored in Table;
%% if not, it is in inserted and the images of the generators
%% are pushed into the work queue.
handle_vertex(StaticMachConf, X, Table, Queue) ->
  Gs = get_gens(StaticMachConf),

  % compute Slot
  Slot = hash_vertex(StaticMachConf, X),

  % check whether X is already in Table
  case table:is_member(X, Slot, Table) of
    true  -> {Table, Queue};  % X already in table; do nothing

    _Else ->                  % X not in table
      % insert X at Slot
      NewTable = table:insert(X, Slot, Table),

      % compute images of X under generators Gs
      Xs = [G(X) || G <- Gs],

      % enquene Xs
      NewQueue = lists:foldl(fun queue:in/2, Queue, Xs),

      % return updated table and queue
      {NewTable, NewQueue}
  end.


%% hash_vertex computes the hash table slot of vertex X
hash_vertex(StaticMachConf, X) ->
  TableSize = get_table_size(StaticMachConf),
  Hash = erlang:phash2(X),
  Hash rem TableSize.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions

%% functions operating on the StaticMachConf
mk_static_mach_conf(Gs, TableSize) -> {Gs, TableSize}.
get_gens(StaticMachConf)       -> element(1, StaticMachConf).
get_table_size(StaticMachConf) -> element(2, StaticMachConf).


%% produce readable statistics
seq_stats(ElapsedTime, Frequency, VertsRecvd) ->
  [{wall_time, ElapsedTime},
   {vertices_recvd, VertsRecvd} | table:freq_to_stat(Frequency)].
