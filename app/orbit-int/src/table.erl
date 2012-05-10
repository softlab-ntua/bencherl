%% orbit-int hash table (storing vertices on a worker)
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%


%% Note: Hash tables have a fixed number of slots but each slot can store
%%       a list of vertices. The functions is_member/3 and insert/3
%%       expect its slot argument to be in range.


-module(table).

-export([new/1, to_list/1, is_member/3, insert/3, get_freq/1,
         sum_freqs/2, sum_freqs/1,
         freq_to_slots/1, freq_to_nonempty_slots/1, freq_to_vertices/1,
         max_freq/1, avg_freq/1, avg_nonempty_freq/1, fill_deg/1,
         freq_to_stat/1, freq_from_stat/1]).

%% new(Size) creates a table with Size slots, each containing an empty list.
new(Size) ->
  array:new([{size,Size}, {default,[]}, {fixed,true}]).

%% to_list(T) converts a table T into a list of its entries.
to_list(T) ->
  lists:flatten(array:to_list(T)).

%% is_member(X, I, T) is true iff X is stored in table T at slot I.
is_member(X, I, T) ->
  L = array:get(I, T), lists:member(X, L).

%% insert(X, I, T) inserts X into table T at slot I.
insert(X, I, T) ->
  L = array:get(I, T), array:set(I, [X|L], T).


%% get_freq computes the fill frequency of table T;
%% the output is a list of integers where the number at position I
%% indicates how many slots of T are filled with I entries;
%% the sum of the output lists equals the number of slots of T.
get_freq(T) ->
  F0 = array:new([{default,0}, {fixed,false}]),
  F = array:foldl(fun(_, L, F) -> inc(length(L), F) end, F0, T),
  array:to_list(F).

%% freq_to_slots computes the number of slots from a table fill frequency.
freq_to_slots(F) -> lists:sum(F).

%% freq_to_nonempty_slots computes the number of non empty slots
%% from a table fill frequency.
freq_to_nonempty_slots(F) -> lists:sum(tl(F)).

%% freq_to_vertices computes the number of vertices
%% from a table fill frequency.
freq_to_vertices(F) ->
  {_, V} = lists:foldl(fun(N, {I,X}) -> {I + 1, (I * N) + X} end, {0,0}, F),
  V.

%% max_freq returns the maximum fill frequency.
max_freq(F) -> length(F) - 1.

%% avg_freq returns the average fill frequency
avg_freq(F) -> freq_to_vertices(F) / freq_to_slots(F).

%% avg_nonempty_freq returns the average fill frequency of non empty slots.
avg_nonempty_freq(F) ->
  case freq_to_vertices(F) of
    Verts when Verts > 0 -> Verts / freq_to_nonempty_slots(F);
    _Verts               -> 0.0  %% Verts = 0 <=> freq_to_nonempty_slots(F) = 0
  end.

%% fill_deg determines the filling degree of the table.
fill_deg(F) -> freq_to_nonempty_slots(F) / freq_to_slots(F).

%% sum_freqs/2 sums two fill frequencies.
sum_freqs([],    SumF)     -> SumF;
sum_freqs(F,     [])       -> F;
sum_freqs([N|F], [M|SumF]) -> [N + M | sum_freqs(F, SumF)].

%% sum_freqs/1 sums a list of fill frequencies.
sum_freqs(Fs) -> lists:foldl(fun(F, SumF) -> sum_freqs(F, SumF) end, [], Fs).


%% freq_to_stat produces a readable statistics from a table fill frequency;
%% the input frequency F is itself part of the statistics
freq_to_stat(Frequency) ->
  [{freq, Frequency},
   {size, table:freq_to_vertices(Frequency)},
   {slots, table:freq_to_slots(Frequency)},
   {nonempty_slots, table:freq_to_nonempty_slots(Frequency)},
   {fill_deg, table:fill_deg(Frequency)},
   {max_freq, table:max_freq(Frequency)},
   {avg_freq, table:avg_freq(Frequency)},
   {nonempty_avg_freq, table:avg_nonempty_freq(Frequency)}].

%% freq_from_stat extracts a table fill frequency from a statistics Stat
%% (assuming Stat was produced by freq_to_stat/1, otherwise returns []);
freq_from_stat(Stat) ->
  case lists:keyfind(freq, 1, Stat) of
    {_, F} -> F;
    _Else  -> []
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions

inc(I, F) ->
  N = array:get(I, F),
  array:set(I, N + 1, F).
