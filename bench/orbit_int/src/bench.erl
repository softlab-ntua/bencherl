%% orbit-int benchmarks (for RELEASE)
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%

-module(bench).

-export([%% sets of generators
         g/1,
         g1/1, g2/1, g3/1, g4/1, g5/1,
         g12/1, g13/1, g14/1, g15/1, g23/1, g24/1, g25/1, g34/1, g35/1, g45/1,
         g123/1, g124/1, g125/1, g134/1, g135/1, g145/1, g234/1, g235/1, g245/1, g345/1,
         g1234/1, g1235/1, g1245/1, g1345/1, g2345/1,
         g12345/1,
         %% sequential benchmarks
         seq/2,
         %% parallel benchmarks
         par/3, par_seq/3,
         %% distributed benchmarks
         dist/4, dist_seq/4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generators

%% Fibonacci numbers
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% mixing polynomials (up to degree 3)
p(A0,_N)          -> A0.
p(A1,A0, N)       -> A1 * N + p(A0, N).
p(A2,A1,A0, N)    -> A2 * N * N + p(A1,A0, N).
p(A3,A2,A1,A0, N) -> A3 * N * N * N + p(A2,A1,A0, N).

%% step functions (up to 4 steps)
s(B0, N)          -> if N < B0 -> 0; true -> 1 end.
s(B0,B1, N)       -> if N < B0 -> 0; true -> 1 + s(B1, N) end.
s(B0,B1,B2, N)    -> if N < B0 -> 0; true -> 1 + s(B1,B2, N) end.
s(B0,B1,B2,B3, N) -> if N < B0 -> 0; true -> 1 + s(B1,B2,B3, N) end.

%% remainder function (range 0..R-1)
r(R, N) -> abs(N) rem R.


%% generators based on Fibonacci numbers;
%% functions f1(N,_),...,f5(N,_) produce numbers in the range 0 .. N-1;
%% computationally f1 = fib(0..15),
%%                 f2 = fib(5..20),
%%                 f3 = fib(10..25),
%%                 f4 = fib(11,19,27), bias 49% to 11, 49% to 19, 2% to 27
%%                 f5 = fib(10,20,30), bias 90% to 10, 9.9% to 20, 0.1% to 30
f1(N,X) -> r(N, fib(p(1,0, r(16, X))) + p(1,0, X)).

f2(N,X) -> r(N, fib(p(1,5, r(16, X))) + p(2,5,-1, X)).

f3(N,X) -> r(N, fib(p(1,10, r(16, X))) + p(-1,0,8,0,X)).

f4(N,X) -> r(N, fib(p(8,3, s(0,49,98,100, r(100, X)))) + p(-1, X)).

f5(N,X) -> r(N, fib(p(10,0, s(0,900,999,1000, r(1000, X)))) + p(1, X)).


%% sets (= lists) of generators
g(_N) -> [].

g1(N) -> [fun(X) -> f1(N,X) end].
g2(N) -> [fun(X) -> f2(N,X) end].
g3(N) -> [fun(X) -> f3(N,X) end].
g4(N) -> [fun(X) -> f4(N,X) end].
g5(N) -> [fun(X) -> f5(N,X) end].

g12(N) -> g1(N) ++ g2(N).
g13(N) -> g1(N) ++ g3(N).
g14(N) -> g1(N) ++ g4(N).
g15(N) -> g1(N) ++ g5(N).
g23(N) -> g2(N) ++ g3(N).
g24(N) -> g2(N) ++ g4(N).
g25(N) -> g2(N) ++ g5(N).
g34(N) -> g3(N) ++ g4(N).
g35(N) -> g3(N) ++ g5(N).
g45(N) -> g4(N) ++ g5(N).

g123(N) -> g12(N) ++ g3(N).
g124(N) -> g12(N) ++ g4(N).
g125(N) -> g12(N) ++ g5(N).
g134(N) -> g13(N) ++ g4(N).
g135(N) -> g13(N) ++ g5(N).
g145(N) -> g14(N) ++ g5(N).
g234(N) -> g23(N) ++ g4(N).
g235(N) -> g23(N) ++ g5(N).
g245(N) -> g24(N) ++ g5(N).
g345(N) -> g34(N) ++ g5(N).

g1234(N) -> g123(N) ++ g4(N).
g1235(N) -> g123(N) ++ g5(N).
g1245(N) -> g124(N) ++ g5(N).
g1345(N) -> g134(N) ++ g5(N).
g2345(N) -> g234(N) ++ g5(N).

g12345(N) -> g1234(N) ++ g5(N).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% benchmarks, parametrised by
%% * list of Generators
%% * size of space N > 0
%% * number of processors P > 0 (per node)
%% * list of Workers (in short node name format 'name@host')


%% sequential orbit computation
seq(Generators, N) ->
  sz(master:orbit(Generators(N), [0], 2 * N)).


%% parallel orbit computation (par_seq/3 does not spawn image computation)
par(Generators, N, P) ->
  sz(master:orbit(Generators(N), [0], {P, (2 * N) div P + 1, 0, true})).

par_seq(Generators, N, P) ->
  sz(master:orbit(Generators(N), [0], {P, (2 * N) div P + 1, 0, false})).


%% distributed orbit computation (dist_seq/4 does not spawn image computation)
dist(Generators, N, P, Workers) ->
  W = length(Workers),
  sz(master:orbit(Generators(N), [0], [{H, P, (2 * N) div (W * P) + 1, 1, true} || H <- Workers])).

dist_seq(Generators, N, P, Workers) ->
  W = length(Workers),
  sz(master:orbit(Generators(N), [0], [{H, P, (2 * N) div (W * P) + 1, 1, false} || H <- Workers])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% auxiliary functions

%% print size of generated orbit
sz({_Orbit, [MainStats|_OtherStats]}) ->
  io:write(lists:keyfind(size, 1, MainStats)), io:nl().
