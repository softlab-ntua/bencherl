%% orbit-int credits (for checking termination of orbit computation)
%%
%% Author: Patrick Maier <P.Maier@hw.ac.uk>
%%

-module(credit).

-export([credit/2, credit_atomic/2, debit_atomic/1, debit_atomic_nz/1,
         zero/0, one/0, is_zero/1, is_one/1]).

%% An *atomic credit* is represented as a non-negative integer k;
%% it stands for the credit 1/{2^k}.
%%
%% A *credit* is represented as list of non-negative integers, sorted in
%% strict descending order; it represents the sum of atomic credits
%% represented by the integers on the list, where zero credit is
%% represented by the empty list. The maximally possible credit, 1,
%% is represented by [0].


%% credit_atomic(K, C) adds the atomic credit 1/{2^K} to the credit C.
credit_atomic(K, [])     -> [K];
credit_atomic(K, [C|Cs]) -> if
                              K > C  -> [K, C | Cs];
                              K == C -> credit_atomic(K - 1, Cs);
                              K < C  -> [C | credit_atomic(K, Cs)]
                            end.

%% credit(C1, C2) returns a list representing the sum of the credit
%% represented by the lists C1 and C2.
credit(C1, C2) -> lists:foldl(fun credit_atomic/2, C2, C1).
% Alternative fomulation:
% credit([],     C2) -> C2;
% credit([K|Ks], C2) -> credit(Ks, credit_atomic(K, C2)).

%% debit_atomic(C) returns a pair {K',C'} where K' is an integer
%% representing some atomic credit and C' is a list of integers representing
%% some credit (which may be zero) such that the sum of the credits
%% represented by K' and C' equals the credit represented by C.
%% Precondition: C must represent non-zero credit.
debit_atomic([C|Cs]) -> {C, Cs}.  %% debit smallest unit of credit

%% debit_atomic_nz(C) returns a pair {K',C'} where K' is an integer
%% representing some atomic credit and C' is a list of integers representing
%% some non-zero credit such that the sum of the credits
%% represented by K' and C' equals the credit represented by C.
%% Precondition: C must represent non-zero credit.
debit_atomic_nz([C])    -> {C+1, [C+1]};  %% debit half the credit
debit_atomic_nz([C|Cs]) -> {C,   Cs}.     %% debit smallest unit of credit;
                                          %% case only applies if Cs non-empty

%% zero/0 produces zero credit.
zero() -> [].

%% one/0 produces credit one.
one() -> [0].

%% is_zero/1 tests whether its argument represents zero credit.
is_zero([]) -> true;
is_zero(_)  -> false.

%% is_one/1 tests whether its argument represents maximal credit 1.
is_one([0]) -> true;
is_one(_)   -> false.
