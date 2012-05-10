## orbit-int README
##
## Author: Patrick Maier <P.Maier@hw.ac.uk>
##

Given a space X, a list of generators f_1,...,f_n : X -> X and an initial
vertex x_0 : X, the /orbit problem/ is to compute the least subset Orb of X
such that Orb contains x_0 and is closed under all generators.

The orbit implementation presented here is specialised to the case where
the space X is a finite subset of the natural numbers. For the purpose
of benchmarking, the generators perform some deliberately irregular
computation (by calling a naive recursive implementation of Fibonacci).


ARCHITECTURE

This orbit implementation operates on a distributed hash table.
It follows a master/worker architecture, where each worker hosts a
chunk of the hash table; the master simply initiates the computation
and waits for termination.

All workers execute the following algorithm:
* A worker receives a vertex to be stored in a slot in its chunk of the table.
* If that vertex is already present
  then the worker waits to receive the next vertex.
* Otherwise, the worker inserts the vertex into the table and applies
  the generators.
* The worker hashes all newly generated vertices into slots of the hash table
  and sends them to the workers hosting those slots.
* Finally, the worker waits to receive the next vertex.

There are 3 main variants of the algorithm:
* A sequential implementation (ie. master, and the table is not distributed).
* A parallel implementation where master and all workers are processes
  within the same Erlang node.
* A distributed parallel implementation where master and worker proceses
  reside on different Erlang nodes.

The latter two variants are further subdivided into sub-variants according
to whether the workers themselves are sequential, or whether they perform
image computation (ie. applying the generators) in parallel. The latter
generates lots (millions, easily) of short-lived processes, whereas the
former only generates long-lived processes (one per worker).


BUILD

\begin{terminal}
  $> make all
  erlc bench.erl
  erlc credit.erl
  erlc master.erl
  erlc sequential.erl
  erlc table.erl
  erlc worker.erl
\end{terminal}


SEQUENTIAL BENCHMARKS

Function bench:seq/2 expects two parameters:
* a list of generators, parametrised by the size of the space (technically,
  a function taking the size of the space and returning a list of generators),
* the size of the space.

The function creates a hash table with twice as many slots as the size of
the space, computes the orbit of vertex 0 under the given generators,
and returns the size of that orbit. For example, the following call
computes the orbit of 0 under generators 1, 2 and 4 in the interval [0,10^6).
The call took about 500 seconds to complete on a 2.13GHz Xeon server.

\begin{terminal}
  $> erl -eval "bench:seq(fun bench:g124/1, 1000000), init:stop()."
  Erlang R14B04 (erts-5.8.5) [source] [smp:8:8] [rq:8] [async-threads:0] [hipe] [kernel-poll:false]

  Eshell V5.8.5  (abort with ^G)
  1> {size,688547}
\end{terminal}

To obtain other benchmarks, vary the arguments of bench:seq/2. A smaller
sized space will generally result in a smaller benchmark, as will a smaller
set of generators.


PARALLEL BENCHMARKS

Functions bench:par/3 and bench:par_seq/3 expect three parameters:
* the first two are the same as the parameters of bench:seq/2,
* the third is the number of workers to start.

These functions create a hash table with twice as many slots as the size
the space, distribute chunks of that table evenly across all workers,
and compute, in parallel, the orbit of vertex 0 under the given generators,
returning the size of the orbit. The difference between the functions is
that bench:par/3 spawns lots of short lived processes to apply generators
in parallel, whereas bench:par_seq/3 does not exploit intra-worker
parallelism. Below are some example calls; they took about 65 and
60 seconds, respectively, to complete on a 2.13GHz Xeon server with 8 cores.

\begin{terminal}
  $> erl -eval "bench:par(fun bench:g124/1, 1000000, 8), init:stop()."
  Erlang R14B04 (erts-5.8.5) ...

  Eshell V5.8.5  (abort with ^G)
  1> {size,688547}

  $> erl -eval "bench:par_seq(fun bench:g124/1, 1000000, 8), init:stop()."
  Erlang R14B04 (erts-5.8.5) ...

  Eshell V5.8.5  (abort with ^G)
  1> {size,688547}
\end{terminal}


DISTRIBUTED BENCHMARKS

Functions bench:dist/4 and bench:dist_seq/4 expect four parameters:
* the first two are the same as the parameters of bench:seq/2,
* the third is the number of workers to start on each Erlang node,
* the fourth is a list of Erlang nodes to start workers on.

The functions bench:dist/4 and bench:dist_seq/4 operate similarly to
bench:par/3 and bench:par_seq/3, respectively, except that they distribute
the hash table and the workers across several Erlang nodes. These nodes
must already be started up at the time the functions are called; a setup
script does this, reading the list of nodes (in short name format) from
the command line. Below is a sample session with setup and two calls.
The calls took about 13 and 12 seconds, respectively, to complete on 64 cores
(8 2.13GHz Xeon servers, with 8 cores each). Note the shell command 'fg' at
the end of the session to stop the previously started Erlang nodes;
alternatively, one could have run './cleanup.sh node1@bwlf21 node2@bwlf22 ...'.

\begin{terminal}
  $> ./setup.sh node1@bwlf21 node2@bwlf22 node3@bwlf23 node4@bwlf24 node5@bwlf25 node6@bwlf26 node7@bwlf27 node8@bwlf28
  Starting Erlang nodes ... started

  [1]+  Stopped                 ./setup.sh node1@bwlf21 node2@bwlf22 node3@bwlf23 node4@bwlf24 node5@bwlf25 node6@bwlf26 node7@bwlf27 node8@bwlf28

  $> erl -sname master@bwlf25 -eval "bench:dist(fun bench:g124/1, 1000000, 8, [node1@bwlf21, node2@bwlf22, node3@bwlf23, node4@bwlf24, node5@bwlf25, node6@bwlf26, node7@bwlf27, node8@bwlf28]), init:stop()."
  Erlang R14B04 (erts-5.8.5) ...

  Eshell V5.8.5  (abort with ^G)
  (master@bwlf25)1> {size,688547}

  $> erl -sname master@bwlf25 -eval "bench:dist_seq(fun bench:g124/1, 1000000, 8, [node1@bwlf21, node2@bwlf22, node3@bwlf23, node4@bwlf24, node5@bwlf25, node6@bwlf26, node7@bwlf27, node8@bwlf28]), init:stop()."
  Erlang R14B04 (erts-5.8.5) ...

  Eshell V5.8.5  (abort with ^G)
  (master@bwlf25)1> {size,688547}

  $> fg
  ./setup.sh node1@bwlf21 node2@bwlf22 node3@bwlf23 node4@bwlf24 node5@bwlf25 node6@bwlf26 node7@bwlf27 node8@bwlf28
  Stopping Erlang nodes ... done
\end{terminal}
