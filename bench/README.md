This is where all benchmarks reside.

### What does a benchmark directory look like ###

A benchmark directory should contain:

* a `Makefile` that builds the benchmark
* an `src` directory that contains the source files of the benchmark
* a `conf` directory that contains the `pre_bench`, the `post_bench` and the `bench.conf` scripts

### How to write a benchmark driver module ###

The `src` subdirectory of a benchmark directory `X` should contain at least one 
file: `X.erl`.

The `X` module must expose the following functions:

	% Return the different argument sets to use for running the benchmark.
	bench_args() -> [[term()]]

	% Run the benchmark with the specified arguments on the specified nodes.
	run(Args, Nodes, Opts) -> ok or {error, Reason}
		where
			Args = [term()]
			Nodes = [node()]
			Opts = [{Key :: atom, Val :: term()}, ...]

### What is the pre_bench script ###

It is a BASH script that can be put in the `conf` subdirectory of a benchmark
directory. This script is executed before running the corresponding benchmark.

### What is the post_bench script ###

It is a BASH script that can be put in the `conf` subdirectory of a benchmark
directory. This script is executed after having finished with the execution of
the corresponding benchmark.

### What is the bench.conf script ###

It is a BASH scipt that can be put in the `conf` subdirectory of a benchmark 
directory. It is responsible for configuring the corresponding benchmark. 

A variable set in the `bench.conf` file overrides for the execution of the 
corresponding benchmark a variable with the same name that is set in the 
`conf/suite.conf` file.

