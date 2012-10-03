`bencherl` is a scalability benchmark suite for Erlang.

### How to build the benchmark suite ###

	$ make clean
	$ make

### How to build just the benchmarks ###

	$ make bench

### How to build just the applications ###

	$ make app
 
### How to run the benchmark suite ###

Specify what you want to run and how in `conf/run.conf`, and then use 
`bencherl` to run the benchmark suite.

	$ ./bencherl

### How to specify a mnemonic name for a run ###

Use the `-m` option of the `bencherl` script.

	$ ./bencherl -m everything-but-big

### How to specify which benchmarks to run ###

Set the `INCLUDE_BENCH` variable in `conf/run.conf`, if you want to specify 
which benchmarks to run.
 
	INCLUDE_BENCH=bang,big

Set the `EXCLUDE_BENCH` variable in `conf/run.conf`, if you want to specify
which benchmarks not to run.

	EXCLUDE_BENCH=dialyzer_bench

The values of both variables are one or more benchmark names separated with 
commas.

By default, all benchmarks are run.

### How to list all benchmarks ###

Use the `-l` option of the `bencherl` script.

	$ ./run_bench -l

### How to specify the number of schedulers to run benchmarks with ###

Set the `NUMBER_OF_SCHEDULERS` variable in `conf/run.conf`.

The value of this variable can be either one or more integers separated with 
commas:
	
	NUMBER_OF_SCHEDULERS=1,2,4,8,16,32,64

or a range of integers:

	NUMBER_OF_SCHEDULERS=1..16

By default, benchmarks are run with as many schedulers as the number of logical
processors.
 
### How to specify the versions/flavors of Erlang/OTP to run benchmarks with ###

Set the `OTPS` variable in `conf/run.conf`.

The value of this variable is one or more alias-path pairs separated with commas.

	OTPS="R14B04=/usr/local/otp_src_R14B04,R15B01=/usr/local/otp_src_R15B01"

By default, benchmarks are compiled and run with the `erlc` and `erl` programs
found in the OS path.

### How to specify the `erl` command-line arguments to run benchmarks with ###

Set the `ERL_ARGS` variable in `conf/run.conf`.

The value of this variable is one or more alias-arguments pairs separated with
commas.

	ERL_ARGS="SOME_ARGS=+sbt db +swt low,SOME_OTHER_ARGS=+sbt u"

### How to specify the number of slave nodes to run the benchmarks with ###

Set the `NUMBER_OF_SLAVE_NODES` variable in `conf/run.conf`.

The value of this variable can be either one or more integers separated with
commas:

	NUMBER_OF_SLAVE_NODES=1,2,4,6,8

or a range of integers:

	NUMBER_OF_SLAVE_NODES=2..4

Benchmarks are executed with at most as many slave nodes as specified in the 
`SLAVE_NODES` variable.

By default, benchmarks are run with one master node and no slave nodes.

### How to specify the slave nodes to run benchmarks with ###

Set the `SLAVE_NODES` variable in `conf/run.conf`.

The value of this variable is zero or more long or short node names separated 
with commas. 

	SLAVE_NODES=somenode@somehost,someothernode@someotherhost

The `USE_LONG_NAMES` variable determines whether long or short names are
expected.

By default, benchmarks are run with no slave nodes.

### How to specify the master node to run benchmarks with ###

Set the `MASTER_NODE` variable in `conf/run.conf`.

The value of this variabe is the short or the long name of the master node.

    MASTER_NODE=somenode@somehost

The `USE_LONG_NAMES` variable determines whether long or short names are
expected.

The default long name of the master node is:

    master@`hostname -f`

and its default short name:

    master@`hostname`


### How to specify the magic cookie that master and slave nodes share ###

Set the `COOKIE` variable in `conf/run.conf`.

	COOKIE="some_cookie"

The default cookie is `cookie`.

### How to specify which version of the benchmarks to run ###

Set the `VERSION` variable in `conf/run.conf`.

The value of this variable can be `short`, `intermediate` or `long`.

	VERSION=short

The default version is `short`.

### How to specify whether to produce scalability graphs or not ###

Set the `PLOT` variable in `conf/run.conf`.

The value of this variable can be either 0 (do not produce any scalability 
graphs) or 1 (produce scalability graphs).

	PLOT=1

The default value is 1.

### How to specify whether to perform a sanity check or not ###

Set the `CHECK_SANITY` variable in `conf/run.conf`.

The value of this variable can be either 0 (do not perform sanity check) or
1 (perform sanity check).

	SANITY_CHECK=1

By default, the sanity of the benchmark execution results is not checked.

### How to specify the number of iterations ###

Set the `ITERATIONS` variable in `conf/run.conf`.

The value of this variable is an integer that is greater than or equal to 1.

	ITERATIONS=5

The default number of iterations is 1.
 
### What is the result of running the benchmark suite ###

A new directory is created under the `results` directory. The name of this 
directory is the mnemonic name or, if no mnemonic name has been specified, a
string that contains the date and time when the run started.

In the result directory, there is one subdirectory for each one of the 
benchmarks that was run, with the same name as the benchmark. Each such 
directory has three sub-directories:
* `graphs`, which contains the scalability graphs;
* `output`, which contains the output that the benchmark produced during its execution;
* `measurements`, which contains the scalability measurements collected during the execution of the benchmark
. 

