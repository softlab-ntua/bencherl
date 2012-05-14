This is a benchmark suite for the Erlang VM.

### How to build the benchmark suite ###

	$ make clean
	$ make

### How to build just the benchmarks ###

	$ make bench
 
### How to run benchmarks ###

Use the `run_bench` script.

### How to run specific benchmarks ###

Use the `-b` option of the `run_bench` script and specify the names of one or more 
of the available benchmarks, separated with commas.

	e.g. ./run_bench -b bang,big

### How to run all the benchmarks ###

Use the `run_bench` script without its `-b` option.

### How to get a list of all the available benchmarks ###

Use the `-l` option of the `run_bench` script.

	$ ./run_bench -l

### How to ignore one or more benchmarks ###

Edit the `conf/suite.conf` file and set the value of the `IGNORE_BENCH` variable 
to the names of the benchmarks to be ignored separated with commas.

	e.g. IGNORE_BENCH=bang,big

### How to specify the number of schedulers to use for running benchmarks ###

Use the `-s` option of the `run_bench` script. The argument of this option is 
either a single number (e.g. `1` -> run the benchmark with 1 scheduler), or one 
or more numbers separated with commas (e.g. `2,3` -> run the benchmark with 2 
and 3 schedulers), or a range of numbers (e.g. `2..4` -> run the benchmark with 
2, 3 and 4 schedulers).

	e.g. 

	$ ./run_bench -s 1
	$ ./run_bench -s 1..16
	$ ./run_bench -s 1,2,4,8,16,32

Benchmarks are executed by default with as many schedulers as the number of 
processors.

### How to specify one or more OTP's to use for running benchmarks ###

Edit the `conf/suite.conf` file and set the value of the `OTPS` variable to a 
a string that contains information about all the OTP's that should be used
separated with commas.
For each OTP both an alias and the path to it should be provided
(separated with a `=`).

	e.g. OTPS="R15B=~/otps/otp_src_R15B,R15B01=~/otps/otp_src_R15B01"

Benchmarks are executed by default using the erl program found on path.

### How to specify the different sets of VM arguments to use for running benchmarks ###

Edit the `conf/suite.conf` file and set the value of the `ARGS` variable to a
string that contains information abotu the different sets separated with 
commas.
An alias should be specified for each set of arguments. The alias is separated 
from the arguments with a `=`.

	e.g. ARGS="DEFAULT_BIND=+sbt db,UNBOUND=+sbt u"

### How to specify the Erlang compiler to use for compiling the benchmarks, the suite and the applications ###

Use the `ERLC` environment variable.

### What is the result of running one or more benchmarks ###

A new directory in the `results` directory. The name of the directory is the
date and time when the execution of the benchmarks took place in the format 
ddMMyyHHmmss.

In the `results` directory there is one directory for each benchmark `X`, which 
has been executed, with the same name. 

In each X directory there are 3 subdirectories:
* a `diagrams` directory
* an `ouput` directory
* a `statistics` directory

The `diagrams` directory contains the time and speedup diagrams.
The `output` directory contains the output of the benchmark for each execution.
The `statistics` directory contains time and speedup information for the 
execution of the benchmark.

### How to plot diagrams ###

Use the `-t` and the `-d` options of the `run_bench` script. Use the `-t` option for
time diagrams, and the `-d` option for speedup diagrams.

### How to run distributed benchmarks ###

Edit `conf/suite.conf` (or `X/conf/bench.conf`, where `X` is a benchmark 
directory) and set the value of the `NODES` variable to the short names of the 
Erlang nodes that should be used for running the benchmark. 

The suite is responsible for:
* starting these nodes before running the benchmark
* making them available to the benchmark (so that the benchmark can spawn 
  processes on them)
* stopping these nodes after having run the benchmark

