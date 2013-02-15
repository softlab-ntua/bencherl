DEbench (Distributed Erlang Benchmark)

-------------- Introduction

DEbench is a benchmark suite for distributed Erlang. It measures the throughput and latency of distributed Erlang commands and saves them in appropriate CSV files. In DEbench implementation, we have used Basho Bensh modules (an open source benchmarking tool for Riak database).

Thus, you can find more details about the common parts between Basho Bench and DEbench here: (https://github.com/basho/basho_bench).

--------------- How to build and run the benchmark suite 

1. Clean up after any previous builds (if necessary).

$ make clean

2. Build DEbench

$ make

3. Run DEbench 

$ ./de_bench bench.config

Note) bench.config is a sample config file. Benchmark's duration, commands and number of worker processes are defined in the config file. After finishing the benchmark, the results are saved in "test/current" directory

4. Create graphs

$ make results

If everything goes well, you should have a graph (a png file) in "test/current" directory. 

Note) to create graphs, the R statistics language is needed. 




Find more about R:  http://www.r-project.org/
