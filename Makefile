ERLC = erlc
ERL = erl
ESCRIPT = escript
#ERL_LIB_DIR = /path/to/otp/lib
ERLC_OPTS = +debug_info

.PHONY: all app bench clean clean-app clean-bench clean-res clean-suite suite

# Compile everything.
all: app suite bench

# Compile only the applications.
app:
	@(cd app && $(MAKE) ERL=$(ERL) ERL_LIB_DIR=$(ERL_LIB_DIR) ERLANG_ROOT_DIR=$(ERLANG_ROOT_DIR) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile only the suite.
suite: 
	@(GNUPLOT=`which gnuplot`; \
	if [ -z $$GNUPLOT ]; then \
		echo "where is gnuplot?"; \
		exit 1; \
	fi)
	@(cd suite && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile the benchmarks.
bench: 
	@(cd bench && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Clean up everything.
clean: clean-app clean-bench clean-res clean-suite
	@(cd scratch && $(RM) -rf *)

# Clean up the applications.
clean-app:
	@(cd app && $(MAKE) clean)

# Clean up the benchmarks.
clean-bench:
	@(cd bench && $(MAKE) clean)

# Clean up the results.
clean-res:
	@(cd results && $(RM) -rf *)

# Clean up the suite.
clean-suite:
	@(cd suite && $(MAKE) clean)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

