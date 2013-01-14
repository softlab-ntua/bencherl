#ERL_LIB_DIR = /path/to/otp/lib
#ERLC_OPTS = +debug_info

.PHONY: all app bench clean clean-app clean-bench clean-res clean-suite clean-ui suite ui

# Compile everything.
all: directories app suite bench

# Create the relevant dirs:
directories:
	mkdir -p scratch/

# Compile only the applications.
app:
	@(cd app && $(MAKE) ERL_LIB_DIR=$(ERL_LIB_DIR) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile only the suite.
suite: 
	@(GNUPLOT=`which gnuplot`; \
	if [ -z $$GNUPLOT ]; then \
		echo "where is gnuplot?"; \
		exit 1; \
	fi)
	@(cd suite && $(MAKE) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile the UI.
ui:
	@(cd ui && $(MAKE) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile the benchmarks.
bench: 
	@(cd bench && $(MAKE) ERLC_OPTS=$(ERLC_OPTS) $@)

# Clean up everything.
clean: clean-app clean-bench clean-res clean-suite clean-ui
	@(if test -f scratch; then cd scratch && $(RM) -rf *; fi)

# Clean up the applications.
clean-app:
	@(cd app && $(MAKE) clean)

# Clean up the benchmarks.
clean-bench:
	@(cd bench && $(MAKE) clean)

# Clean up the results.
clean-res:
	@(if [ -f results ]; then \
		cd results; \
		$(RM) -rf *; \
		exit 2; \
	fi)

# Clean up the suite.
clean-suite:
	@(cd suite && $(MAKE) clean)

# Clean up the UI.
clean-ui:
	@(cd ui && $(MAKE) clean)

%.beam: %.erl
	erlc $(ERLC_OPTS) $<
