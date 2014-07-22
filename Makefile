ERLC = 
ERLC_OPTS = 

.PHONY: all clean clean-app clean-bench clean-res clean-suite clean-ui ui

# Create the relevant dirs.
directories:
	mkdir -p scratch/

# Make everything. Not the default as maybe not required by everyone.
all: directories ui

# Compile the UI.
ui:
	@(cd ui && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Clean up everything.
clean: clean-app clean-bench clean-suite clean-ui
	@(if test -f scratch; then cd scratch && $(RM) -rf *; fi)

# Clean up the applications.
clean-app:
	for d in $(wildcard app/*/); do (if [ -d $$d ]; then cd $$d && $(MAKE) clean; fi); done

# Clean up the benchmarks.
clean-bench:
	for d in $(wildcard bench/*/); do (if [ -d $$d ]; then cd $$d && $(MAKE) clean; fi); done

# Clean up the results.
clean-all-results:
	@(if [ -d results ]; then $(RM) -rf results/*; fi)

# Clean up the suite.
clean-suite:
	@(cd suite && $(MAKE) clean)

# Clean up the UI.
clean-ui:
	@(cd ui && $(MAKE) clean)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<
