ERLC = erlc 
ERLC_OPTS = +debug_info

.PHONY: all app bench clean suite clean-res

# Compile everything.
all: app suite bench

# Compile only the applications.
app:
	@(cd app && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile only the suite.
suite: 
	@(cd suite && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Compile both the suite and the benchmarks.
bench: suite
	@(cd bench && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

# Clean up everything.
clean:
	@(cd bench && $(MAKE) $@)
	@(cd app && $(MAKE) $@)
	@(cd suite && $(MAKE) $@)
	@(cd results && $(RM) -rf *)
	@(cd scratch && $(RM) -rf *)

# Clean up the results.
clean-res:
	@(cd results && $(RM) -rf *)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

