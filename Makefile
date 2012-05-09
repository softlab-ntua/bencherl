ERLC = erlc 
ERLCF = +debug_info

ERLF = $(wildcard *.erl)
BEAMF = $(subst .erl,.beam,$(ERLF))

.PHONY: all app bench clean suite clean-res

# Compile everything.
all: app suite bench

# Compile only the applications.
app:
	@(cd app && $(MAKE) ERLC=$(ERLC) ERLCF=$(ERLCF) $@)

# Compile only the suite.
suite: $(BEAMF)
	@(cd suite && $(MAKE) ERLC=$(ERLC) ERLCF=$(ERLCF) $@)

# Compile both the suite and the benchmarks.
bench: suite
	@(cd bench && $(MAKE) ERLC=$(ERLC) ERLCF=$(ERLCF) $@)

# Clean up everything.
clean:
	$(RM) $(BEAMF)
	@(cd bench && $(MAKE) $@)
	@(cd app && $(MAKE) $@)
	@(cd results && $(RM) -rf *)
	@(cd scratch && $(RM) -rf *)

# Clean up the results.
clean-res:
	@(cd results && $(RM) -rf *)

%.beam: %.erl
	$(ERLC) $(ERLCF) $<

