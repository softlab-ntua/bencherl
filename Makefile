ERLC = ~/otps/otp_src_R15B01/bin/erlc
#ERLC = erlc
ERL = ~/otps/otp_src_R15B01/bin/erl
ERL_LIB_DIR = ~/otps/otp_src_R15B01/lib
ERLC_OPTS = +debug_info

.PHONY: all app bench clean suite clean-res

# Compile everything.
all: app suite bench

# Compile only the applications.
app:
	@(cd app && $(MAKE) ERL=$(ERL) ERL_LIB_DIR=$(ERL_LIB_DIR) ERLANG_ROOT_DIR=$(ERLANG_ROOT_DIR) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@)

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

