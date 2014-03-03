ERLC = erlc
ERLC_OPTS =
ERL = erl
ERL_LIB_DIR = 

EBIND = ebin-${OTP}

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: clean suite

suite: $(BEAMF)

$(EBIND)/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

clean:
	$(RM) -rf ebin-*
