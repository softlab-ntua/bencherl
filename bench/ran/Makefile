ERLC = erlc
ERLC_OPTS =
ERL = erl
ERL_LIB_DIR =

SRCD = src
EBIND = ebin-$(OTP)

ERLF = $(wildcard $(SRCD)/*.erl)
BEAMF = $(patsubst $(SRCD)/%.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: bench clean

bench: $(BEAMF)

$(EBIND)/%.beam: $(SRCD)/%.erl
	$(ERLC) $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

clean:
	$(RM) -rf ebin-*

