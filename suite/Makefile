ERLC =
ERLC_OPTS =

EBIND = ebin

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: all clean suite

all: suite

suite: $(BEAMF)

$(EBIND)/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

clean:
	$(RM) -rf $(EBIND)

