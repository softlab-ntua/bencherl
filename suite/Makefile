ERLC =
ERLCF =

EBIND = ebin

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: all clean suite

all: suite

suite: $(BEAMF)

$(EBIND)/%.beam: %.erl
	$(ERLC) $(ERLCF) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

clean:
	$(RM) -rf $(EBIND)

