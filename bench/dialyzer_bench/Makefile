ERLC_OPTS = 

SRCD = src
EBIND = ebin

ERLF = $(wildcard $(SRCD)/*.erl)
BEAMF = $(patsubst $(SRCD)/%.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: all bench clean

all: bench

bench: $(BEAMF)

$(EBIND)/%.beam: $(SRCD)/%.erl
	erlc $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

clean:
	$(RM) -rf $(EBIND)

