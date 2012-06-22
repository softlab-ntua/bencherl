override ERLC_OPTS = -I ../../app/rabbitmq-server/plugins-src/rabbitmq-erlang-client/dist 

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

