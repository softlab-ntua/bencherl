ERLC_OPTS =

EBIND = ebin
UI_BIN = ui/bencherlui/ebin/bencherlui.app

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: all clean suite

all: suite

suite: $(UI_BIN) $(BEAMF)

$(EBIND)/%.beam: %.erl
	erlc $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

$(UI_BIN):
	cd ui/ChicagoBoss-0.8.0 ; \
	echo "Building Chicago Boss" ; \
	./rebar get-deps compile; \
	make
	cd ui/bencherlui ; \
	echo "Building UI" ; \
	./rebar compile

clean:
	$(RM) -rf $(EBIND)
	cd ui/bencherlui ; \
	echo "Cleaning UI" ; \
	./rebar clean

