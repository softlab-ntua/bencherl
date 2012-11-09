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
	echo "Building Chicago Boss" ; \
	cd ui ; \
	rm -rf ChicagoBoss-0.8.0 ; \
	tar xvzf ChicagoBoss-0.8.0.tar.gz
	cd ui/ChicagoBoss-0.8.0 ; \
	make
	sed -i -e "s@BENCHERL_SUITE_PATH@`pwd`@g" ui/bencherlui/boss.config
	cd ui/bencherlui ; \
	echo "Building UI" ; \
	./rebar compile

clean:
	$(RM) -rf $(EBIND)
	cd ui/bencherlui ; \
	echo "Cleaning UI" ; \
	./rebar clean

