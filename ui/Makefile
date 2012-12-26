ERLC_OPTS =

EBIND = ebin
UI_BIN = bencherlui/ebin/bencherlui.app

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: all clean suite

ui: $(UI_BIN) $(BEAMF)

$(EBIND)/%.beam: %.erl
	erlc $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

$(UI_BIN):
	echo "Building Chicago Boss" ; \
	rm -rf ChicagoBoss-0.8.0 ; \
	tar xvzf ChicagoBoss-0.8.0.tar.gz
	cd ChicagoBoss-0.8.0 ; \
	make
	rm -rf bencherlui/boss.config
	cp bencherlui/boss.config.in bencherlui/boss.config
	sed -i -e "s@BENCHERL_UI_PATH@`pwd`@g" bencherlui/boss.config
	cd bencherlui ; \
	echo "Building UI" ; \
	./rebar compile

clean:
	$(RM) -rf $(EBIND)
	cd bencherlui ; \
	echo "Cleaning UI" ; \
	./rebar clean; \
	rm -rf boss.config;
