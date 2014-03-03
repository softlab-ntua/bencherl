ERLC_OPTS =

EBIND = ebin
UI_BIN = bencherlui/ebin/bencherlui.app

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: clean ui

ui: $(UI_BIN) $(BEAMF)

$(EBIND)/%.beam: %.erl
	erlc $(ERLC_OPTS) -o$(EBIND) $<

$(BEAMF): | $(EBIND)

$(EBIND):
	mkdir -p $(EBIND)

$(UI_BIN):
	if [ ! -d "ChicagoBoss" ]; then \
		echo "Downloading Chicago Boss" ; \
		git clone https://github.com/ChicagoBoss/ChicagoBoss.git ; \
		cd ChicagoBoss ; \
		git checkout bbe661ccef900fe12f4c437b6fc0a254ce1ba640 ; \
		./rebar get-deps ; \
		./rebar compile ; \
	fi
	rm -rf bencherlui/boss.config
	cp bencherlui/boss.config.in bencherlui/boss.config
	sed -i -e "s@BENCHERL_UI_PATH@`pwd`@g" bencherlui/boss.config
	cd bencherlui ; \
	./rebar compile

clean:
	$(RM) -rf $(EBIND)
	cd bencherlui ; \
	./rebar clean; \
	rm -rf boss.config;
