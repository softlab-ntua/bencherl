ERLC_OPTS =

EBIND = ebin
UI_BIN = bencherlui/ebin/bencherlui.app

ERLF = $(wildcard *.erl)
BEAMF = $(patsubst %.erl,$(EBIND)/%.beam,$(ERLF))

.PHONY: clean ui

ui: $(UI_BIN) $(BEAMF)
	cd bencherlui ; \
	./rebar compile

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
		git checkout bb6509dd4aa5be8e0445fe160317b655dc1625ee ; \
		./rebar get-deps ; \
		./rebar compile ; \
	fi
	rm -rf bencherlui/boss.config ; \
	cp bencherlui/boss.config.in bencherlui/boss.config ; \
	sed -i -e "s@BENCHERL_UI_PATH@`pwd`@g" bencherlui/boss.config ; \
	NUMBER=`awk -vmin=0 -vmax=1000000000 'BEGIN{srand(); print int(min+rand()*(max-min+1))}'` ; \
	ALPHA_NUMBERIC_PATH=$$(echo `pwd` | sed 's/[^a-zA-Z0-9]//g') ; \
	NODE_NAME="$$ALPHA_NUMBERIC_PATH""$$NUMBER" ; \
	sed -i -e 's@VM_SHORT_NAME@'"$$NODE_NAME"'@g' bencherlui/boss.config ; \
	sed -i -e 's@VM_NAME@'"$$NODE_NAME"'@g' bencherlui/boss.config ; \
	cd bencherlui ; \
	./rebar compile

clean:
	$(RM) -rf $(EBIND)
	cd bencherlui ; \
	./rebar clean; \
	rm -rf boss.config;
