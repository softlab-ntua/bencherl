ERLC = 
ERLC_OPTS =
 
DIRS = $(wildcard */)

.PHONY: all app clean

all: app

app:
	for d in $(DIRS); do (cd $$d; if test -x configure; then ./configure; fi; $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) all); done

clean: 
	for d in $(DIRS); do (cd $$d && $(MAKE) $@); done

