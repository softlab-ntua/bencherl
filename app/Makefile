ERLC = 
ERLCF =
 
DIRS = $(wildcard */)

.PHONY: all app clean

all: app

app:
	for d in $(DIRS); do (cd $$d; if test -x configure; then ./configure; fi; $(MAKE) ERLC=$(ERLC) ERLCF=$(ERLCF) all); done

clean: 
	for d in $(DIRS); do (cd $$d && $(MAKE) $@); done

