ERLC =
ERLC_OPTS = 

DIRS = $(wildcard */)

.PHONY: all bench clean

all: bench

bench:
	for d in $(DIRS); do (cd $$d && $(MAKE) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) $@);done

clean: 
	for d in $(DIRS); do (cd $$d && $(MAKE) $@); done

