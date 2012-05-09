ERLC =
ERLCF = 

DIRS = $(wildcard */)

.PHONY: all bench clean

all: bench

bench:
	for d in $(DIRS); do (cd $$d && $(MAKE) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@);done

clean: 
	for d in $(DIRS); do (cd $$d && $(MAKE) $@); done

