ERLC = 
ERLC_OPTS =
 
DIRS = $(wildcard */)

.PHONY: all app clean

all: app

app:
for d in $(DIRS); do (cd $$d; if test -x configure; then ./configure; fi; $(MAKE) ERL=$(ERL) ERL_LIB_DIR=$(ERL_LIB_DIR) ERLANG_ROOT_DIR=$(ERLANG_ROOT_DIR) ERLC=$(ERLC) ERLC_OPTS=$(ERLC_OPTS) DIALYZER=$(DIALYZER) ESCRIPT=$(ESCRIPT)  all); done

clean: 
	for d in $(DIRS); do (cd $$d && $(MAKE) $@); done

