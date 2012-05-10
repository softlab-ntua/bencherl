## orbit-int Makefile
##
## Author: Patrick Maier <P.Maier@hw.ac.uk>
##

ERLC=
ERLC_OPTS=

ERLFILES=$(wildcard src/*.erl)
BEAMFILES=$(ERLFILES:src/%.erl=ebin/%.beam)

.DELETE_ON_ERROR:

.PHONY: all clean emacsclean objclean distclean

all: $(BEAMFILES)

ebin/%.beam: src/%.erl | ebin
	$(ERLC) $(ERLC_OPTS) -o ebin $<

emacsclean:
	$(RM) -f *~

clean: objclean

objclean:
	$(RM) -rf ebin

distclean: objclean emacsclean

ebin:
	mkdir $@

