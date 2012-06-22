ERLC_OPTS =

SOURCES=$(wildcard src/*.erl)
BEAMS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(BEAMS)

ebin/%.beam: src/%.erl src/dialyzer.hrl | ebin
	erlc -DVSN="\"42\"" +native -o ebin $<

clean:
	rm -rf ebin

ebin:
	mkdir $@

