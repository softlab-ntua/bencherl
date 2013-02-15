
all: deps compile
	./rebar skip_deps=true escriptize

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	@./rebar clean
	
results: compile
	priv/summary.r -i tests/current
