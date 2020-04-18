.PHONY: all build run test

all: run

build: test
	rebar3 compile
	#rebar3 release

run: build
	rebar3 shell

test:
	rebar3 dialyzer
	rebar3 eunit
	rebar3 ct

clean:
	rm -rf ./_build
