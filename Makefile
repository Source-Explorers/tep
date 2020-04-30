.PHONY: all build run test

all: run

build:
	rebar3 compile
	rebar3 edoc
	#rebar3 release

run: test
	rebar3 shell

test: build
	rebar3 dialyzer
	rebar3 eunit
	rebar3 ct

clean:
	rm -rf ./_build
