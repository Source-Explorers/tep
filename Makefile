.PHONY: all build run test

all: run

build:
	rebar3 compile
	rebar3 edoc
	#rebar3 release

run: test
	rebar3 shell

test: build
	rebar3 as test dialyzer
	rebar3 as test eunit
	rebar3 as test ct

clean:
	rm -rf ./_build
