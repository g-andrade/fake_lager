SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

## General Rules

all: compile doc
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: xref dialyzer
.NOTPARALLEL: check
.PHONY: check

test: eunit ct
.NOTPARALLEL: test
.PHONY: test

## Tests

ct:
	@rebar3 ct
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

## Checks

dialyzer:
	@rebar3 as test dialyzer
.PHONY: dialyzer

xref:
	@rebar3 as test xref
.PHONY: xref

## Shell, docs and publication

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as test shell

doc:
	./support/scripts/generate_docs.sh

publish:
publish: doc
	@rebar3 hex publish
