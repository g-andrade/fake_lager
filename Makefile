SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

export ERL_FLAGS = -enable-feature maybe_expr # needed for katana-code under OTP 25

## General Rules

all: compile
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: xref hank-dead-code-cleaner elvis-linter dialyzer
.NOTPARALLEL: check
.PHONY: check

test: eunit ct
.NOTPARALLEL: test
.PHONY: test

## Tests

ct:
	@rebar3 do ct, cover
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

## Checks

xref:
	@rebar3 as test xref
.PHONY: xref

hank-dead-code-cleaner:
	@if rebar3 plugins list | grep '^rebar3_hank\>' >/dev/null; then \
		rebar3 hank; \
	else \
		echo >&2 "WARN: skipping rebar3_hank check"; \
	fi
.PHONY: hank-dead-code-cleaner

elvis-linter:
	@if rebar3 plugins list | grep '^rebar3_lint\>' >/dev/null; then \
		rebar3 lint; \
	else \
		echo >&2 "WARN: skipping rebar3_lint check"; \
	fi
.PHONY: elvis-linter

dialyzer:
	@rebar3 as test dialyzer
.PHONY: dialyzer

## Shell, docs and publication

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as test shell

doc-dry:
	@rebar3 hex build
.PHONY: doc-dry

publish:
publish: doc
	@rebar3 hex publish
