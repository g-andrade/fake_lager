# fake-lager

[![](https://img.shields.io/hexpm/v/fake-lager.svg?style=flat)](https://hex.pm/packages/fake-lager)
[![](https://github.com/g-andrade/fake-lager/workflows/ci/badge.svg)](https://github.com/g-andrade/fake-lager/actions/workflows/ci.yml)

`fake-lager` is a drop-in replacement for
[`lager`](https://github.com/erlang-lager/lager/) that forwards logs to
[`logger`](http://erlang.org/doc/man/logger.html) instead.

It intends on easing transitions to `logger` on codebases for which a
straightforward move away from `lager` is impractical or unattainable in
the near future.

Basic lager functionality is covered:

- distinct log levels
- lazy evaluation of logging calls
- process and message metadata

Yet to achieve:

- support for pretty printing of records and stacktraces

Likely unattainable:

- multiple sinks
- tracing

**`fake-lager` is experimental** and might not behave as you expect it to.
