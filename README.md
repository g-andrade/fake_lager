# fake_lager

[![](https://img.shields.io/hexpm/v/fake_lager.svg?style=flat)](https://hex.pm/packages/fake_lager)
[![](https://github.com/g-andrade/fake_lager/actions/workflows/ci.yml/badge.svg)](https://github.com/g-andrade/fake_lager/actions/workflows/ci.yml)
[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-22%20to%2025-blue)](https://www.erlang.org)

`fake_lager` is a drop-in replacement for
[`lager`](https://github.com/erlang-lager/lager/) that forwards logs to
[`logger`](http://erlang.org/doc/man/logger.html) instead.

It intends on easing transitions to `logger` on codebases for which a
straightforward move away from `lager` is impractical or unattainable in
the near future.

Basic lager functionality is covered:

- distinct log levels
- lazy evaluation of logging calls
- process and message metadata

Extra sinks:

- messages sent to extra sinks will be sent to the default log;
if you wish to send those messages to a separate log, you can
leverage
[extra logger handlers](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#example--add-a-handler-to-log-info-events-to-file)
as well as [filters](https://www.erlang.org/doc/apps/kernel/logger_chapter.html#filters)

Yet to achieve:

- support for pretty printing of records and stacktraces

Likely unattainable:

- tracing

**`fake_lager` is experimental** and might not behave as you expect it to.
