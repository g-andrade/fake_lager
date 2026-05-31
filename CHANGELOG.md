# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- OTP 28.5 to CI
- OTP 29.0 to CI

### Changed

- logging calls to gate evaluation on a compile-time `logger:allow/2` level
check (mirroring `logger`'s own `?LOG_*` macros and lager's original inlined
level check), replacing the previous heuristic that conditionally wrapped
"expensive" arguments in a fun
- log format and argument expressions to be evaluated only when the log level
is enabled, restoring lager's original semantics; previously, arguments deemed
inexpensive were evaluated unconditionally. As a result, **the evaluation of a
log argument — including any side effects and exceptions it may raise — now
occurs only when the level is enabled; a previously-unconditional crash in a
log argument will be masked while that level is disabled**
- parse transform to walk the AST through `erl_syntax_lib` rather than by
blindly decomposing tuples, hardening the rewrite across all expression shapes

## [1.0.1] - 2026-04-06

### Added

- OTP 25.3 to CI
- OTP 26.2 to CI
- OTP 27.3 to CI
- OTP 28.4 to CI

### Changed

- CI to use latest rebar3 version that's compatible with each covered OTP release
- CI from Ubuntu 20.04 to 22.04
- oldest OTP version supported from 22 to 24

### Fixed

- documentation links to source code

## [1.0.0] - 2023-02-27

### Added

- OTP 25.2 to CI
- unused code check (Hank)
- code linter (Elvis)

### Changed

- doc generation to `rebar3_ex_doc` plugin

## [0.5.0] - 2022-12-06

### Added

- support for pretty printing of records as tagged maps
(with `lager:pr/2` and `lager:pr/3`)

## [0.4.0] - 2022-11-29

### Added

- name of custom lager sink to event metadata
- OTP 25.1 to CI

## [0.3.0] - 2022-08-05

### Changed

- API calls to `:log`, `:do_log` and `:dispatch_log` to output logger `mfa` metadata as expected

## [0.2.1] - 2022-06-15

### Fixed

- parse transform crash upon encountering 1, 2 or 3-arity non-logging calls to the `lager` module
[Guilherme Andrade]

## [0.2.0] - 2022-05-23

### Added

- logging of lager messages sent to extra sinks

### Removed

- parse transform failure upon encountering `lager_extra_sinks`

### Fixed

- dispatching of bad `file` attribute in messages' metadata

## [0.1.0] - 2021-11-25

### Added

- `lager_transform` that routes events to `logger` instead
