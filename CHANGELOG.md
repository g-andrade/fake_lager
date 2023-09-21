# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- OTP 25.3 to CI
- OTP 26.0 to CI
- OTP 26.1 to CI

### Changed

- CI to use latest rebar3 version that's compatible with each covered OTP release

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
