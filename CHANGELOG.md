# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

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
