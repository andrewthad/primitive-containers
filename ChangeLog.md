# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.4.0] - 2018-??-??
### Added
- Non-empty unlifted sets. Currently, the API is similar to the API for Sets.
  However, functions that would be partial are removed (intersection,
  difference, fromList), and functions that would be constant are removed (null).
  There is no `Monoid` instance and no `IsList` instance.

