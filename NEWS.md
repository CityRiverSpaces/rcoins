All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Added

- DOI is added to CITATION.cff and README (badge) [#43](https://github.com/CityRiverSpaces/rcoins/pull/43)
- Contributing guidelines are added to the package [#44](https://github.com/CityRiverSpaces/rcoins/pull/44)
- Added gitHub action to compare stroke to momepy on pull request [#47](https://github.com/CityRiverSpaces/rcoins/pull/47)
- A test on real data is included [#46](https://github.com/CityRiverSpaces/rcoins/pull/46)

## Changed

- Data was first resized to the city boundary (buffer is dropped) [#46](https://github.com/CityRiverSpaces/rcoins/pull/46), then completely removed from the package [#49](https://github.com/CityRiverSpaces/rcoins/pull/49). 
  We now use data packaged in the [CityRiverSpaces/CRiSpData](https://github.com/CityRiverSpaces/CRiSpData) repository.

## Fixed

- Fix rendering of package information on the pkgdown website [#42](https://github.com/CityRiverSpaces/rcoins/pull/42)
- Small fixes to docstrings [#45](https://github.com/CityRiverSpaces/rcoins/pull/45)
- When an initial set of edges is provided, we do not skip  through segments that we have already considered in other strokes [#48](https://github.com/CityRiverSpaces/rcoins/pull/48)
- Remove explicit return statement to comply with linting warning [#50](https://github.com/CityRiverSpaces/rcoins/pull/50)

# Version 0.1.0 - 2024-12-16

## Added

- First release of the package
