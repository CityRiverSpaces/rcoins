All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Added

- DOI is added to CITATION.cff and README (badge)
- Contributing guidelines are added to the package
- Added gitHub action to compare stroke to momepy on pull request
- A test on real data is included

## Changed

- Data was first resized to the city boundary (buffer is dropped), then completely removed from the package. We now use data packaged in the [CityRiverSpaces/CRiSpData](https://github.com/CityRiverSpaces/CRiSpData) repository ([#49](https://github.com/CityRiverSpaces/rcoins/pull/49))

## Fixed

- Fix rendering of package information on the pkgdown website
- Small fixes to docstrings

# Version 0.1.0 - 2024-12-16

## Added

- First release of the package
