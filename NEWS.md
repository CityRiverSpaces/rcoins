All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Changed

- Removed dependency from CRiSpData, a function is added to retrieve the same dataset from a 4TU.ResearchData repository [#64](https://github.com/CityRiverSpaces/rcoins/pull/64)
- Remove GitHub action to update codemeta file - quite buggy, passing when it should not [#65](https://github.com/CityRiverSpaces/rcoins/pull/65)

## Fixed

- Fixes so that tests can run and vignettes built even if CRiSpData is not available [#59](https://github.com/CityRiverSpaces/rcoins/pull/59)
- Other minor fixes, most notably the minimum R version set to 4.1 [#60](https://github.com/CityRiverSpaces/rcoins/pull/60)
- Fix for typo that prevents parsing of the CFF file [#61](https://github.com/CityRiverSpaces/rcoins/pull/61)
- Fixed the changelog (this file), moving commits after the 0.2.0 tag to the new release [#62](https://github.com/CityRiverSpaces/rcoins/pull/62)

# Version 0.2.0 - 2025-04-03

## Added

- DOI is added to CITATION.cff and README (badge) [#43](https://github.com/CityRiverSpaces/rcoins/pull/43)
- Contributing guidelines are added to the package [#44](https://github.com/CityRiverSpaces/rcoins/pull/44)
- Added gitHub action to compare stroke to momepy on pull request [#47](https://github.com/CityRiverSpaces/rcoins/pull/47)
- A test on real data is included [#46](https://github.com/CityRiverSpaces/rcoins/pull/46), then made more robust [#52](https://github.com/CityRiverSpaces/rcoins/pull/52)
- Added codemeta.json file, together with a GitHub action to keep it updated [#55](https://github.com/CityRiverSpaces/rcoins/pull/55)

## Changed

- Data was first resized to the city boundary (buffer is dropped) [#46](https://github.com/CityRiverSpaces/rcoins/pull/46), then completely removed from the package [#49](https://github.com/CityRiverSpaces/rcoins/pull/49).
  We now use data packaged in the CityRiverSpaces/CRiSpData repository.
- Bumped version, author field adjusted [#57](https://github.com/CityRiverSpaces/rcoins/pull/57)

## Fixed

- Fix rendering of package information on the pkgdown website [#42](https://github.com/CityRiverSpaces/rcoins/pull/42)
- Small fixes to docstrings [#45](https://github.com/CityRiverSpaces/rcoins/pull/45)
- When an initial set of edges is provided, we do not skip  through segments that we have already considered in other strokes [#48](https://github.com/CityRiverSpaces/rcoins/pull/48)
- Remove explicit return statement to comply with linting warning [#50](https://github.com/CityRiverSpaces/rcoins/pull/50)
- Overall improvements of metadata files and documentation in preparation to CRAN submission [#52](https://github.com/CityRiverSpaces/rcoins/pull/52)
- Typo fixed in README [#53](https://github.com/CityRiverSpaces/rcoins/pull/53)
- Fix for the GitHub action introduced in [#55](https://github.com/CityRiverSpaces/rcoins/pull/56) [#56](https://github.com/CityRiverSpaces/rcoins/pull/56) [#58](https://github.com/CityRiverSpaces/rcoins/pull/58)

# Version 0.1.0 - 2024-12-16

## Added

- First release of the package
