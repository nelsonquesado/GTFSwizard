## GTFSwizard 1.2.2

This update adds spatial selection, auditable feed repairs, scenario comparisons,
and consistent trip and segment distance calculations. `tidy_raptor()` prepares
routing data internally. Additional changes include optional MobilityData
validation, UTF-16 import support, an AMB Barcelona example feed, and fixes for
frequency-based services and calendar handling.

The license and maintainer are unchanged.

## Test environment

macOS Tahoe 26.6.2, arm64, R 4.6.2 (2026-10-29).
Pandoc 3.10 and HTML Tidy 5.8.0 were configured for the latest maintainer check.

## R CMD check results

The maintainer ran `devtools::check(args = "--as-cran", run_dont_test = TRUE)`
on 2026-11-17. The supplied console log reports successful vignette creation
and **0 errors, 0 warnings and 0 notes**.

That run used `NOT_CRAN=true`, disabled CRAN incoming checks, and set
`_R_CHECK_FORCE_SUGGESTS_=FALSE`. No Windows, Linux or R-devel check is claimed.

An earlier independent archive check required all suggested packages and passed
425 assertions with 0 failures, 0 warnings and 0 skips. Its sole environment
note concerned the older HTML Tidy available before the maintainer's update.
The complete verification history is retained in the development record.

## Documentation

The README, NEWS, reference documentation and vignettes describe the current
API. `prepare_raptor()` is internal. All five vignettes were built successfully.
The pkgdown site was rebuilt and its 73 HTML pages passed local-link checks.
