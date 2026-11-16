## Update to version 1.2.2

Prepared on 2026-11-16 as an update to CRAN version 1.2.1 (published 2026-08-21).
The package license and maintainer are unchanged.

This update adds an optional external GTFS validator, UTF-16 import support,
interactive file selection, a historical Barcelona feed, and fixes for parent
stations and frequency-based service calculations. Validator downloads require
an explicit opt-in; examples and tests do not download or run external Java code.

## Verification

Verified on macOS arm64 with R 4.6.2 (2026-10-29), on 2026-11-16:

- R CMD build completed successfully, including all vignettes.
- Installation of the built source tarball completed successfully.
- Tests of the installed package: 344 passed, 0 failures, 0 warnings, 0 skips.
- Help files and dataset compression were verified.
- URLs listed in DESCRIPTION returned HTTP 200.
- The source tarball is approximately 3.8 MiB. The source data files total
  approximately 3.5 MiB and use xz compression.

The test suite does not require downloading the optional Java validator or
opening a browser. Interactive file dialogs are exercised with mocks.

R CMD check has not been rerun for this final source revision, at the
maintainer's request. Run the final CRAN-style check and replace this paragraph
with its actual results before submitting. No Windows or R-devel checks are
claimed here.

