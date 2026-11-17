## Update to version 1.2.2

Prepared on 2026-11-16 as an update to CRAN version 1.2.1 (published 2026-08-21).
The package license and maintainer are unchanged.

This update adds an optional external GTFS validator, UTF-16 import support,
interactive file selection, a historical AMB Barcelona feed, and fixes for parent
stations and frequency-based service calculations. Validator downloads require
an explicit opt-in; examples and tests do not download or run external Java code.

## Verification

Verified on macOS arm64 with R 4.6.2 (2026-10-29), on 2026-11-17:

- R CMD build completed successfully, including all vignettes.
- Installation of the built source tarball completed successfully.
- Latest full source test suite: 395 passed, 0 failures, 0 warnings, 0 skips.
- All 46 help files passed Rd validation; code references and dataset compression
  were verified.
- The pkgdown website and all vignettes were rebuilt successfully.
- The AMB dataset retains its full calendar, 2017-01-27 through 2026-12-31.
- The source archive excludes raw ZIP files and development artifacts.
- Source tarball: 3,598,118 bytes; installed data: 5,514,243 bytes.

The test suite does not require downloading the optional Java validator or
opening a browser. Interactive file dialogs are exercised with mocks.

The maintainer ran `devtools::check(remote = TRUE, force_suggests = TRUE,
run_dont_test = TRUE)` on macOS Tahoe 26.6.2 (aarch64-apple-darwin23), with
R 4.6.2, on 2026-11-17. The supplied log reports `--run-donttest --no-manual
--as-cran`: 0 errors, 0 warnings, and 0 notes, including CRAN incoming
feasibility, examples, tests, and vignette rebuilding. A subsequent
`devtools::test()` reports 393 passes, 0 failures, 0 warnings, and 0 skips.
No Windows or R-devel checks are claimed here.

Since that check, the only code change wraps two simulated validation reports
in `expect_output()` inside the tests. This prevents confusing console output
and adds two assertions; no package functions were changed. The full source
test suite was rerun successfully with 395 passes, and the source tarball was
rebuilt including all vignettes. R CMD check was not rerun after this test-only
adjustment, at the maintainer's request.

