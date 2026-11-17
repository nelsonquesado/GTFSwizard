## GTFSwizard 1.2.2

This update adds spatial selection, auditable feed repairs, scenario comparisons,
and consistent distance and speed calculations. Routing prepares service dates
and frequency-based trips internally. The public routing entry point is
`tidy_raptor()`. Other changes include optional MobilityData validation, UTF-16
import, the AMB Barcelona example feed, and fixes for frequency and calendar
handling. The license and maintainer are unchanged.

## Latest maintainer check

On 2026-11-17 the maintainer ran the following from R.app on macOS arm64
with R 4.6.2, Pandoc 3.10 and HTML Tidy 5.8.0 configured:

```r
Sys.setenv(RSTUDIO_PANDOC = "/opt/homebrew/bin",
           R_TIDYCMD = "/opt/homebrew/bin/tidy")
devtools::check(pkg = "path/to/GTFSwizard", args = "--as-cran",
                run_dont_test = TRUE)
```

The supplied console log reports successful vignette creation and a final
result of **0 errors, 0 warnings and 0 notes**, in 3 minutes 13 seconds.
This run used `NOT_CRAN=true`, disabled both CRAN incoming checks and set
`_R_CHECK_FORCE_SUGGESTS_=FALSE`. Its console summary does not provide an
assertion count. The count below belongs to the earlier independent check.

## Earlier independent archive verification

Verified on 2026-11-17 with R 4.6.2, macOS Tahoe 26.6.2, arm64.

- `R CMD build` completed with all five rendered vignettes.
- The source archive passed `R CMD check --as-cran --run-donttest` with
  0 errors, 0 warnings and 1 environment-related note.
- Tests ran with `NOT_CRAN=true`: 425 passes, 0 failures, 0 warnings, 0 skips.
- Static code analysis, S3 consistency, namespace loading, dataset checks,
  Rd validation, examples, vignette rebuilding and the PDF manual passed.
- The note reports that macOS HTML Tidy (2006) is too old for HTML-manual
  validation. The check skipped that validation. This predates the maintainer installing HTML Tidy 5.8.0.
- Network-dependent CRAN incoming checks and system-clock verification were
  disabled for this local run. Dependencies were checked with all suggested
  packages required. No Windows, Linux or R-devel result is claimed.
- The pkgdown reference index passed. The site rebuilt successfully, with
  73 HTML pages and no missing relative link targets.
- Routing matched the previous implementation in 32 comparisons, covering
  scheduled and frequency-based service, two dates, both routing directions,
  and all four journey-retention modes.
- Fresh source archive contents match the current R, man, vignettes and tests
  files. The archive excludes raw feed ZIP files and development output.

Earlier project decisions on service-pattern definitions, dataset documentation,
licensing, citation and generated S3 exports were included in this verification.

## Earlier verified archive

`GTFSwizard_1.2.2.tar.gz`

SHA-256: 4a4debda637044dacf3a558c338dc661f614463c45a2a39e39c9aa6b84642cb2
