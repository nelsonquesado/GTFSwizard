# GTFSwizard 1.2.2 release verification

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

## Earlier independent check configuration

```sh
R CMD build <package-source-directory>
env NOT_CRAN=true _R_CHECK_CRAN_INCOMING_=false \
  _R_CHECK_CRAN_INCOMING_REMOTE_=false _R_CHECK_SYSTEM_CLOCK_=false \
  _R_CHECK_FORCE_SUGGESTS_=true \
  R CMD check --as-cran --run-donttest GTFSwizard_1.2.2.tar.gz
```

The checked archive and full check logs are available in the package working
folder as `GTFSwizard_1.2.2.tar.gz` and `GTFSwizard.Rcheck/`.
The local documentation in `docs/` is ready for the existing GitHub Pages
workflow, which builds and deploys on a push to `main`.

Direct app access to the other project conversations failed. Relevant earlier
requirements were recovered through personal-context retrieval.
