#' Validate a GTFS Schedule Feed
#'
#' Run the MobilityData Canonical GTFS Schedule Validator on a ZIP archive or
#' a GTFS object and return its findings. The validator checks the official
#' GTFS Schedule specification and produces additional best-practice notices.
#'
#' @param gtfs Path to a local GTFS ZIP archive or a named list of GTFS tables,
#'   including a `wizardgtfs` object.
#' @param validator_jar Optional path to a MobilityData validator CLI JAR.
#'   When `NULL`, use the path in `GTFS_VALIDATOR_JAR`. The JAR must support
#'   `--skip_validator_update`.
#' @param output_dir Optional directory in which to keep the validator's
#'   reports. It must not exist or must be empty. When `NULL`, temporary files
#'   are removed after validation and no reports are saved.
#' @param quiet Logical. If `FALSE` (the default), print a summary of the
#'   findings. Set to `TRUE` to return the result without printing.
#' @param download_validator Logical. If `TRUE` and no JAR path is supplied,
#'   download the official validator to the user's cache if necessary and reuse
#'   it on subsequent calls. Requires internet access on the first download
#'   and the suggested package `openssl` for SHA-256 verification.
#'
#' @return A list with class `gtfs_validation`: `notices`, a data frame with
#'   rule codes, severities, occurrence counts and sample details;
#'   `system_notices`, a data frame of system errors; `report` and
#'   `system_errors`, the parsed JSON reports; `has_errors`, `TRUE` or `FALSE`
#'   when validation completed with recognized severities and without system
#'   errors, otherwise `NA`; and
#'   `report_path`, `system_errors_path`, `html_path`, and `output_dir`, which
#'   are `NULL` when reports are not saved. The result is returned invisibly.
#'
#' @details
#' Requires Java 17 or newer and the suggested package `jsonlite`. Supply a
#' MobilityData CLI JAR from <https://github.com/MobilityData/gtfs-validator/releases>
#' (version 8.0.1 or newer recommended), or explicitly set
#' `download_validator = TRUE` to download version 8.0.1. The cached download's
#' SHA-256 is checked against the published release checksum before each use.
#' The ZIP is passed directly to the validator without importing or repairing
#' it. An object is first exported with [write_gtfs()]; its report describes the
#' exported representation, not the original file or any previously repaired
#' data. No data are uploaded by this function. Results depend on the validator
#' version and its implemented rules; no errors found is not a certification.
#'
#' The output directory must be empty to avoid overwriting existing reports or
#' confusing a failed execution with a previous successful one.
#'
#' @examples
#' # Requires Java 17 or newer, jsonlite and openssl, and initially internet:
#' \dontrun{
#' result <- validate_gtfs(for_rail_gtfs, download_validator = TRUE)
#' result$notices
#' # Supply output_dir to keep the HTML and JSON reports:
#' result <- validate_gtfs(for_rail_gtfs, output_dir = "validation-report",
#'                         download_validator = TRUE)
#' }
#'
#' @seealso [read_gtfs()], [write_gtfs()],
#'   <https://gtfs.org/getting-started/validate/>
#' @md
#' @export
validate_gtfs <- function(gtfs, validator_jar = NULL, output_dir = NULL,
                          quiet = FALSE, download_validator = FALSE){
  require_pkg("jsonlite", "validating GTFS feeds")
  gw_assert_flag(quiet, "quiet")
  gw_assert_flag(download_validator, "download_validator")
  java <- find_gtfs_validator_java()
  if(!nzchar(java)){
    gw_stop("Java 17 or newer is required to run the GTFS validator.")
  }
  if(is.null(validator_jar)){
    validator_jar <- Sys.getenv("GTFS_VALIDATOR_JAR")
    if(!nzchar(validator_jar) && download_validator){
      validator_jar <- download_gtfs_validator(quiet = quiet)
    }
  }
  if(!is.character(validator_jar) || length(validator_jar) != 1L ||
     is.na(validator_jar) || !nzchar(validator_jar) ||
     !file.exists(validator_jar) || dir.exists(validator_jar)){
    gw_stop("the MobilityData validator JAR was not found; specify `validator_jar` ",
            "or set `GTFS_VALIDATOR_JAR`, or use `download_validator = TRUE`.")
  }
  save_reports <- !is.null(output_dir)
  if(!save_reports){
    output_dir <- tempfile("gtfs-validation-")
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  }
  if(!is.character(output_dir) || length(output_dir) != 1L ||
     is.na(output_dir) || !nzchar(output_dir)){
    gw_stop("`output_dir` must be one directory path.")
  }
  if(file.exists(output_dir) && !dir.exists(output_dir)){
    gw_stop("`output_dir` must be a directory.")
  }
  if(dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE,
                                                no.. = TRUE))){
    gw_stop("`output_dir` must be empty to avoid overwriting existing files.")
  }

  if(is.character(gtfs) && length(gtfs) == 1L && !is.na(gtfs) &&
     nzchar(gtfs) && file.exists(gtfs) && !dir.exists(gtfs)){
    input <- normalizePath(gtfs, mustWork = TRUE)
  } else if(is.list(gtfs) && !is.null(names(gtfs))){
    input <- tempfile("gtfs-validation-input-", fileext = ".zip")
    on.exit(unlink(input), add = TRUE)
    write_gtfs(gtfs, input)
    input <- normalizePath(input, mustWork = TRUE)
  } else {
    gw_stop("`gtfs` must be an existing GTFS ZIP path or a named list of tables.")
  }
  if(!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)){
    gw_stop("could not create `output_dir`.")
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  validator_jar <- normalizePath(validator_jar, mustWork = TRUE)
  process <- run_gtfs_validator_cli(java, validator_jar, input, output_dir)
  report_path <- file.path(output_dir, "report.json")
  system_path <- file.path(output_dir, "system_errors.json")
  if(!file.exists(report_path) || !file.exists(system_path)){
    gw_stop("validator did not produce both JSON reports (exit status ",
            process$status, "). Output: ", paste(tail(process$output, 5L), collapse = " "))
  }
  report <- read_gtfs_validation_json(report_path)
  system_errors <- read_gtfs_validation_json(system_path)
  notices <- gtfs_validation_notices(report)
  system_notices <- gtfs_validation_notices(system_errors)
  if(process$status != 0L && !nrow(system_notices)){
    gw_stop("validator exited with status ", process$status,
            if(save_reports) paste0("; inspect reports in `", output_dir, "`.")
            else paste0(". Output: ", paste(tail(process$output, 5L), collapse = " ")))
  }
  unknown_severity <- any(!notices$severity %in% c("ERROR", "WARNING", "INFO"))
  has_errors <- if(nrow(system_notices) || unknown_severity) NA else {
    any(notices$severity == "ERROR")
  }
  html_path <- file.path(output_dir, "report.html")
  result <- structure(list(
    notices = notices, system_notices = system_notices,
    report = report, system_errors = system_errors,
    has_errors = has_errors, report_path = if(save_reports) report_path else NULL,
    system_errors_path = if(save_reports) system_path else NULL,
    html_path = if(save_reports && file.exists(html_path)) html_path else NULL,
    output_dir = if(save_reports) output_dir else NULL
  ), class = "gtfs_validation")
  if(!quiet){
    print(result)
  }
  invisible(result)
}

#' @export
print.gtfs_validation <- function(x, ...){
  cat("<gtfs_validation>\n")
  cat("  Status:", if(is.na(x$has_errors)) "incomplete or unknown" else if(x$has_errors)
    "errors found" else "no errors found", "\n")
  if(nrow(x$notices)){
    print(x$notices[c("severity", "code", "total_notices")], row.names = FALSE)
  } else {
    cat("  No notices reported.\n")
  }
  if(nrow(x$system_notices)){
    cat("  Validator system errors:\n")
    print(x$system_notices[c("severity", "code", "total_notices")], row.names = FALSE)
  }
  if(!is.null(x$output_dir)) cat("  Reports:", x$output_dir, "\n")
  invisible(x)
}

find_gtfs_validator_java <- function(){
  Sys.which("java")
}

download_gtfs_validator <- function(cache_dir = tools::R_user_dir("GTFSwizard", "cache"),
                                    quiet = FALSE){
  require_pkg("openssl", "verifying the downloaded GTFS validator")
  expected <- "19293ddd9b6f954f216d4f12054bd8a3232921751c4484339e339764a91000e2"
  name <- "gtfs-validator-8.0.1-cli.jar"
  url <- paste0("https://github.com/MobilityData/gtfs-validator/releases/download/v8.0.1/", name)
  if(!dir.exists(cache_dir) && !dir.create(cache_dir, recursive = TRUE)){
    gw_stop("could not create the validator cache directory.")
  }
  path <- file.path(cache_dir, name)
  if(file.exists(path)){
    if(dir.exists(path) || !identical(gtfs_validator_checksum(path), expected)){
      gw_stop("cached validator failed SHA-256 verification; remove `", path, "` and retry.")
    }
    return(path)
  }
  tmp <- tempfile("gtfs-validator-download-", tmpdir = cache_dir)
  on.exit(unlink(tmp), add = TRUE)
  if(!quiet) message("GTFSwizard: downloading the MobilityData validator (about 38 MB).")
  status <- tryCatch(fetch_gtfs_validator(url, tmp, quiet),
                     error = function(e) gw_stop("validator download failed: ", conditionMessage(e)))
  if(!identical(status, 0L) || !file.exists(tmp) || !identical(gtfs_validator_checksum(tmp), expected)){
    gw_stop("validator download failed or did not pass SHA-256 verification.")
  }
  if(file.exists(path) || !file.rename(tmp, path)){
    gw_stop("could not save the verified validator to the cache; retry the operation.")
  }
  path
}

fetch_gtfs_validator <- function(url, path, quiet){
  utils::download.file(url, path, mode = "wb", quiet = quiet)
}

gtfs_validator_checksum <- function(path){
  con <- file(path, "rb")
  on.exit(close(con))
  unclass(as.character(openssl::sha256(con)))
}

run_gtfs_validator_cli <- function(java, validator_jar, input, output_dir){
  output <- suppressWarnings(system2(
    java, c("-jar", shQuote(validator_jar), "-i", shQuote(input),
            "-o", shQuote(output_dir), "--skip_validator_update", "--threads", "2"),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  list(status = if(is.null(status)) 0L else as.integer(status), output = output)
}

read_gtfs_validation_json <- function(path){
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e){
    gw_stop("could not read validator report `", path, "`: ", conditionMessage(e))
  })
}

gtfs_validation_notices <- function(report){
  if(!is.list(report) || is.null(report$notices) || !is.list(report$notices)){
    gw_stop("validator report has an unrecognized format: missing `notices`.")
  }
  notices <- report$notices
  if(!length(notices)){
    result <- data.frame(code = character(), severity = character(),
                         total_notices = numeric(), stringsAsFactors = FALSE)
    result$samples <- list()
    return(result)
  }
  valid <- vapply(notices, function(x){
    is.list(x) && is.character(x$code) && length(x$code) == 1L &&
      is.character(x$severity) && length(x$severity) == 1L &&
      is.numeric(x$totalNotices) && length(x$totalNotices) == 1L &&
      is.finite(x$totalNotices) && x$totalNotices >= 0
  }, logical(1))
  if(!all(valid)){
    gw_stop("validator report contains an unrecognized notice format.")
  }
  result <- data.frame(
    code = vapply(notices, `[[`, character(1), "code"),
    severity = toupper(vapply(notices, `[[`, character(1), "severity")),
    total_notices = vapply(notices, `[[`, numeric(1), "totalNotices"),
    stringsAsFactors = FALSE
  )
  result$samples <- lapply(notices, function(x) x$sampleNotices)
  result
}
