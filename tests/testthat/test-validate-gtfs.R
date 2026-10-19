test_that("validation reports notices without modifying a source archive", {
  skip_if_not_installed("jsonlite")
  jar <- tempfile(fileext = ".jar")
  zip <- tempfile(fileext = ".zip")
  writeLines("validator", jar)
  writeLines("original archive bytes", zip)
  output <- tempfile("gtfs report with spaces ")
  on.exit(unlink(c(jar, zip, output), recursive = TRUE), add = TRUE)
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      expect_equal(input, normalizePath(zip))
      writeLines('{"notices":[{"code":"bad_fk","severity":"ERROR","totalNotices":2,"sampleNotices":[{"row":5}]}]}',
                 file.path(output_dir, "report.json"))
      writeLines('{"notices":[]}', file.path(output_dir, "system_errors.json"))
      writeLines("report", file.path(output_dir, "report.html"))
      list(status = 0L, output = character())
    },
    .package = "GTFSwizard"
  )
  result <- validate_gtfs(zip, validator_jar = jar, output_dir = output)
  expect_s3_class(result, "gtfs_validation")
  expect_true(result$has_errors)
  expect_equal(result$notices$code, "bad_fk")
  expect_equal(result$notices$total_notices, 2)
  expect_equal(result$notices$samples[[1]][[1]]$row, 5)
  expect_true(file.exists(result$html_path))
  expect_identical(readLines(zip), "original archive bytes")
  expect_output(print(result), "errors found")
  expect_error(validate_gtfs(zip, jar, output), "must be empty")
})

test_that("validation exports an object and treats system errors as incomplete", {
  skip_if_not_installed("jsonlite")
  jar <- tempfile(fileext = ".jar")
  output <- tempfile("gtfs-validation-")
  writeLines("validator", jar)
  on.exit(unlink(c(jar, output), recursive = TRUE), add = TRUE)
  feed <- minimal_feed()
  original <- feed
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      expect_true(file.exists(input))
      expect_true("trips.txt" %in% utils::unzip(input, list = TRUE)$Name)
      writeLines('{"notices":[]}', file.path(output_dir, "report.json"))
      writeLines('{"notices":[{"code":"io_error","severity":"ERROR","totalNotices":1,"sampleNotices":[]}]}',
                 file.path(output_dir, "system_errors.json"))
      list(status = 1L, output = "validator error")
    },
    .package = "GTFSwizard"
  )
  result <- validate_gtfs(feed, jar, output)
  expect_true(is.na(result$has_errors))
  expect_equal(result$system_notices$code, "io_error")
  expect_identical(feed, original)
  expect_null(result$html_path)
  expect_output(print(result), "incomplete or unknown")
  expect_output(print(result), "io_error")
})

test_that("validation never treats failed or malformed runs as clean", {
  skip_if_not_installed("jsonlite")
  jar <- tempfile(fileext = ".jar")
  zip <- tempfile(fileext = ".zip")
  writeLines("validator", jar)
  writeLines("archive", zip)
  on.exit(unlink(c(jar, zip)), add = TRUE)
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      list(status = 1L, output = "failed to start")
    },
    .package = "GTFSwizard"
  )
  expect_error(validate_gtfs(zip, jar), "did not produce both JSON reports")
  expect_error(validate_gtfs(zip, "missing.jar"), "validator_jar")
  expect_error(gtfs_validation_notices(list()), "unrecognized format")
  expect_error(gtfs_validation_notices(list(notices = list(list(code = "x")))),
               "unrecognized notice format")
})

test_that("validation prints and removes temporary reports by default", {
  skip_if_not_installed("jsonlite")
  zip <- tempfile(fileext = ".zip")
  jar <- tempfile(fileext = ".jar")
  writeLines("archive", zip)
  writeLines("validator", jar)
  on.exit(unlink(c(zip, jar)), add = TRUE)
  old_jar <- Sys.getenv("GTFS_VALIDATOR_JAR", unset = NA_character_)
  Sys.setenv(GTFS_VALIDATOR_JAR = jar)
  on.exit(if(is.na(old_jar)) Sys.unsetenv("GTFS_VALIDATOR_JAR") else
    Sys.setenv(GTFS_VALIDATOR_JAR = old_jar), add = TRUE)
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      expect_equal(validator_jar, normalizePath(jar))
      expect_true(file.exists(validator_jar))
      seen$output <- output_dir
      writeLines('{"notices":[]}', file.path(output_dir, "report.json"))
      writeLines('{"notices":[]}', file.path(output_dir, "system_errors.json"))
      list(status = 0L, output = character())
    },
    .package = "GTFSwizard"
  )
  expect_output(visible <- withVisible(validate_gtfs(zip)), "no errors found")
  expect_false(visible$visible)
  expect_false(visible$value$has_errors)
  expect_null(visible$value$output_dir)
  expect_null(visible$value$report_path)
  expect_false(dir.exists(seen$output))
  expect_output(validate_gtfs(zip, quiet = TRUE), NA)
  expect_false(dir.exists(seen$output))
})

test_that("failed validation removes temporary reports", {
  skip_if_not_installed("jsonlite")
  jar <- tempfile(fileext = ".jar")
  zip <- tempfile(fileext = ".zip")
  writeLines("validator", jar)
  writeLines("archive", zip)
  on.exit(unlink(c(jar, zip)), add = TRUE)
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      seen$output <- output_dir
      list(status = 1L, output = "failed")
    },
    .package = "GTFSwizard"
  )
  expect_error(validate_gtfs(zip, jar), "did not produce both JSON reports")
  expect_false(dir.exists(seen$output))
  expect_error(validate_gtfs(zip, jar, quiet = NA), "quiet")
})

test_that("validation explains when Java is unavailable", {
  skip_if_not_installed("jsonlite")
  jar <- tempfile(fileext = ".jar")
  writeLines("validator", jar)
  on.exit(unlink(jar), add = TRUE)
  local_mocked_bindings(find_gtfs_validator_java = function() "", .package = "GTFSwizard")
  expect_error(validate_gtfs("feed.zip", jar), "Java 17 or newer")
})

test_that("optional download is cached and checked before every use", {
  skip_if_not_installed("openssl")
  cache <- tempfile("validator-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(
    fetch_gtfs_validator = function(url, path, quiet){
      calls$n <- calls$n + 1L
      expect_match(url, "https://github.com/MobilityData/gtfs-validator/releases/download/v8.0.1/")
      writeLines("verified", path)
      0L
    },
    gtfs_validator_checksum = function(path){
      if(identical(readLines(path), "verified"))
        "19293ddd9b6f954f216d4f12054bd8a3232921751c4484339e339764a91000e2"
      else "bad"
    },
    .package = "GTFSwizard"
  )
  jar <- download_gtfs_validator(cache, quiet = TRUE)
  expect_true(file.exists(jar))
  expect_identical(download_gtfs_validator(cache, quiet = TRUE), jar)
  expect_identical(calls$n, 1L)
  writeLines("corrupted", jar)
  expect_error(download_gtfs_validator(cache, quiet = TRUE), "SHA-256")
  expect_identical(readLines(jar), "corrupted")
})

test_that("invalid downloads are removed and never cached", {
  skip_if_not_installed("openssl")
  cache <- tempfile("validator-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  local_mocked_bindings(
    fetch_gtfs_validator = function(url, path, quiet){
      writeLines("invalid", path)
      0L
    },
    .package = "GTFSwizard"
  )
  expect_error(download_gtfs_validator(cache, quiet = TRUE), "SHA-256")
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("SHA-256 checksum is a plain string", {
  skip_if_not_installed("openssl")
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw("abc"), path)
  expect_identical(gtfs_validator_checksum(path),
                   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
})

test_that("automatic download requires explicit opt-in", {
  skip_if_not_installed("jsonlite")
  zip <- tempfile(fileext = ".zip")
  jar <- tempfile(fileext = ".jar")
  writeLines("archive", zip)
  writeLines("validator", jar)
  on.exit(unlink(c(zip, jar)), add = TRUE)
  old_jar <- Sys.getenv("GTFS_VALIDATOR_JAR", unset = NA_character_)
  Sys.unsetenv("GTFS_VALIDATOR_JAR")
  on.exit(if(is.na(old_jar)) Sys.unsetenv("GTFS_VALIDATOR_JAR") else
    Sys.setenv(GTFS_VALIDATOR_JAR = old_jar), add = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(
    find_gtfs_validator_java = function() "java",
    download_gtfs_validator = function(quiet){ calls$n <- calls$n + 1L; jar },
    run_gtfs_validator_cli = function(java, validator_jar, input, output_dir){
      writeLines('{"notices":[]}', file.path(output_dir, "report.json"))
      writeLines('{"notices":[]}', file.path(output_dir, "system_errors.json"))
      list(status = 0L, output = character())
    },
    .package = "GTFSwizard"
  )
  expect_error(validate_gtfs(zip), "download_validator")
  expect_identical(calls$n, 0L)
  result <- validate_gtfs(zip, quiet = TRUE, download_validator = TRUE)
  expect_false(result$has_errors)
  expect_identical(calls$n, 1L)
  validate_gtfs(zip, validator_jar = jar, quiet = TRUE, download_validator = TRUE)
  expect_identical(calls$n, 1L)
})
