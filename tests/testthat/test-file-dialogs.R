test_that("read_gtfs opens the file selector when no path is supplied", {
  archive <- tempfile(fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  write_gtfs(minimal_feed(), archive)
  local_mocked_bindings(
    choose_gtfs_path = function(save = FALSE){
      expect_false(save)
      archive
    },
    .package = "GTFSwizard"
  )
  expect_s3_class(read_gtfs(), "wizardgtfs")
})

test_that("write_gtfs opens the save selector and adds the zip extension", {
  destination <- tempfile("gtfs-export-")
  on.exit(unlink(paste0(destination, ".zip")), add = TRUE)
  local_mocked_bindings(
    choose_gtfs_path = function(save = FALSE){
      expect_true(save)
      paste0(destination, ".zip")
    },
    .package = "GTFSwizard"
  )
  result <- write_gtfs(minimal_feed())
  expect_identical(result, normalizePath(paste0(destination, ".zip"), mustWork = TRUE))
})

test_that("file selectors report non-interactive use and cancellation", {
  local_mocked_bindings(
    is_interactive_session = function() FALSE,
    .package = "GTFSwizard"
  )
  expect_error(choose_gtfs_path(), "non-interactive")
  expect_error(choose_gtfs_path(save = TRUE), "non-interactive")

  local_mocked_bindings(
    is_interactive_session = function() TRUE,
    select_gtfs_file = function(save) character(),
    .package = "GTFSwizard"
  )
  expect_error(choose_gtfs_path(), "no GTFS file")
  expect_error(choose_gtfs_path(save = TRUE), "no output location")
})

test_that("save selector normalizes zip extension", {
  destination <- tempfile("gtfs-export-")
  local_mocked_bindings(
    is_interactive_session = function() TRUE,
    select_gtfs_file = function(save) destination,
    .package = "GTFSwizard"
  )
  expect_identical(choose_gtfs_path(save = TRUE),
                   normalizePath(paste0(destination, ".zip"), mustWork = FALSE))
})
