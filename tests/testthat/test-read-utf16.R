make_utf16_feed <- function(encodings, bom = TRUE){
  feed <- minimal_feed()
  feed$stops$stop_name[1] <- "Esta\u00e7\u00e3o"
  feed$stops$custom_code <- seq_len(nrow(feed$stops))
  original <- tempfile(fileext = ".zip")
  folder <- tempfile("utf16-fixture-")
  dir.create(folder)
  on.exit(unlink(c(original, folder), recursive = TRUE), add = TRUE)
  write_gtfs(feed, original)
  utils::unzip(original, exdir = folder)
  for(name in names(encodings)){
    path <- file.path(folder, paste0(name, ".txt"))
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    bytes <- iconv(paste0(paste(lines, collapse = "\n"), "\n"),
                   from = "UTF-8", to = encodings[[name]], toRaw = TRUE)[[1]]
    marker <- if(!bom) raw() else if(encodings[[name]] == "UTF-16LE")
      as.raw(c(255, 254)) else as.raw(c(254, 255))
    con <- file(path, "wb")
    writeBin(c(marker, bytes), con)
    close(con)
  }
  result <- tempfile(fileext = ".zip")
  zip::zipr(result, files = list.files(folder, recursive = TRUE), root = folder,
            mode = "mirror", include_directories = FALSE)
  result
}

test_that("mixed UTF-16 files are converted without changing the original ZIP", {
  archive <- make_utf16_feed(c(agency = "UTF-16LE", stops = "UTF-16BE"))
  on.exit(unlink(archive), add = TRUE)
  original <- tools::md5sum(archive)
  expect_warning(
    result <- read_gtfs(archive, quiet = FALSE,
                        extra_spec = list(stops = c(custom_code = "integer"))),
    "UTF-16.*UTF-8"
  )
  expect_identical(result$stops$stop_name[1], "Esta\u00e7\u00e3o")
  expect_type(result$stops$custom_code, "integer")
  expect_identical(tools::md5sum(archive), original)
  expect_warning(read_gtfs(archive, quiet = TRUE), NA)
})

test_that("UTF-16 without a BOM is detected in a later table", {
  archive <- make_utf16_feed(c(stops = "UTF-16BE"), bom = FALSE)
  on.exit(unlink(archive), add = TRUE)
  expect_true(gtfs_zip_uses_utf16(archive))
  expect_false(gtfs_zip_uses_utf16(archive, skip = "stops"))
  expect_warning(result <- read_gtfs(archive, quiet = FALSE), "UTF-16.*UTF-8")
  expect_identical(result$stops$stop_name[1], "Esta\u00e7\u00e3o")
})

test_that("UTF-8 feeds do not trigger UTF-16 conversion", {
  archive <- tempfile(fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  write_gtfs(minimal_feed(), archive)
  expect_false(gtfs_zip_uses_utf16(archive))
  expect_warning(read_gtfs(archive, quiet = TRUE), NA)
})

test_that("UTF-16 in a later table preserves field selection and skipping", {
  archive <- make_utf16_feed(c(stops = "UTF-16LE"))
  on.exit(unlink(archive), add = TRUE)
  expect_warning(
    result <- read_gtfs(
      archive, quiet = FALSE, skip = "shapes",
      fields = list(stops = c("stop_id", "stop_name", "stop_lat", "stop_lon", "custom_code")),
      extra_spec = list(stops = c(custom_code = "integer"))
    ),
    "UTF-16.*UTF-8"
  )
  expect_identical(result$stops$stop_name[1], "Esta\u00e7\u00e3o")
  expect_type(result$stops$custom_code, "integer")
  expect_false("stop_desc" %in% names(result$stops))
  expect_error(read_gtfs(archive, files = "stops", skip = "shapes"),
               "either|only one|both")
})

test_that("conversion preserves Unicode and line endings across chunks", {
  for(encoding in c("UTF-16LE", "UTF-16BE")){
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    value <- paste0(paste(rep("a", 32766L), collapse = ""), "\U0001f680\r\n",
                    "Esta\u00e7\u00e3o\n")
    bytes <- iconv(value, from = "UTF-8", to = encoding, toRaw = TRUE)[[1]]
    bom <- if(encoding == "UTF-16LE") c(255, 254) else c(254, 255)
    writeBin(c(as.raw(bom), bytes), path)
    convert_gtfs_utf16(path, encoding)
    expect_identical(readBin(path, "raw", n = 100000L), charToRaw(value))
  }
})

test_that("conversion rejects incomplete UTF-16 without overwriting the file", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  bytes <- as.raw(c(255, 254, 65))
  writeBin(bytes, path)
  expect_error(convert_gtfs_utf16(path, "UTF-16LE"), "incomplete UTF-16")
  expect_identical(readBin(path, "raw", n = 10), bytes)
})
