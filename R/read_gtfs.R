#' Read a GTFS Feed
#'
#' Imports a GTFS zip archive and converts it to `wizardgtfs`.
#'
#' @param file_path Path to a GTFS `.zip` archive.
#' @param files Optional character vector of table names without `.txt`.
#' @param quiet Logical. Suppress importer messages.
#' @param ... Additional arguments passed to [gtfsio::import_gtfs()], including
#'   the legacy argument `file.path`.
#'
#' @return A validated `wizardgtfs` object.
#'
#' @examples
#' path <- tempfile(fileext = ".zip")
#' write_gtfs(for_rail_gtfs, path)
#' gtfs <- read_gtfs(path)
#'
#' @seealso [GTFSwizard::write_gtfs()], [GTFSwizard::as_wizardgtfs()]
#' @export
read_gtfs <- function(file_path = NULL, files = NULL, quiet = TRUE, ...){
  resolved <- resolve_legacy_argument(
    file_path, missing(file_path), list(...), "file.path", "file_path"
  )
  file_path <- resolved$value
  dots <- resolved$dots
  if(!is.character(file_path) || length(file_path) != 1L ||
     !file.exists(file_path)){
    gw_stop("`file_path` must point to an existing GTFS zip archive.")
  }
  gw_assert_flag(quiet, "quiet")
  object <- if(gtfs_zip_uses_utf16(file_path, files = files, skip = dots$skip)){
    if(!quiet){
      gw_warn("GTFS text files are encoded in UTF-16. Recoding to UTF-8 before import.")
    }
    import_utf16_gtfs(file_path, files = files, skip = dots$skip)
  } else tryCatch(
    do.call(
      gtfsio::import_gtfs,
      c(list(path = file_path, files = files, quiet = quiet), dots)
    ),
    error = function(error){
      import_error <- conditionMessage(error)
      unsupported_utf16 <- grepl("UTF-16", import_error, fixed = TRUE) ||
        grepl("embedded nul", import_error, fixed = TRUE)
      if(!unsupported_utf16){
        stop(error)
      }
      if(!quiet){
        gw_warn("GTFS text files are encoded in UTF-16. Recoding to UTF-8 before import.")
      }
      import_utf16_gtfs(file_path, files = files, skip = dots$skip)
    }
  )
  as_wizardgtfs(object)
}

gtfs_zip_uses_utf16 <- function(file_path, files = NULL, skip = NULL){
  listed <- tryCatch(utils::unzip(file_path, list = TRUE), error = function(error) NULL)
  if(is.null(listed)){
    return(FALSE)
  }
  filenames <- listed$Name
  filenames <- filenames[grepl("\\.txt$", filenames, ignore.case = TRUE)]
  if(!is.null(files)){
    filenames <- intersect(append_file_ext_local(files), filenames)
  } else if(!is.null(skip)){
    filenames <- setdiff(filenames, append_file_ext_local(skip))
  }
  priority <- c(
    "agency.txt", "routes.txt", "trips.txt", "stops.txt",
    "stop_times.txt", "calendar.txt", "calendar_dates.txt"
  )
  candidate <- priority[priority %in% basename(filenames)]
  if(length(candidate)){
    filenames <- filenames[match(candidate[1], basename(filenames))]
  } else {
    filenames <- filenames[1]
  }
  if(!length(filenames) || is.na(filenames)){
    return(FALSE)
  }
  tmpdir <- tempfile("gtfswizard-encoding-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  path <- tryCatch({
    utils::unzip(file_path, files = filenames, exdir = tmpdir)
    file.path(tmpdir, filenames)
  }, error = function(error) NA_character_)
  if(is.na(path) || !file.exists(path)){
    return(FALSE)
  }
  nzchar(detect_text_encoding(path))
}

import_utf16_gtfs <- function(file_path, files = NULL, skip = NULL){
  if(!is.null(files) && !is.null(skip)){
    gw_stop("supply only one of `files` or `skip`.")
  }
  listed <- utils::unzip(file_path, list = TRUE)
  filenames <- listed$Name
  filenames <- filenames[grepl("\\.(txt|geojson)$", filenames, ignore.case = TRUE)]
  if(!is.null(files)){
    requested <- append_file_ext_local(files)
  } else if(!is.null(skip)){
    requested <- setdiff(filenames, append_file_ext_local(skip))
  } else {
    requested <- filenames
  }
  missing <- setdiff(requested, filenames)
  if(length(missing)){
    gw_stop("missing GTFS file(s): ", paste(missing, collapse = ", "), ".")
  }

  tmpdir <- tempfile("gtfswizard-read-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  utils::unzip(file_path, files = requested, exdir = tmpdir)

  tables <- lapply(requested, function(filename){
    path <- file.path(tmpdir, filename)
    if(grepl("\\.geojson$", filename, ignore.case = TRUE)){
      return(paste(readLines(path, warn = FALSE), collapse = "\n"))
    }
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = "",
      colClasses = "character",
      fileEncoding = detect_text_encoding(path)
    )
  })
  names(tables) <- tools::file_path_sans_ext(basename(requested))
  tables <- coerce_gtfs_import_types(tables)
  class(tables) <- c("gtfs", "list")
  tables
}

coerce_gtfs_import_types <- function(tables){
  integer_fields <- list(
    routes = "route_type",
    trips = c("direction_id", "wheelchair_accessible", "bikes_allowed"),
    stop_times = c(
      "stop_sequence", "pickup_type", "drop_off_type",
      "continuous_pickup", "continuous_drop_off", "timepoint"
    ),
    stops = c("location_type", "wheelchair_boarding"),
    calendar = c(
      "monday", "tuesday", "wednesday", "thursday",
      "friday", "saturday", "sunday"
    ),
    calendar_dates = "exception_type",
    shapes = "shape_pt_sequence",
    frequencies = c("headway_secs", "exact_times"),
    transfers = "transfer_type",
    fare_attributes = c("payment_method", "transfers")
  )
  numeric_fields <- list(
    stop_times = "shape_dist_traveled",
    stops = c("stop_lat", "stop_lon"),
    shapes = c("shape_pt_lat", "shape_pt_lon", "shape_dist_traveled"),
    transfers = "min_transfer_time",
    fare_attributes = c("price", "transfer_duration")
  )
  for(table_name in intersect(names(integer_fields), names(tables))){
    tables[[table_name]] <- coerce_table_fields(
      tables[[table_name]], integer_fields[[table_name]], as.integer
    )
  }
  for(table_name in intersect(names(numeric_fields), names(tables))){
    tables[[table_name]] <- coerce_table_fields(
      tables[[table_name]], numeric_fields[[table_name]], as.numeric
    )
  }
  tables
}

coerce_table_fields <- function(table, fields, fun){
  for(field in intersect(fields, names(table))){
    table[[field]] <- suppressWarnings(fun(table[[field]]))
  }
  table
}

append_file_ext_local <- function(x){
  x <- as.character(x)
  ifelse(grepl("\\.(txt|geojson)$", x, ignore.case = TRUE), x, paste0(x, ".txt"))
}

detect_text_encoding <- function(path){
  bytes <- readBin(path, "raw", n = min(1024L, file.info(path)$size))
  if(length(bytes) >= 2L){
    if(identical(bytes[1:2], as.raw(c(0xff, 0xfe)))){
      return("UTF-16LE")
    }
    if(identical(bytes[1:2], as.raw(c(0xfe, 0xff)))){
      return("UTF-16BE")
    }
  }
  if(length(bytes) >= 4L){
    even_zero <- sum(bytes[seq(2L, length(bytes), by = 2L)] == as.raw(0))
    odd_zero <- sum(bytes[seq(1L, length(bytes), by = 2L)] == as.raw(0))
    if(even_zero > length(bytes) / 4){
      return("UTF-16LE")
    }
    if(odd_zero > length(bytes) / 4){
      return("UTF-16BE")
    }
  }
  ""
}
