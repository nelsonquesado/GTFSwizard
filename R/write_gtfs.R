#' Write a GTFS Feed
#'
#' Exports a GTFS object to a zip archive. Internal `dates_services` data are
#' omitted, GTFS dates use `YYYYMMDD`, and spatial stops/shapes are converted
#' back to standard text-table columns.
#'
#' @param gtfs A GTFS object.
#' @param zipfile Output `.zip` path.
#' @param ... Additional arguments passed to the format-specific writer.
#'
#' @return The normalized output path, invisibly.
#'
#' @examples
#' path <- tempfile(fileext = ".zip")
#' write_gtfs(for_rail_gtfs, path)
#'
#' @seealso [GTFSwizard::read_gtfs()]
#' @export
write_gtfs <- function(gtfs, zipfile, ...){
  UseMethod("write_gtfs")
}

#' @exportS3Method GTFSwizard::write_gtfs wizardgtfs
write_gtfs.wizardgtfs <- function(gtfs, zipfile, ...){
  write_gtfs.list(gtfs, zipfile, ...)
}

#' @exportS3Method GTFSwizard::write_gtfs list
write_gtfs.list <- function(gtfs, zipfile, ...){
  if(!is.character(zipfile) || length(zipfile) != 1L || !nzchar(zipfile)){
    gw_stop("`zipfile` must be one non-empty path.")
  }
  gtfs <- sf_to_gtfs_tables(gtfs)
  gtfs$dates_services <- NULL
  gtfs <- lapply(gtfs, normalize_gtfs_table)
  class(gtfs) <- c("gtfs", "list")
  gtfsio::export_gtfs(gtfs = gtfs, path = zipfile, ...)
  invisible(normalizePath(zipfile, mustWork = FALSE))
}

#' @exportS3Method GTFSwizard::write_gtfs tidygtfs
write_gtfs.tidygtfs <- function(gtfs, zipfile, ...){
  require_pkg("tidytransit", "writing `tidygtfs` objects")
  tidytransit::write_gtfs(gtfs, zipfile, ...)
  invisible(normalizePath(zipfile, mustWork = FALSE))
}

#' @exportS3Method GTFSwizard::write_gtfs dt_gtfs
write_gtfs.dt_gtfs <- function(gtfs, zipfile, ...){
  require_pkg("gtfstools", "writing `dt_gtfs` objects")
  gtfstools::write_gtfs(gtfs, path = zipfile, ...)
  invisible(normalizePath(zipfile, mustWork = FALSE))
}

sf_to_gtfs_tables <- function(gtfs){
  if(inherits(gtfs$shapes, "sf")){
    gtfs$shapes <- get_shapes_df(gtfs$shapes)
  }
  if(inherits(gtfs$stops, "sf")){
    stops <- sf::st_transform(gtfs$stops, 4326)
    coordinates <- sf::st_coordinates(stops)
    stops$stop_lon <- coordinates[, 1]
    stops$stop_lat <- coordinates[, 2]
    gtfs$stops <- sf::st_drop_geometry(stops)
  }
  gtfs
}

normalize_gtfs_table <- function(table){
  table <- as.data.frame(table)
  table[] <- lapply(table, function(column){
    if(inherits(column, "Date")){
      return(format(column, "%Y%m%d"))
    }
    if(inherits(column, c("POSIXct", "POSIXlt"))){
      return(format(as.Date(column), "%Y%m%d"))
    }
    if(inherits(column, "hms")){
      return(as.character(column))
    }
    if(is.character(column)){
      column[is.na(column)] <- ""
    }
    column
  })
  table
}
