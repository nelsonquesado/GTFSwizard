#' Convert GTFS Stops to Simple Features
#'
#' @param gtfs A GTFS object or a `stops` data frame.
#'
#' @return For a data frame, a WGS84 point `sf` object. For a GTFS object, the
#'   same object with its `stops` table converted to `sf`.
#'
#' @examples
#' gtfs_sf <- get_stops_sf(for_rail_gtfs)
#' stops_sf <- get_stops_sf(for_rail_gtfs$stops)
#'
#' @export
get_stops_sf <- function(gtfs){
  UseMethod("get_stops_sf")
}

#' @exportS3Method GTFSwizard::get_stops_sf wizardgtfs
get_stops_sf.wizardgtfs <- function(gtfs){
  gtfs$stops <- get_stops_sf.data.frame(gtfs$stops)
  gtfs
}

#' @exportS3Method GTFSwizard::get_stops_sf list
get_stops_sf.list <- function(gtfs){
  gtfs$stops <- get_stops_sf.data.frame(gtfs$stops)
  gtfs
}

#' @exportS3Method GTFSwizard::get_stops_sf gtfs
get_stops_sf.gtfs <- get_stops_sf.list

#' @exportS3Method GTFSwizard::get_stops_sf data.frame
get_stops_sf.data.frame <- function(gtfs){
  if(inherits(gtfs, "sf")){
    if(is.na(sf::st_crs(gtfs))){
      gw_stop("the stops `sf` object must have a defined CRS.")
    }
    return(sf::st_transform(gtfs, 4326))
  }
  missing <- setdiff(c("stop_lon", "stop_lat"), names(gtfs))
  if(length(missing)){
    gw_stop("the stops table is missing: ", paste(missing, collapse = ", "), ".")
  }
  sf::st_as_sf(
    gtfs, coords = c("stop_lon", "stop_lat"), remove = FALSE, crs = 4326
  )
}
