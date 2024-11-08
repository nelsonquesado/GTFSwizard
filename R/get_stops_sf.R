#' Convert GTFS Stops Table to Simple Features (sf) Format
#'
#' `get_stops_sf` converts the stops table in a `wizardgtfs` object into a simple features (`sf`) object, making it suitable for spatial analysis. This function checks the format of the `stops` data and structures it as point geometries.
#'
#' @param gtfs A `wizardgtfs` object containing a `stops` table or the stops table itself as a data frame.
#'
#' @return An `sf` object with stops as point geometries or a `wizardgtfs` object.
#'
#' @details
#' - When the input `stops` table is not in `sf` format, this function converts it to `sf` by using the coordinates in the `stop_lon` and `stop_lat` columns.
#'
#' - The resulting `sf` object is assigned a CRS of WGS 84 (EPSG:4326) for geographic compatibility.
#'
#' - If the `stops` table is already in `sf` format, the function simply reassigns the CRS and returns it unchanged.
#'
#' @examples
#' # Convert stops data in a GTFS object to sf format
#' gtfs_sf <- get_stops_sf(for_rail_gtfs)
#'
#' @seealso
#' [GTFSwizard::get_shapes()], [GTFSwizard::get_shapes_sf()], [GTFSwizard::get_shapes_df()]
#'
#' @importFrom sf st_as_sf st_crs
#' @export
get_stops_sf <- function(gtfs){
  UseMethod('get_stops_sf')
}

#' @exportS3Method GTFSwizard::get_stops_sf
get_stops_sf.wizardgtfs <- function(gtfs){
  gtfs$stops <- get_stops_sf.data.frame(gtfs$stops)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_stops_sf
get_stops_sf.list <- function(gtfs){
  gtfs$stops <- get_stops_sf.data.frame(gtfs$stops)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_stops_sf
get_stops_sf.gtfs <- function(gtfs){
  gtfs$stops <- get_stops_sf.data.frame(gtfs$stops)
  return(gtfs)
}

#' @exportS3Method GTFSwizard::get_stops_sf
get_stops_sf.data.frame <- function(gtfs){
  if('sf'%in%class(gtfs)){
    st_crs(gtfs) <- 4326
    return(gtfs)
  }else{
    gtfs %>%
      sf::st_as_sf(coords = c('stop_lon','stop_lat'), crs = 4326) %>%
      return()
  }
}
