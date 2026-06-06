#' Transform Spatial Data to a Local Metric CRS
#'
#' Selects a UTM zone from the geographic centroid, or a polar stereographic
#' CRS outside UTM's latitude range.
#'
#' @param sf_obj An `sf` or `sfc` object with a defined CRS.
#'
#' @return The input transformed to EPSG 326xx/327xx, EPSG 3413, or EPSG 3031.
#'
#' @examples
#' shapes <- get_shapes_sf(for_rail_gtfs)$shapes
#' metric_shapes <- latlon2epsg(shapes)
#'
#' @seealso [sf::st_transform()]
#' @export
latlon2epsg <- function(sf_obj){
  if(!inherits(sf_obj, c("sf", "sfc"))){
    gw_stop("`sf_obj` must be an `sf` or `sfc` object.")
  }
  if(is.na(sf::st_crs(sf_obj))){
    gw_stop("`sf_obj` must have a defined coordinate reference system.")
  }
  geographic <- sf::st_transform(sf_obj, 4326)
  centroid <- suppressWarnings(sf::st_centroid(sf::st_union(geographic)))
  coordinates <- sf::st_coordinates(centroid)[1L, ]
  lon <- coordinates[1L]
  lat <- coordinates[2L]
  if(lat > 84){
    epsg <- 3413L
  } else if(lat < -80){
    epsg <- 3031L
  } else {
    zone <- min(60L, max(1L, floor((lon + 180) / 6) + 1L))
    epsg <- if(lat >= 0) 32600L + zone else 32700L + zone
  }
  sf::st_transform(sf_obj, epsg)
}
