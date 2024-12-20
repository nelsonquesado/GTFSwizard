#' Transform Spatial Data to a Local UTM Coordinate System
#'
#' @description
#' The `latlon2epsg` function determines the appropriate UTM (Universal Transverse Mercator) EPSG code for a given `sf` object
#' based on its centroid's latitude and longitude. It then transforms the object to the identified coordinate reference system (CRS).
#'
#' @param sf_obj An `sf` object representing spatial features. This object must have a valid CRS with latitude and longitude coordinates.
#'
#' @return An `sf` object transformed to the appropriate UTM coordinate reference system.
#'
#' @details
#' The function calculates the geographic centroid of the input spatial object and determines its latitude and longitude.
#' Based on the latitude:
#' \describe{
#'   \item{Latitudes above 84°}{The object is transformed to EPSG:3413 (North Pole).}
#'   \item{Latitudes below -80°}{The object is transformed to EPSG:3031 (South Pole).}
#'   \item{Latitudes between -80° and 84°}{The function calculates the UTM zone based on longitude and transforms the object
#'   to the appropriate UTM EPSG code (EPSG:326XX for the Northern Hemisphere, EPSG:327XX for the Southern Hemisphere).}
#' }
#'
#' @note The function requires that the input `sf` object already has a valid CRS defined (e.g., WGS84).
#'
#' @examples
#' latlon2epsg(get_shapes_sf(for_bus_gtfs)$shapes)
#'
#' @seealso [sf::st_transform()], [sf::st_centroid()], [sf::st_union()]
#'
#' @importFrom sf st_centroid st_union st_transform
#' @importFrom magrittr %>%
#' @export


latlon2epsg <- function(sf_obj) {

  centroid <- sf::st_centroid(sf::st_union(sf_obj))

  lat <- centroid[[1]] %>% as.numeric() %>% .[2]

  if (lat > 84) {

    crs_code <- "EPSG:3413"  # North Pole

  } else if (lat < -80) {

    crs_code <- "EPSG:3031"  # South Pole

  } else {

    lon <- centroid[[1]] %>% as.numeric() %>% .[1]
    zone <- floor((lon + 180) / 6) + 1
    crs_code <- ifelse(lat >= 0, paste0("EPSG:326", zone), paste0("EPSG:327", zone))

  }

  obj.transformed <- sf::st_transform(sf_obj, crs = crs_code)

  return(obj.transformed)

}
