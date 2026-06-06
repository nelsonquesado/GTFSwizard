#' Calculate Distances in GTFS Data
#'
#' The `get_distances` function calculates distances within a `wizardgtfs` object based on various methods.
#' Depending on the `method` chosen, it can calculate average route distances, trip-specific distances, or detailed distances between stops.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If it is not of this class, it will be converted.
#' @param method A character string indicating the calculation method. Choices are:
#'   \describe{
#'     \item{"by_route"}{Calculates average distances for each route.}
#'     \item{"by_trip"}{Calculates distances for each trip, associating each trip ID with its total distance.}
#'     \item{"detailed"}{Calculates detailed distances between each consecutive stop for all trips. This is the most computationally intensive option and may take several minutes to complete.}
#'   }
#' @param trips A character vector of trip IDs to consider. When set to `all`, includes all trips.
#'
#' @return A data frame with calculated distances based on the specified method:
#'   \describe{
#'     \item{If `method = "by_route"`}{Returns a summary by route and direction with trip counts, average distance, service pattern, and pattern frequency.}
#'     \item{If `method = "by_trip"`}{Returns trip distance with route, direction, service pattern, and pattern frequency.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `shape_id`, `from_stop_id`, `to_stop_id`, and `distance`.}
#'   }
#'
#' @details
#' The function calls specific sub-functions based on the selected method:
#'
#' - "by_route": Calculates average distances per route.
#'
#' - "by_trip": Calculate distances per trip.
#'
#' - "detailed": Calculates detailed stop-to-stop distances within each route. Note that this method may be slow for large datasets.
#'
#' Legacy dotted values remain accepted. If an invalid `method` is provided,
#' the function defaults to `"by_trip"` and issues a warning.
#'
#' @examples
#' # Calculate average route distances
#' distances_by_route <- get_distances(gtfs = for_rail_gtfs, method = "by_route", trips = 'all')
#'
#' # Calculate distances by trip
#' distances_by_trip <- get_distances(gtfs = for_rail_gtfs, method = "by_trip", trips = 'all')
#'
#' \donttest{
#' # Calculate detailed distances between stops
#' detailed_distances <- get_distances(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
#' }
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @export
get_distances <- function(gtfs, method = 'by_trip', trips = 'all'){
  if(!any(trips == 'all')) {gtfs <- GTFSwizard::filter_trip(gtfs, trip = trips)}

  method <- normalize_method(
    method, c('by_route', 'by_trip', 'detailed'), 'by_trip'
  )

  if (method == 'by_route') {
    distances <- get_distances_byroute(gtfs)
  }

  if (method == 'by_trip') {
    distances <- get_distances_bytrip(gtfs)
  }

  if (method == 'detailed') {
    gw_msg('calculating detailed stop-to-stop distances.')
    distances <- get_distances_detailed(gtfs)
  }

  return(distances)

}

get_distances_byroute <- function(gtfs){

  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <- GTFSwizard::get_shapes_sf(gtfs$shapes)
  distances$distance <- as.numeric(sf::st_length(latlon2epsg(distances)))

  distances <-
    gtfs$trips %>%
    dplyr::left_join(distances, by = 'shape_id') %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(
        "route_id", direction_field(gtfs$trips),
        "service_pattern", "pattern_frequency"
      )))
    ) %>%
    dplyr::reframe(average.distance = mean(distance, na.rm = TRUE),
                   trips = dplyr::n())

  return(distances)

}

get_distances_bytrip <- function(gtfs){

  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))

  service_pattern <-
    GTFSwizard::get_servicepattern(gtfs)

  distances <- GTFSwizard::get_shapes_sf(gtfs$shapes)
  distances$distance <- as.numeric(sf::st_length(latlon2epsg(distances)))

  distances <-
    gtfs$trips %>%
    dplyr::left_join(distances, by = 'shape_id') %>%
    dplyr::left_join(service_pattern, by = 'service_id', relationship = 'many-to-many') %>%
    dplyr::select(
      dplyr::all_of(c(
        "route_id", "trip_id", direction_field(gtfs$trips), "distance",
        "service_pattern", "pattern_frequency"
      ))
    )

  return(distances)

}

get_distances_detailed <- function(gtfs){

  gtfs <- ensure_wizardgtfs(gtfs)

  pairs <-
    gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(from_stop_id = stop_id,
                   to_stop_id = dplyr::lead(stop_id)) %>%
    stats::na.omit() %>%
    dplyr::left_join(gtfs$trips %>% dplyr::select(trip_id, shape_id), by = 'trip_id') %>%
    dplyr::group_by(from_stop_id, to_stop_id, shape_id) %>%
    dplyr::reframe(trips = list(trip_id), .groups = "drop")

  stop_coords <- gtfs$stops %>%
    dplyr::select(stop_id, stop_lon, stop_lat)

  pairs <- pairs %>%
    dplyr::left_join(stop_coords, by = c("from_stop_id" = "stop_id")) %>%
    dplyr::rename(from_lon = stop_lon, from_lat = stop_lat) %>%
    dplyr::left_join(stop_coords, by = c("to_stop_id" = "stop_id")) %>%
    dplyr::rename(to_lon = stop_lon, to_lat = stop_lat)

  pairs$distance <- haversine_m(
    pairs$from_lon,
    pairs$from_lat,
    pairs$to_lon,
    pairs$to_lat
  )

  pairs %>%
    dplyr::select(shape_id, from_stop_id, to_stop_id, distance)

}
