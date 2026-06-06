#' Calculate Trip Durations in GTFS Data
#'
#' Calculates scheduled trip and segment durations in seconds.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by_route"}{Calculates the average duration for each route.}
#'     \item{"by_trip"}{Calculates the total duration for each trip.}
#'     \item{"detailed"`}{Calculates detailed durations for each stop-to-stop segment within a trip.}
#'   }
#' @param trips A character vector of trip IDs to consider. When set to `all`, includes all trips.
#'
#' @return A data frame containing trip durations based on the specified method:
#'   \describe{
#'     \item{If `method = "by_route"`}{Includes dwell from first departure to final arrival and groups by direction when available.}
#'     \item{If `method = "by_trip"`}{Includes dwell from first departure to final arrival and retains direction when available.}
#'     \item{If `method = "detailed"`}{It does not include dwell times. Returns a data frame with columns: `route_id`, `trip_id`, `hour`, `from_stop_id`, `to_stop_id`, `duration`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by_route": Calculates average durations for each route.
#'
#' - "by_trip": Calculates the total duration of each trip.
#'
#' - "detailed": Calculates detailed durations between consecutive stops within each trip, excluding dwell times.
#'
#' Legacy dotted values remain accepted. If an invalid `method` is specified,
#' the function defaults to `"by_trip"` and provides a warning.
#'
#' @examples
#' # Calculate average route durations
#' durations_by_route <- get_durations(gtfs = for_rail_gtfs, method = "by_route", trips = 'all')
#'
#' # Calculate trip durations
#' durations_by_trip <- get_durations(gtfs = for_rail_gtfs, method = "by_trip", trips = 'all')
#'
#' # Calculate detailed durations between stops
#' detailed_durations <- get_durations(gtfs = for_rail_gtfs, method = "detailed", trips = 'all')
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @export
get_durations <- function(gtfs, method = 'by_trip', trips = 'all'){

  if(!any(trips == 'all')) {gtfs <- GTFSwizard::filter_trip(gtfs, trip = trips)}

  method <- normalize_method(
    method, c('by_route', 'by_trip', 'detailed'), 'by_trip'
  )

  switch(
    method,
    by_route = get_durations_byroute(gtfs),
    by_trip = get_durations_bytrip(gtfs),
    detailed = get_durations_detailed(gtfs)
  )

}

get_durations_byroute <- function(gtfs){

  gtfs <- ensure_wizardgtfs(gtfs)
  service_pattern <- GTFSwizard::get_servicepattern(gtfs)

  trip_duration_table(gtfs) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(
        "route_id", direction_field(gtfs$trips),
        "service_pattern", "pattern_frequency"
      )))
    ) %>%
    dplyr::reframe(average.duration = mean(duration),
                   trips = dplyr::n())

}

get_durations_bytrip <- function(gtfs){

  gtfs <- ensure_wizardgtfs(gtfs)
  service_pattern <- GTFSwizard::get_servicepattern(gtfs)

  trip_duration_table(gtfs) %>%
    dplyr::left_join(gtfs$trips,
                     by = 'trip_id') %>%
    dplyr::left_join(service_pattern,
                     by = 'service_id',
                     relationship = 'many-to-many') %>%
    dplyr::select(
      dplyr::all_of(c(
        "route_id", "trip_id", direction_field(gtfs$trips), "duration",
        "service_pattern", "pattern_frequency"
      ))
    )

}

get_durations_detailed <- function(gtfs){

  gtfs <- ensure_wizardgtfs(gtfs)
  service_pattern <- GTFSwizard::get_servicepattern(gtfs)

  gtfs$stop_times %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::filter(!arrival_time == '', !departure_time == '') %>%
    dplyr::mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      hour = floor(arrival_seconds / 3600)
    ) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::mutate(
      from_stop_id = stop_id,
      to_stop_id = dplyr::lead(stop_id),
      lead_arrival_time = dplyr::lead(arrival_seconds),
      duration = lead_arrival_time - departure_seconds
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(gtfs$trips, by = 'trip_id') %>%
    dplyr::left_join(
      service_pattern,
      by = 'service_id',
      relationship = 'many-to-many'
    ) %>%
    dplyr::mutate(arrival_time = seconds_to_gtfs_time(lead_arrival_time)) %>%
    dplyr::select(dplyr::all_of(c(
      "route_id", "trip_id", direction_field(gtfs$trips),
      "arrival_time", "hour", "from_stop_id", "to_stop_id",
      "duration", "service_pattern", "pattern_frequency"
    ))) %>%
    stats::na.omit()

}

trip_duration_table <- function(gtfs){
  gtfs$stop_times %>%
    dplyr::arrange(trip_id, stop_sequence) %>%
    dplyr::filter(!arrival_time == '') %>%
    dplyr::mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time)
    ) %>%
    dplyr::group_by(trip_id) %>%
    dplyr::reframe(
      duration = arrival_seconds[dplyr::n()] - departure_seconds[1],
      .groups = "drop"
    )
}
