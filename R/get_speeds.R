#' Calculate Scheduled Speeds
#'
#' Combines distance in meters and duration in seconds to calculate kilometers
#' per hour.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by_trip"`, `"by_route"`, or `"detailed"`.
#'   Legacy dotted values remain accepted.
#' @param trips Character trip IDs or `"all"`.
#'
#' @return A tibble with `average.speed` for route and trip methods, or
#'   segment-level `speed` for the detailed method. `direction_id` is retained
#'   when available.
#'
#' @details
#' Detailed distances are straight-line geodesic distances between stops.
#' Route and trip distances follow `shapes.txt`, or inferred straight-line
#' shapes if the feed has none.
#'
#' @examples
#' get_speeds(for_rail_gtfs, "by_route")
#' get_speeds(for_rail_gtfs, "by_trip")
#'
#' @seealso [GTFSwizard::get_distances()], [GTFSwizard::get_durations()]
#' @export
get_speeds <- function(gtfs, method = "by_trip", trips = "all"){
  choices <- c("by_route", "by_trip", "detailed")
  method <- normalize_method(method, choices, "by_trip")
  if(!(length(trips) == 1L && identical(trips, "all"))){
    gtfs <- filter_trip(gtfs, trips)
  }
  gtfs <- ensure_shapes(ensure_wizardgtfs(gtfs))
  if(method == "by_route"){
    durations <- get_durations(gtfs, "by_route")
    distances <- get_distances(gtfs, "by_route")
    result <- dplyr::left_join(
      durations, distances,
      by = c(
        "route_id", direction_field(gtfs$trips),
        "service_pattern", "pattern_frequency"
      ),
      suffix = c(".duration", ".distance")
    ) |>
      dplyr::mutate(
        trips = trips.duration,
        average.speed = (average.distance / 1000) / (average.duration / 3600)
      )
    return(result |>
      dplyr::select(dplyr::all_of(c(
        "route_id", direction_field(result), "trips", "average.speed",
        "service_pattern", "pattern_frequency"
      ))))
  }
  if(method == "by_trip"){
    durations <- get_durations(gtfs, "by_trip")
    distances <- get_distances(gtfs, "by_trip")
    result <- dplyr::left_join(
      durations, distances,
      by = c(
        "route_id", "trip_id", direction_field(gtfs$trips),
        "service_pattern", "pattern_frequency"
      )
    ) |>
      dplyr::mutate(
        average.speed = (distance / 1000) / (duration / 3600)
      )
    return(result |>
      dplyr::select(dplyr::all_of(c(
        "route_id", "trip_id", direction_field(result), "average.speed",
        "service_pattern", "pattern_frequency"
      ))))
  }
  durations <- get_durations(gtfs, "detailed")
  distances <- get_distances(gtfs, "detailed") |>
    dplyr::select(from_stop_id, to_stop_id, distance) |>
    dplyr::distinct()
  dplyr::left_join(
    durations, distances, by = c("from_stop_id", "to_stop_id")
  ) |>
    dplyr::mutate(speed = (distance / 1000) / (duration / 3600)) |>
    dplyr::select(
      route_id, trip_id, dplyr::any_of("direction_id"),
      hour, from_stop_id, to_stop_id, speed,
      service_pattern, pattern_frequency
    )
}
