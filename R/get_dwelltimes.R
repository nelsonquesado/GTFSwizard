#' Calculate Dwell Times in GTFS Data
#'
#' The `get_dwelltimes` function calculates dwell times within a `wizardgtfs` object using different methods. Depending on the selected `method`, it can provide average dwell times per route, per trip, by hour, or detailed dwell times at each stop.
#'
#' @param gtfs A GTFS object, ideally of class `wizardgtfs`. If not, it will be converted.
#' @param max_dwelltime Numeric. Maximum allowable dwell time in seconds.
#' @param ... Supports the legacy argument `max.dwelltime`.
#' @param method A character string specifying the calculation method. Options include:
#'   \describe{
#'     \item{"by_hour"}{Calculates the average dwell time per hour of the day across all trips.}
#'     \item{"by_route"}{Calculates the average dwell time for each route.}
#'     \item{"by_trip"}{Calculates the average dwell time for each trip.}
#'     \item{"detailed"}{Calculates detailed dwell times at each stop within every trip.}
#'   }
#'
#' @return A data frame containing dwell times based on the specified method:
#'   \describe{
#'     \item{If `method = "by_hour"`}{Returns a data frame with columns: `hour`, `trips`, `average.dwelltime`, `service_pattern`, and `pattern_frequency`.}
#'     \item{If `method = "by_route"`}{Returns average dwell time by route, direction, and service pattern.}
#'     \item{If `method = "by_trip"`}{Returns average dwell time by trip, retaining direction when available.}
#'     \item{If `method = "detailed"`}{Returns a data frame with columns: `route_id`, `trip_id`, `stop_id`, `hour`, `dwell_time`, `service_pattern`, and `pattern_frequency`.}
#'   }
#'
#' @details
#' This function calls specific sub-functions based on the selected method:
#'
#' - "by_hour": Calculates the average dwell time for each hour of the day.
#'
#' - "by_route": Calculates average dwell times across each route.
#'
#' - "by_trip": Calculates the mean dwell time for each trip.
#'
#' - "detailed": Calculates departure minus arrival at each stop call.
#'
#' Legacy dotted values remain accepted. If an invalid `method` is specified,
#' the function defaults to `"by_trip"` and provides a warning.
#'
#' @examples
#' # Calculate dwell times by hour
#' dwelltimes_by_hour <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 120, method = "by_hour")
#'
#' # Calculate dwell times by route
#' dwelltimes_by_route <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 90, method = "by_route")
#'
#' # Calculate dwell times by trip
#' dwelltimes_by_trip <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 45, method = "by_trip")
#'
#' # Calculate detailed dwell times between stops
#' detailed_dwelltimes <- get_dwelltimes(gtfs = for_rail_gtfs, max_dwelltime = 60, method = "detailed")
#'
#' @seealso
#' [GTFSwizard::as_wizardgtfs()], [GTFSwizard::get_servicepattern()]
#'
#' @export
get_dwelltimes <- function(gtfs, max_dwelltime = 90, method = "by_trip", ...){
  resolved <- resolve_legacy_argument(
    max_dwelltime, missing(max_dwelltime), list(...),
    "max.dwelltime", "max_dwelltime"
  )
  max_dwelltime <- resolved$value
  gw_check_unused_dots(resolved$dots)
  if(!is.numeric(max_dwelltime) || length(max_dwelltime) != 1L ||
     is.na(max_dwelltime) || max_dwelltime < 0){
    gw_stop("`max_dwelltime` must be one non-negative number of seconds.")
  }

  method <- normalize_method(
    method,
    c("by_hour", "by_route", "by_trip", "detailed"),
    "by_trip"
  )

  switch(
    method,
    by_hour = get_dwelltime_byhour(gtfs, max_dwelltime),
    by_route = get_dwelltime_byroute(gtfs, max_dwelltime),
    by_trip = get_dwelltime_bytrip(gtfs, max_dwelltime),
    detailed = get_dwelltime_detailed(gtfs, max_dwelltime)
  )

}

get_dwelltime_byhour <- function(gtfs, max_dwelltime = 90){

  dwell_time_data(gtfs, max_dwelltime) %>%
    dplyr::group_by(hour, service_pattern, pattern_frequency) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      trips = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(hour, trips, average.dwelltime, service_pattern, pattern_frequency)

}

get_dwelltime_byroute <- function(gtfs, max_dwelltime = 90){
  data <- dwell_time_data(gtfs, max_dwelltime)
  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(
        "route_id", direction_field(data),
        "service_pattern", "pattern_frequency"
      )))
    ) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      trips = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::select(
      route_id, dplyr::any_of("direction_id"), trips,
      average.dwelltime, service_pattern, pattern_frequency
    )

}

get_dwelltime_bytrip <- function(gtfs, max_dwelltime = 90){
  data <- dwell_time_data(gtfs, max_dwelltime)
  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(
        "route_id", "trip_id",
        direction_field(data),
        "service_pattern", "pattern_frequency"
      )))
    ) %>%
    dplyr::reframe(
      average.dwelltime = mean(dwell_time),
      .groups = "drop"
    ) %>%
    dplyr::select(
      route_id, trip_id, dplyr::any_of("direction_id"),
      average.dwelltime, service_pattern, pattern_frequency
    )

}

get_dwelltime_detailed <- function(gtfs, max_dwelltime = 90){

  dwell_time_data(gtfs, max_dwelltime) %>%
    dplyr::select(
      route_id, trip_id, dplyr::any_of("direction_id"),
      stop_id, hour, dwell_time, service_pattern, pattern_frequency
    )

}

dwell_time_data <- function(gtfs, max_dwelltime){
  gtfs <- ensure_wizardgtfs(gtfs)
  service_pattern <- GTFSwizard::get_servicepattern(gtfs)

  gtfs$stop_times %>%
    dplyr::filter(!arrival_time == '' & !departure_time == "") %>%
    dplyr::mutate(
      arrival_seconds = gtfs_time_to_seconds(arrival_time),
      departure_seconds = gtfs_time_to_seconds(departure_time),
      hour = floor(arrival_seconds / 3600),
      dwell_time = departure_seconds - arrival_seconds
    ) %>%
    dplyr::filter(
      !is.na(dwell_time),
      dwell_time >= 0,
      dwell_time <= max_dwelltime
    ) %>%
    dplyr::left_join(gtfs$trips, by = "trip_id") %>%
    dplyr::left_join(
      service_pattern,
      by = "service_id",
      relationship = "many-to-many"
    )
}
