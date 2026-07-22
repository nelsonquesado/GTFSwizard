#' Calculate Scheduled Service Frequency
#'
#' Counts trip departures by route, shape, stop, or hour. Trips referenced by
#' `frequencies.txt` are expanded using the period's inclusive `start_time`,
#' exclusive `end_time`, and `headway_secs`, as required by GTFS.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by_trip"`, `"by_route"`, `"by_shape"`, `"by_stop"`,
#'   or `"detailed"`. Legacy dotted values remain accepted.
#'
#' @return A tibble whose observational unit depends on `method`:
#'   \describe{
#'     \item{`"by_trip"`}{`route_id`, `trip_id`, optional `direction_id`, `service_pattern`, `pattern_frequency`, and `daily.frequency`.}
#'     \item{`"by_route"`}{`route_id`, optional `direction_id`, `service_pattern`, `pattern_frequency`, and `daily.frequency`.}
#'     \item{`"by_shape"`}{`shape_id`, optional `direction_id`, `service_pattern`, `pattern_frequency`, and `daily.frequency`.}
#'     \item{`"by_stop"`}{`stop_id`, optional `direction_id`, `service_pattern`, `pattern_frequency`, and `daily.frequency`.}
#'     \item{`"detailed"`}{`route_id`, optional `direction_id`, departure `hour`, `service_pattern`, `pattern_frequency`, and `frequency`.}
#'   }
#'
#' @examples
#' get_frequency(for_rail_gtfs, "by_route")
#' get_frequency(for_rail_gtfs, "detailed")
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/#frequenciestxt)
#' @seealso [GTFSwizard::get_headways()]
#' @export
get_frequency <- function(gtfs, method = "by_trip"){
  choices <- c("by_trip", "by_route", "by_shape", "by_stop", "detailed")
  method <- normalize_method(method, choices, "by_trip")
  gtfs <- ensure_wizardgtfs(gtfs)
  instances <- trip_instance_starts(gtfs) |>
    dplyr::left_join(gtfs$trips, by = "trip_id") |>
    dplyr::left_join(
      get_servicepattern(gtfs), by = "service_id",
      relationship = "many-to-many"
    )
  direction <- direction_field(instances)

  if(method == "by_stop"){
    calls <- gtfs$stop_times |>
      dplyr::distinct(trip_id, stop_id)
    data <- dplyr::left_join(calls, instances, by = "trip_id")
    groups <- c("stop_id", direction, "service_pattern", "pattern_frequency")
    return(data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop"))
  }
  if(method == "by_shape"){
    if(!"shape_id" %in% names(instances)){
      gw_stop("`by_shape` requires `trips$shape_id`.")
    }
    groups <- c("shape_id", direction, "service_pattern", "pattern_frequency")
    return(instances |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop"))
  }
  if(method == "detailed"){
    instances$hour <- floor(instances$start_seconds / 3600)
    groups <- c(
      "route_id", direction, "hour", "service_pattern", "pattern_frequency"
    )
    return(instances |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(frequency = dplyr::n(), .groups = "drop"))
  }
  if(method == "by_trip"){
    groups <- c(
      "route_id", "trip_id", direction,
      "service_pattern", "pattern_frequency"
    )
    return(instances |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop"))
  }
  groups <- c("route_id", direction, "service_pattern", "pattern_frequency")
  instances |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(daily.frequency = dplyr::n(), .groups = "drop")
}
