#' Calculate Scheduled Headways
#'
#' Calculates elapsed minutes between successive service instances. Frequency
#' based trips are expanded according to `frequencies.txt`.
#'
#' @param gtfs A GTFS object.
#' @param method One of `"by_trip"`, `"by_route"`, `"by_hour"`, `"by_stop"`,
#'   `"by_shape"`, or `"detailed"`. Legacy dotted values remain accepted.
#'
#' @return A tibble. Headway values are always returned in
#'   `headway_minutes`; `valid_trips` is the number of intervals summarized.
#'
#' @details
#' Route, hour, and trip methods compare first departures. Stop and detailed
#' methods compare arrivals at the same stop, route, direction, and service
#' pattern. The first service instance in each group has no headway and is
#' omitted.
#'
#' @examples
#' get_headways(for_rail_gtfs, "by_route")
#' get_headways(for_rail_gtfs, "by_hour")
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/#frequenciestxt)
#' @seealso [GTFSwizard::get_frequency()]
#' @export
get_headways <- function(gtfs, method = "by_trip"){
  choices <- c("by_route", "by_hour", "by_trip", "by_stop", "by_shape", "detailed")
  method <- normalize_method(method, choices, "by_trip")
  gtfs <- ensure_wizardgtfs(gtfs)
  patterns <- get_servicepattern(gtfs)
  trips <- gtfs$trips
  direction <- direction_field(trips)

  if(method %in% c("by_stop", "detailed")){
    data <- stop_call_instances(gtfs) |>
      dplyr::left_join(trips, by = "trip_id") |>
      dplyr::left_join(patterns, by = "service_id")
    grouping <- c(
      "route_id", direction, "stop_id", "service_pattern", "pattern_frequency"
    )
    data <- calculate_headway_rows(data, grouping, "time_seconds")
    data$hour <- floor(data$time_seconds / 3600)
    if(method == "detailed"){
      fields <- c(
        "route_id", "trip_id", direction, "stop_id", "hour",
        "headway_minutes", "service_pattern", "pattern_frequency"
      )
      return(data[, fields, drop = FALSE])
    }
    groups <- c("stop_id", direction, "service_pattern", "pattern_frequency")
    return(data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::summarise(
        headway_minutes = mean(headway_minutes),
        valid_trips = dplyr::n(),
        .groups = "drop"
      ))
  }

  data <- trip_instance_starts(gtfs) |>
    dplyr::left_join(trips, by = "trip_id") |>
    dplyr::left_join(patterns, by = "service_id")
  grouping <- c(
    "route_id", direction, "service_pattern", "pattern_frequency"
  )
  data <- calculate_headway_rows(data, grouping, "start_seconds")
  data$hour <- floor(data$start_seconds / 3600)

  if(method == "by_trip"){
    fields <- c(
      "route_id", "trip_id", direction, "headway_minutes",
      "service_pattern", "pattern_frequency"
    )
    return(data[, fields, drop = FALSE])
  }
  if(method == "by_shape"){
    if(!"shape_id" %in% names(data)){
      gw_stop("`by_shape` requires `trips$shape_id`.")
    }
    groups <- c("shape_id", direction, "service_pattern", "pattern_frequency")
  } else if(method == "by_hour"){
    groups <- c("hour", "service_pattern", "pattern_frequency")
  } else {
    groups <- c("route_id", direction, "service_pattern", "pattern_frequency")
  }
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(
      headway_minutes = mean(headway_minutes),
      valid_trips = dplyr::n(),
      .groups = "drop"
    )
}

calculate_headway_rows <- function(data, grouping, time_field){
  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(grouping, time_field)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping))) |>
    dplyr::mutate(
      headway_minutes = (
        .data[[time_field]] - dplyr::lag(.data[[time_field]])
      ) / 60
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(headway_minutes), headway_minutes >= 0)
}
