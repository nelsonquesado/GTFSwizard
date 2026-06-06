#' Create a GTFS Feed from Tables
#'
#' Creates and validates a GTFS Schedule feed from data frames. The required
#' files and fields follow the current GTFS Schedule reference, including its
#' conditional rules for route names, stop locations, and service calendars.
#'
#' @param agency,routes,trips,stop_times,stops Required GTFS tables supplied as
#'   data frames. `agency_id` may be omitted when the feed contains one agency.
#' @param calendar,calendar_dates Service calendar tables. At least one must
#'   define every `service_id` used by `trips`.
#' @param shapes,frequencies,transfers,fare_attributes,fare_rules Optional GTFS
#'   tables.
#' @param build_shapes Logical. If `TRUE`, infer straight-line shapes when
#'   `shapes` is not supplied.
#' @param zipfile Optional output path ending in `.zip`.
#' @param ... Additional named GTFS tables, such as `pathways`, `levels`,
#'   `feed_info`, or GTFS Fares v2 tables.
#'
#' @return A validated `wizardgtfs` object, invisibly written as well when
#'   `zipfile` is supplied.
#'
#' @examples
#' feed <- create_gtfs(
#'   agency = data.frame(
#'     agency_id = "A", agency_name = "Demo Transit",
#'     agency_url = "https://example.com",
#'     agency_timezone = "America/Fortaleza"
#'   ),
#'   routes = data.frame(
#'     route_id = "R1", agency_id = "A", route_short_name = "1",
#'     route_long_name = "Central", route_type = 3
#'   ),
#'   trips = data.frame(route_id = "R1", service_id = "WK", trip_id = "T1"),
#'   stop_times = data.frame(
#'     trip_id = "T1", arrival_time = c("08:00:00", "08:10:00"),
#'     departure_time = c("08:00:00", "08:10:00"),
#'     stop_id = c("S1", "S2"), stop_sequence = c(1, 2)
#'   ),
#'   stops = data.frame(
#'     stop_id = c("S1", "S2"), stop_name = c("First", "Second"),
#'     stop_lat = c(-3.73, -3.74), stop_lon = c(-38.52, -38.53)
#'   ),
#'   calendar = data.frame(
#'     service_id = "WK", monday = 1, tuesday = 1, wednesday = 1,
#'     thursday = 1, friday = 1, saturday = 0, sunday = 0,
#'     start_date = "20260101", end_date = "20261231"
#'   )
#' )
#'
#' @references
#' [GTFS Schedule Reference](https://gtfs.org/documentation/schedule/reference/)
#' @export
create_gtfs <- function(agency,
                        routes,
                        trips,
                        stop_times,
                        stops,
                        calendar = NULL,
                        calendar_dates = NULL,
                        shapes = NULL,
                        frequencies = NULL,
                        transfers = NULL,
                        fare_attributes = NULL,
                        fare_rules = NULL,
                        build_shapes = TRUE,
                        zipfile = NULL,
                        ...){
  additional <- list(...)
  if(length(additional) && (is.null(names(additional)) || any(!nzchar(names(additional))))){
    gw_stop("additional GTFS tables supplied through `...` must be named.")
  }
  tables <- Filter(
    Negate(is.null),
    c(list(
      agency = agency, routes = routes, trips = trips,
      stop_times = stop_times, stops = stops, calendar = calendar,
      calendar_dates = calendar_dates, shapes = shapes,
      frequencies = frequencies, transfers = transfers,
      fare_attributes = fare_attributes, fare_rules = fare_rules
    ), additional)
  )

  gtfs <- GTFSwizard::as_wizardgtfs(tables, build_shapes = build_shapes)
  if(!is.null(zipfile)){
    GTFSwizard::write_gtfs(gtfs, zipfile)
  }
  gtfs
}
